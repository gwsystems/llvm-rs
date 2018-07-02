use libc::{c_char, c_uint};
use ffi::prelude::{LLVMBasicBlockRef, LLVMBuilderRef, LLVMValueRef};
use ffi::{core, LLVMBuilder, LLVMRealPredicate, LLVMIntPredicate};
use cbox::CSemiBox;
use std::marker::PhantomData;
use std::mem;
use block::BasicBlock;
use context::Context;
use types::Type;
use value::{Function, Value, Predicate};

static NULL_NAME:[c_char; 1] = [0];


/// This provides a uniform API for creating instructions and inserting them into a basic block.
pub struct Builder(PhantomData<[u8]>);
native_ref!(&Builder = LLVMBuilderRef);
dispose!{Builder, LLVMBuilder, core::LLVMDisposeBuilder}
macro_rules! bin_op(
    ($name:ident, $func:ident) => (
        pub fn $name(&self, left: &Value, right: &Value) -> &Value {
            unsafe { core::$func(self.into(), left.into(), right.into(), NULL_NAME.as_ptr()) }.into()
        }
    );
    ($name:ident, $ifunc:ident, $ffunc:ident) => (
        pub fn $name(&self, left: &Value, right: &Value) -> &Value {
            let ty = left.get_type();
            unsafe {
                (if ty.is_integer() {
                    core::$ifunc
                } else {
                    core::$ffunc
                })(self.into(), left.into(), right.into(), NULL_NAME.as_ptr()).into()
            }
        }
    );
);
macro_rules! un_op(
    ($name:ident, $func:ident) => (
        pub fn $name(&self, value: &Value) -> &Value {
            unsafe { core::$func(self.into(), value.into(), NULL_NAME.as_ptr() as *const c_char) }.into()
        }
    );
);
impl Builder {
    /// Create a new builder in the context given.
    pub fn new(context: &Context) -> CSemiBox<Builder> {
        CSemiBox::new(unsafe { core::LLVMCreateBuilderInContext(context.into()) }.into())
    }
    /// Position the builder at the end of `block`.
    pub fn position_at_end(&self, block: &BasicBlock) {
        unsafe { core::LLVMPositionBuilderAtEnd(self.into(), block.into()) }
    }
    /// Build a PHI instruction with no useful values
    pub fn build_phi(&self, incoming: Vec<(&BasicBlock, &Value)>) -> &Value{
        let count = incoming.len() as u32;
        assert!(count > 0);
        let ty = incoming[0].1.get_type();

        let mut incoming_values: Vec<LLVMValueRef> = Vec::with_capacity(incoming.len());
        let mut incoming_blocks: Vec<LLVMBasicBlockRef> = Vec::with_capacity(incoming.len());
        for (bb, v) in incoming {
            incoming_values.push(v.into());
            incoming_blocks.push(bb.into());
        }
        unsafe {
            let phi = core::LLVMBuildPhi(self.into(), ty.into(), NULL_NAME.as_ptr());
            core::LLVMAddIncoming(phi, incoming_values.as_mut_ptr(), incoming_blocks.as_mut_ptr(), count);
            phi
        }.into()
    }
    /// Build an instruction that returns from the function with void.
    pub fn build_ret_void(&self) -> &Value {
        unsafe { core::LLVMBuildRetVoid(self.into()) }.into()
    }
    /// Build an instruction that returns from the function with `value`.
    pub fn build_ret(&self, value: &Value) -> &Value {
        unsafe { core::LLVMBuildRet(self.into(), value.into()) }.into()
    }
    /// Build an instruction that allocates an array with the element type `elem` and the size `size`.
    ///
    /// The size of this array will be the size of `elem` times `size`.
    pub fn build_array_alloca(&self, elem: &Type, size: &Value) -> &Value {
        unsafe { core::LLVMBuildArrayAlloca(self.into(), elem.into(), size.into(), NULL_NAME.as_ptr() as *const c_char) }.into()
    }
    /// Build an instruction that allocates a pointer to fit the size of `ty` then returns this pointer.
    pub fn build_alloca(&self, ty: &Type) -> &Value {
        unsafe { core::LLVMBuildAlloca(self.into(), ty.into(), NULL_NAME.as_ptr() as *const c_char) }.into()
    }
    /// Build an instruction that frees the `val`, which _MUST_ be a pointer that was returned
    /// from `build_alloca`.
    pub fn build_free(&self, val: &Value) -> &Value {
        unsafe { core::LLVMBuildFree(self.into(), val.into()) }.into()
    }

    /// Build an unreachable instruction
    pub fn build_unreachable(&self) -> &Value {
        unsafe { core::LLVMBuildUnreachable(self.into()).into() }
    }
    /// Build an instruction that store the value `val` in the pointer `ptr`.
    pub fn build_store(&self, val: &Value, ptr: &Value) -> &Value {
        unsafe { core::LLVMBuildStore(self.into(), val.into(), ptr.into()) }.into()
    }
    /// Build an instruction that branches to the block `dest`.
    pub fn build_br(&self, dest: &BasicBlock) -> &Value {
        unsafe { core::LLVMBuildBr(self.into(), dest.into()).into() }
    }
    /// Build an instruction that branches to `if_block` if `cond` evaluates to true, and `else_block` otherwise.
    pub fn build_cond_br(&self, cond: &Value, if_block: &BasicBlock, else_block: Option<&BasicBlock>) -> &Value {
        unsafe { core::LLVMBuildCondBr(self.into(), cond.into(), if_block.into(), mem::transmute(else_block)).into() }
    }
    /// Build an instruction that calls the function `func` with the arguments `args`.
    ///
    /// This will return the return value of the function.
    pub fn build_call(&self, func: &Function, args: &[&Value]) -> &Value {
        unsafe {
            let call = core::LLVMBuildCall(self.into(), func.into(), args.as_ptr() as *mut LLVMValueRef, args.len() as c_uint, NULL_NAME.as_ptr());
            core::LLVMSetTailCall(call, 0);
            call.into()
        }
    }

    pub fn build_value_call(&self, func: &Value, args: &[&Value]) -> &Value {
        unsafe {
            let call = core::LLVMBuildCall(self.into(), func.into(), args.as_ptr() as *mut LLVMValueRef, args.len() as c_uint, NULL_NAME.as_ptr());
            core::LLVMSetTailCall(call, 0);
            call.into()
        }
    }


    /// Build an instruction that calls the function `func` with the arguments `args`.
    ///
    /// This will return the return value of the function.
    pub fn build_tail_call(&self, func: &Function, args: &[&Value]) -> &Value {
        unsafe {
            let call = core::LLVMBuildCall(self.into(), func.into(), args.as_ptr() as *mut LLVMValueRef, args.len() as c_uint, NULL_NAME.as_ptr());
            core::LLVMSetTailCall(call, 1);
            call.into()
        }
    }
    /// Build an instruction that yields to `true_val` if `cond` is equal to `1`, and `false_val` otherwise.
    pub fn build_select(&self, cond: &Value, true_val: &Value, false_val: &Value) -> &Value {
        unsafe { core::LLVMBuildSelect(self.into(), cond.into(), true_val.into(), false_val.into(), NULL_NAME.as_ptr()).into() }
    }
    /// Build an instruction that casts a value into a certain type.
    pub fn build_bit_cast(&self, value: &Value, dest: &Type) -> &Value {
        unsafe { core::LLVMBuildBitCast(self.into(), value.into(), dest.into(), NULL_NAME.as_ptr()).into() }
    }
    /// Build an instruction to bitcast in integer into a pointer.
    pub fn build_int_to_ptr(&self, value: &Value, dest: &Type) -> &Value {
        unsafe { core::LLVMBuildIntToPtr(self.into(), value.into(), dest.into(), NULL_NAME.as_ptr()).into() }
    }
    /// Build an instruction that zero extends its operand to the type `dest`.
    pub fn build_zext(&self, value: &Value, dest: &Type) -> &Value {
        unsafe { core::LLVMBuildZExtOrBitCast(self.into(), value.into(), dest.into(), NULL_NAME.as_ptr()).into() }
    }

    /// Build an instruction that sign extends its operand to the type `dest`.
    pub fn build_sext(&self, value: &Value, dest: &Type) -> &Value {
        unsafe { core::LLVMBuildSExtOrBitCast(self.into(), value.into(), dest.into(), NULL_NAME.as_ptr()).into() }
    }

    /// Build an instruction that truncates the high-order bits of value to fit into a certain type.
    pub fn build_trunc(&self, value: &Value, dest: &Type) -> &Value {
        unsafe { core::LLVMBuildTrunc(self.into(), value.into(), dest.into(), NULL_NAME.as_ptr()).into() }
    }
    /// Build an instruction that converts a floating point value to an signed int type
    pub fn build_fptosi(&self, value: &Value, dest: &Type) -> &Value {
        unsafe { core::LLVMBuildFPToSI(self.into(), value.into(), dest.into(), NULL_NAME.as_ptr()).into() }
    }
    /// Build an instruction that converts a floating point value to an unsigned int type
    pub fn build_fptoui(&self, value: &Value, dest: &Type) -> &Value {
        unsafe { core::LLVMBuildFPToUI(self.into(), value.into(), dest.into(), NULL_NAME.as_ptr()).into() }
    }
    /// Build an instruction that converts a signed int to an floating point type
    pub fn build_sitofp(&self, value: &Value, dest: &Type) -> &Value {
        unsafe { core::LLVMBuildSIToFP(self.into(), value.into(), dest.into(), NULL_NAME.as_ptr()).into() }
    }
    /// Build an instruction that converts a unsigned int to an floating point type
    pub fn build_uitofp(&self, value: &Value, dest: &Type) -> &Value {
        unsafe { core::LLVMBuildUIToFP(self.into(), value.into(), dest.into(), NULL_NAME.as_ptr()).into() }
    }

    pub fn build_fpext(&self, value: &Value, dest: &Type) -> &Value {
        unsafe { core::LLVMBuildFPExt(self.into(), value.into(), dest.into(), NULL_NAME.as_ptr()).into() }
    }

    pub fn build_fptrunc(&self, value: &Value, dest: &Type) -> &Value {
        unsafe { core::LLVMBuildFPTrunc(self.into(), value.into(), dest.into(), NULL_NAME.as_ptr()).into() }
    }


    /// Build an instruction that inserts a value into an aggregate data value.
    pub fn build_insert_value(&self, agg: &Value, elem: &Value, index: usize) -> &Value {
        unsafe { core::LLVMBuildInsertValue(self.into(), agg.into(), elem.into(), index as c_uint, NULL_NAME.as_ptr()).into() }
    }
    /// Build an instruction that extracts a value from an aggregate data value.
    pub fn build_extract_value(&self, agg: &Value, index: usize) -> &Value {
        unsafe { core::LLVMBuildExtractValue(self.into(), agg.into(), index as c_uint, NULL_NAME.as_ptr()).into() }
    }
    /// Build an instruction that computes the address of a subelement of an aggregate data structure.
    ///
    /// Basically type-safe pointer arithmetic.
    pub fn build_gep(&self, pointer: &Value, indices: &[&Value]) -> &Value {
        unsafe { core::LLVMBuildInBoundsGEP(self.into(), pointer.into(), indices.as_ptr() as *mut LLVMValueRef, indices.len() as c_uint, NULL_NAME.as_ptr()).into() }
    }
    /// Build an instruction that runs whichever block matches the value, or `default` if none of them matched it.
    pub fn build_switch(&self, value: &Value, default: &BasicBlock, cases: &[(&Value, &BasicBlock)]) -> &Value {
        unsafe {
            let switch = core::LLVMBuildSwitch(self.into(), value.into(), default.into(), cases.len() as c_uint);
            for case in cases {
                core::LLVMAddCase(switch, case.0.into(), case.1.into());
            }
            switch.into()
        }
    }

    un_op!{build_load, LLVMBuildLoad}
    un_op!{build_neg, LLVMBuildNeg}
    un_op!{build_fneg, LLVMBuildFNeg}
    un_op!{build_not, LLVMBuildNot}
    bin_op!{build_add, LLVMBuildAdd, LLVMBuildFAdd}
    bin_op!{build_sub, LLVMBuildSub, LLVMBuildFSub}
    bin_op!{build_mul, LLVMBuildMul, LLVMBuildFMul}
    bin_op!{build_div, LLVMBuildSDiv, LLVMBuildFDiv}
    bin_op!{build_udiv, LLVMBuildUDiv}
    bin_op!{build_urem, LLVMBuildURem}
    bin_op!{build_srem, LLVMBuildSRem}
    bin_op!{build_shl, LLVMBuildShl}
    bin_op!{build_ashr, LLVMBuildAShr}
    bin_op!{build_lshr, LLVMBuildLShr}
    bin_op!{build_and, LLVMBuildAnd}
    bin_op!{build_or, LLVMBuildOr}
    bin_op!{build_xor, LLVMBuildXor}
    /// Build an instruction to compare the values `a` and `b` with the predicate / comparative operator `pred`.
    pub fn build_signed_cmp(&self, a: &Value, b: &Value, pred: Predicate) -> &Value {
        let (at, bt) = (a.get_type(), b.get_type());
        assert_eq!(at, bt);
        if at.is_integer() {
            let pred = match pred {
                Predicate::Equal => LLVMIntPredicate::LLVMIntEQ,
                Predicate::NotEqual => LLVMIntPredicate::LLVMIntNE,
                Predicate::GreaterThan => LLVMIntPredicate::LLVMIntSGT,
                Predicate::GreaterThanOrEqual => LLVMIntPredicate::LLVMIntSGE,
                Predicate::LessThan => LLVMIntPredicate::LLVMIntSLT,
                Predicate::LessThanOrEqual => LLVMIntPredicate::LLVMIntSLE
            };
            unsafe { core::LLVMBuildICmp(self.into(), pred, a.into(), b.into(), NULL_NAME.as_ptr()) }.into()
        } else if at.is_float() {
            let pred = match pred {
                Predicate::Equal => LLVMRealPredicate::LLVMRealOEQ,
                Predicate::NotEqual => LLVMRealPredicate::LLVMRealONE,
                Predicate::GreaterThan => LLVMRealPredicate::LLVMRealOGT,
                Predicate::GreaterThanOrEqual => LLVMRealPredicate::LLVMRealOGE,
                Predicate::LessThan => LLVMRealPredicate::LLVMRealOLT,
                Predicate::LessThanOrEqual => LLVMRealPredicate::LLVMRealOLE
            };
            unsafe { core::LLVMBuildFCmp(self.into(), pred, a.into(), b.into(), NULL_NAME.as_ptr()) }.into()
        } else {
            panic!("expected numzextbers, got {:?}", at)
        }
    }

    pub fn build_unsigned_cmp(&self, a: &Value, b: &Value, pred: Predicate) -> &Value {
        let (at, bt) = (a.get_type(), b.get_type());
        assert_eq!(at, bt);
        assert!(at.is_integer());


        let pred = match pred {
            Predicate::Equal => LLVMIntPredicate::LLVMIntEQ,
            Predicate::NotEqual => LLVMIntPredicate::LLVMIntNE,
            Predicate::GreaterThan => LLVMIntPredicate::LLVMIntUGT,
            Predicate::GreaterThanOrEqual => LLVMIntPredicate::LLVMIntUGE,
            Predicate::LessThan => LLVMIntPredicate::LLVMIntULT,
            Predicate::LessThanOrEqual => LLVMIntPredicate::LLVMIntULE
        };
        unsafe { core::LLVMBuildICmp(self.into(), pred, a.into(), b.into(), NULL_NAME.as_ptr()) }.into()
    }

}