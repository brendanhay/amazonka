{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.AccountGateResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.AccountGateResult
  ( AccountGateResult (..)
  -- * Smart constructor
  , mkAccountGateResult
  -- * Lenses
  , agrStatus
  , agrStatusReason
  ) where

import qualified Network.AWS.CloudFormation.Types.AccountGateStatus as Types
import qualified Network.AWS.CloudFormation.Types.StatusReason as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Structure that contains the results of the account gate function which AWS CloudFormation invokes, if present, before proceeding with a stack set operation in an account and Region.
--
-- For each account and Region, AWS CloudFormation lets you specify a Lamdba function that encapsulates any requirements that must be met before CloudFormation can proceed with a stack set operation in that account and Region. CloudFormation invokes the function each time a stack set operation is requested for that account and Region; if the function returns @FAILED@ , CloudFormation cancels the operation in that account and Region, and sets the stack set operation result status for that account and Region to @FAILED@ . 
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-account-gating.html Configuring a target account gate> .
--
-- /See:/ 'mkAccountGateResult' smart constructor.
data AccountGateResult = AccountGateResult'
  { status :: Core.Maybe Types.AccountGateStatus
    -- ^ The status of the account gate function.
--
--
--     * @SUCCEEDED@ : The account gate function has determined that the account and Region passes any requirements for a stack set operation to occur. AWS CloudFormation proceeds with the stack operation in that account and Region. 
--
--
--     * @FAILED@ : The account gate function has determined that the account and Region does not meet the requirements for a stack set operation to occur. AWS CloudFormation cancels the stack set operation in that account and Region, and sets the stack set operation result status for that account and Region to @FAILED@ . 
--
--
--     * @SKIPPED@ : AWS CloudFormation has skipped calling the account gate function for this account and Region, for one of the following reasons:
--
--     * An account gate function has not been specified for the account and Region. AWS CloudFormation proceeds with the stack set operation in this account and Region.
--
--
--     * The @AWSCloudFormationStackSetExecutionRole@ of the stack set adminstration account lacks permissions to invoke the function. AWS CloudFormation proceeds with the stack set operation in this account and Region.
--
--
--     * Either no action is necessary, or no action is possible, on the stack. AWS CloudFormation skips the stack set operation in this account and Region.
--
--
--
--
  , statusReason :: Core.Maybe Types.StatusReason
    -- ^ The reason for the account gate status assigned to this account and Region for the stack set operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccountGateResult' value with any optional fields omitted.
mkAccountGateResult
    :: AccountGateResult
mkAccountGateResult
  = AccountGateResult'{status = Core.Nothing,
                       statusReason = Core.Nothing}

-- | The status of the account gate function.
--
--
--     * @SUCCEEDED@ : The account gate function has determined that the account and Region passes any requirements for a stack set operation to occur. AWS CloudFormation proceeds with the stack operation in that account and Region. 
--
--
--     * @FAILED@ : The account gate function has determined that the account and Region does not meet the requirements for a stack set operation to occur. AWS CloudFormation cancels the stack set operation in that account and Region, and sets the stack set operation result status for that account and Region to @FAILED@ . 
--
--
--     * @SKIPPED@ : AWS CloudFormation has skipped calling the account gate function for this account and Region, for one of the following reasons:
--
--     * An account gate function has not been specified for the account and Region. AWS CloudFormation proceeds with the stack set operation in this account and Region.
--
--
--     * The @AWSCloudFormationStackSetExecutionRole@ of the stack set adminstration account lacks permissions to invoke the function. AWS CloudFormation proceeds with the stack set operation in this account and Region.
--
--
--     * Either no action is necessary, or no action is possible, on the stack. AWS CloudFormation skips the stack set operation in this account and Region.
--
--
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agrStatus :: Lens.Lens' AccountGateResult (Core.Maybe Types.AccountGateStatus)
agrStatus = Lens.field @"status"
{-# INLINEABLE agrStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The reason for the account gate status assigned to this account and Region for the stack set operation.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
agrStatusReason :: Lens.Lens' AccountGateResult (Core.Maybe Types.StatusReason)
agrStatusReason = Lens.field @"statusReason"
{-# INLINEABLE agrStatusReason #-}
{-# DEPRECATED statusReason "Use generic-lens or generic-optics with 'statusReason' instead"  #-}

instance Core.FromXML AccountGateResult where
        parseXML x
          = AccountGateResult' Core.<$>
              (x Core..@? "Status") Core.<*> x Core..@? "StatusReason"
