{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationResultSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.StackSetOperationResultSummary
  ( StackSetOperationResultSummary (..)
  -- * Smart constructor
  , mkStackSetOperationResultSummary
  -- * Lenses
  , ssorsAccount
  , ssorsAccountGateResult
  , ssorsOrganizationalUnitId
  , ssorsRegion
  , ssorsStatus
  , ssorsStatusReason
  ) where

import qualified Network.AWS.CloudFormation.Types.Account as Types
import qualified Network.AWS.CloudFormation.Types.AccountGateResult as Types
import qualified Network.AWS.CloudFormation.Types.OrganizationalUnitId as Types
import qualified Network.AWS.CloudFormation.Types.Region as Types
import qualified Network.AWS.CloudFormation.Types.StackSetOperationResultStatus as Types
import qualified Network.AWS.CloudFormation.Types.StatusReason as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The structure that contains information about a specified operation's results for a given account in a given Region.
--
-- /See:/ 'mkStackSetOperationResultSummary' smart constructor.
data StackSetOperationResultSummary = StackSetOperationResultSummary'
  { account :: Core.Maybe Types.Account
    -- ^ [@Self-managed@ permissions] The name of the AWS account for this operation result.
  , accountGateResult :: Core.Maybe Types.AccountGateResult
    -- ^ The results of the account gate function AWS CloudFormation invokes, if present, before proceeding with stack set operations in an account
  , organizationalUnitId :: Core.Maybe Types.OrganizationalUnitId
    -- ^ [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
  , region :: Core.Maybe Types.Region
    -- ^ The name of the AWS Region for this operation result.
  , status :: Core.Maybe Types.StackSetOperationResultStatus
    -- ^ The result status of the stack set operation for the given account in the given Region.
--
--
--     * @CANCELLED@ : The operation in the specified account and Region has been cancelled. This is either because a user has stopped the stack set operation, or because the failure tolerance of the stack set operation has been exceeded.
--
--
--     * @FAILED@ : The operation in the specified account and Region failed. 
-- If the stack set operation fails in enough accounts within a Region, the failure tolerance for the stack set operation as a whole might be exceeded. 
--
--
--     * @RUNNING@ : The operation in the specified account and Region is currently in progress.
--
--
--     * @PENDING@ : The operation in the specified account and Region has yet to start. 
--
--
--     * @SUCCEEDED@ : The operation in the specified account and Region completed successfully.
--
--
  , statusReason :: Core.Maybe Types.StatusReason
    -- ^ The reason for the assigned result status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StackSetOperationResultSummary' value with any optional fields omitted.
mkStackSetOperationResultSummary
    :: StackSetOperationResultSummary
mkStackSetOperationResultSummary
  = StackSetOperationResultSummary'{account = Core.Nothing,
                                    accountGateResult = Core.Nothing,
                                    organizationalUnitId = Core.Nothing, region = Core.Nothing,
                                    status = Core.Nothing, statusReason = Core.Nothing}

-- | [@Self-managed@ permissions] The name of the AWS account for this operation result.
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssorsAccount :: Lens.Lens' StackSetOperationResultSummary (Core.Maybe Types.Account)
ssorsAccount = Lens.field @"account"
{-# INLINEABLE ssorsAccount #-}
{-# DEPRECATED account "Use generic-lens or generic-optics with 'account' instead"  #-}

-- | The results of the account gate function AWS CloudFormation invokes, if present, before proceeding with stack set operations in an account
--
-- /Note:/ Consider using 'accountGateResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssorsAccountGateResult :: Lens.Lens' StackSetOperationResultSummary (Core.Maybe Types.AccountGateResult)
ssorsAccountGateResult = Lens.field @"accountGateResult"
{-# INLINEABLE ssorsAccountGateResult #-}
{-# DEPRECATED accountGateResult "Use generic-lens or generic-optics with 'accountGateResult' instead"  #-}

-- | [@Service-managed@ permissions] The organization root ID or organizational unit (OU) IDs that you specified for <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets> .
--
-- /Note:/ Consider using 'organizationalUnitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssorsOrganizationalUnitId :: Lens.Lens' StackSetOperationResultSummary (Core.Maybe Types.OrganizationalUnitId)
ssorsOrganizationalUnitId = Lens.field @"organizationalUnitId"
{-# INLINEABLE ssorsOrganizationalUnitId #-}
{-# DEPRECATED organizationalUnitId "Use generic-lens or generic-optics with 'organizationalUnitId' instead"  #-}

-- | The name of the AWS Region for this operation result.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssorsRegion :: Lens.Lens' StackSetOperationResultSummary (Core.Maybe Types.Region)
ssorsRegion = Lens.field @"region"
{-# INLINEABLE ssorsRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | The result status of the stack set operation for the given account in the given Region.
--
--
--     * @CANCELLED@ : The operation in the specified account and Region has been cancelled. This is either because a user has stopped the stack set operation, or because the failure tolerance of the stack set operation has been exceeded.
--
--
--     * @FAILED@ : The operation in the specified account and Region failed. 
-- If the stack set operation fails in enough accounts within a Region, the failure tolerance for the stack set operation as a whole might be exceeded. 
--
--
--     * @RUNNING@ : The operation in the specified account and Region is currently in progress.
--
--
--     * @PENDING@ : The operation in the specified account and Region has yet to start. 
--
--
--     * @SUCCEEDED@ : The operation in the specified account and Region completed successfully.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssorsStatus :: Lens.Lens' StackSetOperationResultSummary (Core.Maybe Types.StackSetOperationResultStatus)
ssorsStatus = Lens.field @"status"
{-# INLINEABLE ssorsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The reason for the assigned result status.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssorsStatusReason :: Lens.Lens' StackSetOperationResultSummary (Core.Maybe Types.StatusReason)
ssorsStatusReason = Lens.field @"statusReason"
{-# INLINEABLE ssorsStatusReason #-}
{-# DEPRECATED statusReason "Use generic-lens or generic-optics with 'statusReason' instead"  #-}

instance Core.FromXML StackSetOperationResultSummary where
        parseXML x
          = StackSetOperationResultSummary' Core.<$>
              (x Core..@? "Account") Core.<*> x Core..@? "AccountGateResult"
                Core.<*> x Core..@? "OrganizationalUnitId"
                Core.<*> x Core..@? "Region"
                Core.<*> x Core..@? "Status"
                Core.<*> x Core..@? "StatusReason"
