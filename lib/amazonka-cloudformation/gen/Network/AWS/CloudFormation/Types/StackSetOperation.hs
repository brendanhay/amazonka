{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.StackSetOperation
  ( StackSetOperation (..)
  -- * Smart constructor
  , mkStackSetOperation
  -- * Lenses
  , ssoAction
  , ssoAdministrationRoleARN
  , ssoCreationTimestamp
  , ssoDeploymentTargets
  , ssoEndTimestamp
  , ssoExecutionRoleName
  , ssoOperationId
  , ssoOperationPreferences
  , ssoRetainStacks
  , ssoStackSetDriftDetectionDetails
  , ssoStackSetId
  , ssoStatus
  ) where

import qualified Network.AWS.CloudFormation.Types.AdministrationRoleARN as Types
import qualified Network.AWS.CloudFormation.Types.DeploymentTargets as Types
import qualified Network.AWS.CloudFormation.Types.ExecutionRoleName as Types
import qualified Network.AWS.CloudFormation.Types.OperationId as Types
import qualified Network.AWS.CloudFormation.Types.StackSetDriftDetectionDetails as Types
import qualified Network.AWS.CloudFormation.Types.StackSetId as Types
import qualified Network.AWS.CloudFormation.Types.StackSetOperationAction as Types
import qualified Network.AWS.CloudFormation.Types.StackSetOperationPreferences as Types
import qualified Network.AWS.CloudFormation.Types.StackSetOperationStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The structure that contains information about a stack set operation. 
--
-- /See:/ 'mkStackSetOperation' smart constructor.
data StackSetOperation = StackSetOperation'
  { action :: Core.Maybe Types.StackSetOperationAction
    -- ^ The type of stack set operation: @CREATE@ , @UPDATE@ , or @DELETE@ . Create and delete operations affect only the specified stack set instances that are associated with the specified stack set. Update operations affect both the stack set itself, as well as /all/ associated stack set instances.
  , administrationRoleARN :: Core.Maybe Types.AdministrationRoleARN
    -- ^ The Amazon Resource Number (ARN) of the IAM role used to perform this stack set operation. 
--
-- Use customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Define Permissions for Multiple Administrators> in the /AWS CloudFormation User Guide/ .
  , creationTimestamp :: Core.Maybe Core.UTCTime
    -- ^ The time at which the operation was initiated. Note that the creation times for the stack set operation might differ from the creation time of the individual stacks themselves. This is because AWS CloudFormation needs to perform preparatory work for the operation, such as dispatching the work to the requested Regions, before actually creating the first stacks.
  , deploymentTargets :: Core.Maybe Types.DeploymentTargets
    -- ^ [@Service-managed@ permissions] The AWS Organizations accounts affected by the stack operation.
  , endTimestamp :: Core.Maybe Core.UTCTime
    -- ^ The time at which the stack set operation ended, across all accounts and Regions specified. Note that this doesn't necessarily mean that the stack set operation was successful, or even attempted, in each account or Region.
  , executionRoleName :: Core.Maybe Types.ExecutionRoleName
    -- ^ The name of the IAM execution role used to create or update the stack set.
--
-- Use customized execution roles to control which stack resources users and groups can include in their stack sets. 
  , operationId :: Core.Maybe Types.OperationId
    -- ^ The unique ID of a stack set operation.
  , operationPreferences :: Core.Maybe Types.StackSetOperationPreferences
    -- ^ The preferences for how AWS CloudFormation performs this stack set operation.
  , retainStacks :: Core.Maybe Core.Bool
    -- ^ For stack set operations of action type @DELETE@ , specifies whether to remove the stack instances from the specified stack set, but doesn't delete the stacks. You can't reassociate a retained stack, or add an existing, saved stack to a new stack set.
  , stackSetDriftDetectionDetails :: Core.Maybe Types.StackSetDriftDetectionDetails
    -- ^ Detailed information about the drift status of the stack set. This includes information about drift operations currently being performed on the stack set.
--
-- this information will only be present for stack set operations whose @Action@ type is @DETECT_DRIFT@ .
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html Detecting Unmanaged Changes in Stack Sets> in the AWS CloudFormation User Guide.
  , stackSetId :: Core.Maybe Types.StackSetId
    -- ^ The ID of the stack set.
  , status :: Core.Maybe Types.StackSetOperationStatus
    -- ^ The status of the operation. 
--
--
--     * @FAILED@ : The operation exceeded the specified failure tolerance. The failure tolerance value that you've set for an operation is applied for each Region during stack create and update operations. If the number of failed stacks within a Region exceeds the failure tolerance, the status of the operation in the Region is set to @FAILED@ . This in turn sets the status of the operation as a whole to @FAILED@ , and AWS CloudFormation cancels the operation in any remaining Regions.
--
--
--     * @QUEUED@ : [@Service-managed@ permissions] For automatic deployments that require a sequence of operations, the operation is queued to be performed. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-status-codes stack set operation status codes> in the AWS CloudFormation User Guide.
--
--
--     * @RUNNING@ : The operation is currently being performed.
--
--
--     * @STOPPED@ : The user has cancelled the operation.
--
--
--     * @STOPPING@ : The operation is in the process of stopping, at user request. 
--
--
--     * @SUCCEEDED@ : The operation completed creating or updating all the specified stacks without exceeding the failure tolerance for the operation.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StackSetOperation' value with any optional fields omitted.
mkStackSetOperation
    :: StackSetOperation
mkStackSetOperation
  = StackSetOperation'{action = Core.Nothing,
                       administrationRoleARN = Core.Nothing,
                       creationTimestamp = Core.Nothing, deploymentTargets = Core.Nothing,
                       endTimestamp = Core.Nothing, executionRoleName = Core.Nothing,
                       operationId = Core.Nothing, operationPreferences = Core.Nothing,
                       retainStacks = Core.Nothing,
                       stackSetDriftDetectionDetails = Core.Nothing,
                       stackSetId = Core.Nothing, status = Core.Nothing}

-- | The type of stack set operation: @CREATE@ , @UPDATE@ , or @DELETE@ . Create and delete operations affect only the specified stack set instances that are associated with the specified stack set. Update operations affect both the stack set itself, as well as /all/ associated stack set instances.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoAction :: Lens.Lens' StackSetOperation (Core.Maybe Types.StackSetOperationAction)
ssoAction = Lens.field @"action"
{-# INLINEABLE ssoAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The Amazon Resource Number (ARN) of the IAM role used to perform this stack set operation. 
--
-- Use customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Define Permissions for Multiple Administrators> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'administrationRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoAdministrationRoleARN :: Lens.Lens' StackSetOperation (Core.Maybe Types.AdministrationRoleARN)
ssoAdministrationRoleARN = Lens.field @"administrationRoleARN"
{-# INLINEABLE ssoAdministrationRoleARN #-}
{-# DEPRECATED administrationRoleARN "Use generic-lens or generic-optics with 'administrationRoleARN' instead"  #-}

-- | The time at which the operation was initiated. Note that the creation times for the stack set operation might differ from the creation time of the individual stacks themselves. This is because AWS CloudFormation needs to perform preparatory work for the operation, such as dispatching the work to the requested Regions, before actually creating the first stacks.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoCreationTimestamp :: Lens.Lens' StackSetOperation (Core.Maybe Core.UTCTime)
ssoCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE ssoCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | [@Service-managed@ permissions] The AWS Organizations accounts affected by the stack operation.
--
-- /Note:/ Consider using 'deploymentTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoDeploymentTargets :: Lens.Lens' StackSetOperation (Core.Maybe Types.DeploymentTargets)
ssoDeploymentTargets = Lens.field @"deploymentTargets"
{-# INLINEABLE ssoDeploymentTargets #-}
{-# DEPRECATED deploymentTargets "Use generic-lens or generic-optics with 'deploymentTargets' instead"  #-}

-- | The time at which the stack set operation ended, across all accounts and Regions specified. Note that this doesn't necessarily mean that the stack set operation was successful, or even attempted, in each account or Region.
--
-- /Note:/ Consider using 'endTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoEndTimestamp :: Lens.Lens' StackSetOperation (Core.Maybe Core.UTCTime)
ssoEndTimestamp = Lens.field @"endTimestamp"
{-# INLINEABLE ssoEndTimestamp #-}
{-# DEPRECATED endTimestamp "Use generic-lens or generic-optics with 'endTimestamp' instead"  #-}

-- | The name of the IAM execution role used to create or update the stack set.
--
-- Use customized execution roles to control which stack resources users and groups can include in their stack sets. 
--
-- /Note:/ Consider using 'executionRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoExecutionRoleName :: Lens.Lens' StackSetOperation (Core.Maybe Types.ExecutionRoleName)
ssoExecutionRoleName = Lens.field @"executionRoleName"
{-# INLINEABLE ssoExecutionRoleName #-}
{-# DEPRECATED executionRoleName "Use generic-lens or generic-optics with 'executionRoleName' instead"  #-}

-- | The unique ID of a stack set operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoOperationId :: Lens.Lens' StackSetOperation (Core.Maybe Types.OperationId)
ssoOperationId = Lens.field @"operationId"
{-# INLINEABLE ssoOperationId #-}
{-# DEPRECATED operationId "Use generic-lens or generic-optics with 'operationId' instead"  #-}

-- | The preferences for how AWS CloudFormation performs this stack set operation.
--
-- /Note:/ Consider using 'operationPreferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoOperationPreferences :: Lens.Lens' StackSetOperation (Core.Maybe Types.StackSetOperationPreferences)
ssoOperationPreferences = Lens.field @"operationPreferences"
{-# INLINEABLE ssoOperationPreferences #-}
{-# DEPRECATED operationPreferences "Use generic-lens or generic-optics with 'operationPreferences' instead"  #-}

-- | For stack set operations of action type @DELETE@ , specifies whether to remove the stack instances from the specified stack set, but doesn't delete the stacks. You can't reassociate a retained stack, or add an existing, saved stack to a new stack set.
--
-- /Note:/ Consider using 'retainStacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoRetainStacks :: Lens.Lens' StackSetOperation (Core.Maybe Core.Bool)
ssoRetainStacks = Lens.field @"retainStacks"
{-# INLINEABLE ssoRetainStacks #-}
{-# DEPRECATED retainStacks "Use generic-lens or generic-optics with 'retainStacks' instead"  #-}

-- | Detailed information about the drift status of the stack set. This includes information about drift operations currently being performed on the stack set.
--
-- this information will only be present for stack set operations whose @Action@ type is @DETECT_DRIFT@ .
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html Detecting Unmanaged Changes in Stack Sets> in the AWS CloudFormation User Guide.
--
-- /Note:/ Consider using 'stackSetDriftDetectionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoStackSetDriftDetectionDetails :: Lens.Lens' StackSetOperation (Core.Maybe Types.StackSetDriftDetectionDetails)
ssoStackSetDriftDetectionDetails = Lens.field @"stackSetDriftDetectionDetails"
{-# INLINEABLE ssoStackSetDriftDetectionDetails #-}
{-# DEPRECATED stackSetDriftDetectionDetails "Use generic-lens or generic-optics with 'stackSetDriftDetectionDetails' instead"  #-}

-- | The ID of the stack set.
--
-- /Note:/ Consider using 'stackSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoStackSetId :: Lens.Lens' StackSetOperation (Core.Maybe Types.StackSetId)
ssoStackSetId = Lens.field @"stackSetId"
{-# INLINEABLE ssoStackSetId #-}
{-# DEPRECATED stackSetId "Use generic-lens or generic-optics with 'stackSetId' instead"  #-}

-- | The status of the operation. 
--
--
--     * @FAILED@ : The operation exceeded the specified failure tolerance. The failure tolerance value that you've set for an operation is applied for each Region during stack create and update operations. If the number of failed stacks within a Region exceeds the failure tolerance, the status of the operation in the Region is set to @FAILED@ . This in turn sets the status of the operation as a whole to @FAILED@ , and AWS CloudFormation cancels the operation in any remaining Regions.
--
--
--     * @QUEUED@ : [@Service-managed@ permissions] For automatic deployments that require a sequence of operations, the operation is queued to be performed. For more information, see the <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-status-codes stack set operation status codes> in the AWS CloudFormation User Guide.
--
--
--     * @RUNNING@ : The operation is currently being performed.
--
--
--     * @STOPPED@ : The user has cancelled the operation.
--
--
--     * @STOPPING@ : The operation is in the process of stopping, at user request. 
--
--
--     * @SUCCEEDED@ : The operation completed creating or updating all the specified stacks without exceeding the failure tolerance for the operation.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoStatus :: Lens.Lens' StackSetOperation (Core.Maybe Types.StackSetOperationStatus)
ssoStatus = Lens.field @"status"
{-# INLINEABLE ssoStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML StackSetOperation where
        parseXML x
          = StackSetOperation' Core.<$>
              (x Core..@? "Action") Core.<*> x Core..@? "AdministrationRoleARN"
                Core.<*> x Core..@? "CreationTimestamp"
                Core.<*> x Core..@? "DeploymentTargets"
                Core.<*> x Core..@? "EndTimestamp"
                Core.<*> x Core..@? "ExecutionRoleName"
                Core.<*> x Core..@? "OperationId"
                Core.<*> x Core..@? "OperationPreferences"
                Core.<*> x Core..@? "RetainStacks"
                Core.<*> x Core..@? "StackSetDriftDetectionDetails"
                Core.<*> x Core..@? "StackSetId"
                Core.<*> x Core..@? "Status"
