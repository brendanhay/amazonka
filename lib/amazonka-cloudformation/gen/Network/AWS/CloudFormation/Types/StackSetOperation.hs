{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetOperation
  ( StackSetOperation (..),

    -- * Smart constructor
    mkStackSetOperation,

    -- * Lenses
    ssoStackSetDriftDetectionDetails,
    ssoStatus,
    ssoAdministrationRoleARN,
    ssoAction,
    ssoEndTimestamp,
    ssoCreationTimestamp,
    ssoOperationPreferences,
    ssoOperationId,
    ssoRetainStacks,
    ssoDeploymentTargets,
    ssoStackSetId,
    ssoExecutionRoleName,
  )
where

import Network.AWS.CloudFormation.Types.DeploymentTargets
import Network.AWS.CloudFormation.Types.StackSetDriftDetectionDetails
import Network.AWS.CloudFormation.Types.StackSetOperationAction
import Network.AWS.CloudFormation.Types.StackSetOperationPreferences
import Network.AWS.CloudFormation.Types.StackSetOperationStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The structure that contains information about a stack set operation.
--
-- /See:/ 'mkStackSetOperation' smart constructor.
data StackSetOperation = StackSetOperation'
  { stackSetDriftDetectionDetails ::
      Lude.Maybe StackSetDriftDetectionDetails,
    status :: Lude.Maybe StackSetOperationStatus,
    administrationRoleARN :: Lude.Maybe Lude.Text,
    action :: Lude.Maybe StackSetOperationAction,
    endTimestamp :: Lude.Maybe Lude.DateTime,
    creationTimestamp :: Lude.Maybe Lude.DateTime,
    operationPreferences ::
      Lude.Maybe StackSetOperationPreferences,
    operationId :: Lude.Maybe Lude.Text,
    retainStacks :: Lude.Maybe Lude.Bool,
    deploymentTargets :: Lude.Maybe DeploymentTargets,
    stackSetId :: Lude.Maybe Lude.Text,
    executionRoleName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackSetOperation' with the minimum fields required to make a request.
--
-- * 'action' - The type of stack set operation: @CREATE@ , @UPDATE@ , or @DELETE@ . Create and delete operations affect only the specified stack set instances that are associated with the specified stack set. Update operations affect both the stack set itself, as well as /all/ associated stack set instances.
-- * 'administrationRoleARN' - The Amazon Resource Number (ARN) of the IAM role used to perform this stack set operation.
--
-- Use customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Define Permissions for Multiple Administrators> in the /AWS CloudFormation User Guide/ .
-- * 'creationTimestamp' - The time at which the operation was initiated. Note that the creation times for the stack set operation might differ from the creation time of the individual stacks themselves. This is because AWS CloudFormation needs to perform preparatory work for the operation, such as dispatching the work to the requested Regions, before actually creating the first stacks.
-- * 'deploymentTargets' - [@Service-managed@ permissions] The AWS Organizations accounts affected by the stack operation.
-- * 'endTimestamp' - The time at which the stack set operation ended, across all accounts and Regions specified. Note that this doesn't necessarily mean that the stack set operation was successful, or even attempted, in each account or Region.
-- * 'executionRoleName' - The name of the IAM execution role used to create or update the stack set.
--
-- Use customized execution roles to control which stack resources users and groups can include in their stack sets.
-- * 'operationId' - The unique ID of a stack set operation.
-- * 'operationPreferences' - The preferences for how AWS CloudFormation performs this stack set operation.
-- * 'retainStacks' - For stack set operations of action type @DELETE@ , specifies whether to remove the stack instances from the specified stack set, but doesn't delete the stacks. You can't reassociate a retained stack, or add an existing, saved stack to a new stack set.
-- * 'stackSetDriftDetectionDetails' - Detailed information about the drift status of the stack set. This includes information about drift operations currently being performed on the stack set.
--
-- this information will only be present for stack set operations whose @Action@ type is @DETECT_DRIFT@ .
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html Detecting Unmanaged Changes in Stack Sets> in the AWS CloudFormation User Guide.
-- * 'stackSetId' - The ID of the stack set.
-- * 'status' - The status of the operation.
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
mkStackSetOperation ::
  StackSetOperation
mkStackSetOperation =
  StackSetOperation'
    { stackSetDriftDetectionDetails = Lude.Nothing,
      status = Lude.Nothing,
      administrationRoleARN = Lude.Nothing,
      action = Lude.Nothing,
      endTimestamp = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      operationPreferences = Lude.Nothing,
      operationId = Lude.Nothing,
      retainStacks = Lude.Nothing,
      deploymentTargets = Lude.Nothing,
      stackSetId = Lude.Nothing,
      executionRoleName = Lude.Nothing
    }

-- | Detailed information about the drift status of the stack set. This includes information about drift operations currently being performed on the stack set.
--
-- this information will only be present for stack set operations whose @Action@ type is @DETECT_DRIFT@ .
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html Detecting Unmanaged Changes in Stack Sets> in the AWS CloudFormation User Guide.
--
-- /Note:/ Consider using 'stackSetDriftDetectionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoStackSetDriftDetectionDetails :: Lens.Lens' StackSetOperation (Lude.Maybe StackSetDriftDetectionDetails)
ssoStackSetDriftDetectionDetails = Lens.lens (stackSetDriftDetectionDetails :: StackSetOperation -> Lude.Maybe StackSetDriftDetectionDetails) (\s a -> s {stackSetDriftDetectionDetails = a} :: StackSetOperation)
{-# DEPRECATED ssoStackSetDriftDetectionDetails "Use generic-lens or generic-optics with 'stackSetDriftDetectionDetails' instead." #-}

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
ssoStatus :: Lens.Lens' StackSetOperation (Lude.Maybe StackSetOperationStatus)
ssoStatus = Lens.lens (status :: StackSetOperation -> Lude.Maybe StackSetOperationStatus) (\s a -> s {status = a} :: StackSetOperation)
{-# DEPRECATED ssoStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Amazon Resource Number (ARN) of the IAM role used to perform this stack set operation.
--
-- Use customized administrator roles to control which users or groups can manage specific stack sets within the same administrator account. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Define Permissions for Multiple Administrators> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'administrationRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoAdministrationRoleARN :: Lens.Lens' StackSetOperation (Lude.Maybe Lude.Text)
ssoAdministrationRoleARN = Lens.lens (administrationRoleARN :: StackSetOperation -> Lude.Maybe Lude.Text) (\s a -> s {administrationRoleARN = a} :: StackSetOperation)
{-# DEPRECATED ssoAdministrationRoleARN "Use generic-lens or generic-optics with 'administrationRoleARN' instead." #-}

-- | The type of stack set operation: @CREATE@ , @UPDATE@ , or @DELETE@ . Create and delete operations affect only the specified stack set instances that are associated with the specified stack set. Update operations affect both the stack set itself, as well as /all/ associated stack set instances.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoAction :: Lens.Lens' StackSetOperation (Lude.Maybe StackSetOperationAction)
ssoAction = Lens.lens (action :: StackSetOperation -> Lude.Maybe StackSetOperationAction) (\s a -> s {action = a} :: StackSetOperation)
{-# DEPRECATED ssoAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The time at which the stack set operation ended, across all accounts and Regions specified. Note that this doesn't necessarily mean that the stack set operation was successful, or even attempted, in each account or Region.
--
-- /Note:/ Consider using 'endTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoEndTimestamp :: Lens.Lens' StackSetOperation (Lude.Maybe Lude.DateTime)
ssoEndTimestamp = Lens.lens (endTimestamp :: StackSetOperation -> Lude.Maybe Lude.DateTime) (\s a -> s {endTimestamp = a} :: StackSetOperation)
{-# DEPRECATED ssoEndTimestamp "Use generic-lens or generic-optics with 'endTimestamp' instead." #-}

-- | The time at which the operation was initiated. Note that the creation times for the stack set operation might differ from the creation time of the individual stacks themselves. This is because AWS CloudFormation needs to perform preparatory work for the operation, such as dispatching the work to the requested Regions, before actually creating the first stacks.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoCreationTimestamp :: Lens.Lens' StackSetOperation (Lude.Maybe Lude.DateTime)
ssoCreationTimestamp = Lens.lens (creationTimestamp :: StackSetOperation -> Lude.Maybe Lude.DateTime) (\s a -> s {creationTimestamp = a} :: StackSetOperation)
{-# DEPRECATED ssoCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The preferences for how AWS CloudFormation performs this stack set operation.
--
-- /Note:/ Consider using 'operationPreferences' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoOperationPreferences :: Lens.Lens' StackSetOperation (Lude.Maybe StackSetOperationPreferences)
ssoOperationPreferences = Lens.lens (operationPreferences :: StackSetOperation -> Lude.Maybe StackSetOperationPreferences) (\s a -> s {operationPreferences = a} :: StackSetOperation)
{-# DEPRECATED ssoOperationPreferences "Use generic-lens or generic-optics with 'operationPreferences' instead." #-}

-- | The unique ID of a stack set operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoOperationId :: Lens.Lens' StackSetOperation (Lude.Maybe Lude.Text)
ssoOperationId = Lens.lens (operationId :: StackSetOperation -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: StackSetOperation)
{-# DEPRECATED ssoOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | For stack set operations of action type @DELETE@ , specifies whether to remove the stack instances from the specified stack set, but doesn't delete the stacks. You can't reassociate a retained stack, or add an existing, saved stack to a new stack set.
--
-- /Note:/ Consider using 'retainStacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoRetainStacks :: Lens.Lens' StackSetOperation (Lude.Maybe Lude.Bool)
ssoRetainStacks = Lens.lens (retainStacks :: StackSetOperation -> Lude.Maybe Lude.Bool) (\s a -> s {retainStacks = a} :: StackSetOperation)
{-# DEPRECATED ssoRetainStacks "Use generic-lens or generic-optics with 'retainStacks' instead." #-}

-- | [@Service-managed@ permissions] The AWS Organizations accounts affected by the stack operation.
--
-- /Note:/ Consider using 'deploymentTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoDeploymentTargets :: Lens.Lens' StackSetOperation (Lude.Maybe DeploymentTargets)
ssoDeploymentTargets = Lens.lens (deploymentTargets :: StackSetOperation -> Lude.Maybe DeploymentTargets) (\s a -> s {deploymentTargets = a} :: StackSetOperation)
{-# DEPRECATED ssoDeploymentTargets "Use generic-lens or generic-optics with 'deploymentTargets' instead." #-}

-- | The ID of the stack set.
--
-- /Note:/ Consider using 'stackSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoStackSetId :: Lens.Lens' StackSetOperation (Lude.Maybe Lude.Text)
ssoStackSetId = Lens.lens (stackSetId :: StackSetOperation -> Lude.Maybe Lude.Text) (\s a -> s {stackSetId = a} :: StackSetOperation)
{-# DEPRECATED ssoStackSetId "Use generic-lens or generic-optics with 'stackSetId' instead." #-}

-- | The name of the IAM execution role used to create or update the stack set.
--
-- Use customized execution roles to control which stack resources users and groups can include in their stack sets.
--
-- /Note:/ Consider using 'executionRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssoExecutionRoleName :: Lens.Lens' StackSetOperation (Lude.Maybe Lude.Text)
ssoExecutionRoleName = Lens.lens (executionRoleName :: StackSetOperation -> Lude.Maybe Lude.Text) (\s a -> s {executionRoleName = a} :: StackSetOperation)
{-# DEPRECATED ssoExecutionRoleName "Use generic-lens or generic-optics with 'executionRoleName' instead." #-}

instance Lude.FromXML StackSetOperation where
  parseXML x =
    StackSetOperation'
      Lude.<$> (x Lude..@? "StackSetDriftDetectionDetails")
      Lude.<*> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "AdministrationRoleARN")
      Lude.<*> (x Lude..@? "Action")
      Lude.<*> (x Lude..@? "EndTimestamp")
      Lude.<*> (x Lude..@? "CreationTimestamp")
      Lude.<*> (x Lude..@? "OperationPreferences")
      Lude.<*> (x Lude..@? "OperationId")
      Lude.<*> (x Lude..@? "RetainStacks")
      Lude.<*> (x Lude..@? "DeploymentTargets")
      Lude.<*> (x Lude..@? "StackSetId")
      Lude.<*> (x Lude..@? "ExecutionRoleName")
