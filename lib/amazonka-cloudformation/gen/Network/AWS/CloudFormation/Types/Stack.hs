{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Stack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Stack
  ( Stack (..),

    -- * Smart constructor
    mkStack,

    -- * Lenses
    sfStackName,
    sfCreationTime,
    sfStackStatus,
    sfCapabilities,
    sfChangeSetId,
    sfDeletionTime,
    sfDescription,
    sfDisableRollback,
    sfDriftInformation,
    sfEnableTerminationProtection,
    sfLastUpdatedTime,
    sfNotificationARNs,
    sfOutputs,
    sfParameters,
    sfParentId,
    sfRoleARN,
    sfRollbackConfiguration,
    sfRootId,
    sfStackId,
    sfStackStatusReason,
    sfTags,
    sfTimeoutInMinutes,
  )
where

import qualified Network.AWS.CloudFormation.Types.Capability as Types
import qualified Network.AWS.CloudFormation.Types.ChangeSetId as Types
import qualified Network.AWS.CloudFormation.Types.Description as Types
import qualified Network.AWS.CloudFormation.Types.NotificationARN as Types
import qualified Network.AWS.CloudFormation.Types.Output as Types
import qualified Network.AWS.CloudFormation.Types.Parameter as Types
import qualified Network.AWS.CloudFormation.Types.RoleARN as Types
import qualified Network.AWS.CloudFormation.Types.RollbackConfiguration as Types
import qualified Network.AWS.CloudFormation.Types.StackDriftInformation as Types
import qualified Network.AWS.CloudFormation.Types.StackId as Types
import qualified Network.AWS.CloudFormation.Types.StackName as Types
import qualified Network.AWS.CloudFormation.Types.StackStatus as Types
import qualified Network.AWS.CloudFormation.Types.StackStatusReason as Types
import qualified Network.AWS.CloudFormation.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Stack data type.
--
-- /See:/ 'mkStack' smart constructor.
data Stack = Stack'
  { -- | The name associated with the stack.
    stackName :: Types.StackName,
    -- | The time at which the stack was created.
    creationTime :: Core.UTCTime,
    -- | Current status of the stack.
    stackStatus :: Types.StackStatus,
    -- | The capabilities allowed in the stack.
    capabilities :: Core.Maybe [Types.Capability],
    -- | The unique ID of the change set.
    changeSetId :: Core.Maybe Types.ChangeSetId,
    -- | The time the stack was deleted.
    deletionTime :: Core.Maybe Core.UTCTime,
    -- | A user-defined description associated with the stack.
    description :: Core.Maybe Types.Description,
    -- | Boolean to enable or disable rollback on stack creation failures:
    --
    --
    --     * @true@ : disable rollback
    --
    --
    --     * @false@ : enable rollback
    disableRollback :: Core.Maybe Core.Bool,
    -- | Information on whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
    driftInformation :: Core.Maybe Types.StackDriftInformation,
    -- | Whether termination protection is enabled for the stack.
    --
    -- For <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks> , termination protection is set on the root stack and cannot be changed directly on the nested stack. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted> in the /AWS CloudFormation User Guide/ .
    enableTerminationProtection :: Core.Maybe Core.Bool,
    -- | The time the stack was last updated. This field will only be returned if the stack has been updated at least once.
    lastUpdatedTime :: Core.Maybe Core.UTCTime,
    -- | SNS topic ARNs to which stack related events are published.
    notificationARNs :: Core.Maybe [Types.NotificationARN],
    -- | A list of output structures.
    outputs :: Core.Maybe [Types.Output],
    -- | A list of @Parameter@ structures.
    parameters :: Core.Maybe [Types.Parameter],
    -- | For nested stacks--stacks created as resources for another stack--the stack ID of the direct parent of this stack. For the first level of nested stacks, the root stack is also the parent stack.
    --
    -- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
    parentId :: Core.Maybe Types.StackId,
    -- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that is associated with the stack. During a stack operation, AWS CloudFormation uses this role's credentials to make calls on your behalf.
    roleARN :: Core.Maybe Types.RoleARN,
    -- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
    rollbackConfiguration :: Core.Maybe Types.RollbackConfiguration,
    -- | For nested stacks--stacks created as resources for another stack--the stack ID of the top-level stack to which the nested stack ultimately belongs.
    --
    -- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
    rootId :: Core.Maybe Types.StackId,
    -- | Unique identifier of the stack.
    stackId :: Core.Maybe Types.StackId,
    -- | Success/failure message associated with the stack status.
    stackStatusReason :: Core.Maybe Types.StackStatusReason,
    -- | A list of @Tag@ s that specify information about the stack.
    tags :: Core.Maybe [Types.Tag],
    -- | The amount of time within which stack creation should complete.
    timeoutInMinutes :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Stack' value with any optional fields omitted.
mkStack ::
  -- | 'stackName'
  Types.StackName ->
  -- | 'creationTime'
  Core.UTCTime ->
  -- | 'stackStatus'
  Types.StackStatus ->
  Stack
mkStack stackName creationTime stackStatus =
  Stack'
    { stackName,
      creationTime,
      stackStatus,
      capabilities = Core.Nothing,
      changeSetId = Core.Nothing,
      deletionTime = Core.Nothing,
      description = Core.Nothing,
      disableRollback = Core.Nothing,
      driftInformation = Core.Nothing,
      enableTerminationProtection = Core.Nothing,
      lastUpdatedTime = Core.Nothing,
      notificationARNs = Core.Nothing,
      outputs = Core.Nothing,
      parameters = Core.Nothing,
      parentId = Core.Nothing,
      roleARN = Core.Nothing,
      rollbackConfiguration = Core.Nothing,
      rootId = Core.Nothing,
      stackId = Core.Nothing,
      stackStatusReason = Core.Nothing,
      tags = Core.Nothing,
      timeoutInMinutes = Core.Nothing
    }

-- | The name associated with the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfStackName :: Lens.Lens' Stack Types.StackName
sfStackName = Lens.field @"stackName"
{-# DEPRECATED sfStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The time at which the stack was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfCreationTime :: Lens.Lens' Stack Core.UTCTime
sfCreationTime = Lens.field @"creationTime"
{-# DEPRECATED sfCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Current status of the stack.
--
-- /Note:/ Consider using 'stackStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfStackStatus :: Lens.Lens' Stack Types.StackStatus
sfStackStatus = Lens.field @"stackStatus"
{-# DEPRECATED sfStackStatus "Use generic-lens or generic-optics with 'stackStatus' instead." #-}

-- | The capabilities allowed in the stack.
--
-- /Note:/ Consider using 'capabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfCapabilities :: Lens.Lens' Stack (Core.Maybe [Types.Capability])
sfCapabilities = Lens.field @"capabilities"
{-# DEPRECATED sfCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | The unique ID of the change set.
--
-- /Note:/ Consider using 'changeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfChangeSetId :: Lens.Lens' Stack (Core.Maybe Types.ChangeSetId)
sfChangeSetId = Lens.field @"changeSetId"
{-# DEPRECATED sfChangeSetId "Use generic-lens or generic-optics with 'changeSetId' instead." #-}

-- | The time the stack was deleted.
--
-- /Note:/ Consider using 'deletionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDeletionTime :: Lens.Lens' Stack (Core.Maybe Core.UTCTime)
sfDeletionTime = Lens.field @"deletionTime"
{-# DEPRECATED sfDeletionTime "Use generic-lens or generic-optics with 'deletionTime' instead." #-}

-- | A user-defined description associated with the stack.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDescription :: Lens.Lens' Stack (Core.Maybe Types.Description)
sfDescription = Lens.field @"description"
{-# DEPRECATED sfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Boolean to enable or disable rollback on stack creation failures:
--
--
--     * @true@ : disable rollback
--
--
--     * @false@ : enable rollback
--
--
--
-- /Note:/ Consider using 'disableRollback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDisableRollback :: Lens.Lens' Stack (Core.Maybe Core.Bool)
sfDisableRollback = Lens.field @"disableRollback"
{-# DEPRECATED sfDisableRollback "Use generic-lens or generic-optics with 'disableRollback' instead." #-}

-- | Information on whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- /Note:/ Consider using 'driftInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDriftInformation :: Lens.Lens' Stack (Core.Maybe Types.StackDriftInformation)
sfDriftInformation = Lens.field @"driftInformation"
{-# DEPRECATED sfDriftInformation "Use generic-lens or generic-optics with 'driftInformation' instead." #-}

-- | Whether termination protection is enabled for the stack.
--
-- For <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks> , termination protection is set on the root stack and cannot be changed directly on the nested stack. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'enableTerminationProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfEnableTerminationProtection :: Lens.Lens' Stack (Core.Maybe Core.Bool)
sfEnableTerminationProtection = Lens.field @"enableTerminationProtection"
{-# DEPRECATED sfEnableTerminationProtection "Use generic-lens or generic-optics with 'enableTerminationProtection' instead." #-}

-- | The time the stack was last updated. This field will only be returned if the stack has been updated at least once.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfLastUpdatedTime :: Lens.Lens' Stack (Core.Maybe Core.UTCTime)
sfLastUpdatedTime = Lens.field @"lastUpdatedTime"
{-# DEPRECATED sfLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | SNS topic ARNs to which stack related events are published.
--
-- /Note:/ Consider using 'notificationARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfNotificationARNs :: Lens.Lens' Stack (Core.Maybe [Types.NotificationARN])
sfNotificationARNs = Lens.field @"notificationARNs"
{-# DEPRECATED sfNotificationARNs "Use generic-lens or generic-optics with 'notificationARNs' instead." #-}

-- | A list of output structures.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfOutputs :: Lens.Lens' Stack (Core.Maybe [Types.Output])
sfOutputs = Lens.field @"outputs"
{-# DEPRECATED sfOutputs "Use generic-lens or generic-optics with 'outputs' instead." #-}

-- | A list of @Parameter@ structures.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfParameters :: Lens.Lens' Stack (Core.Maybe [Types.Parameter])
sfParameters = Lens.field @"parameters"
{-# DEPRECATED sfParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | For nested stacks--stacks created as resources for another stack--the stack ID of the direct parent of this stack. For the first level of nested stacks, the root stack is also the parent stack.
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfParentId :: Lens.Lens' Stack (Core.Maybe Types.StackId)
sfParentId = Lens.field @"parentId"
{-# DEPRECATED sfParentId "Use generic-lens or generic-optics with 'parentId' instead." #-}

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that is associated with the stack. During a stack operation, AWS CloudFormation uses this role's credentials to make calls on your behalf.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfRoleARN :: Lens.Lens' Stack (Core.Maybe Types.RoleARN)
sfRoleARN = Lens.field @"roleARN"
{-# DEPRECATED sfRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
--
-- /Note:/ Consider using 'rollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfRollbackConfiguration :: Lens.Lens' Stack (Core.Maybe Types.RollbackConfiguration)
sfRollbackConfiguration = Lens.field @"rollbackConfiguration"
{-# DEPRECATED sfRollbackConfiguration "Use generic-lens or generic-optics with 'rollbackConfiguration' instead." #-}

-- | For nested stacks--stacks created as resources for another stack--the stack ID of the top-level stack to which the nested stack ultimately belongs.
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'rootId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfRootId :: Lens.Lens' Stack (Core.Maybe Types.StackId)
sfRootId = Lens.field @"rootId"
{-# DEPRECATED sfRootId "Use generic-lens or generic-optics with 'rootId' instead." #-}

-- | Unique identifier of the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfStackId :: Lens.Lens' Stack (Core.Maybe Types.StackId)
sfStackId = Lens.field @"stackId"
{-# DEPRECATED sfStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | Success/failure message associated with the stack status.
--
-- /Note:/ Consider using 'stackStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfStackStatusReason :: Lens.Lens' Stack (Core.Maybe Types.StackStatusReason)
sfStackStatusReason = Lens.field @"stackStatusReason"
{-# DEPRECATED sfStackStatusReason "Use generic-lens or generic-optics with 'stackStatusReason' instead." #-}

-- | A list of @Tag@ s that specify information about the stack.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfTags :: Lens.Lens' Stack (Core.Maybe [Types.Tag])
sfTags = Lens.field @"tags"
{-# DEPRECATED sfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The amount of time within which stack creation should complete.
--
-- /Note:/ Consider using 'timeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfTimeoutInMinutes :: Lens.Lens' Stack (Core.Maybe Core.Natural)
sfTimeoutInMinutes = Lens.field @"timeoutInMinutes"
{-# DEPRECATED sfTimeoutInMinutes "Use generic-lens or generic-optics with 'timeoutInMinutes' instead." #-}

instance Core.FromXML Stack where
  parseXML x =
    Stack'
      Core.<$> (x Core..@ "StackName")
      Core.<*> (x Core..@ "CreationTime")
      Core.<*> (x Core..@ "StackStatus")
      Core.<*> (x Core..@? "Capabilities" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "ChangeSetId")
      Core.<*> (x Core..@? "DeletionTime")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "DisableRollback")
      Core.<*> (x Core..@? "DriftInformation")
      Core.<*> (x Core..@? "EnableTerminationProtection")
      Core.<*> (x Core..@? "LastUpdatedTime")
      Core.<*> ( x Core..@? "NotificationARNs"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> (x Core..@? "Outputs" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "Parameters" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "ParentId")
      Core.<*> (x Core..@? "RoleARN")
      Core.<*> (x Core..@? "RollbackConfiguration")
      Core.<*> (x Core..@? "RootId")
      Core.<*> (x Core..@? "StackId")
      Core.<*> (x Core..@? "StackStatusReason")
      Core.<*> (x Core..@? "Tags" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "TimeoutInMinutes")
