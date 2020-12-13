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
    sfCreationTime,
    sfStackStatus,
    sfDisableRollback,
    sfLastUpdatedTime,
    sfRootId,
    sfNotificationARNs,
    sfStackStatusReason,
    sfEnableTerminationProtection,
    sfDriftInformation,
    sfChangeSetId,
    sfDeletionTime,
    sfOutputs,
    sfParameters,
    sfStackId,
    sfDescription,
    sfCapabilities,
    sfRollbackConfiguration,
    sfTags,
    sfTimeoutInMinutes,
    sfParentId,
    sfRoleARN,
    sfStackName,
  )
where

import Network.AWS.CloudFormation.Types.Capability
import Network.AWS.CloudFormation.Types.Output
import Network.AWS.CloudFormation.Types.Parameter
import Network.AWS.CloudFormation.Types.RollbackConfiguration
import Network.AWS.CloudFormation.Types.StackDriftInformation
import Network.AWS.CloudFormation.Types.StackStatus
import Network.AWS.CloudFormation.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Stack data type.
--
-- /See:/ 'mkStack' smart constructor.
data Stack = Stack'
  { -- | The time at which the stack was created.
    creationTime :: Lude.DateTime,
    -- | Current status of the stack.
    stackStatus :: StackStatus,
    -- | Boolean to enable or disable rollback on stack creation failures:
    --
    --
    --     * @true@ : disable rollback
    --
    --
    --     * @false@ : enable rollback
    disableRollback :: Lude.Maybe Lude.Bool,
    -- | The time the stack was last updated. This field will only be returned if the stack has been updated at least once.
    lastUpdatedTime :: Lude.Maybe Lude.DateTime,
    -- | For nested stacks--stacks created as resources for another stack--the stack ID of the top-level stack to which the nested stack ultimately belongs.
    --
    -- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
    rootId :: Lude.Maybe Lude.Text,
    -- | SNS topic ARNs to which stack related events are published.
    notificationARNs :: Lude.Maybe [Lude.Text],
    -- | Success/failure message associated with the stack status.
    stackStatusReason :: Lude.Maybe Lude.Text,
    -- | Whether termination protection is enabled for the stack.
    --
    -- For <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks> , termination protection is set on the root stack and cannot be changed directly on the nested stack. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted> in the /AWS CloudFormation User Guide/ .
    enableTerminationProtection :: Lude.Maybe Lude.Bool,
    -- | Information on whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
    driftInformation :: Lude.Maybe StackDriftInformation,
    -- | The unique ID of the change set.
    changeSetId :: Lude.Maybe Lude.Text,
    -- | The time the stack was deleted.
    deletionTime :: Lude.Maybe Lude.DateTime,
    -- | A list of output structures.
    outputs :: Lude.Maybe [Output],
    -- | A list of @Parameter@ structures.
    parameters :: Lude.Maybe [Parameter],
    -- | Unique identifier of the stack.
    stackId :: Lude.Maybe Lude.Text,
    -- | A user-defined description associated with the stack.
    description :: Lude.Maybe Lude.Text,
    -- | The capabilities allowed in the stack.
    capabilities :: Lude.Maybe [Capability],
    -- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
    rollbackConfiguration :: Lude.Maybe RollbackConfiguration,
    -- | A list of @Tag@ s that specify information about the stack.
    tags :: Lude.Maybe [Tag],
    -- | The amount of time within which stack creation should complete.
    timeoutInMinutes :: Lude.Maybe Lude.Natural,
    -- | For nested stacks--stacks created as resources for another stack--the stack ID of the direct parent of this stack. For the first level of nested stacks, the root stack is also the parent stack.
    --
    -- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
    parentId :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that is associated with the stack. During a stack operation, AWS CloudFormation uses this role's credentials to make calls on your behalf.
    roleARN :: Lude.Maybe Lude.Text,
    -- | The name associated with the stack.
    stackName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Stack' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time at which the stack was created.
-- * 'stackStatus' - Current status of the stack.
-- * 'disableRollback' - Boolean to enable or disable rollback on stack creation failures:
--
--
--     * @true@ : disable rollback
--
--
--     * @false@ : enable rollback
--
--
-- * 'lastUpdatedTime' - The time the stack was last updated. This field will only be returned if the stack has been updated at least once.
-- * 'rootId' - For nested stacks--stacks created as resources for another stack--the stack ID of the top-level stack to which the nested stack ultimately belongs.
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
-- * 'notificationARNs' - SNS topic ARNs to which stack related events are published.
-- * 'stackStatusReason' - Success/failure message associated with the stack status.
-- * 'enableTerminationProtection' - Whether termination protection is enabled for the stack.
--
-- For <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks> , termination protection is set on the root stack and cannot be changed directly on the nested stack. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted> in the /AWS CloudFormation User Guide/ .
-- * 'driftInformation' - Information on whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
-- * 'changeSetId' - The unique ID of the change set.
-- * 'deletionTime' - The time the stack was deleted.
-- * 'outputs' - A list of output structures.
-- * 'parameters' - A list of @Parameter@ structures.
-- * 'stackId' - Unique identifier of the stack.
-- * 'description' - A user-defined description associated with the stack.
-- * 'capabilities' - The capabilities allowed in the stack.
-- * 'rollbackConfiguration' - The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
-- * 'tags' - A list of @Tag@ s that specify information about the stack.
-- * 'timeoutInMinutes' - The amount of time within which stack creation should complete.
-- * 'parentId' - For nested stacks--stacks created as resources for another stack--the stack ID of the direct parent of this stack. For the first level of nested stacks, the root stack is also the parent stack.
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
-- * 'roleARN' - The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that is associated with the stack. During a stack operation, AWS CloudFormation uses this role's credentials to make calls on your behalf.
-- * 'stackName' - The name associated with the stack.
mkStack ::
  -- | 'creationTime'
  Lude.DateTime ->
  -- | 'stackStatus'
  StackStatus ->
  -- | 'stackName'
  Lude.Text ->
  Stack
mkStack pCreationTime_ pStackStatus_ pStackName_ =
  Stack'
    { creationTime = pCreationTime_,
      stackStatus = pStackStatus_,
      disableRollback = Lude.Nothing,
      lastUpdatedTime = Lude.Nothing,
      rootId = Lude.Nothing,
      notificationARNs = Lude.Nothing,
      stackStatusReason = Lude.Nothing,
      enableTerminationProtection = Lude.Nothing,
      driftInformation = Lude.Nothing,
      changeSetId = Lude.Nothing,
      deletionTime = Lude.Nothing,
      outputs = Lude.Nothing,
      parameters = Lude.Nothing,
      stackId = Lude.Nothing,
      description = Lude.Nothing,
      capabilities = Lude.Nothing,
      rollbackConfiguration = Lude.Nothing,
      tags = Lude.Nothing,
      timeoutInMinutes = Lude.Nothing,
      parentId = Lude.Nothing,
      roleARN = Lude.Nothing,
      stackName = pStackName_
    }

-- | The time at which the stack was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfCreationTime :: Lens.Lens' Stack Lude.DateTime
sfCreationTime = Lens.lens (creationTime :: Stack -> Lude.DateTime) (\s a -> s {creationTime = a} :: Stack)
{-# DEPRECATED sfCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Current status of the stack.
--
-- /Note:/ Consider using 'stackStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfStackStatus :: Lens.Lens' Stack StackStatus
sfStackStatus = Lens.lens (stackStatus :: Stack -> StackStatus) (\s a -> s {stackStatus = a} :: Stack)
{-# DEPRECATED sfStackStatus "Use generic-lens or generic-optics with 'stackStatus' instead." #-}

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
sfDisableRollback :: Lens.Lens' Stack (Lude.Maybe Lude.Bool)
sfDisableRollback = Lens.lens (disableRollback :: Stack -> Lude.Maybe Lude.Bool) (\s a -> s {disableRollback = a} :: Stack)
{-# DEPRECATED sfDisableRollback "Use generic-lens or generic-optics with 'disableRollback' instead." #-}

-- | The time the stack was last updated. This field will only be returned if the stack has been updated at least once.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfLastUpdatedTime :: Lens.Lens' Stack (Lude.Maybe Lude.DateTime)
sfLastUpdatedTime = Lens.lens (lastUpdatedTime :: Stack -> Lude.Maybe Lude.DateTime) (\s a -> s {lastUpdatedTime = a} :: Stack)
{-# DEPRECATED sfLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | For nested stacks--stacks created as resources for another stack--the stack ID of the top-level stack to which the nested stack ultimately belongs.
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'rootId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfRootId :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfRootId = Lens.lens (rootId :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {rootId = a} :: Stack)
{-# DEPRECATED sfRootId "Use generic-lens or generic-optics with 'rootId' instead." #-}

-- | SNS topic ARNs to which stack related events are published.
--
-- /Note:/ Consider using 'notificationARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfNotificationARNs :: Lens.Lens' Stack (Lude.Maybe [Lude.Text])
sfNotificationARNs = Lens.lens (notificationARNs :: Stack -> Lude.Maybe [Lude.Text]) (\s a -> s {notificationARNs = a} :: Stack)
{-# DEPRECATED sfNotificationARNs "Use generic-lens or generic-optics with 'notificationARNs' instead." #-}

-- | Success/failure message associated with the stack status.
--
-- /Note:/ Consider using 'stackStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfStackStatusReason :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfStackStatusReason = Lens.lens (stackStatusReason :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {stackStatusReason = a} :: Stack)
{-# DEPRECATED sfStackStatusReason "Use generic-lens or generic-optics with 'stackStatusReason' instead." #-}

-- | Whether termination protection is enabled for the stack.
--
-- For <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks> , termination protection is set on the root stack and cannot be changed directly on the nested stack. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'enableTerminationProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfEnableTerminationProtection :: Lens.Lens' Stack (Lude.Maybe Lude.Bool)
sfEnableTerminationProtection = Lens.lens (enableTerminationProtection :: Stack -> Lude.Maybe Lude.Bool) (\s a -> s {enableTerminationProtection = a} :: Stack)
{-# DEPRECATED sfEnableTerminationProtection "Use generic-lens or generic-optics with 'enableTerminationProtection' instead." #-}

-- | Information on whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- /Note:/ Consider using 'driftInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDriftInformation :: Lens.Lens' Stack (Lude.Maybe StackDriftInformation)
sfDriftInformation = Lens.lens (driftInformation :: Stack -> Lude.Maybe StackDriftInformation) (\s a -> s {driftInformation = a} :: Stack)
{-# DEPRECATED sfDriftInformation "Use generic-lens or generic-optics with 'driftInformation' instead." #-}

-- | The unique ID of the change set.
--
-- /Note:/ Consider using 'changeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfChangeSetId :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfChangeSetId = Lens.lens (changeSetId :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {changeSetId = a} :: Stack)
{-# DEPRECATED sfChangeSetId "Use generic-lens or generic-optics with 'changeSetId' instead." #-}

-- | The time the stack was deleted.
--
-- /Note:/ Consider using 'deletionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDeletionTime :: Lens.Lens' Stack (Lude.Maybe Lude.DateTime)
sfDeletionTime = Lens.lens (deletionTime :: Stack -> Lude.Maybe Lude.DateTime) (\s a -> s {deletionTime = a} :: Stack)
{-# DEPRECATED sfDeletionTime "Use generic-lens or generic-optics with 'deletionTime' instead." #-}

-- | A list of output structures.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfOutputs :: Lens.Lens' Stack (Lude.Maybe [Output])
sfOutputs = Lens.lens (outputs :: Stack -> Lude.Maybe [Output]) (\s a -> s {outputs = a} :: Stack)
{-# DEPRECATED sfOutputs "Use generic-lens or generic-optics with 'outputs' instead." #-}

-- | A list of @Parameter@ structures.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfParameters :: Lens.Lens' Stack (Lude.Maybe [Parameter])
sfParameters = Lens.lens (parameters :: Stack -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: Stack)
{-# DEPRECATED sfParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | Unique identifier of the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfStackId :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfStackId = Lens.lens (stackId :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: Stack)
{-# DEPRECATED sfStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | A user-defined description associated with the stack.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfDescription :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfDescription = Lens.lens (description :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Stack)
{-# DEPRECATED sfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The capabilities allowed in the stack.
--
-- /Note:/ Consider using 'capabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfCapabilities :: Lens.Lens' Stack (Lude.Maybe [Capability])
sfCapabilities = Lens.lens (capabilities :: Stack -> Lude.Maybe [Capability]) (\s a -> s {capabilities = a} :: Stack)
{-# DEPRECATED sfCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
--
-- /Note:/ Consider using 'rollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfRollbackConfiguration :: Lens.Lens' Stack (Lude.Maybe RollbackConfiguration)
sfRollbackConfiguration = Lens.lens (rollbackConfiguration :: Stack -> Lude.Maybe RollbackConfiguration) (\s a -> s {rollbackConfiguration = a} :: Stack)
{-# DEPRECATED sfRollbackConfiguration "Use generic-lens or generic-optics with 'rollbackConfiguration' instead." #-}

-- | A list of @Tag@ s that specify information about the stack.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfTags :: Lens.Lens' Stack (Lude.Maybe [Tag])
sfTags = Lens.lens (tags :: Stack -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Stack)
{-# DEPRECATED sfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The amount of time within which stack creation should complete.
--
-- /Note:/ Consider using 'timeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfTimeoutInMinutes :: Lens.Lens' Stack (Lude.Maybe Lude.Natural)
sfTimeoutInMinutes = Lens.lens (timeoutInMinutes :: Stack -> Lude.Maybe Lude.Natural) (\s a -> s {timeoutInMinutes = a} :: Stack)
{-# DEPRECATED sfTimeoutInMinutes "Use generic-lens or generic-optics with 'timeoutInMinutes' instead." #-}

-- | For nested stacks--stacks created as resources for another stack--the stack ID of the direct parent of this stack. For the first level of nested stacks, the root stack is also the parent stack.
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfParentId :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfParentId = Lens.lens (parentId :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {parentId = a} :: Stack)
{-# DEPRECATED sfParentId "Use generic-lens or generic-optics with 'parentId' instead." #-}

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that is associated with the stack. During a stack operation, AWS CloudFormation uses this role's credentials to make calls on your behalf.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfRoleARN :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
sfRoleARN = Lens.lens (roleARN :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: Stack)
{-# DEPRECATED sfRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The name associated with the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfStackName :: Lens.Lens' Stack Lude.Text
sfStackName = Lens.lens (stackName :: Stack -> Lude.Text) (\s a -> s {stackName = a} :: Stack)
{-# DEPRECATED sfStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.FromXML Stack where
  parseXML x =
    Stack'
      Lude.<$> (x Lude..@ "CreationTime")
      Lude.<*> (x Lude..@ "StackStatus")
      Lude.<*> (x Lude..@? "DisableRollback")
      Lude.<*> (x Lude..@? "LastUpdatedTime")
      Lude.<*> (x Lude..@? "RootId")
      Lude.<*> ( x Lude..@? "NotificationARNs" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "StackStatusReason")
      Lude.<*> (x Lude..@? "EnableTerminationProtection")
      Lude.<*> (x Lude..@? "DriftInformation")
      Lude.<*> (x Lude..@? "ChangeSetId")
      Lude.<*> (x Lude..@? "DeletionTime")
      Lude.<*> ( x Lude..@? "Outputs" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> ( x Lude..@? "Parameters" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "StackId")
      Lude.<*> (x Lude..@? "Description")
      Lude.<*> ( x Lude..@? "Capabilities" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "RollbackConfiguration")
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "TimeoutInMinutes")
      Lude.<*> (x Lude..@? "ParentId")
      Lude.<*> (x Lude..@? "RoleARN")
      Lude.<*> (x Lude..@ "StackName")
