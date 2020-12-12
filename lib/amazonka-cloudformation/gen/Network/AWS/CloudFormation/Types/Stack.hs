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
    staDisableRollback,
    staLastUpdatedTime,
    staRootId,
    staNotificationARNs,
    staStackStatusReason,
    staEnableTerminationProtection,
    staDriftInformation,
    staChangeSetId,
    staDeletionTime,
    staOutputs,
    staParameters,
    staStackId,
    staDescription,
    staCapabilities,
    staRollbackConfiguration,
    staTags,
    staTimeoutInMinutes,
    staParentId,
    staRoleARN,
    staStackName,
    staCreationTime,
    staStackStatus,
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
  { disableRollback :: Lude.Maybe Lude.Bool,
    lastUpdatedTime :: Lude.Maybe Lude.DateTime,
    rootId :: Lude.Maybe Lude.Text,
    notificationARNs :: Lude.Maybe [Lude.Text],
    stackStatusReason :: Lude.Maybe Lude.Text,
    enableTerminationProtection :: Lude.Maybe Lude.Bool,
    driftInformation :: Lude.Maybe StackDriftInformation,
    changeSetId :: Lude.Maybe Lude.Text,
    deletionTime :: Lude.Maybe Lude.DateTime,
    outputs :: Lude.Maybe [Output],
    parameters :: Lude.Maybe [Parameter],
    stackId :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    capabilities :: Lude.Maybe [Capability],
    rollbackConfiguration :: Lude.Maybe RollbackConfiguration,
    tags :: Lude.Maybe [Tag],
    timeoutInMinutes :: Lude.Maybe Lude.Natural,
    parentId :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text,
    stackName :: Lude.Text,
    creationTime :: Lude.DateTime,
    stackStatus :: StackStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Stack' with the minimum fields required to make a request.
--
-- * 'capabilities' - The capabilities allowed in the stack.
-- * 'changeSetId' - The unique ID of the change set.
-- * 'creationTime' - The time at which the stack was created.
-- * 'deletionTime' - The time the stack was deleted.
-- * 'description' - A user-defined description associated with the stack.
-- * 'disableRollback' - Boolean to enable or disable rollback on stack creation failures:
--
--
--     * @true@ : disable rollback
--
--
--     * @false@ : enable rollback
--
--
-- * 'driftInformation' - Information on whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
-- * 'enableTerminationProtection' - Whether termination protection is enabled for the stack.
--
-- For <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks> , termination protection is set on the root stack and cannot be changed directly on the nested stack. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted> in the /AWS CloudFormation User Guide/ .
-- * 'lastUpdatedTime' - The time the stack was last updated. This field will only be returned if the stack has been updated at least once.
-- * 'notificationARNs' - SNS topic ARNs to which stack related events are published.
-- * 'outputs' - A list of output structures.
-- * 'parameters' - A list of @Parameter@ structures.
-- * 'parentId' - For nested stacks--stacks created as resources for another stack--the stack ID of the direct parent of this stack. For the first level of nested stacks, the root stack is also the parent stack.
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
-- * 'roleARN' - The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that is associated with the stack. During a stack operation, AWS CloudFormation uses this role's credentials to make calls on your behalf.
-- * 'rollbackConfiguration' - The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
-- * 'rootId' - For nested stacks--stacks created as resources for another stack--the stack ID of the top-level stack to which the nested stack ultimately belongs.
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
-- * 'stackId' - Unique identifier of the stack.
-- * 'stackName' - The name associated with the stack.
-- * 'stackStatus' - Current status of the stack.
-- * 'stackStatusReason' - Success/failure message associated with the stack status.
-- * 'tags' - A list of @Tag@ s that specify information about the stack.
-- * 'timeoutInMinutes' - The amount of time within which stack creation should complete.
mkStack ::
  -- | 'stackName'
  Lude.Text ->
  -- | 'creationTime'
  Lude.DateTime ->
  -- | 'stackStatus'
  StackStatus ->
  Stack
mkStack pStackName_ pCreationTime_ pStackStatus_ =
  Stack'
    { disableRollback = Lude.Nothing,
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
      stackName = pStackName_,
      creationTime = pCreationTime_,
      stackStatus = pStackStatus_
    }

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
staDisableRollback :: Lens.Lens' Stack (Lude.Maybe Lude.Bool)
staDisableRollback = Lens.lens (disableRollback :: Stack -> Lude.Maybe Lude.Bool) (\s a -> s {disableRollback = a} :: Stack)
{-# DEPRECATED staDisableRollback "Use generic-lens or generic-optics with 'disableRollback' instead." #-}

-- | The time the stack was last updated. This field will only be returned if the stack has been updated at least once.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staLastUpdatedTime :: Lens.Lens' Stack (Lude.Maybe Lude.DateTime)
staLastUpdatedTime = Lens.lens (lastUpdatedTime :: Stack -> Lude.Maybe Lude.DateTime) (\s a -> s {lastUpdatedTime = a} :: Stack)
{-# DEPRECATED staLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

-- | For nested stacks--stacks created as resources for another stack--the stack ID of the top-level stack to which the nested stack ultimately belongs.
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'rootId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staRootId :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
staRootId = Lens.lens (rootId :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {rootId = a} :: Stack)
{-# DEPRECATED staRootId "Use generic-lens or generic-optics with 'rootId' instead." #-}

-- | SNS topic ARNs to which stack related events are published.
--
-- /Note:/ Consider using 'notificationARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staNotificationARNs :: Lens.Lens' Stack (Lude.Maybe [Lude.Text])
staNotificationARNs = Lens.lens (notificationARNs :: Stack -> Lude.Maybe [Lude.Text]) (\s a -> s {notificationARNs = a} :: Stack)
{-# DEPRECATED staNotificationARNs "Use generic-lens or generic-optics with 'notificationARNs' instead." #-}

-- | Success/failure message associated with the stack status.
--
-- /Note:/ Consider using 'stackStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staStackStatusReason :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
staStackStatusReason = Lens.lens (stackStatusReason :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {stackStatusReason = a} :: Stack)
{-# DEPRECATED staStackStatusReason "Use generic-lens or generic-optics with 'stackStatusReason' instead." #-}

-- | Whether termination protection is enabled for the stack.
--
-- For <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks> , termination protection is set on the root stack and cannot be changed directly on the nested stack. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'enableTerminationProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staEnableTerminationProtection :: Lens.Lens' Stack (Lude.Maybe Lude.Bool)
staEnableTerminationProtection = Lens.lens (enableTerminationProtection :: Stack -> Lude.Maybe Lude.Bool) (\s a -> s {enableTerminationProtection = a} :: Stack)
{-# DEPRECATED staEnableTerminationProtection "Use generic-lens or generic-optics with 'enableTerminationProtection' instead." #-}

-- | Information on whether a stack's actual configuration differs, or has /drifted/ , from it's expected configuration, as defined in the stack template and any values specified as template parameters. For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources> .
--
-- /Note:/ Consider using 'driftInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staDriftInformation :: Lens.Lens' Stack (Lude.Maybe StackDriftInformation)
staDriftInformation = Lens.lens (driftInformation :: Stack -> Lude.Maybe StackDriftInformation) (\s a -> s {driftInformation = a} :: Stack)
{-# DEPRECATED staDriftInformation "Use generic-lens or generic-optics with 'driftInformation' instead." #-}

-- | The unique ID of the change set.
--
-- /Note:/ Consider using 'changeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staChangeSetId :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
staChangeSetId = Lens.lens (changeSetId :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {changeSetId = a} :: Stack)
{-# DEPRECATED staChangeSetId "Use generic-lens or generic-optics with 'changeSetId' instead." #-}

-- | The time the stack was deleted.
--
-- /Note:/ Consider using 'deletionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staDeletionTime :: Lens.Lens' Stack (Lude.Maybe Lude.DateTime)
staDeletionTime = Lens.lens (deletionTime :: Stack -> Lude.Maybe Lude.DateTime) (\s a -> s {deletionTime = a} :: Stack)
{-# DEPRECATED staDeletionTime "Use generic-lens or generic-optics with 'deletionTime' instead." #-}

-- | A list of output structures.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staOutputs :: Lens.Lens' Stack (Lude.Maybe [Output])
staOutputs = Lens.lens (outputs :: Stack -> Lude.Maybe [Output]) (\s a -> s {outputs = a} :: Stack)
{-# DEPRECATED staOutputs "Use generic-lens or generic-optics with 'outputs' instead." #-}

-- | A list of @Parameter@ structures.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staParameters :: Lens.Lens' Stack (Lude.Maybe [Parameter])
staParameters = Lens.lens (parameters :: Stack -> Lude.Maybe [Parameter]) (\s a -> s {parameters = a} :: Stack)
{-# DEPRECATED staParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | Unique identifier of the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staStackId :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
staStackId = Lens.lens (stackId :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: Stack)
{-# DEPRECATED staStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | A user-defined description associated with the stack.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staDescription :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
staDescription = Lens.lens (description :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Stack)
{-# DEPRECATED staDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The capabilities allowed in the stack.
--
-- /Note:/ Consider using 'capabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staCapabilities :: Lens.Lens' Stack (Lude.Maybe [Capability])
staCapabilities = Lens.lens (capabilities :: Stack -> Lude.Maybe [Capability]) (\s a -> s {capabilities = a} :: Stack)
{-# DEPRECATED staCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | The rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.
--
-- /Note:/ Consider using 'rollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staRollbackConfiguration :: Lens.Lens' Stack (Lude.Maybe RollbackConfiguration)
staRollbackConfiguration = Lens.lens (rollbackConfiguration :: Stack -> Lude.Maybe RollbackConfiguration) (\s a -> s {rollbackConfiguration = a} :: Stack)
{-# DEPRECATED staRollbackConfiguration "Use generic-lens or generic-optics with 'rollbackConfiguration' instead." #-}

-- | A list of @Tag@ s that specify information about the stack.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staTags :: Lens.Lens' Stack (Lude.Maybe [Tag])
staTags = Lens.lens (tags :: Stack -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Stack)
{-# DEPRECATED staTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The amount of time within which stack creation should complete.
--
-- /Note:/ Consider using 'timeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staTimeoutInMinutes :: Lens.Lens' Stack (Lude.Maybe Lude.Natural)
staTimeoutInMinutes = Lens.lens (timeoutInMinutes :: Stack -> Lude.Maybe Lude.Natural) (\s a -> s {timeoutInMinutes = a} :: Stack)
{-# DEPRECATED staTimeoutInMinutes "Use generic-lens or generic-optics with 'timeoutInMinutes' instead." #-}

-- | For nested stacks--stacks created as resources for another stack--the stack ID of the direct parent of this stack. For the first level of nested stacks, the root stack is also the parent stack.
--
-- For more information, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks> in the /AWS CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'parentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staParentId :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
staParentId = Lens.lens (parentId :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {parentId = a} :: Stack)
{-# DEPRECATED staParentId "Use generic-lens or generic-optics with 'parentId' instead." #-}

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management (IAM) role that is associated with the stack. During a stack operation, AWS CloudFormation uses this role's credentials to make calls on your behalf.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staRoleARN :: Lens.Lens' Stack (Lude.Maybe Lude.Text)
staRoleARN = Lens.lens (roleARN :: Stack -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: Stack)
{-# DEPRECATED staRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The name associated with the stack.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staStackName :: Lens.Lens' Stack Lude.Text
staStackName = Lens.lens (stackName :: Stack -> Lude.Text) (\s a -> s {stackName = a} :: Stack)
{-# DEPRECATED staStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The time at which the stack was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staCreationTime :: Lens.Lens' Stack Lude.DateTime
staCreationTime = Lens.lens (creationTime :: Stack -> Lude.DateTime) (\s a -> s {creationTime = a} :: Stack)
{-# DEPRECATED staCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Current status of the stack.
--
-- /Note:/ Consider using 'stackStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staStackStatus :: Lens.Lens' Stack StackStatus
staStackStatus = Lens.lens (stackStatus :: Stack -> StackStatus) (\s a -> s {stackStatus = a} :: Stack)
{-# DEPRECATED staStackStatus "Use generic-lens or generic-optics with 'stackStatus' instead." #-}

instance Lude.FromXML Stack where
  parseXML x =
    Stack'
      Lude.<$> (x Lude..@? "DisableRollback")
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
      Lude.<*> (x Lude..@ "CreationTime")
      Lude.<*> (x Lude..@ "StackStatus")
