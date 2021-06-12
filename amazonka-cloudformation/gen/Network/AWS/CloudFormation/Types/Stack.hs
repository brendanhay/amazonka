{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Stack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Stack where

import Network.AWS.CloudFormation.Types.Capability
import Network.AWS.CloudFormation.Types.Output
import Network.AWS.CloudFormation.Types.Parameter
import Network.AWS.CloudFormation.Types.RollbackConfiguration
import Network.AWS.CloudFormation.Types.StackDriftInformation
import Network.AWS.CloudFormation.Types.StackStatus
import Network.AWS.CloudFormation.Types.Tag
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The Stack data type.
--
-- /See:/ 'newStack' smart constructor.
data Stack = Stack'
  { -- | A list of output structures.
    outputs :: Core.Maybe [Output],
    -- | Information on whether a stack\'s actual configuration differs, or has
    -- /drifted/, from it\'s expected configuration, as defined in the stack
    -- template and any values specified as template parameters. For more
    -- information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
    driftInformation :: Core.Maybe StackDriftInformation,
    -- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management
    -- (IAM) role that is associated with the stack. During a stack operation,
    -- AWS CloudFormation uses this role\'s credentials to make calls on your
    -- behalf.
    roleARN :: Core.Maybe Core.Text,
    -- | The time the stack was deleted.
    deletionTime :: Core.Maybe Core.ISO8601,
    -- | The capabilities allowed in the stack.
    capabilities :: Core.Maybe [Capability],
    -- | Success\/failure message associated with the stack status.
    stackStatusReason :: Core.Maybe Core.Text,
    -- | Whether termination protection is enabled for the stack.
    --
    -- For
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks>,
    -- termination protection is set on the root stack and cannot be changed
    -- directly on the nested stack. For more information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted>
    -- in the /AWS CloudFormation User Guide/.
    enableTerminationProtection :: Core.Maybe Core.Bool,
    -- | Unique identifier of the stack.
    stackId :: Core.Maybe Core.Text,
    -- | SNS topic ARNs to which stack related events are published.
    notificationARNs :: Core.Maybe [Core.Text],
    -- | For nested stacks--stacks created as resources for another stack--the
    -- stack ID of the top-level stack to which the nested stack ultimately
    -- belongs.
    --
    -- For more information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
    -- in the /AWS CloudFormation User Guide/.
    rootId :: Core.Maybe Core.Text,
    -- | A list of @Tag@s that specify information about the stack.
    tags :: Core.Maybe [Tag],
    -- | The unique ID of the change set.
    changeSetId :: Core.Maybe Core.Text,
    -- | The amount of time within which stack creation should complete.
    timeoutInMinutes :: Core.Maybe Core.Natural,
    -- | For nested stacks--stacks created as resources for another stack--the
    -- stack ID of the direct parent of this stack. For the first level of
    -- nested stacks, the root stack is also the parent stack.
    --
    -- For more information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
    -- in the /AWS CloudFormation User Guide/.
    parentId :: Core.Maybe Core.Text,
    -- | The rollback triggers for AWS CloudFormation to monitor during stack
    -- creation and updating operations, and for the specified monitoring
    -- period afterwards.
    rollbackConfiguration :: Core.Maybe RollbackConfiguration,
    -- | A user-defined description associated with the stack.
    description :: Core.Maybe Core.Text,
    -- | Boolean to enable or disable rollback on stack creation failures:
    --
    -- -   @true@: disable rollback
    --
    -- -   @false@: enable rollback
    disableRollback :: Core.Maybe Core.Bool,
    -- | A list of @Parameter@ structures.
    parameters :: Core.Maybe [Parameter],
    -- | The time the stack was last updated. This field will only be returned if
    -- the stack has been updated at least once.
    lastUpdatedTime :: Core.Maybe Core.ISO8601,
    -- | The name associated with the stack.
    stackName :: Core.Text,
    -- | The time at which the stack was created.
    creationTime :: Core.ISO8601,
    -- | Current status of the stack.
    stackStatus :: StackStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Stack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputs', 'stack_outputs' - A list of output structures.
--
-- 'driftInformation', 'stack_driftInformation' - Information on whether a stack\'s actual configuration differs, or has
-- /drifted/, from it\'s expected configuration, as defined in the stack
-- template and any values specified as template parameters. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
--
-- 'roleARN', 'stack_roleARN' - The Amazon Resource Name (ARN) of an AWS Identity and Access Management
-- (IAM) role that is associated with the stack. During a stack operation,
-- AWS CloudFormation uses this role\'s credentials to make calls on your
-- behalf.
--
-- 'deletionTime', 'stack_deletionTime' - The time the stack was deleted.
--
-- 'capabilities', 'stack_capabilities' - The capabilities allowed in the stack.
--
-- 'stackStatusReason', 'stack_stackStatusReason' - Success\/failure message associated with the stack status.
--
-- 'enableTerminationProtection', 'stack_enableTerminationProtection' - Whether termination protection is enabled for the stack.
--
-- For
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks>,
-- termination protection is set on the root stack and cannot be changed
-- directly on the nested stack. For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted>
-- in the /AWS CloudFormation User Guide/.
--
-- 'stackId', 'stack_stackId' - Unique identifier of the stack.
--
-- 'notificationARNs', 'stack_notificationARNs' - SNS topic ARNs to which stack related events are published.
--
-- 'rootId', 'stack_rootId' - For nested stacks--stacks created as resources for another stack--the
-- stack ID of the top-level stack to which the nested stack ultimately
-- belongs.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
-- in the /AWS CloudFormation User Guide/.
--
-- 'tags', 'stack_tags' - A list of @Tag@s that specify information about the stack.
--
-- 'changeSetId', 'stack_changeSetId' - The unique ID of the change set.
--
-- 'timeoutInMinutes', 'stack_timeoutInMinutes' - The amount of time within which stack creation should complete.
--
-- 'parentId', 'stack_parentId' - For nested stacks--stacks created as resources for another stack--the
-- stack ID of the direct parent of this stack. For the first level of
-- nested stacks, the root stack is also the parent stack.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
-- in the /AWS CloudFormation User Guide/.
--
-- 'rollbackConfiguration', 'stack_rollbackConfiguration' - The rollback triggers for AWS CloudFormation to monitor during stack
-- creation and updating operations, and for the specified monitoring
-- period afterwards.
--
-- 'description', 'stack_description' - A user-defined description associated with the stack.
--
-- 'disableRollback', 'stack_disableRollback' - Boolean to enable or disable rollback on stack creation failures:
--
-- -   @true@: disable rollback
--
-- -   @false@: enable rollback
--
-- 'parameters', 'stack_parameters' - A list of @Parameter@ structures.
--
-- 'lastUpdatedTime', 'stack_lastUpdatedTime' - The time the stack was last updated. This field will only be returned if
-- the stack has been updated at least once.
--
-- 'stackName', 'stack_stackName' - The name associated with the stack.
--
-- 'creationTime', 'stack_creationTime' - The time at which the stack was created.
--
-- 'stackStatus', 'stack_stackStatus' - Current status of the stack.
newStack ::
  -- | 'stackName'
  Core.Text ->
  -- | 'creationTime'
  Core.UTCTime ->
  -- | 'stackStatus'
  StackStatus ->
  Stack
newStack pStackName_ pCreationTime_ pStackStatus_ =
  Stack'
    { outputs = Core.Nothing,
      driftInformation = Core.Nothing,
      roleARN = Core.Nothing,
      deletionTime = Core.Nothing,
      capabilities = Core.Nothing,
      stackStatusReason = Core.Nothing,
      enableTerminationProtection = Core.Nothing,
      stackId = Core.Nothing,
      notificationARNs = Core.Nothing,
      rootId = Core.Nothing,
      tags = Core.Nothing,
      changeSetId = Core.Nothing,
      timeoutInMinutes = Core.Nothing,
      parentId = Core.Nothing,
      rollbackConfiguration = Core.Nothing,
      description = Core.Nothing,
      disableRollback = Core.Nothing,
      parameters = Core.Nothing,
      lastUpdatedTime = Core.Nothing,
      stackName = pStackName_,
      creationTime = Core._Time Lens.# pCreationTime_,
      stackStatus = pStackStatus_
    }

-- | A list of output structures.
stack_outputs :: Lens.Lens' Stack (Core.Maybe [Output])
stack_outputs = Lens.lens (\Stack' {outputs} -> outputs) (\s@Stack' {} a -> s {outputs = a} :: Stack) Core.. Lens.mapping Lens._Coerce

-- | Information on whether a stack\'s actual configuration differs, or has
-- /drifted/, from it\'s expected configuration, as defined in the stack
-- template and any values specified as template parameters. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
stack_driftInformation :: Lens.Lens' Stack (Core.Maybe StackDriftInformation)
stack_driftInformation = Lens.lens (\Stack' {driftInformation} -> driftInformation) (\s@Stack' {} a -> s {driftInformation = a} :: Stack)

-- | The Amazon Resource Name (ARN) of an AWS Identity and Access Management
-- (IAM) role that is associated with the stack. During a stack operation,
-- AWS CloudFormation uses this role\'s credentials to make calls on your
-- behalf.
stack_roleARN :: Lens.Lens' Stack (Core.Maybe Core.Text)
stack_roleARN = Lens.lens (\Stack' {roleARN} -> roleARN) (\s@Stack' {} a -> s {roleARN = a} :: Stack)

-- | The time the stack was deleted.
stack_deletionTime :: Lens.Lens' Stack (Core.Maybe Core.UTCTime)
stack_deletionTime = Lens.lens (\Stack' {deletionTime} -> deletionTime) (\s@Stack' {} a -> s {deletionTime = a} :: Stack) Core.. Lens.mapping Core._Time

-- | The capabilities allowed in the stack.
stack_capabilities :: Lens.Lens' Stack (Core.Maybe [Capability])
stack_capabilities = Lens.lens (\Stack' {capabilities} -> capabilities) (\s@Stack' {} a -> s {capabilities = a} :: Stack) Core.. Lens.mapping Lens._Coerce

-- | Success\/failure message associated with the stack status.
stack_stackStatusReason :: Lens.Lens' Stack (Core.Maybe Core.Text)
stack_stackStatusReason = Lens.lens (\Stack' {stackStatusReason} -> stackStatusReason) (\s@Stack' {} a -> s {stackStatusReason = a} :: Stack)

-- | Whether termination protection is enabled for the stack.
--
-- For
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks>,
-- termination protection is set on the root stack and cannot be changed
-- directly on the nested stack. For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted>
-- in the /AWS CloudFormation User Guide/.
stack_enableTerminationProtection :: Lens.Lens' Stack (Core.Maybe Core.Bool)
stack_enableTerminationProtection = Lens.lens (\Stack' {enableTerminationProtection} -> enableTerminationProtection) (\s@Stack' {} a -> s {enableTerminationProtection = a} :: Stack)

-- | Unique identifier of the stack.
stack_stackId :: Lens.Lens' Stack (Core.Maybe Core.Text)
stack_stackId = Lens.lens (\Stack' {stackId} -> stackId) (\s@Stack' {} a -> s {stackId = a} :: Stack)

-- | SNS topic ARNs to which stack related events are published.
stack_notificationARNs :: Lens.Lens' Stack (Core.Maybe [Core.Text])
stack_notificationARNs = Lens.lens (\Stack' {notificationARNs} -> notificationARNs) (\s@Stack' {} a -> s {notificationARNs = a} :: Stack) Core.. Lens.mapping Lens._Coerce

-- | For nested stacks--stacks created as resources for another stack--the
-- stack ID of the top-level stack to which the nested stack ultimately
-- belongs.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
-- in the /AWS CloudFormation User Guide/.
stack_rootId :: Lens.Lens' Stack (Core.Maybe Core.Text)
stack_rootId = Lens.lens (\Stack' {rootId} -> rootId) (\s@Stack' {} a -> s {rootId = a} :: Stack)

-- | A list of @Tag@s that specify information about the stack.
stack_tags :: Lens.Lens' Stack (Core.Maybe [Tag])
stack_tags = Lens.lens (\Stack' {tags} -> tags) (\s@Stack' {} a -> s {tags = a} :: Stack) Core.. Lens.mapping Lens._Coerce

-- | The unique ID of the change set.
stack_changeSetId :: Lens.Lens' Stack (Core.Maybe Core.Text)
stack_changeSetId = Lens.lens (\Stack' {changeSetId} -> changeSetId) (\s@Stack' {} a -> s {changeSetId = a} :: Stack)

-- | The amount of time within which stack creation should complete.
stack_timeoutInMinutes :: Lens.Lens' Stack (Core.Maybe Core.Natural)
stack_timeoutInMinutes = Lens.lens (\Stack' {timeoutInMinutes} -> timeoutInMinutes) (\s@Stack' {} a -> s {timeoutInMinutes = a} :: Stack)

-- | For nested stacks--stacks created as resources for another stack--the
-- stack ID of the direct parent of this stack. For the first level of
-- nested stacks, the root stack is also the parent stack.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
-- in the /AWS CloudFormation User Guide/.
stack_parentId :: Lens.Lens' Stack (Core.Maybe Core.Text)
stack_parentId = Lens.lens (\Stack' {parentId} -> parentId) (\s@Stack' {} a -> s {parentId = a} :: Stack)

-- | The rollback triggers for AWS CloudFormation to monitor during stack
-- creation and updating operations, and for the specified monitoring
-- period afterwards.
stack_rollbackConfiguration :: Lens.Lens' Stack (Core.Maybe RollbackConfiguration)
stack_rollbackConfiguration = Lens.lens (\Stack' {rollbackConfiguration} -> rollbackConfiguration) (\s@Stack' {} a -> s {rollbackConfiguration = a} :: Stack)

-- | A user-defined description associated with the stack.
stack_description :: Lens.Lens' Stack (Core.Maybe Core.Text)
stack_description = Lens.lens (\Stack' {description} -> description) (\s@Stack' {} a -> s {description = a} :: Stack)

-- | Boolean to enable or disable rollback on stack creation failures:
--
-- -   @true@: disable rollback
--
-- -   @false@: enable rollback
stack_disableRollback :: Lens.Lens' Stack (Core.Maybe Core.Bool)
stack_disableRollback = Lens.lens (\Stack' {disableRollback} -> disableRollback) (\s@Stack' {} a -> s {disableRollback = a} :: Stack)

-- | A list of @Parameter@ structures.
stack_parameters :: Lens.Lens' Stack (Core.Maybe [Parameter])
stack_parameters = Lens.lens (\Stack' {parameters} -> parameters) (\s@Stack' {} a -> s {parameters = a} :: Stack) Core.. Lens.mapping Lens._Coerce

-- | The time the stack was last updated. This field will only be returned if
-- the stack has been updated at least once.
stack_lastUpdatedTime :: Lens.Lens' Stack (Core.Maybe Core.UTCTime)
stack_lastUpdatedTime = Lens.lens (\Stack' {lastUpdatedTime} -> lastUpdatedTime) (\s@Stack' {} a -> s {lastUpdatedTime = a} :: Stack) Core.. Lens.mapping Core._Time

-- | The name associated with the stack.
stack_stackName :: Lens.Lens' Stack Core.Text
stack_stackName = Lens.lens (\Stack' {stackName} -> stackName) (\s@Stack' {} a -> s {stackName = a} :: Stack)

-- | The time at which the stack was created.
stack_creationTime :: Lens.Lens' Stack Core.UTCTime
stack_creationTime = Lens.lens (\Stack' {creationTime} -> creationTime) (\s@Stack' {} a -> s {creationTime = a} :: Stack) Core.. Core._Time

-- | Current status of the stack.
stack_stackStatus :: Lens.Lens' Stack StackStatus
stack_stackStatus = Lens.lens (\Stack' {stackStatus} -> stackStatus) (\s@Stack' {} a -> s {stackStatus = a} :: Stack)

instance Core.FromXML Stack where
  parseXML x =
    Stack'
      Core.<$> ( x Core..@? "Outputs" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "DriftInformation")
      Core.<*> (x Core..@? "RoleARN")
      Core.<*> (x Core..@? "DeletionTime")
      Core.<*> ( x Core..@? "Capabilities" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "StackStatusReason")
      Core.<*> (x Core..@? "EnableTerminationProtection")
      Core.<*> (x Core..@? "StackId")
      Core.<*> ( x Core..@? "NotificationARNs" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "RootId")
      Core.<*> ( x Core..@? "Tags" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "ChangeSetId")
      Core.<*> (x Core..@? "TimeoutInMinutes")
      Core.<*> (x Core..@? "ParentId")
      Core.<*> (x Core..@? "RollbackConfiguration")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "DisableRollback")
      Core.<*> ( x Core..@? "Parameters" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "LastUpdatedTime")
      Core.<*> (x Core..@ "StackName")
      Core.<*> (x Core..@ "CreationTime")
      Core.<*> (x Core..@ "StackStatus")

instance Core.Hashable Stack

instance Core.NFData Stack
