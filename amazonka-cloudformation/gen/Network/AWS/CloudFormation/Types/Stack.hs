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
import qualified Network.AWS.Prelude as Prelude

-- | The Stack data type.
--
-- /See:/ 'newStack' smart constructor.
data Stack = Stack'
  { -- | A list of output structures.
    outputs :: Prelude.Maybe [Output],
    -- | The Amazon Resource Name (ARN) of an Identity and Access Management
    -- (IAM) role that is associated with the stack. During a stack operation,
    -- CloudFormation uses this role\'s credentials to make calls on your
    -- behalf.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The time the stack was deleted.
    deletionTime :: Prelude.Maybe Core.ISO8601,
    -- | Information on whether a stack\'s actual configuration differs, or has
    -- /drifted/, from it\'s expected configuration, as defined in the stack
    -- template and any values specified as template parameters. For more
    -- information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
    driftInformation :: Prelude.Maybe StackDriftInformation,
    -- | Whether termination protection is enabled for the stack.
    --
    -- For
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks>,
    -- termination protection is set on the root stack and cannot be changed
    -- directly on the nested stack. For more information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted>
    -- in the /CloudFormation User Guide/.
    enableTerminationProtection :: Prelude.Maybe Prelude.Bool,
    -- | Success\/failure message associated with the stack status.
    stackStatusReason :: Prelude.Maybe Prelude.Text,
    -- | The capabilities allowed in the stack.
    capabilities :: Prelude.Maybe [Capability],
    -- | Unique identifier of the stack.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | SNS topic ARNs to which stack related events are published.
    notificationARNs :: Prelude.Maybe [Prelude.Text],
    -- | For nested stacks--stacks created as resources for another stack--the
    -- stack ID of the top-level stack to which the nested stack ultimately
    -- belongs.
    --
    -- For more information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
    -- in the /CloudFormation User Guide/.
    rootId :: Prelude.Maybe Prelude.Text,
    -- | A list of @Tag@s that specify information about the stack.
    tags :: Prelude.Maybe [Tag],
    -- | The unique ID of the change set.
    changeSetId :: Prelude.Maybe Prelude.Text,
    -- | The amount of time within which stack creation should complete.
    timeoutInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | For nested stacks--stacks created as resources for another stack--the
    -- stack ID of the direct parent of this stack. For the first level of
    -- nested stacks, the root stack is also the parent stack.
    --
    -- For more information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
    -- in the /CloudFormation User Guide/.
    parentId :: Prelude.Maybe Prelude.Text,
    -- | The rollback triggers for CloudFormation to monitor during stack
    -- creation and updating operations, and for the specified monitoring
    -- period afterwards.
    rollbackConfiguration :: Prelude.Maybe RollbackConfiguration,
    -- | A user-defined description associated with the stack.
    description :: Prelude.Maybe Prelude.Text,
    -- | Boolean to enable or disable rollback on stack creation failures:
    --
    -- -   @true@: disable rollback
    --
    -- -   @false@: enable rollback
    disableRollback :: Prelude.Maybe Prelude.Bool,
    -- | The time the stack was last updated. This field will only be returned if
    -- the stack has been updated at least once.
    lastUpdatedTime :: Prelude.Maybe Core.ISO8601,
    -- | A list of @Parameter@ structures.
    parameters :: Prelude.Maybe [Parameter],
    -- | The name associated with the stack.
    stackName :: Prelude.Text,
    -- | The time at which the stack was created.
    creationTime :: Core.ISO8601,
    -- | Current status of the stack.
    stackStatus :: StackStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'roleARN', 'stack_roleARN' - The Amazon Resource Name (ARN) of an Identity and Access Management
-- (IAM) role that is associated with the stack. During a stack operation,
-- CloudFormation uses this role\'s credentials to make calls on your
-- behalf.
--
-- 'deletionTime', 'stack_deletionTime' - The time the stack was deleted.
--
-- 'driftInformation', 'stack_driftInformation' - Information on whether a stack\'s actual configuration differs, or has
-- /drifted/, from it\'s expected configuration, as defined in the stack
-- template and any values specified as template parameters. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
--
-- 'enableTerminationProtection', 'stack_enableTerminationProtection' - Whether termination protection is enabled for the stack.
--
-- For
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks>,
-- termination protection is set on the root stack and cannot be changed
-- directly on the nested stack. For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted>
-- in the /CloudFormation User Guide/.
--
-- 'stackStatusReason', 'stack_stackStatusReason' - Success\/failure message associated with the stack status.
--
-- 'capabilities', 'stack_capabilities' - The capabilities allowed in the stack.
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
-- in the /CloudFormation User Guide/.
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
-- in the /CloudFormation User Guide/.
--
-- 'rollbackConfiguration', 'stack_rollbackConfiguration' - The rollback triggers for CloudFormation to monitor during stack
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
-- 'lastUpdatedTime', 'stack_lastUpdatedTime' - The time the stack was last updated. This field will only be returned if
-- the stack has been updated at least once.
--
-- 'parameters', 'stack_parameters' - A list of @Parameter@ structures.
--
-- 'stackName', 'stack_stackName' - The name associated with the stack.
--
-- 'creationTime', 'stack_creationTime' - The time at which the stack was created.
--
-- 'stackStatus', 'stack_stackStatus' - Current status of the stack.
newStack ::
  -- | 'stackName'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'stackStatus'
  StackStatus ->
  Stack
newStack pStackName_ pCreationTime_ pStackStatus_ =
  Stack'
    { outputs = Prelude.Nothing,
      roleARN = Prelude.Nothing,
      deletionTime = Prelude.Nothing,
      driftInformation = Prelude.Nothing,
      enableTerminationProtection = Prelude.Nothing,
      stackStatusReason = Prelude.Nothing,
      capabilities = Prelude.Nothing,
      stackId = Prelude.Nothing,
      notificationARNs = Prelude.Nothing,
      rootId = Prelude.Nothing,
      tags = Prelude.Nothing,
      changeSetId = Prelude.Nothing,
      timeoutInMinutes = Prelude.Nothing,
      parentId = Prelude.Nothing,
      rollbackConfiguration = Prelude.Nothing,
      description = Prelude.Nothing,
      disableRollback = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      parameters = Prelude.Nothing,
      stackName = pStackName_,
      creationTime = Core._Time Lens.# pCreationTime_,
      stackStatus = pStackStatus_
    }

-- | A list of output structures.
stack_outputs :: Lens.Lens' Stack (Prelude.Maybe [Output])
stack_outputs = Lens.lens (\Stack' {outputs} -> outputs) (\s@Stack' {} a -> s {outputs = a} :: Stack) Prelude.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of an Identity and Access Management
-- (IAM) role that is associated with the stack. During a stack operation,
-- CloudFormation uses this role\'s credentials to make calls on your
-- behalf.
stack_roleARN :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_roleARN = Lens.lens (\Stack' {roleARN} -> roleARN) (\s@Stack' {} a -> s {roleARN = a} :: Stack)

-- | The time the stack was deleted.
stack_deletionTime :: Lens.Lens' Stack (Prelude.Maybe Prelude.UTCTime)
stack_deletionTime = Lens.lens (\Stack' {deletionTime} -> deletionTime) (\s@Stack' {} a -> s {deletionTime = a} :: Stack) Prelude.. Lens.mapping Core._Time

-- | Information on whether a stack\'s actual configuration differs, or has
-- /drifted/, from it\'s expected configuration, as defined in the stack
-- template and any values specified as template parameters. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
stack_driftInformation :: Lens.Lens' Stack (Prelude.Maybe StackDriftInformation)
stack_driftInformation = Lens.lens (\Stack' {driftInformation} -> driftInformation) (\s@Stack' {} a -> s {driftInformation = a} :: Stack)

-- | Whether termination protection is enabled for the stack.
--
-- For
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks>,
-- termination protection is set on the root stack and cannot be changed
-- directly on the nested stack. For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted>
-- in the /CloudFormation User Guide/.
stack_enableTerminationProtection :: Lens.Lens' Stack (Prelude.Maybe Prelude.Bool)
stack_enableTerminationProtection = Lens.lens (\Stack' {enableTerminationProtection} -> enableTerminationProtection) (\s@Stack' {} a -> s {enableTerminationProtection = a} :: Stack)

-- | Success\/failure message associated with the stack status.
stack_stackStatusReason :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_stackStatusReason = Lens.lens (\Stack' {stackStatusReason} -> stackStatusReason) (\s@Stack' {} a -> s {stackStatusReason = a} :: Stack)

-- | The capabilities allowed in the stack.
stack_capabilities :: Lens.Lens' Stack (Prelude.Maybe [Capability])
stack_capabilities = Lens.lens (\Stack' {capabilities} -> capabilities) (\s@Stack' {} a -> s {capabilities = a} :: Stack) Prelude.. Lens.mapping Lens._Coerce

-- | Unique identifier of the stack.
stack_stackId :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_stackId = Lens.lens (\Stack' {stackId} -> stackId) (\s@Stack' {} a -> s {stackId = a} :: Stack)

-- | SNS topic ARNs to which stack related events are published.
stack_notificationARNs :: Lens.Lens' Stack (Prelude.Maybe [Prelude.Text])
stack_notificationARNs = Lens.lens (\Stack' {notificationARNs} -> notificationARNs) (\s@Stack' {} a -> s {notificationARNs = a} :: Stack) Prelude.. Lens.mapping Lens._Coerce

-- | For nested stacks--stacks created as resources for another stack--the
-- stack ID of the top-level stack to which the nested stack ultimately
-- belongs.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
-- in the /CloudFormation User Guide/.
stack_rootId :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_rootId = Lens.lens (\Stack' {rootId} -> rootId) (\s@Stack' {} a -> s {rootId = a} :: Stack)

-- | A list of @Tag@s that specify information about the stack.
stack_tags :: Lens.Lens' Stack (Prelude.Maybe [Tag])
stack_tags = Lens.lens (\Stack' {tags} -> tags) (\s@Stack' {} a -> s {tags = a} :: Stack) Prelude.. Lens.mapping Lens._Coerce

-- | The unique ID of the change set.
stack_changeSetId :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_changeSetId = Lens.lens (\Stack' {changeSetId} -> changeSetId) (\s@Stack' {} a -> s {changeSetId = a} :: Stack)

-- | The amount of time within which stack creation should complete.
stack_timeoutInMinutes :: Lens.Lens' Stack (Prelude.Maybe Prelude.Natural)
stack_timeoutInMinutes = Lens.lens (\Stack' {timeoutInMinutes} -> timeoutInMinutes) (\s@Stack' {} a -> s {timeoutInMinutes = a} :: Stack)

-- | For nested stacks--stacks created as resources for another stack--the
-- stack ID of the direct parent of this stack. For the first level of
-- nested stacks, the root stack is also the parent stack.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
-- in the /CloudFormation User Guide/.
stack_parentId :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_parentId = Lens.lens (\Stack' {parentId} -> parentId) (\s@Stack' {} a -> s {parentId = a} :: Stack)

-- | The rollback triggers for CloudFormation to monitor during stack
-- creation and updating operations, and for the specified monitoring
-- period afterwards.
stack_rollbackConfiguration :: Lens.Lens' Stack (Prelude.Maybe RollbackConfiguration)
stack_rollbackConfiguration = Lens.lens (\Stack' {rollbackConfiguration} -> rollbackConfiguration) (\s@Stack' {} a -> s {rollbackConfiguration = a} :: Stack)

-- | A user-defined description associated with the stack.
stack_description :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_description = Lens.lens (\Stack' {description} -> description) (\s@Stack' {} a -> s {description = a} :: Stack)

-- | Boolean to enable or disable rollback on stack creation failures:
--
-- -   @true@: disable rollback
--
-- -   @false@: enable rollback
stack_disableRollback :: Lens.Lens' Stack (Prelude.Maybe Prelude.Bool)
stack_disableRollback = Lens.lens (\Stack' {disableRollback} -> disableRollback) (\s@Stack' {} a -> s {disableRollback = a} :: Stack)

-- | The time the stack was last updated. This field will only be returned if
-- the stack has been updated at least once.
stack_lastUpdatedTime :: Lens.Lens' Stack (Prelude.Maybe Prelude.UTCTime)
stack_lastUpdatedTime = Lens.lens (\Stack' {lastUpdatedTime} -> lastUpdatedTime) (\s@Stack' {} a -> s {lastUpdatedTime = a} :: Stack) Prelude.. Lens.mapping Core._Time

-- | A list of @Parameter@ structures.
stack_parameters :: Lens.Lens' Stack (Prelude.Maybe [Parameter])
stack_parameters = Lens.lens (\Stack' {parameters} -> parameters) (\s@Stack' {} a -> s {parameters = a} :: Stack) Prelude.. Lens.mapping Lens._Coerce

-- | The name associated with the stack.
stack_stackName :: Lens.Lens' Stack Prelude.Text
stack_stackName = Lens.lens (\Stack' {stackName} -> stackName) (\s@Stack' {} a -> s {stackName = a} :: Stack)

-- | The time at which the stack was created.
stack_creationTime :: Lens.Lens' Stack Prelude.UTCTime
stack_creationTime = Lens.lens (\Stack' {creationTime} -> creationTime) (\s@Stack' {} a -> s {creationTime = a} :: Stack) Prelude.. Core._Time

-- | Current status of the stack.
stack_stackStatus :: Lens.Lens' Stack StackStatus
stack_stackStatus = Lens.lens (\Stack' {stackStatus} -> stackStatus) (\s@Stack' {} a -> s {stackStatus = a} :: Stack)

instance Core.FromXML Stack where
  parseXML x =
    Stack'
      Prelude.<$> ( x Core..@? "Outputs" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "RoleARN")
      Prelude.<*> (x Core..@? "DeletionTime")
      Prelude.<*> (x Core..@? "DriftInformation")
      Prelude.<*> (x Core..@? "EnableTerminationProtection")
      Prelude.<*> (x Core..@? "StackStatusReason")
      Prelude.<*> ( x Core..@? "Capabilities" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "StackId")
      Prelude.<*> ( x Core..@? "NotificationARNs"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "RootId")
      Prelude.<*> ( x Core..@? "Tags" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "ChangeSetId")
      Prelude.<*> (x Core..@? "TimeoutInMinutes")
      Prelude.<*> (x Core..@? "ParentId")
      Prelude.<*> (x Core..@? "RollbackConfiguration")
      Prelude.<*> (x Core..@? "Description")
      Prelude.<*> (x Core..@? "DisableRollback")
      Prelude.<*> (x Core..@? "LastUpdatedTime")
      Prelude.<*> ( x Core..@? "Parameters" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@ "StackName")
      Prelude.<*> (x Core..@ "CreationTime")
      Prelude.<*> (x Core..@ "StackStatus")

instance Prelude.Hashable Stack

instance Prelude.NFData Stack
