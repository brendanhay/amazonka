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
-- Module      : Amazonka.CloudFormation.Types.Stack
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.Stack where

import Amazonka.CloudFormation.Types.Capability
import Amazonka.CloudFormation.Types.Output
import Amazonka.CloudFormation.Types.Parameter
import Amazonka.CloudFormation.Types.RollbackConfiguration
import Amazonka.CloudFormation.Types.StackDriftInformation
import Amazonka.CloudFormation.Types.StackStatus
import Amazonka.CloudFormation.Types.Tag
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Stack data type.
--
-- /See:/ 'newStack' smart constructor.
data Stack = Stack'
  { -- | The capabilities allowed in the stack.
    capabilities :: Prelude.Maybe [Capability],
    -- | The unique ID of the change set.
    changeSetId :: Prelude.Maybe Prelude.Text,
    -- | The time the stack was deleted.
    deletionTime :: Prelude.Maybe Data.ISO8601,
    -- | A user-defined description associated with the stack.
    description :: Prelude.Maybe Prelude.Text,
    -- | Boolean to enable or disable rollback on stack creation failures:
    --
    -- -   @true@: disable rollback.
    --
    -- -   @false@: enable rollback.
    disableRollback :: Prelude.Maybe Prelude.Bool,
    -- | Information about whether a stack\'s actual configuration differs, or
    -- has /drifted/, from it\'s expected configuration, as defined in the
    -- stack template and any values specified as template parameters. For more
    -- information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
    driftInformation :: Prelude.Maybe StackDriftInformation,
    -- | Whether termination protection is enabled for the stack.
    --
    -- For
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks>,
    -- termination protection is set on the root stack and can\'t be changed
    -- directly on the nested stack. For more information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted>
    -- in the /CloudFormation User Guide/.
    enableTerminationProtection :: Prelude.Maybe Prelude.Bool,
    -- | The time the stack was last updated. This field will only be returned if
    -- the stack has been updated at least once.
    lastUpdatedTime :: Prelude.Maybe Data.ISO8601,
    -- | Amazon SNS topic Amazon Resource Names (ARNs) to which stack related
    -- events are published.
    notificationARNs :: Prelude.Maybe [Prelude.Text],
    -- | A list of output structures.
    outputs :: Prelude.Maybe [Output],
    -- | A list of @Parameter@ structures.
    parameters :: Prelude.Maybe [Parameter],
    -- | For nested stacks--stacks created as resources for another stack--the
    -- stack ID of the direct parent of this stack. For the first level of
    -- nested stacks, the root stack is also the parent stack.
    --
    -- For more information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
    -- in the /CloudFormation User Guide/.
    parentId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an Identity and Access Management
    -- (IAM) role that\'s associated with the stack. During a stack operation,
    -- CloudFormation uses this role\'s credentials to make calls on your
    -- behalf.
    roleARN :: Prelude.Maybe Prelude.Text,
    -- | The rollback triggers for CloudFormation to monitor during stack
    -- creation and updating operations, and for the specified monitoring
    -- period afterwards.
    rollbackConfiguration :: Prelude.Maybe RollbackConfiguration,
    -- | For nested stacks--stacks created as resources for another stack--the
    -- stack ID of the top-level stack to which the nested stack ultimately
    -- belongs.
    --
    -- For more information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
    -- in the /CloudFormation User Guide/.
    rootId :: Prelude.Maybe Prelude.Text,
    -- | Unique identifier of the stack.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | Success\/failure message associated with the stack status.
    stackStatusReason :: Prelude.Maybe Prelude.Text,
    -- | A list of @Tag@s that specify information about the stack.
    tags :: Prelude.Maybe [Tag],
    -- | The amount of time within which stack creation should complete.
    timeoutInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The name associated with the stack.
    stackName :: Prelude.Text,
    -- | The time at which the stack was created.
    creationTime :: Data.ISO8601,
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
-- 'capabilities', 'stack_capabilities' - The capabilities allowed in the stack.
--
-- 'changeSetId', 'stack_changeSetId' - The unique ID of the change set.
--
-- 'deletionTime', 'stack_deletionTime' - The time the stack was deleted.
--
-- 'description', 'stack_description' - A user-defined description associated with the stack.
--
-- 'disableRollback', 'stack_disableRollback' - Boolean to enable or disable rollback on stack creation failures:
--
-- -   @true@: disable rollback.
--
-- -   @false@: enable rollback.
--
-- 'driftInformation', 'stack_driftInformation' - Information about whether a stack\'s actual configuration differs, or
-- has /drifted/, from it\'s expected configuration, as defined in the
-- stack template and any values specified as template parameters. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
--
-- 'enableTerminationProtection', 'stack_enableTerminationProtection' - Whether termination protection is enabled for the stack.
--
-- For
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks>,
-- termination protection is set on the root stack and can\'t be changed
-- directly on the nested stack. For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted>
-- in the /CloudFormation User Guide/.
--
-- 'lastUpdatedTime', 'stack_lastUpdatedTime' - The time the stack was last updated. This field will only be returned if
-- the stack has been updated at least once.
--
-- 'notificationARNs', 'stack_notificationARNs' - Amazon SNS topic Amazon Resource Names (ARNs) to which stack related
-- events are published.
--
-- 'outputs', 'stack_outputs' - A list of output structures.
--
-- 'parameters', 'stack_parameters' - A list of @Parameter@ structures.
--
-- 'parentId', 'stack_parentId' - For nested stacks--stacks created as resources for another stack--the
-- stack ID of the direct parent of this stack. For the first level of
-- nested stacks, the root stack is also the parent stack.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
-- in the /CloudFormation User Guide/.
--
-- 'roleARN', 'stack_roleARN' - The Amazon Resource Name (ARN) of an Identity and Access Management
-- (IAM) role that\'s associated with the stack. During a stack operation,
-- CloudFormation uses this role\'s credentials to make calls on your
-- behalf.
--
-- 'rollbackConfiguration', 'stack_rollbackConfiguration' - The rollback triggers for CloudFormation to monitor during stack
-- creation and updating operations, and for the specified monitoring
-- period afterwards.
--
-- 'rootId', 'stack_rootId' - For nested stacks--stacks created as resources for another stack--the
-- stack ID of the top-level stack to which the nested stack ultimately
-- belongs.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
-- in the /CloudFormation User Guide/.
--
-- 'stackId', 'stack_stackId' - Unique identifier of the stack.
--
-- 'stackStatusReason', 'stack_stackStatusReason' - Success\/failure message associated with the stack status.
--
-- 'tags', 'stack_tags' - A list of @Tag@s that specify information about the stack.
--
-- 'timeoutInMinutes', 'stack_timeoutInMinutes' - The amount of time within which stack creation should complete.
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
    { capabilities = Prelude.Nothing,
      changeSetId = Prelude.Nothing,
      deletionTime = Prelude.Nothing,
      description = Prelude.Nothing,
      disableRollback = Prelude.Nothing,
      driftInformation = Prelude.Nothing,
      enableTerminationProtection = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      notificationARNs = Prelude.Nothing,
      outputs = Prelude.Nothing,
      parameters = Prelude.Nothing,
      parentId = Prelude.Nothing,
      roleARN = Prelude.Nothing,
      rollbackConfiguration = Prelude.Nothing,
      rootId = Prelude.Nothing,
      stackId = Prelude.Nothing,
      stackStatusReason = Prelude.Nothing,
      tags = Prelude.Nothing,
      timeoutInMinutes = Prelude.Nothing,
      stackName = pStackName_,
      creationTime = Data._Time Lens.# pCreationTime_,
      stackStatus = pStackStatus_
    }

-- | The capabilities allowed in the stack.
stack_capabilities :: Lens.Lens' Stack (Prelude.Maybe [Capability])
stack_capabilities = Lens.lens (\Stack' {capabilities} -> capabilities) (\s@Stack' {} a -> s {capabilities = a} :: Stack) Prelude.. Lens.mapping Lens.coerced

-- | The unique ID of the change set.
stack_changeSetId :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_changeSetId = Lens.lens (\Stack' {changeSetId} -> changeSetId) (\s@Stack' {} a -> s {changeSetId = a} :: Stack)

-- | The time the stack was deleted.
stack_deletionTime :: Lens.Lens' Stack (Prelude.Maybe Prelude.UTCTime)
stack_deletionTime = Lens.lens (\Stack' {deletionTime} -> deletionTime) (\s@Stack' {} a -> s {deletionTime = a} :: Stack) Prelude.. Lens.mapping Data._Time

-- | A user-defined description associated with the stack.
stack_description :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_description = Lens.lens (\Stack' {description} -> description) (\s@Stack' {} a -> s {description = a} :: Stack)

-- | Boolean to enable or disable rollback on stack creation failures:
--
-- -   @true@: disable rollback.
--
-- -   @false@: enable rollback.
stack_disableRollback :: Lens.Lens' Stack (Prelude.Maybe Prelude.Bool)
stack_disableRollback = Lens.lens (\Stack' {disableRollback} -> disableRollback) (\s@Stack' {} a -> s {disableRollback = a} :: Stack)

-- | Information about whether a stack\'s actual configuration differs, or
-- has /drifted/, from it\'s expected configuration, as defined in the
-- stack template and any values specified as template parameters. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift.html Detecting Unregulated Configuration Changes to Stacks and Resources>.
stack_driftInformation :: Lens.Lens' Stack (Prelude.Maybe StackDriftInformation)
stack_driftInformation = Lens.lens (\Stack' {driftInformation} -> driftInformation) (\s@Stack' {} a -> s {driftInformation = a} :: Stack)

-- | Whether termination protection is enabled for the stack.
--
-- For
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html nested stacks>,
-- termination protection is set on the root stack and can\'t be changed
-- directly on the nested stack. For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html Protecting a Stack From Being Deleted>
-- in the /CloudFormation User Guide/.
stack_enableTerminationProtection :: Lens.Lens' Stack (Prelude.Maybe Prelude.Bool)
stack_enableTerminationProtection = Lens.lens (\Stack' {enableTerminationProtection} -> enableTerminationProtection) (\s@Stack' {} a -> s {enableTerminationProtection = a} :: Stack)

-- | The time the stack was last updated. This field will only be returned if
-- the stack has been updated at least once.
stack_lastUpdatedTime :: Lens.Lens' Stack (Prelude.Maybe Prelude.UTCTime)
stack_lastUpdatedTime = Lens.lens (\Stack' {lastUpdatedTime} -> lastUpdatedTime) (\s@Stack' {} a -> s {lastUpdatedTime = a} :: Stack) Prelude.. Lens.mapping Data._Time

-- | Amazon SNS topic Amazon Resource Names (ARNs) to which stack related
-- events are published.
stack_notificationARNs :: Lens.Lens' Stack (Prelude.Maybe [Prelude.Text])
stack_notificationARNs = Lens.lens (\Stack' {notificationARNs} -> notificationARNs) (\s@Stack' {} a -> s {notificationARNs = a} :: Stack) Prelude.. Lens.mapping Lens.coerced

-- | A list of output structures.
stack_outputs :: Lens.Lens' Stack (Prelude.Maybe [Output])
stack_outputs = Lens.lens (\Stack' {outputs} -> outputs) (\s@Stack' {} a -> s {outputs = a} :: Stack) Prelude.. Lens.mapping Lens.coerced

-- | A list of @Parameter@ structures.
stack_parameters :: Lens.Lens' Stack (Prelude.Maybe [Parameter])
stack_parameters = Lens.lens (\Stack' {parameters} -> parameters) (\s@Stack' {} a -> s {parameters = a} :: Stack) Prelude.. Lens.mapping Lens.coerced

-- | For nested stacks--stacks created as resources for another stack--the
-- stack ID of the direct parent of this stack. For the first level of
-- nested stacks, the root stack is also the parent stack.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
-- in the /CloudFormation User Guide/.
stack_parentId :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_parentId = Lens.lens (\Stack' {parentId} -> parentId) (\s@Stack' {} a -> s {parentId = a} :: Stack)

-- | The Amazon Resource Name (ARN) of an Identity and Access Management
-- (IAM) role that\'s associated with the stack. During a stack operation,
-- CloudFormation uses this role\'s credentials to make calls on your
-- behalf.
stack_roleARN :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_roleARN = Lens.lens (\Stack' {roleARN} -> roleARN) (\s@Stack' {} a -> s {roleARN = a} :: Stack)

-- | The rollback triggers for CloudFormation to monitor during stack
-- creation and updating operations, and for the specified monitoring
-- period afterwards.
stack_rollbackConfiguration :: Lens.Lens' Stack (Prelude.Maybe RollbackConfiguration)
stack_rollbackConfiguration = Lens.lens (\Stack' {rollbackConfiguration} -> rollbackConfiguration) (\s@Stack' {} a -> s {rollbackConfiguration = a} :: Stack)

-- | For nested stacks--stacks created as resources for another stack--the
-- stack ID of the top-level stack to which the nested stack ultimately
-- belongs.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html Working with Nested Stacks>
-- in the /CloudFormation User Guide/.
stack_rootId :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_rootId = Lens.lens (\Stack' {rootId} -> rootId) (\s@Stack' {} a -> s {rootId = a} :: Stack)

-- | Unique identifier of the stack.
stack_stackId :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_stackId = Lens.lens (\Stack' {stackId} -> stackId) (\s@Stack' {} a -> s {stackId = a} :: Stack)

-- | Success\/failure message associated with the stack status.
stack_stackStatusReason :: Lens.Lens' Stack (Prelude.Maybe Prelude.Text)
stack_stackStatusReason = Lens.lens (\Stack' {stackStatusReason} -> stackStatusReason) (\s@Stack' {} a -> s {stackStatusReason = a} :: Stack)

-- | A list of @Tag@s that specify information about the stack.
stack_tags :: Lens.Lens' Stack (Prelude.Maybe [Tag])
stack_tags = Lens.lens (\Stack' {tags} -> tags) (\s@Stack' {} a -> s {tags = a} :: Stack) Prelude.. Lens.mapping Lens.coerced

-- | The amount of time within which stack creation should complete.
stack_timeoutInMinutes :: Lens.Lens' Stack (Prelude.Maybe Prelude.Natural)
stack_timeoutInMinutes = Lens.lens (\Stack' {timeoutInMinutes} -> timeoutInMinutes) (\s@Stack' {} a -> s {timeoutInMinutes = a} :: Stack)

-- | The name associated with the stack.
stack_stackName :: Lens.Lens' Stack Prelude.Text
stack_stackName = Lens.lens (\Stack' {stackName} -> stackName) (\s@Stack' {} a -> s {stackName = a} :: Stack)

-- | The time at which the stack was created.
stack_creationTime :: Lens.Lens' Stack Prelude.UTCTime
stack_creationTime = Lens.lens (\Stack' {creationTime} -> creationTime) (\s@Stack' {} a -> s {creationTime = a} :: Stack) Prelude.. Data._Time

-- | Current status of the stack.
stack_stackStatus :: Lens.Lens' Stack StackStatus
stack_stackStatus = Lens.lens (\Stack' {stackStatus} -> stackStatus) (\s@Stack' {} a -> s {stackStatus = a} :: Stack)

instance Data.FromXML Stack where
  parseXML x =
    Stack'
      Prelude.<$> ( x
                      Data..@? "Capabilities"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "ChangeSetId")
      Prelude.<*> (x Data..@? "DeletionTime")
      Prelude.<*> (x Data..@? "Description")
      Prelude.<*> (x Data..@? "DisableRollback")
      Prelude.<*> (x Data..@? "DriftInformation")
      Prelude.<*> (x Data..@? "EnableTerminationProtection")
      Prelude.<*> (x Data..@? "LastUpdatedTime")
      Prelude.<*> ( x
                      Data..@? "NotificationARNs"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "Outputs"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> ( x
                      Data..@? "Parameters"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "ParentId")
      Prelude.<*> (x Data..@? "RoleARN")
      Prelude.<*> (x Data..@? "RollbackConfiguration")
      Prelude.<*> (x Data..@? "RootId")
      Prelude.<*> (x Data..@? "StackId")
      Prelude.<*> (x Data..@? "StackStatusReason")
      Prelude.<*> ( x
                      Data..@? "Tags"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "TimeoutInMinutes")
      Prelude.<*> (x Data..@ "StackName")
      Prelude.<*> (x Data..@ "CreationTime")
      Prelude.<*> (x Data..@ "StackStatus")

instance Prelude.Hashable Stack where
  hashWithSalt _salt Stack' {..} =
    _salt
      `Prelude.hashWithSalt` capabilities
      `Prelude.hashWithSalt` changeSetId
      `Prelude.hashWithSalt` deletionTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` disableRollback
      `Prelude.hashWithSalt` driftInformation
      `Prelude.hashWithSalt` enableTerminationProtection
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` notificationARNs
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` parentId
      `Prelude.hashWithSalt` roleARN
      `Prelude.hashWithSalt` rollbackConfiguration
      `Prelude.hashWithSalt` rootId
      `Prelude.hashWithSalt` stackId
      `Prelude.hashWithSalt` stackStatusReason
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timeoutInMinutes
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` stackStatus

instance Prelude.NFData Stack where
  rnf Stack' {..} =
    Prelude.rnf capabilities
      `Prelude.seq` Prelude.rnf changeSetId
      `Prelude.seq` Prelude.rnf deletionTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf disableRollback
      `Prelude.seq` Prelude.rnf driftInformation
      `Prelude.seq` Prelude.rnf enableTerminationProtection
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf notificationARNs
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf parentId
      `Prelude.seq` Prelude.rnf roleARN
      `Prelude.seq` Prelude.rnf rollbackConfiguration
      `Prelude.seq` Prelude.rnf rootId
      `Prelude.seq` Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf stackStatusReason
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf timeoutInMinutes
      `Prelude.seq` Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf stackStatus
