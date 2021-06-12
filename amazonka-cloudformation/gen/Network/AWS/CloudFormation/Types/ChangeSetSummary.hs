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
-- Module      : Network.AWS.CloudFormation.Types.ChangeSetSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ChangeSetSummary where

import Network.AWS.CloudFormation.Types.ChangeSetStatus
import Network.AWS.CloudFormation.Types.ExecutionStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The @ChangeSetSummary@ structure describes a change set, its status, and
-- the stack with which it\'s associated.
--
-- /See:/ 'newChangeSetSummary' smart constructor.
data ChangeSetSummary = ChangeSetSummary'
  { -- | The root change set ID.
    rootChangeSetId :: Core.Maybe Core.Text,
    -- | The state of the change set, such as @CREATE_IN_PROGRESS@,
    -- @CREATE_COMPLETE@, or @FAILED@.
    status :: Core.Maybe ChangeSetStatus,
    -- | The start time when the change set was created, in UTC.
    creationTime :: Core.Maybe Core.ISO8601,
    -- | Specifies the current setting of @IncludeNestedStacks@ for the change
    -- set.
    includeNestedStacks :: Core.Maybe Core.Bool,
    -- | The name of the stack with which the change set is associated.
    stackName :: Core.Maybe Core.Text,
    -- | If the change set execution status is @AVAILABLE@, you can execute the
    -- change set. If you can’t execute the change set, the status indicates
    -- why. For example, a change set might be in an @UNAVAILABLE@ state
    -- because AWS CloudFormation is still creating it or in an @OBSOLETE@
    -- state because the stack was already updated.
    executionStatus :: Core.Maybe ExecutionStatus,
    -- | The ID of the stack with which the change set is associated.
    stackId :: Core.Maybe Core.Text,
    -- | The parent change set ID.
    parentChangeSetId :: Core.Maybe Core.Text,
    -- | The ID of the change set.
    changeSetId :: Core.Maybe Core.Text,
    -- | Descriptive information about the change set.
    description :: Core.Maybe Core.Text,
    -- | The name of the change set.
    changeSetName :: Core.Maybe Core.Text,
    -- | A description of the change set\'s status. For example, if your change
    -- set is in the @FAILED@ state, AWS CloudFormation shows the error
    -- message.
    statusReason :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ChangeSetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rootChangeSetId', 'changeSetSummary_rootChangeSetId' - The root change set ID.
--
-- 'status', 'changeSetSummary_status' - The state of the change set, such as @CREATE_IN_PROGRESS@,
-- @CREATE_COMPLETE@, or @FAILED@.
--
-- 'creationTime', 'changeSetSummary_creationTime' - The start time when the change set was created, in UTC.
--
-- 'includeNestedStacks', 'changeSetSummary_includeNestedStacks' - Specifies the current setting of @IncludeNestedStacks@ for the change
-- set.
--
-- 'stackName', 'changeSetSummary_stackName' - The name of the stack with which the change set is associated.
--
-- 'executionStatus', 'changeSetSummary_executionStatus' - If the change set execution status is @AVAILABLE@, you can execute the
-- change set. If you can’t execute the change set, the status indicates
-- why. For example, a change set might be in an @UNAVAILABLE@ state
-- because AWS CloudFormation is still creating it or in an @OBSOLETE@
-- state because the stack was already updated.
--
-- 'stackId', 'changeSetSummary_stackId' - The ID of the stack with which the change set is associated.
--
-- 'parentChangeSetId', 'changeSetSummary_parentChangeSetId' - The parent change set ID.
--
-- 'changeSetId', 'changeSetSummary_changeSetId' - The ID of the change set.
--
-- 'description', 'changeSetSummary_description' - Descriptive information about the change set.
--
-- 'changeSetName', 'changeSetSummary_changeSetName' - The name of the change set.
--
-- 'statusReason', 'changeSetSummary_statusReason' - A description of the change set\'s status. For example, if your change
-- set is in the @FAILED@ state, AWS CloudFormation shows the error
-- message.
newChangeSetSummary ::
  ChangeSetSummary
newChangeSetSummary =
  ChangeSetSummary'
    { rootChangeSetId = Core.Nothing,
      status = Core.Nothing,
      creationTime = Core.Nothing,
      includeNestedStacks = Core.Nothing,
      stackName = Core.Nothing,
      executionStatus = Core.Nothing,
      stackId = Core.Nothing,
      parentChangeSetId = Core.Nothing,
      changeSetId = Core.Nothing,
      description = Core.Nothing,
      changeSetName = Core.Nothing,
      statusReason = Core.Nothing
    }

-- | The root change set ID.
changeSetSummary_rootChangeSetId :: Lens.Lens' ChangeSetSummary (Core.Maybe Core.Text)
changeSetSummary_rootChangeSetId = Lens.lens (\ChangeSetSummary' {rootChangeSetId} -> rootChangeSetId) (\s@ChangeSetSummary' {} a -> s {rootChangeSetId = a} :: ChangeSetSummary)

-- | The state of the change set, such as @CREATE_IN_PROGRESS@,
-- @CREATE_COMPLETE@, or @FAILED@.
changeSetSummary_status :: Lens.Lens' ChangeSetSummary (Core.Maybe ChangeSetStatus)
changeSetSummary_status = Lens.lens (\ChangeSetSummary' {status} -> status) (\s@ChangeSetSummary' {} a -> s {status = a} :: ChangeSetSummary)

-- | The start time when the change set was created, in UTC.
changeSetSummary_creationTime :: Lens.Lens' ChangeSetSummary (Core.Maybe Core.UTCTime)
changeSetSummary_creationTime = Lens.lens (\ChangeSetSummary' {creationTime} -> creationTime) (\s@ChangeSetSummary' {} a -> s {creationTime = a} :: ChangeSetSummary) Core.. Lens.mapping Core._Time

-- | Specifies the current setting of @IncludeNestedStacks@ for the change
-- set.
changeSetSummary_includeNestedStacks :: Lens.Lens' ChangeSetSummary (Core.Maybe Core.Bool)
changeSetSummary_includeNestedStacks = Lens.lens (\ChangeSetSummary' {includeNestedStacks} -> includeNestedStacks) (\s@ChangeSetSummary' {} a -> s {includeNestedStacks = a} :: ChangeSetSummary)

-- | The name of the stack with which the change set is associated.
changeSetSummary_stackName :: Lens.Lens' ChangeSetSummary (Core.Maybe Core.Text)
changeSetSummary_stackName = Lens.lens (\ChangeSetSummary' {stackName} -> stackName) (\s@ChangeSetSummary' {} a -> s {stackName = a} :: ChangeSetSummary)

-- | If the change set execution status is @AVAILABLE@, you can execute the
-- change set. If you can’t execute the change set, the status indicates
-- why. For example, a change set might be in an @UNAVAILABLE@ state
-- because AWS CloudFormation is still creating it or in an @OBSOLETE@
-- state because the stack was already updated.
changeSetSummary_executionStatus :: Lens.Lens' ChangeSetSummary (Core.Maybe ExecutionStatus)
changeSetSummary_executionStatus = Lens.lens (\ChangeSetSummary' {executionStatus} -> executionStatus) (\s@ChangeSetSummary' {} a -> s {executionStatus = a} :: ChangeSetSummary)

-- | The ID of the stack with which the change set is associated.
changeSetSummary_stackId :: Lens.Lens' ChangeSetSummary (Core.Maybe Core.Text)
changeSetSummary_stackId = Lens.lens (\ChangeSetSummary' {stackId} -> stackId) (\s@ChangeSetSummary' {} a -> s {stackId = a} :: ChangeSetSummary)

-- | The parent change set ID.
changeSetSummary_parentChangeSetId :: Lens.Lens' ChangeSetSummary (Core.Maybe Core.Text)
changeSetSummary_parentChangeSetId = Lens.lens (\ChangeSetSummary' {parentChangeSetId} -> parentChangeSetId) (\s@ChangeSetSummary' {} a -> s {parentChangeSetId = a} :: ChangeSetSummary)

-- | The ID of the change set.
changeSetSummary_changeSetId :: Lens.Lens' ChangeSetSummary (Core.Maybe Core.Text)
changeSetSummary_changeSetId = Lens.lens (\ChangeSetSummary' {changeSetId} -> changeSetId) (\s@ChangeSetSummary' {} a -> s {changeSetId = a} :: ChangeSetSummary)

-- | Descriptive information about the change set.
changeSetSummary_description :: Lens.Lens' ChangeSetSummary (Core.Maybe Core.Text)
changeSetSummary_description = Lens.lens (\ChangeSetSummary' {description} -> description) (\s@ChangeSetSummary' {} a -> s {description = a} :: ChangeSetSummary)

-- | The name of the change set.
changeSetSummary_changeSetName :: Lens.Lens' ChangeSetSummary (Core.Maybe Core.Text)
changeSetSummary_changeSetName = Lens.lens (\ChangeSetSummary' {changeSetName} -> changeSetName) (\s@ChangeSetSummary' {} a -> s {changeSetName = a} :: ChangeSetSummary)

-- | A description of the change set\'s status. For example, if your change
-- set is in the @FAILED@ state, AWS CloudFormation shows the error
-- message.
changeSetSummary_statusReason :: Lens.Lens' ChangeSetSummary (Core.Maybe Core.Text)
changeSetSummary_statusReason = Lens.lens (\ChangeSetSummary' {statusReason} -> statusReason) (\s@ChangeSetSummary' {} a -> s {statusReason = a} :: ChangeSetSummary)

instance Core.FromXML ChangeSetSummary where
  parseXML x =
    ChangeSetSummary'
      Core.<$> (x Core..@? "RootChangeSetId")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "CreationTime")
      Core.<*> (x Core..@? "IncludeNestedStacks")
      Core.<*> (x Core..@? "StackName")
      Core.<*> (x Core..@? "ExecutionStatus")
      Core.<*> (x Core..@? "StackId")
      Core.<*> (x Core..@? "ParentChangeSetId")
      Core.<*> (x Core..@? "ChangeSetId")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "ChangeSetName")
      Core.<*> (x Core..@? "StatusReason")

instance Core.Hashable ChangeSetSummary

instance Core.NFData ChangeSetSummary
