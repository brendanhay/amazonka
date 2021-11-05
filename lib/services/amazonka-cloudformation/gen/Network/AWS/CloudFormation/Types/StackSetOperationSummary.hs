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
-- Module      : Amazonka.CloudFormation.Types.StackSetOperationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackSetOperationSummary where

import Amazonka.CloudFormation.Types.StackSetOperationAction
import Amazonka.CloudFormation.Types.StackSetOperationStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The structures that contain summary information about the specified
-- operation.
--
-- /See:/ 'newStackSetOperationSummary' smart constructor.
data StackSetOperationSummary = StackSetOperationSummary'
  { -- | The overall status of the operation.
    --
    -- -   @FAILED@: The operation exceeded the specified failure tolerance.
    --     The failure tolerance value that you\'ve set for an operation is
    --     applied for each Region during stack create and update operations.
    --     If the number of failed stacks within a Region exceeds the failure
    --     tolerance, the status of the operation in the Region is set to
    --     @FAILED@. This in turn sets the status of the operation as a whole
    --     to @FAILED@, and CloudFormation cancels the operation in any
    --     remaining Regions.
    --
    -- -   @QUEUED@: [Service-managed permissions] For automatic deployments
    --     that require a sequence of operations, the operation is queued to be
    --     performed. For more information, see the
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-status-codes stack set operation status codes>
    --     in the CloudFormation User Guide.
    --
    -- -   @RUNNING@: The operation is currently being performed.
    --
    -- -   @STOPPED@: The user has cancelled the operation.
    --
    -- -   @STOPPING@: The operation is in the process of stopping, at user
    --     request.
    --
    -- -   @SUCCEEDED@: The operation completed creating or updating all the
    --     specified stacks without exceeding the failure tolerance for the
    --     operation.
    status :: Prelude.Maybe StackSetOperationStatus,
    -- | The type of operation: @CREATE@, @UPDATE@, or @DELETE@. Create and
    -- delete operations affect only the specified stack instances that are
    -- associated with the specified stack set. Update operations affect both
    -- the stack set itself as well as /all/ associated stack set instances.
    action :: Prelude.Maybe StackSetOperationAction,
    -- | The time at which the stack set operation ended, across all accounts and
    -- Regions specified. Note that this doesn\'t necessarily mean that the
    -- stack set operation was successful, or even attempted, in each account
    -- or Region.
    endTimestamp :: Prelude.Maybe Core.ISO8601,
    -- | The time at which the operation was initiated. Note that the creation
    -- times for the stack set operation might differ from the creation time of
    -- the individual stacks themselves. This is because CloudFormation needs
    -- to perform preparatory work for the operation, such as dispatching the
    -- work to the requested Regions, before actually creating the first
    -- stacks.
    creationTimestamp :: Prelude.Maybe Core.ISO8601,
    -- | The unique ID of the stack set operation.
    operationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StackSetOperationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'stackSetOperationSummary_status' - The overall status of the operation.
--
-- -   @FAILED@: The operation exceeded the specified failure tolerance.
--     The failure tolerance value that you\'ve set for an operation is
--     applied for each Region during stack create and update operations.
--     If the number of failed stacks within a Region exceeds the failure
--     tolerance, the status of the operation in the Region is set to
--     @FAILED@. This in turn sets the status of the operation as a whole
--     to @FAILED@, and CloudFormation cancels the operation in any
--     remaining Regions.
--
-- -   @QUEUED@: [Service-managed permissions] For automatic deployments
--     that require a sequence of operations, the operation is queued to be
--     performed. For more information, see the
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-status-codes stack set operation status codes>
--     in the CloudFormation User Guide.
--
-- -   @RUNNING@: The operation is currently being performed.
--
-- -   @STOPPED@: The user has cancelled the operation.
--
-- -   @STOPPING@: The operation is in the process of stopping, at user
--     request.
--
-- -   @SUCCEEDED@: The operation completed creating or updating all the
--     specified stacks without exceeding the failure tolerance for the
--     operation.
--
-- 'action', 'stackSetOperationSummary_action' - The type of operation: @CREATE@, @UPDATE@, or @DELETE@. Create and
-- delete operations affect only the specified stack instances that are
-- associated with the specified stack set. Update operations affect both
-- the stack set itself as well as /all/ associated stack set instances.
--
-- 'endTimestamp', 'stackSetOperationSummary_endTimestamp' - The time at which the stack set operation ended, across all accounts and
-- Regions specified. Note that this doesn\'t necessarily mean that the
-- stack set operation was successful, or even attempted, in each account
-- or Region.
--
-- 'creationTimestamp', 'stackSetOperationSummary_creationTimestamp' - The time at which the operation was initiated. Note that the creation
-- times for the stack set operation might differ from the creation time of
-- the individual stacks themselves. This is because CloudFormation needs
-- to perform preparatory work for the operation, such as dispatching the
-- work to the requested Regions, before actually creating the first
-- stacks.
--
-- 'operationId', 'stackSetOperationSummary_operationId' - The unique ID of the stack set operation.
newStackSetOperationSummary ::
  StackSetOperationSummary
newStackSetOperationSummary =
  StackSetOperationSummary'
    { status = Prelude.Nothing,
      action = Prelude.Nothing,
      endTimestamp = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      operationId = Prelude.Nothing
    }

-- | The overall status of the operation.
--
-- -   @FAILED@: The operation exceeded the specified failure tolerance.
--     The failure tolerance value that you\'ve set for an operation is
--     applied for each Region during stack create and update operations.
--     If the number of failed stacks within a Region exceeds the failure
--     tolerance, the status of the operation in the Region is set to
--     @FAILED@. This in turn sets the status of the operation as a whole
--     to @FAILED@, and CloudFormation cancels the operation in any
--     remaining Regions.
--
-- -   @QUEUED@: [Service-managed permissions] For automatic deployments
--     that require a sequence of operations, the operation is queued to be
--     performed. For more information, see the
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-status-codes stack set operation status codes>
--     in the CloudFormation User Guide.
--
-- -   @RUNNING@: The operation is currently being performed.
--
-- -   @STOPPED@: The user has cancelled the operation.
--
-- -   @STOPPING@: The operation is in the process of stopping, at user
--     request.
--
-- -   @SUCCEEDED@: The operation completed creating or updating all the
--     specified stacks without exceeding the failure tolerance for the
--     operation.
stackSetOperationSummary_status :: Lens.Lens' StackSetOperationSummary (Prelude.Maybe StackSetOperationStatus)
stackSetOperationSummary_status = Lens.lens (\StackSetOperationSummary' {status} -> status) (\s@StackSetOperationSummary' {} a -> s {status = a} :: StackSetOperationSummary)

-- | The type of operation: @CREATE@, @UPDATE@, or @DELETE@. Create and
-- delete operations affect only the specified stack instances that are
-- associated with the specified stack set. Update operations affect both
-- the stack set itself as well as /all/ associated stack set instances.
stackSetOperationSummary_action :: Lens.Lens' StackSetOperationSummary (Prelude.Maybe StackSetOperationAction)
stackSetOperationSummary_action = Lens.lens (\StackSetOperationSummary' {action} -> action) (\s@StackSetOperationSummary' {} a -> s {action = a} :: StackSetOperationSummary)

-- | The time at which the stack set operation ended, across all accounts and
-- Regions specified. Note that this doesn\'t necessarily mean that the
-- stack set operation was successful, or even attempted, in each account
-- or Region.
stackSetOperationSummary_endTimestamp :: Lens.Lens' StackSetOperationSummary (Prelude.Maybe Prelude.UTCTime)
stackSetOperationSummary_endTimestamp = Lens.lens (\StackSetOperationSummary' {endTimestamp} -> endTimestamp) (\s@StackSetOperationSummary' {} a -> s {endTimestamp = a} :: StackSetOperationSummary) Prelude.. Lens.mapping Core._Time

-- | The time at which the operation was initiated. Note that the creation
-- times for the stack set operation might differ from the creation time of
-- the individual stacks themselves. This is because CloudFormation needs
-- to perform preparatory work for the operation, such as dispatching the
-- work to the requested Regions, before actually creating the first
-- stacks.
stackSetOperationSummary_creationTimestamp :: Lens.Lens' StackSetOperationSummary (Prelude.Maybe Prelude.UTCTime)
stackSetOperationSummary_creationTimestamp = Lens.lens (\StackSetOperationSummary' {creationTimestamp} -> creationTimestamp) (\s@StackSetOperationSummary' {} a -> s {creationTimestamp = a} :: StackSetOperationSummary) Prelude.. Lens.mapping Core._Time

-- | The unique ID of the stack set operation.
stackSetOperationSummary_operationId :: Lens.Lens' StackSetOperationSummary (Prelude.Maybe Prelude.Text)
stackSetOperationSummary_operationId = Lens.lens (\StackSetOperationSummary' {operationId} -> operationId) (\s@StackSetOperationSummary' {} a -> s {operationId = a} :: StackSetOperationSummary)

instance Core.FromXML StackSetOperationSummary where
  parseXML x =
    StackSetOperationSummary'
      Prelude.<$> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "Action")
      Prelude.<*> (x Core..@? "EndTimestamp")
      Prelude.<*> (x Core..@? "CreationTimestamp")
      Prelude.<*> (x Core..@? "OperationId")

instance Prelude.Hashable StackSetOperationSummary

instance Prelude.NFData StackSetOperationSummary
