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
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetOperationSummary where

import Network.AWS.CloudFormation.Types.StackSetOperationAction
import Network.AWS.CloudFormation.Types.StackSetOperationStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The structures that contain summary information about the specified
-- operation.
--
-- /See:/ 'newStackSetOperationSummary' smart constructor.
data StackSetOperationSummary = StackSetOperationSummary'
  { -- | The time at which the operation was initiated. Note that the creation
    -- times for the stack set operation might differ from the creation time of
    -- the individual stacks themselves. This is because AWS CloudFormation
    -- needs to perform preparatory work for the operation, such as dispatching
    -- the work to the requested Regions, before actually creating the first
    -- stacks.
    creationTimestamp :: Core.Maybe Core.ISO8601,
    -- | The overall status of the operation.
    --
    -- -   @FAILED@: The operation exceeded the specified failure tolerance.
    --     The failure tolerance value that you\'ve set for an operation is
    --     applied for each Region during stack create and update operations.
    --     If the number of failed stacks within a Region exceeds the failure
    --     tolerance, the status of the operation in the Region is set to
    --     @FAILED@. This in turn sets the status of the operation as a whole
    --     to @FAILED@, and AWS CloudFormation cancels the operation in any
    --     remaining Regions.
    --
    -- -   @QUEUED@: [Service-managed permissions] For automatic deployments
    --     that require a sequence of operations, the operation is queued to be
    --     performed. For more information, see the
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-status-codes stack set operation status codes>
    --     in the AWS CloudFormation User Guide.
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
    status :: Core.Maybe StackSetOperationStatus,
    -- | The time at which the stack set operation ended, across all accounts and
    -- Regions specified. Note that this doesn\'t necessarily mean that the
    -- stack set operation was successful, or even attempted, in each account
    -- or Region.
    endTimestamp :: Core.Maybe Core.ISO8601,
    -- | The unique ID of the stack set operation.
    operationId :: Core.Maybe Core.Text,
    -- | The type of operation: @CREATE@, @UPDATE@, or @DELETE@. Create and
    -- delete operations affect only the specified stack instances that are
    -- associated with the specified stack set. Update operations affect both
    -- the stack set itself as well as /all/ associated stack set instances.
    action :: Core.Maybe StackSetOperationAction
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StackSetOperationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'stackSetOperationSummary_creationTimestamp' - The time at which the operation was initiated. Note that the creation
-- times for the stack set operation might differ from the creation time of
-- the individual stacks themselves. This is because AWS CloudFormation
-- needs to perform preparatory work for the operation, such as dispatching
-- the work to the requested Regions, before actually creating the first
-- stacks.
--
-- 'status', 'stackSetOperationSummary_status' - The overall status of the operation.
--
-- -   @FAILED@: The operation exceeded the specified failure tolerance.
--     The failure tolerance value that you\'ve set for an operation is
--     applied for each Region during stack create and update operations.
--     If the number of failed stacks within a Region exceeds the failure
--     tolerance, the status of the operation in the Region is set to
--     @FAILED@. This in turn sets the status of the operation as a whole
--     to @FAILED@, and AWS CloudFormation cancels the operation in any
--     remaining Regions.
--
-- -   @QUEUED@: [Service-managed permissions] For automatic deployments
--     that require a sequence of operations, the operation is queued to be
--     performed. For more information, see the
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-status-codes stack set operation status codes>
--     in the AWS CloudFormation User Guide.
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
-- 'endTimestamp', 'stackSetOperationSummary_endTimestamp' - The time at which the stack set operation ended, across all accounts and
-- Regions specified. Note that this doesn\'t necessarily mean that the
-- stack set operation was successful, or even attempted, in each account
-- or Region.
--
-- 'operationId', 'stackSetOperationSummary_operationId' - The unique ID of the stack set operation.
--
-- 'action', 'stackSetOperationSummary_action' - The type of operation: @CREATE@, @UPDATE@, or @DELETE@. Create and
-- delete operations affect only the specified stack instances that are
-- associated with the specified stack set. Update operations affect both
-- the stack set itself as well as /all/ associated stack set instances.
newStackSetOperationSummary ::
  StackSetOperationSummary
newStackSetOperationSummary =
  StackSetOperationSummary'
    { creationTimestamp =
        Core.Nothing,
      status = Core.Nothing,
      endTimestamp = Core.Nothing,
      operationId = Core.Nothing,
      action = Core.Nothing
    }

-- | The time at which the operation was initiated. Note that the creation
-- times for the stack set operation might differ from the creation time of
-- the individual stacks themselves. This is because AWS CloudFormation
-- needs to perform preparatory work for the operation, such as dispatching
-- the work to the requested Regions, before actually creating the first
-- stacks.
stackSetOperationSummary_creationTimestamp :: Lens.Lens' StackSetOperationSummary (Core.Maybe Core.UTCTime)
stackSetOperationSummary_creationTimestamp = Lens.lens (\StackSetOperationSummary' {creationTimestamp} -> creationTimestamp) (\s@StackSetOperationSummary' {} a -> s {creationTimestamp = a} :: StackSetOperationSummary) Core.. Lens.mapping Core._Time

-- | The overall status of the operation.
--
-- -   @FAILED@: The operation exceeded the specified failure tolerance.
--     The failure tolerance value that you\'ve set for an operation is
--     applied for each Region during stack create and update operations.
--     If the number of failed stacks within a Region exceeds the failure
--     tolerance, the status of the operation in the Region is set to
--     @FAILED@. This in turn sets the status of the operation as a whole
--     to @FAILED@, and AWS CloudFormation cancels the operation in any
--     remaining Regions.
--
-- -   @QUEUED@: [Service-managed permissions] For automatic deployments
--     that require a sequence of operations, the operation is queued to be
--     performed. For more information, see the
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-status-codes stack set operation status codes>
--     in the AWS CloudFormation User Guide.
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
stackSetOperationSummary_status :: Lens.Lens' StackSetOperationSummary (Core.Maybe StackSetOperationStatus)
stackSetOperationSummary_status = Lens.lens (\StackSetOperationSummary' {status} -> status) (\s@StackSetOperationSummary' {} a -> s {status = a} :: StackSetOperationSummary)

-- | The time at which the stack set operation ended, across all accounts and
-- Regions specified. Note that this doesn\'t necessarily mean that the
-- stack set operation was successful, or even attempted, in each account
-- or Region.
stackSetOperationSummary_endTimestamp :: Lens.Lens' StackSetOperationSummary (Core.Maybe Core.UTCTime)
stackSetOperationSummary_endTimestamp = Lens.lens (\StackSetOperationSummary' {endTimestamp} -> endTimestamp) (\s@StackSetOperationSummary' {} a -> s {endTimestamp = a} :: StackSetOperationSummary) Core.. Lens.mapping Core._Time

-- | The unique ID of the stack set operation.
stackSetOperationSummary_operationId :: Lens.Lens' StackSetOperationSummary (Core.Maybe Core.Text)
stackSetOperationSummary_operationId = Lens.lens (\StackSetOperationSummary' {operationId} -> operationId) (\s@StackSetOperationSummary' {} a -> s {operationId = a} :: StackSetOperationSummary)

-- | The type of operation: @CREATE@, @UPDATE@, or @DELETE@. Create and
-- delete operations affect only the specified stack instances that are
-- associated with the specified stack set. Update operations affect both
-- the stack set itself as well as /all/ associated stack set instances.
stackSetOperationSummary_action :: Lens.Lens' StackSetOperationSummary (Core.Maybe StackSetOperationAction)
stackSetOperationSummary_action = Lens.lens (\StackSetOperationSummary' {action} -> action) (\s@StackSetOperationSummary' {} a -> s {action = a} :: StackSetOperationSummary)

instance Core.FromXML StackSetOperationSummary where
  parseXML x =
    StackSetOperationSummary'
      Core.<$> (x Core..@? "CreationTimestamp")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "EndTimestamp")
      Core.<*> (x Core..@? "OperationId")
      Core.<*> (x Core..@? "Action")

instance Core.Hashable StackSetOperationSummary

instance Core.NFData StackSetOperationSummary
