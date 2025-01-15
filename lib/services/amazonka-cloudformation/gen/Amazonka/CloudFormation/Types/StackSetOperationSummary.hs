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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackSetOperationSummary where

import Amazonka.CloudFormation.Types.StackSetOperationAction
import Amazonka.CloudFormation.Types.StackSetOperationPreferences
import Amazonka.CloudFormation.Types.StackSetOperationStatus
import Amazonka.CloudFormation.Types.StackSetOperationStatusDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The structures that contain summary information about the specified
-- operation.
--
-- /See:/ 'newStackSetOperationSummary' smart constructor.
data StackSetOperationSummary = StackSetOperationSummary'
  { -- | The type of operation: @CREATE@, @UPDATE@, or @DELETE@. Create and
    -- delete operations affect only the specified stack instances that are
    -- associated with the specified stack set. Update operations affect both
    -- the stack set itself and /all/ associated stack set instances.
    action :: Prelude.Maybe StackSetOperationAction,
    -- | The time at which the operation was initiated. Note that the creation
    -- times for the stack set operation might differ from the creation time of
    -- the individual stacks themselves. This is because CloudFormation needs
    -- to perform preparatory work for the operation, such as dispatching the
    -- work to the requested Regions, before actually creating the first
    -- stacks.
    creationTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The time at which the stack set operation ended, across all accounts and
    -- Regions specified. Note that this doesn\'t necessarily mean that the
    -- stack set operation was successful, or even attempted, in each account
    -- or Region.
    endTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The unique ID of the stack set operation.
    operationId :: Prelude.Maybe Prelude.Text,
    operationPreferences :: Prelude.Maybe StackSetOperationPreferences,
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
    -- -   @STOPPED@: The user has canceled the operation.
    --
    -- -   @STOPPING@: The operation is in the process of stopping, at user
    --     request.
    --
    -- -   @SUCCEEDED@: The operation completed creating or updating all the
    --     specified stacks without exceeding the failure tolerance for the
    --     operation.
    status :: Prelude.Maybe StackSetOperationStatus,
    -- | Detailed information about the stack set operation.
    statusDetails :: Prelude.Maybe StackSetOperationStatusDetails,
    -- | The status of the operation in details.
    statusReason :: Prelude.Maybe Prelude.Text
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
-- 'action', 'stackSetOperationSummary_action' - The type of operation: @CREATE@, @UPDATE@, or @DELETE@. Create and
-- delete operations affect only the specified stack instances that are
-- associated with the specified stack set. Update operations affect both
-- the stack set itself and /all/ associated stack set instances.
--
-- 'creationTimestamp', 'stackSetOperationSummary_creationTimestamp' - The time at which the operation was initiated. Note that the creation
-- times for the stack set operation might differ from the creation time of
-- the individual stacks themselves. This is because CloudFormation needs
-- to perform preparatory work for the operation, such as dispatching the
-- work to the requested Regions, before actually creating the first
-- stacks.
--
-- 'endTimestamp', 'stackSetOperationSummary_endTimestamp' - The time at which the stack set operation ended, across all accounts and
-- Regions specified. Note that this doesn\'t necessarily mean that the
-- stack set operation was successful, or even attempted, in each account
-- or Region.
--
-- 'operationId', 'stackSetOperationSummary_operationId' - The unique ID of the stack set operation.
--
-- 'operationPreferences', 'stackSetOperationSummary_operationPreferences' - Undocumented member.
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
-- -   @STOPPED@: The user has canceled the operation.
--
-- -   @STOPPING@: The operation is in the process of stopping, at user
--     request.
--
-- -   @SUCCEEDED@: The operation completed creating or updating all the
--     specified stacks without exceeding the failure tolerance for the
--     operation.
--
-- 'statusDetails', 'stackSetOperationSummary_statusDetails' - Detailed information about the stack set operation.
--
-- 'statusReason', 'stackSetOperationSummary_statusReason' - The status of the operation in details.
newStackSetOperationSummary ::
  StackSetOperationSummary
newStackSetOperationSummary =
  StackSetOperationSummary'
    { action = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      endTimestamp = Prelude.Nothing,
      operationId = Prelude.Nothing,
      operationPreferences = Prelude.Nothing,
      status = Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      statusReason = Prelude.Nothing
    }

-- | The type of operation: @CREATE@, @UPDATE@, or @DELETE@. Create and
-- delete operations affect only the specified stack instances that are
-- associated with the specified stack set. Update operations affect both
-- the stack set itself and /all/ associated stack set instances.
stackSetOperationSummary_action :: Lens.Lens' StackSetOperationSummary (Prelude.Maybe StackSetOperationAction)
stackSetOperationSummary_action = Lens.lens (\StackSetOperationSummary' {action} -> action) (\s@StackSetOperationSummary' {} a -> s {action = a} :: StackSetOperationSummary)

-- | The time at which the operation was initiated. Note that the creation
-- times for the stack set operation might differ from the creation time of
-- the individual stacks themselves. This is because CloudFormation needs
-- to perform preparatory work for the operation, such as dispatching the
-- work to the requested Regions, before actually creating the first
-- stacks.
stackSetOperationSummary_creationTimestamp :: Lens.Lens' StackSetOperationSummary (Prelude.Maybe Prelude.UTCTime)
stackSetOperationSummary_creationTimestamp = Lens.lens (\StackSetOperationSummary' {creationTimestamp} -> creationTimestamp) (\s@StackSetOperationSummary' {} a -> s {creationTimestamp = a} :: StackSetOperationSummary) Prelude.. Lens.mapping Data._Time

-- | The time at which the stack set operation ended, across all accounts and
-- Regions specified. Note that this doesn\'t necessarily mean that the
-- stack set operation was successful, or even attempted, in each account
-- or Region.
stackSetOperationSummary_endTimestamp :: Lens.Lens' StackSetOperationSummary (Prelude.Maybe Prelude.UTCTime)
stackSetOperationSummary_endTimestamp = Lens.lens (\StackSetOperationSummary' {endTimestamp} -> endTimestamp) (\s@StackSetOperationSummary' {} a -> s {endTimestamp = a} :: StackSetOperationSummary) Prelude.. Lens.mapping Data._Time

-- | The unique ID of the stack set operation.
stackSetOperationSummary_operationId :: Lens.Lens' StackSetOperationSummary (Prelude.Maybe Prelude.Text)
stackSetOperationSummary_operationId = Lens.lens (\StackSetOperationSummary' {operationId} -> operationId) (\s@StackSetOperationSummary' {} a -> s {operationId = a} :: StackSetOperationSummary)

-- | Undocumented member.
stackSetOperationSummary_operationPreferences :: Lens.Lens' StackSetOperationSummary (Prelude.Maybe StackSetOperationPreferences)
stackSetOperationSummary_operationPreferences = Lens.lens (\StackSetOperationSummary' {operationPreferences} -> operationPreferences) (\s@StackSetOperationSummary' {} a -> s {operationPreferences = a} :: StackSetOperationSummary)

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
-- -   @STOPPED@: The user has canceled the operation.
--
-- -   @STOPPING@: The operation is in the process of stopping, at user
--     request.
--
-- -   @SUCCEEDED@: The operation completed creating or updating all the
--     specified stacks without exceeding the failure tolerance for the
--     operation.
stackSetOperationSummary_status :: Lens.Lens' StackSetOperationSummary (Prelude.Maybe StackSetOperationStatus)
stackSetOperationSummary_status = Lens.lens (\StackSetOperationSummary' {status} -> status) (\s@StackSetOperationSummary' {} a -> s {status = a} :: StackSetOperationSummary)

-- | Detailed information about the stack set operation.
stackSetOperationSummary_statusDetails :: Lens.Lens' StackSetOperationSummary (Prelude.Maybe StackSetOperationStatusDetails)
stackSetOperationSummary_statusDetails = Lens.lens (\StackSetOperationSummary' {statusDetails} -> statusDetails) (\s@StackSetOperationSummary' {} a -> s {statusDetails = a} :: StackSetOperationSummary)

-- | The status of the operation in details.
stackSetOperationSummary_statusReason :: Lens.Lens' StackSetOperationSummary (Prelude.Maybe Prelude.Text)
stackSetOperationSummary_statusReason = Lens.lens (\StackSetOperationSummary' {statusReason} -> statusReason) (\s@StackSetOperationSummary' {} a -> s {statusReason = a} :: StackSetOperationSummary)

instance Data.FromXML StackSetOperationSummary where
  parseXML x =
    StackSetOperationSummary'
      Prelude.<$> (x Data..@? "Action")
      Prelude.<*> (x Data..@? "CreationTimestamp")
      Prelude.<*> (x Data..@? "EndTimestamp")
      Prelude.<*> (x Data..@? "OperationId")
      Prelude.<*> (x Data..@? "OperationPreferences")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "StatusDetails")
      Prelude.<*> (x Data..@? "StatusReason")

instance Prelude.Hashable StackSetOperationSummary where
  hashWithSalt _salt StackSetOperationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` endTimestamp
      `Prelude.hashWithSalt` operationId
      `Prelude.hashWithSalt` operationPreferences
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusDetails
      `Prelude.hashWithSalt` statusReason

instance Prelude.NFData StackSetOperationSummary where
  rnf StackSetOperationSummary' {..} =
    Prelude.rnf action `Prelude.seq`
      Prelude.rnf creationTimestamp `Prelude.seq`
        Prelude.rnf endTimestamp `Prelude.seq`
          Prelude.rnf operationId `Prelude.seq`
            Prelude.rnf operationPreferences `Prelude.seq`
              Prelude.rnf status `Prelude.seq`
                Prelude.rnf statusDetails `Prelude.seq`
                  Prelude.rnf statusReason
