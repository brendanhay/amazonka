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
-- Module      : Amazonka.CloudFormation.Types.StackSetOperationResultSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackSetOperationResultSummary where

import Amazonka.CloudFormation.Types.AccountGateResult
import Amazonka.CloudFormation.Types.StackSetOperationResultStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The structure that contains information about a specified operation\'s
-- results for a given account in a given Region.
--
-- /See:/ 'newStackSetOperationResultSummary' smart constructor.
data StackSetOperationResultSummary = StackSetOperationResultSummary'
  { -- | [Self-managed permissions] The name of the Amazon Web Services account
    -- for this operation result.
    account :: Prelude.Maybe Prelude.Text,
    -- | The results of the account gate function CloudFormation invokes, if
    -- present, before proceeding with stack set operations in an account.
    accountGateResult :: Prelude.Maybe AccountGateResult,
    -- | [Service-managed permissions] The organization root ID or organizational
    -- unit (OU) IDs that you specified for
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets>.
    organizationalUnitId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Web Services Region for this operation result.
    region :: Prelude.Maybe Prelude.Text,
    -- | The result status of the stack set operation for the given account in
    -- the given Region.
    --
    -- -   @CANCELLED@: The operation in the specified account and Region has
    --     been canceled. This is either because a user has stopped the stack
    --     set operation, or because the failure tolerance of the stack set
    --     operation has been exceeded.
    --
    -- -   @FAILED@: The operation in the specified account and Region failed.
    --
    --     If the stack set operation fails in enough accounts within a Region,
    --     the failure tolerance for the stack set operation as a whole might
    --     be exceeded.
    --
    -- -   @RUNNING@: The operation in the specified account and Region is
    --     currently in progress.
    --
    -- -   @PENDING@: The operation in the specified account and Region has yet
    --     to start.
    --
    -- -   @SUCCEEDED@: The operation in the specified account and Region
    --     completed successfully.
    status :: Prelude.Maybe StackSetOperationResultStatus,
    -- | The reason for the assigned result status.
    statusReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StackSetOperationResultSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'account', 'stackSetOperationResultSummary_account' - [Self-managed permissions] The name of the Amazon Web Services account
-- for this operation result.
--
-- 'accountGateResult', 'stackSetOperationResultSummary_accountGateResult' - The results of the account gate function CloudFormation invokes, if
-- present, before proceeding with stack set operations in an account.
--
-- 'organizationalUnitId', 'stackSetOperationResultSummary_organizationalUnitId' - [Service-managed permissions] The organization root ID or organizational
-- unit (OU) IDs that you specified for
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets>.
--
-- 'region', 'stackSetOperationResultSummary_region' - The name of the Amazon Web Services Region for this operation result.
--
-- 'status', 'stackSetOperationResultSummary_status' - The result status of the stack set operation for the given account in
-- the given Region.
--
-- -   @CANCELLED@: The operation in the specified account and Region has
--     been canceled. This is either because a user has stopped the stack
--     set operation, or because the failure tolerance of the stack set
--     operation has been exceeded.
--
-- -   @FAILED@: The operation in the specified account and Region failed.
--
--     If the stack set operation fails in enough accounts within a Region,
--     the failure tolerance for the stack set operation as a whole might
--     be exceeded.
--
-- -   @RUNNING@: The operation in the specified account and Region is
--     currently in progress.
--
-- -   @PENDING@: The operation in the specified account and Region has yet
--     to start.
--
-- -   @SUCCEEDED@: The operation in the specified account and Region
--     completed successfully.
--
-- 'statusReason', 'stackSetOperationResultSummary_statusReason' - The reason for the assigned result status.
newStackSetOperationResultSummary ::
  StackSetOperationResultSummary
newStackSetOperationResultSummary =
  StackSetOperationResultSummary'
    { account =
        Prelude.Nothing,
      accountGateResult = Prelude.Nothing,
      organizationalUnitId = Prelude.Nothing,
      region = Prelude.Nothing,
      status = Prelude.Nothing,
      statusReason = Prelude.Nothing
    }

-- | [Self-managed permissions] The name of the Amazon Web Services account
-- for this operation result.
stackSetOperationResultSummary_account :: Lens.Lens' StackSetOperationResultSummary (Prelude.Maybe Prelude.Text)
stackSetOperationResultSummary_account = Lens.lens (\StackSetOperationResultSummary' {account} -> account) (\s@StackSetOperationResultSummary' {} a -> s {account = a} :: StackSetOperationResultSummary)

-- | The results of the account gate function CloudFormation invokes, if
-- present, before proceeding with stack set operations in an account.
stackSetOperationResultSummary_accountGateResult :: Lens.Lens' StackSetOperationResultSummary (Prelude.Maybe AccountGateResult)
stackSetOperationResultSummary_accountGateResult = Lens.lens (\StackSetOperationResultSummary' {accountGateResult} -> accountGateResult) (\s@StackSetOperationResultSummary' {} a -> s {accountGateResult = a} :: StackSetOperationResultSummary)

-- | [Service-managed permissions] The organization root ID or organizational
-- unit (OU) IDs that you specified for
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeploymentTargets.html DeploymentTargets>.
stackSetOperationResultSummary_organizationalUnitId :: Lens.Lens' StackSetOperationResultSummary (Prelude.Maybe Prelude.Text)
stackSetOperationResultSummary_organizationalUnitId = Lens.lens (\StackSetOperationResultSummary' {organizationalUnitId} -> organizationalUnitId) (\s@StackSetOperationResultSummary' {} a -> s {organizationalUnitId = a} :: StackSetOperationResultSummary)

-- | The name of the Amazon Web Services Region for this operation result.
stackSetOperationResultSummary_region :: Lens.Lens' StackSetOperationResultSummary (Prelude.Maybe Prelude.Text)
stackSetOperationResultSummary_region = Lens.lens (\StackSetOperationResultSummary' {region} -> region) (\s@StackSetOperationResultSummary' {} a -> s {region = a} :: StackSetOperationResultSummary)

-- | The result status of the stack set operation for the given account in
-- the given Region.
--
-- -   @CANCELLED@: The operation in the specified account and Region has
--     been canceled. This is either because a user has stopped the stack
--     set operation, or because the failure tolerance of the stack set
--     operation has been exceeded.
--
-- -   @FAILED@: The operation in the specified account and Region failed.
--
--     If the stack set operation fails in enough accounts within a Region,
--     the failure tolerance for the stack set operation as a whole might
--     be exceeded.
--
-- -   @RUNNING@: The operation in the specified account and Region is
--     currently in progress.
--
-- -   @PENDING@: The operation in the specified account and Region has yet
--     to start.
--
-- -   @SUCCEEDED@: The operation in the specified account and Region
--     completed successfully.
stackSetOperationResultSummary_status :: Lens.Lens' StackSetOperationResultSummary (Prelude.Maybe StackSetOperationResultStatus)
stackSetOperationResultSummary_status = Lens.lens (\StackSetOperationResultSummary' {status} -> status) (\s@StackSetOperationResultSummary' {} a -> s {status = a} :: StackSetOperationResultSummary)

-- | The reason for the assigned result status.
stackSetOperationResultSummary_statusReason :: Lens.Lens' StackSetOperationResultSummary (Prelude.Maybe Prelude.Text)
stackSetOperationResultSummary_statusReason = Lens.lens (\StackSetOperationResultSummary' {statusReason} -> statusReason) (\s@StackSetOperationResultSummary' {} a -> s {statusReason = a} :: StackSetOperationResultSummary)

instance Data.FromXML StackSetOperationResultSummary where
  parseXML x =
    StackSetOperationResultSummary'
      Prelude.<$> (x Data..@? "Account")
      Prelude.<*> (x Data..@? "AccountGateResult")
      Prelude.<*> (x Data..@? "OrganizationalUnitId")
      Prelude.<*> (x Data..@? "Region")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "StatusReason")

instance
  Prelude.Hashable
    StackSetOperationResultSummary
  where
  hashWithSalt
    _salt
    StackSetOperationResultSummary' {..} =
      _salt
        `Prelude.hashWithSalt` account
        `Prelude.hashWithSalt` accountGateResult
        `Prelude.hashWithSalt` organizationalUnitId
        `Prelude.hashWithSalt` region
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` statusReason

instance
  Prelude.NFData
    StackSetOperationResultSummary
  where
  rnf StackSetOperationResultSummary' {..} =
    Prelude.rnf account
      `Prelude.seq` Prelude.rnf accountGateResult
      `Prelude.seq` Prelude.rnf organizationalUnitId
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusReason
