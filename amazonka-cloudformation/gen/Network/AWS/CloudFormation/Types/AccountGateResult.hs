{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudFormation.Types.AccountGateResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.AccountGateResult where

import Network.AWS.CloudFormation.Types.AccountGateStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Structure that contains the results of the account gate function which
-- AWS CloudFormation invokes, if present, before proceeding with a stack
-- set operation in an account and Region.
--
-- For each account and Region, AWS CloudFormation lets you specify a
-- Lamdba function that encapsulates any requirements that must be met
-- before CloudFormation can proceed with a stack set operation in that
-- account and Region. CloudFormation invokes the function each time a
-- stack set operation is requested for that account and Region; if the
-- function returns @FAILED@, CloudFormation cancels the operation in that
-- account and Region, and sets the stack set operation result status for
-- that account and Region to @FAILED@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-account-gating.html Configuring a target account gate>.
--
-- /See:/ 'newAccountGateResult' smart constructor.
data AccountGateResult = AccountGateResult'
  { -- | The status of the account gate function.
    --
    -- -   @SUCCEEDED@: The account gate function has determined that the
    --     account and Region passes any requirements for a stack set operation
    --     to occur. AWS CloudFormation proceeds with the stack operation in
    --     that account and Region.
    --
    -- -   @FAILED@: The account gate function has determined that the account
    --     and Region does not meet the requirements for a stack set operation
    --     to occur. AWS CloudFormation cancels the stack set operation in that
    --     account and Region, and sets the stack set operation result status
    --     for that account and Region to @FAILED@.
    --
    -- -   @SKIPPED@: AWS CloudFormation has skipped calling the account gate
    --     function for this account and Region, for one of the following
    --     reasons:
    --
    --     -   An account gate function has not been specified for the account
    --         and Region. AWS CloudFormation proceeds with the stack set
    --         operation in this account and Region.
    --
    --     -   The @AWSCloudFormationStackSetExecutionRole@ of the stack set
    --         adminstration account lacks permissions to invoke the function.
    --         AWS CloudFormation proceeds with the stack set operation in this
    --         account and Region.
    --
    --     -   Either no action is necessary, or no action is possible, on the
    --         stack. AWS CloudFormation skips the stack set operation in this
    --         account and Region.
    status :: Prelude.Maybe AccountGateStatus,
    -- | The reason for the account gate status assigned to this account and
    -- Region for the stack set operation.
    statusReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AccountGateResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'accountGateResult_status' - The status of the account gate function.
--
-- -   @SUCCEEDED@: The account gate function has determined that the
--     account and Region passes any requirements for a stack set operation
--     to occur. AWS CloudFormation proceeds with the stack operation in
--     that account and Region.
--
-- -   @FAILED@: The account gate function has determined that the account
--     and Region does not meet the requirements for a stack set operation
--     to occur. AWS CloudFormation cancels the stack set operation in that
--     account and Region, and sets the stack set operation result status
--     for that account and Region to @FAILED@.
--
-- -   @SKIPPED@: AWS CloudFormation has skipped calling the account gate
--     function for this account and Region, for one of the following
--     reasons:
--
--     -   An account gate function has not been specified for the account
--         and Region. AWS CloudFormation proceeds with the stack set
--         operation in this account and Region.
--
--     -   The @AWSCloudFormationStackSetExecutionRole@ of the stack set
--         adminstration account lacks permissions to invoke the function.
--         AWS CloudFormation proceeds with the stack set operation in this
--         account and Region.
--
--     -   Either no action is necessary, or no action is possible, on the
--         stack. AWS CloudFormation skips the stack set operation in this
--         account and Region.
--
-- 'statusReason', 'accountGateResult_statusReason' - The reason for the account gate status assigned to this account and
-- Region for the stack set operation.
newAccountGateResult ::
  AccountGateResult
newAccountGateResult =
  AccountGateResult'
    { status = Prelude.Nothing,
      statusReason = Prelude.Nothing
    }

-- | The status of the account gate function.
--
-- -   @SUCCEEDED@: The account gate function has determined that the
--     account and Region passes any requirements for a stack set operation
--     to occur. AWS CloudFormation proceeds with the stack operation in
--     that account and Region.
--
-- -   @FAILED@: The account gate function has determined that the account
--     and Region does not meet the requirements for a stack set operation
--     to occur. AWS CloudFormation cancels the stack set operation in that
--     account and Region, and sets the stack set operation result status
--     for that account and Region to @FAILED@.
--
-- -   @SKIPPED@: AWS CloudFormation has skipped calling the account gate
--     function for this account and Region, for one of the following
--     reasons:
--
--     -   An account gate function has not been specified for the account
--         and Region. AWS CloudFormation proceeds with the stack set
--         operation in this account and Region.
--
--     -   The @AWSCloudFormationStackSetExecutionRole@ of the stack set
--         adminstration account lacks permissions to invoke the function.
--         AWS CloudFormation proceeds with the stack set operation in this
--         account and Region.
--
--     -   Either no action is necessary, or no action is possible, on the
--         stack. AWS CloudFormation skips the stack set operation in this
--         account and Region.
accountGateResult_status :: Lens.Lens' AccountGateResult (Prelude.Maybe AccountGateStatus)
accountGateResult_status = Lens.lens (\AccountGateResult' {status} -> status) (\s@AccountGateResult' {} a -> s {status = a} :: AccountGateResult)

-- | The reason for the account gate status assigned to this account and
-- Region for the stack set operation.
accountGateResult_statusReason :: Lens.Lens' AccountGateResult (Prelude.Maybe Prelude.Text)
accountGateResult_statusReason = Lens.lens (\AccountGateResult' {statusReason} -> statusReason) (\s@AccountGateResult' {} a -> s {statusReason = a} :: AccountGateResult)

instance Prelude.FromXML AccountGateResult where
  parseXML x =
    AccountGateResult'
      Prelude.<$> (x Prelude..@? "Status")
      Prelude.<*> (x Prelude..@? "StatusReason")

instance Prelude.Hashable AccountGateResult

instance Prelude.NFData AccountGateResult
