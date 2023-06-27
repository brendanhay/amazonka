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
-- Module      : Amazonka.Inspector2.Types.FailedMemberAccountEc2DeepInspectionStatusState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.FailedMemberAccountEc2DeepInspectionStatusState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.Status
import qualified Amazonka.Prelude as Prelude

-- | An object that contains details about a member account in your
-- organization that failed to activate Amazon Inspector deep inspection.
--
-- /See:/ 'newFailedMemberAccountEc2DeepInspectionStatusState' smart constructor.
data FailedMemberAccountEc2DeepInspectionStatusState = FailedMemberAccountEc2DeepInspectionStatusState'
  { -- | The status of EC2 scanning in the account that failed to activate Amazon
    -- Inspector deep inspection.
    ec2ScanStatus :: Prelude.Maybe Status,
    -- | The error message explaining why the account failed to activate Amazon
    -- Inspector deep inspection.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the Amazon Web Services account of the
    -- organization member that failed to activate Amazon Inspector deep
    -- inspection.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedMemberAccountEc2DeepInspectionStatusState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ec2ScanStatus', 'failedMemberAccountEc2DeepInspectionStatusState_ec2ScanStatus' - The status of EC2 scanning in the account that failed to activate Amazon
-- Inspector deep inspection.
--
-- 'errorMessage', 'failedMemberAccountEc2DeepInspectionStatusState_errorMessage' - The error message explaining why the account failed to activate Amazon
-- Inspector deep inspection.
--
-- 'accountId', 'failedMemberAccountEc2DeepInspectionStatusState_accountId' - The unique identifier for the Amazon Web Services account of the
-- organization member that failed to activate Amazon Inspector deep
-- inspection.
newFailedMemberAccountEc2DeepInspectionStatusState ::
  -- | 'accountId'
  Prelude.Text ->
  FailedMemberAccountEc2DeepInspectionStatusState
newFailedMemberAccountEc2DeepInspectionStatusState
  pAccountId_ =
    FailedMemberAccountEc2DeepInspectionStatusState'
      { ec2ScanStatus =
          Prelude.Nothing,
        errorMessage =
          Prelude.Nothing,
        accountId = pAccountId_
      }

-- | The status of EC2 scanning in the account that failed to activate Amazon
-- Inspector deep inspection.
failedMemberAccountEc2DeepInspectionStatusState_ec2ScanStatus :: Lens.Lens' FailedMemberAccountEc2DeepInspectionStatusState (Prelude.Maybe Status)
failedMemberAccountEc2DeepInspectionStatusState_ec2ScanStatus = Lens.lens (\FailedMemberAccountEc2DeepInspectionStatusState' {ec2ScanStatus} -> ec2ScanStatus) (\s@FailedMemberAccountEc2DeepInspectionStatusState' {} a -> s {ec2ScanStatus = a} :: FailedMemberAccountEc2DeepInspectionStatusState)

-- | The error message explaining why the account failed to activate Amazon
-- Inspector deep inspection.
failedMemberAccountEc2DeepInspectionStatusState_errorMessage :: Lens.Lens' FailedMemberAccountEc2DeepInspectionStatusState (Prelude.Maybe Prelude.Text)
failedMemberAccountEc2DeepInspectionStatusState_errorMessage = Lens.lens (\FailedMemberAccountEc2DeepInspectionStatusState' {errorMessage} -> errorMessage) (\s@FailedMemberAccountEc2DeepInspectionStatusState' {} a -> s {errorMessage = a} :: FailedMemberAccountEc2DeepInspectionStatusState)

-- | The unique identifier for the Amazon Web Services account of the
-- organization member that failed to activate Amazon Inspector deep
-- inspection.
failedMemberAccountEc2DeepInspectionStatusState_accountId :: Lens.Lens' FailedMemberAccountEc2DeepInspectionStatusState Prelude.Text
failedMemberAccountEc2DeepInspectionStatusState_accountId = Lens.lens (\FailedMemberAccountEc2DeepInspectionStatusState' {accountId} -> accountId) (\s@FailedMemberAccountEc2DeepInspectionStatusState' {} a -> s {accountId = a} :: FailedMemberAccountEc2DeepInspectionStatusState)

instance
  Data.FromJSON
    FailedMemberAccountEc2DeepInspectionStatusState
  where
  parseJSON =
    Data.withObject
      "FailedMemberAccountEc2DeepInspectionStatusState"
      ( \x ->
          FailedMemberAccountEc2DeepInspectionStatusState'
            Prelude.<$> (x Data..:? "ec2ScanStatus")
            Prelude.<*> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..: "accountId")
      )

instance
  Prelude.Hashable
    FailedMemberAccountEc2DeepInspectionStatusState
  where
  hashWithSalt
    _salt
    FailedMemberAccountEc2DeepInspectionStatusState' {..} =
      _salt
        `Prelude.hashWithSalt` ec2ScanStatus
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` accountId

instance
  Prelude.NFData
    FailedMemberAccountEc2DeepInspectionStatusState
  where
  rnf
    FailedMemberAccountEc2DeepInspectionStatusState' {..} =
      Prelude.rnf ec2ScanStatus
        `Prelude.seq` Prelude.rnf errorMessage
        `Prelude.seq` Prelude.rnf accountId
