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
-- Module      : Amazonka.Inspector2.Types.MemberAccountEc2DeepInspectionStatusState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.MemberAccountEc2DeepInspectionStatusState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.Ec2DeepInspectionStatus
import qualified Amazonka.Prelude as Prelude

-- | An object that contains details about the state of Amazon Inspector deep
-- inspection for a member account.
--
-- /See:/ 'newMemberAccountEc2DeepInspectionStatusState' smart constructor.
data MemberAccountEc2DeepInspectionStatusState = MemberAccountEc2DeepInspectionStatusState'
  { -- | The error message explaining why the account failed to activate Amazon
    -- Inspector deep inspection.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The state of Amazon Inspector deep inspection in the member account.
    status :: Prelude.Maybe Ec2DeepInspectionStatus,
    -- | The unique identifier for the Amazon Web Services account of the
    -- organization member
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberAccountEc2DeepInspectionStatusState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'memberAccountEc2DeepInspectionStatusState_errorMessage' - The error message explaining why the account failed to activate Amazon
-- Inspector deep inspection.
--
-- 'status', 'memberAccountEc2DeepInspectionStatusState_status' - The state of Amazon Inspector deep inspection in the member account.
--
-- 'accountId', 'memberAccountEc2DeepInspectionStatusState_accountId' - The unique identifier for the Amazon Web Services account of the
-- organization member
newMemberAccountEc2DeepInspectionStatusState ::
  -- | 'accountId'
  Prelude.Text ->
  MemberAccountEc2DeepInspectionStatusState
newMemberAccountEc2DeepInspectionStatusState
  pAccountId_ =
    MemberAccountEc2DeepInspectionStatusState'
      { errorMessage =
          Prelude.Nothing,
        status = Prelude.Nothing,
        accountId = pAccountId_
      }

-- | The error message explaining why the account failed to activate Amazon
-- Inspector deep inspection.
memberAccountEc2DeepInspectionStatusState_errorMessage :: Lens.Lens' MemberAccountEc2DeepInspectionStatusState (Prelude.Maybe Prelude.Text)
memberAccountEc2DeepInspectionStatusState_errorMessage = Lens.lens (\MemberAccountEc2DeepInspectionStatusState' {errorMessage} -> errorMessage) (\s@MemberAccountEc2DeepInspectionStatusState' {} a -> s {errorMessage = a} :: MemberAccountEc2DeepInspectionStatusState)

-- | The state of Amazon Inspector deep inspection in the member account.
memberAccountEc2DeepInspectionStatusState_status :: Lens.Lens' MemberAccountEc2DeepInspectionStatusState (Prelude.Maybe Ec2DeepInspectionStatus)
memberAccountEc2DeepInspectionStatusState_status = Lens.lens (\MemberAccountEc2DeepInspectionStatusState' {status} -> status) (\s@MemberAccountEc2DeepInspectionStatusState' {} a -> s {status = a} :: MemberAccountEc2DeepInspectionStatusState)

-- | The unique identifier for the Amazon Web Services account of the
-- organization member
memberAccountEc2DeepInspectionStatusState_accountId :: Lens.Lens' MemberAccountEc2DeepInspectionStatusState Prelude.Text
memberAccountEc2DeepInspectionStatusState_accountId = Lens.lens (\MemberAccountEc2DeepInspectionStatusState' {accountId} -> accountId) (\s@MemberAccountEc2DeepInspectionStatusState' {} a -> s {accountId = a} :: MemberAccountEc2DeepInspectionStatusState)

instance
  Data.FromJSON
    MemberAccountEc2DeepInspectionStatusState
  where
  parseJSON =
    Data.withObject
      "MemberAccountEc2DeepInspectionStatusState"
      ( \x ->
          MemberAccountEc2DeepInspectionStatusState'
            Prelude.<$> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..: "accountId")
      )

instance
  Prelude.Hashable
    MemberAccountEc2DeepInspectionStatusState
  where
  hashWithSalt
    _salt
    MemberAccountEc2DeepInspectionStatusState' {..} =
      _salt
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` accountId

instance
  Prelude.NFData
    MemberAccountEc2DeepInspectionStatusState
  where
  rnf MemberAccountEc2DeepInspectionStatusState' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf accountId
