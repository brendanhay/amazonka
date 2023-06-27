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
-- Module      : Amazonka.Inspector2.Types.MemberAccountEc2DeepInspectionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.MemberAccountEc2DeepInspectionStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains details about the status of Amazon Inspector
-- deep inspection for a member account in your organization.
--
-- /See:/ 'newMemberAccountEc2DeepInspectionStatus' smart constructor.
data MemberAccountEc2DeepInspectionStatus = MemberAccountEc2DeepInspectionStatus'
  { -- | The unique identifier for the Amazon Web Services account of the
    -- organization member.
    accountId :: Prelude.Text,
    -- | Whether Amazon Inspector deep inspection is active in the account. If
    -- @TRUE@ Amazon Inspector deep inspection is active, if @FALSE@ it is not
    -- active.
    activateDeepInspection :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberAccountEc2DeepInspectionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'memberAccountEc2DeepInspectionStatus_accountId' - The unique identifier for the Amazon Web Services account of the
-- organization member.
--
-- 'activateDeepInspection', 'memberAccountEc2DeepInspectionStatus_activateDeepInspection' - Whether Amazon Inspector deep inspection is active in the account. If
-- @TRUE@ Amazon Inspector deep inspection is active, if @FALSE@ it is not
-- active.
newMemberAccountEc2DeepInspectionStatus ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'activateDeepInspection'
  Prelude.Bool ->
  MemberAccountEc2DeepInspectionStatus
newMemberAccountEc2DeepInspectionStatus
  pAccountId_
  pActivateDeepInspection_ =
    MemberAccountEc2DeepInspectionStatus'
      { accountId =
          pAccountId_,
        activateDeepInspection =
          pActivateDeepInspection_
      }

-- | The unique identifier for the Amazon Web Services account of the
-- organization member.
memberAccountEc2DeepInspectionStatus_accountId :: Lens.Lens' MemberAccountEc2DeepInspectionStatus Prelude.Text
memberAccountEc2DeepInspectionStatus_accountId = Lens.lens (\MemberAccountEc2DeepInspectionStatus' {accountId} -> accountId) (\s@MemberAccountEc2DeepInspectionStatus' {} a -> s {accountId = a} :: MemberAccountEc2DeepInspectionStatus)

-- | Whether Amazon Inspector deep inspection is active in the account. If
-- @TRUE@ Amazon Inspector deep inspection is active, if @FALSE@ it is not
-- active.
memberAccountEc2DeepInspectionStatus_activateDeepInspection :: Lens.Lens' MemberAccountEc2DeepInspectionStatus Prelude.Bool
memberAccountEc2DeepInspectionStatus_activateDeepInspection = Lens.lens (\MemberAccountEc2DeepInspectionStatus' {activateDeepInspection} -> activateDeepInspection) (\s@MemberAccountEc2DeepInspectionStatus' {} a -> s {activateDeepInspection = a} :: MemberAccountEc2DeepInspectionStatus)

instance
  Prelude.Hashable
    MemberAccountEc2DeepInspectionStatus
  where
  hashWithSalt
    _salt
    MemberAccountEc2DeepInspectionStatus' {..} =
      _salt
        `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` activateDeepInspection

instance
  Prelude.NFData
    MemberAccountEc2DeepInspectionStatus
  where
  rnf MemberAccountEc2DeepInspectionStatus' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf activateDeepInspection

instance
  Data.ToJSON
    MemberAccountEc2DeepInspectionStatus
  where
  toJSON MemberAccountEc2DeepInspectionStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("accountId" Data..= accountId),
            Prelude.Just
              ( "activateDeepInspection"
                  Data..= activateDeepInspection
              )
          ]
      )
