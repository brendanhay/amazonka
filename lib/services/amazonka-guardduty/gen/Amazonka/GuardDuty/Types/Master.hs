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
-- Module      : Amazonka.GuardDuty.Types.Master
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.Master where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the administrator account and invitation.
--
-- /See:/ 'newMaster' smart constructor.
data Master = Master'
  { -- | The timestamp when the invitation was sent.
    invitedAt :: Prelude.Maybe Prelude.Text,
    -- | The status of the relationship between the administrator and member
    -- accounts.
    relationshipStatus :: Prelude.Maybe Prelude.Text,
    -- | The value used to validate the administrator account to the member
    -- account.
    invitationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the account used as the administrator account.
    accountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Master' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invitedAt', 'master_invitedAt' - The timestamp when the invitation was sent.
--
-- 'relationshipStatus', 'master_relationshipStatus' - The status of the relationship between the administrator and member
-- accounts.
--
-- 'invitationId', 'master_invitationId' - The value used to validate the administrator account to the member
-- account.
--
-- 'accountId', 'master_accountId' - The ID of the account used as the administrator account.
newMaster ::
  Master
newMaster =
  Master'
    { invitedAt = Prelude.Nothing,
      relationshipStatus = Prelude.Nothing,
      invitationId = Prelude.Nothing,
      accountId = Prelude.Nothing
    }

-- | The timestamp when the invitation was sent.
master_invitedAt :: Lens.Lens' Master (Prelude.Maybe Prelude.Text)
master_invitedAt = Lens.lens (\Master' {invitedAt} -> invitedAt) (\s@Master' {} a -> s {invitedAt = a} :: Master)

-- | The status of the relationship between the administrator and member
-- accounts.
master_relationshipStatus :: Lens.Lens' Master (Prelude.Maybe Prelude.Text)
master_relationshipStatus = Lens.lens (\Master' {relationshipStatus} -> relationshipStatus) (\s@Master' {} a -> s {relationshipStatus = a} :: Master)

-- | The value used to validate the administrator account to the member
-- account.
master_invitationId :: Lens.Lens' Master (Prelude.Maybe Prelude.Text)
master_invitationId = Lens.lens (\Master' {invitationId} -> invitationId) (\s@Master' {} a -> s {invitationId = a} :: Master)

-- | The ID of the account used as the administrator account.
master_accountId :: Lens.Lens' Master (Prelude.Maybe Prelude.Text)
master_accountId = Lens.lens (\Master' {accountId} -> accountId) (\s@Master' {} a -> s {accountId = a} :: Master)

instance Core.FromJSON Master where
  parseJSON =
    Core.withObject
      "Master"
      ( \x ->
          Master'
            Prelude.<$> (x Core..:? "invitedAt")
            Prelude.<*> (x Core..:? "relationshipStatus")
            Prelude.<*> (x Core..:? "invitationId")
            Prelude.<*> (x Core..:? "accountId")
      )

instance Prelude.Hashable Master where
  hashWithSalt _salt Master' {..} =
    _salt `Prelude.hashWithSalt` invitedAt
      `Prelude.hashWithSalt` relationshipStatus
      `Prelude.hashWithSalt` invitationId
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData Master where
  rnf Master' {..} =
    Prelude.rnf invitedAt
      `Prelude.seq` Prelude.rnf relationshipStatus
      `Prelude.seq` Prelude.rnf invitationId
      `Prelude.seq` Prelude.rnf accountId
