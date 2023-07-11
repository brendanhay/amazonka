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
-- Module      : Amazonka.GuardDuty.Types.Administrator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.Administrator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the administrator account and invitation.
--
-- /See:/ 'newAdministrator' smart constructor.
data Administrator = Administrator'
  { -- | The ID of the account used as the administrator account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The value that is used to validate the administrator account to the
    -- member account.
    invitationId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the invitation was sent.
    invitedAt :: Prelude.Maybe Prelude.Text,
    -- | The status of the relationship between the administrator and member
    -- accounts.
    relationshipStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Administrator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'administrator_accountId' - The ID of the account used as the administrator account.
--
-- 'invitationId', 'administrator_invitationId' - The value that is used to validate the administrator account to the
-- member account.
--
-- 'invitedAt', 'administrator_invitedAt' - The timestamp when the invitation was sent.
--
-- 'relationshipStatus', 'administrator_relationshipStatus' - The status of the relationship between the administrator and member
-- accounts.
newAdministrator ::
  Administrator
newAdministrator =
  Administrator'
    { accountId = Prelude.Nothing,
      invitationId = Prelude.Nothing,
      invitedAt = Prelude.Nothing,
      relationshipStatus = Prelude.Nothing
    }

-- | The ID of the account used as the administrator account.
administrator_accountId :: Lens.Lens' Administrator (Prelude.Maybe Prelude.Text)
administrator_accountId = Lens.lens (\Administrator' {accountId} -> accountId) (\s@Administrator' {} a -> s {accountId = a} :: Administrator)

-- | The value that is used to validate the administrator account to the
-- member account.
administrator_invitationId :: Lens.Lens' Administrator (Prelude.Maybe Prelude.Text)
administrator_invitationId = Lens.lens (\Administrator' {invitationId} -> invitationId) (\s@Administrator' {} a -> s {invitationId = a} :: Administrator)

-- | The timestamp when the invitation was sent.
administrator_invitedAt :: Lens.Lens' Administrator (Prelude.Maybe Prelude.Text)
administrator_invitedAt = Lens.lens (\Administrator' {invitedAt} -> invitedAt) (\s@Administrator' {} a -> s {invitedAt = a} :: Administrator)

-- | The status of the relationship between the administrator and member
-- accounts.
administrator_relationshipStatus :: Lens.Lens' Administrator (Prelude.Maybe Prelude.Text)
administrator_relationshipStatus = Lens.lens (\Administrator' {relationshipStatus} -> relationshipStatus) (\s@Administrator' {} a -> s {relationshipStatus = a} :: Administrator)

instance Data.FromJSON Administrator where
  parseJSON =
    Data.withObject
      "Administrator"
      ( \x ->
          Administrator'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "invitationId")
            Prelude.<*> (x Data..:? "invitedAt")
            Prelude.<*> (x Data..:? "relationshipStatus")
      )

instance Prelude.Hashable Administrator where
  hashWithSalt _salt Administrator' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` invitationId
      `Prelude.hashWithSalt` invitedAt
      `Prelude.hashWithSalt` relationshipStatus

instance Prelude.NFData Administrator where
  rnf Administrator' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf invitationId
      `Prelude.seq` Prelude.rnf invitedAt
      `Prelude.seq` Prelude.rnf relationshipStatus
