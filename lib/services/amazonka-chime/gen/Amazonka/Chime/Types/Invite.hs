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
-- Module      : Amazonka.Chime.Types.Invite
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.Invite where

import Amazonka.Chime.Types.EmailStatus
import Amazonka.Chime.Types.InviteStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Invitation object returned after emailing users to invite them to join
-- the Amazon Chime @Team@ account.
--
-- /See:/ 'newInvite' smart constructor.
data Invite = Invite'
  { -- | The status of the invite email.
    emailStatus :: Prelude.Maybe EmailStatus,
    -- | The status of the invite.
    status :: Prelude.Maybe InviteStatus,
    -- | The email address to which the invite is sent.
    emailAddress :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The invite ID.
    inviteId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Invite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailStatus', 'invite_emailStatus' - The status of the invite email.
--
-- 'status', 'invite_status' - The status of the invite.
--
-- 'emailAddress', 'invite_emailAddress' - The email address to which the invite is sent.
--
-- 'inviteId', 'invite_inviteId' - The invite ID.
newInvite ::
  Invite
newInvite =
  Invite'
    { emailStatus = Prelude.Nothing,
      status = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      inviteId = Prelude.Nothing
    }

-- | The status of the invite email.
invite_emailStatus :: Lens.Lens' Invite (Prelude.Maybe EmailStatus)
invite_emailStatus = Lens.lens (\Invite' {emailStatus} -> emailStatus) (\s@Invite' {} a -> s {emailStatus = a} :: Invite)

-- | The status of the invite.
invite_status :: Lens.Lens' Invite (Prelude.Maybe InviteStatus)
invite_status = Lens.lens (\Invite' {status} -> status) (\s@Invite' {} a -> s {status = a} :: Invite)

-- | The email address to which the invite is sent.
invite_emailAddress :: Lens.Lens' Invite (Prelude.Maybe Prelude.Text)
invite_emailAddress = Lens.lens (\Invite' {emailAddress} -> emailAddress) (\s@Invite' {} a -> s {emailAddress = a} :: Invite) Prelude.. Lens.mapping Data._Sensitive

-- | The invite ID.
invite_inviteId :: Lens.Lens' Invite (Prelude.Maybe Prelude.Text)
invite_inviteId = Lens.lens (\Invite' {inviteId} -> inviteId) (\s@Invite' {} a -> s {inviteId = a} :: Invite)

instance Data.FromJSON Invite where
  parseJSON =
    Data.withObject
      "Invite"
      ( \x ->
          Invite'
            Prelude.<$> (x Data..:? "EmailStatus")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "EmailAddress")
            Prelude.<*> (x Data..:? "InviteId")
      )

instance Prelude.Hashable Invite where
  hashWithSalt _salt Invite' {..} =
    _salt `Prelude.hashWithSalt` emailStatus
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` inviteId

instance Prelude.NFData Invite where
  rnf Invite' {..} =
    Prelude.rnf emailStatus
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf inviteId
