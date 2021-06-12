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
-- Module      : Network.AWS.GuardDuty.Types.Master
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Master where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the administrator account and invitation.
--
-- /See:/ 'newMaster' smart constructor.
data Master = Master'
  { -- | The ID of the account used as the administrator account.
    accountId :: Core.Maybe Core.Text,
    -- | The status of the relationship between the administrator and member
    -- accounts.
    relationshipStatus :: Core.Maybe Core.Text,
    -- | The value used to validate the administrator account to the member
    -- account.
    invitationId :: Core.Maybe Core.Text,
    -- | The timestamp when the invitation was sent.
    invitedAt :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Master' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'master_accountId' - The ID of the account used as the administrator account.
--
-- 'relationshipStatus', 'master_relationshipStatus' - The status of the relationship between the administrator and member
-- accounts.
--
-- 'invitationId', 'master_invitationId' - The value used to validate the administrator account to the member
-- account.
--
-- 'invitedAt', 'master_invitedAt' - The timestamp when the invitation was sent.
newMaster ::
  Master
newMaster =
  Master'
    { accountId = Core.Nothing,
      relationshipStatus = Core.Nothing,
      invitationId = Core.Nothing,
      invitedAt = Core.Nothing
    }

-- | The ID of the account used as the administrator account.
master_accountId :: Lens.Lens' Master (Core.Maybe Core.Text)
master_accountId = Lens.lens (\Master' {accountId} -> accountId) (\s@Master' {} a -> s {accountId = a} :: Master)

-- | The status of the relationship between the administrator and member
-- accounts.
master_relationshipStatus :: Lens.Lens' Master (Core.Maybe Core.Text)
master_relationshipStatus = Lens.lens (\Master' {relationshipStatus} -> relationshipStatus) (\s@Master' {} a -> s {relationshipStatus = a} :: Master)

-- | The value used to validate the administrator account to the member
-- account.
master_invitationId :: Lens.Lens' Master (Core.Maybe Core.Text)
master_invitationId = Lens.lens (\Master' {invitationId} -> invitationId) (\s@Master' {} a -> s {invitationId = a} :: Master)

-- | The timestamp when the invitation was sent.
master_invitedAt :: Lens.Lens' Master (Core.Maybe Core.Text)
master_invitedAt = Lens.lens (\Master' {invitedAt} -> invitedAt) (\s@Master' {} a -> s {invitedAt = a} :: Master)

instance Core.FromJSON Master where
  parseJSON =
    Core.withObject
      "Master"
      ( \x ->
          Master'
            Core.<$> (x Core..:? "accountId")
            Core.<*> (x Core..:? "relationshipStatus")
            Core.<*> (x Core..:? "invitationId")
            Core.<*> (x Core..:? "invitedAt")
      )

instance Core.Hashable Master

instance Core.NFData Master
