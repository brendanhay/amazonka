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
-- Module      : Amazonka.Chime.Types.ChannelMembership
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.ChannelMembership where

import Amazonka.Chime.Types.ChannelMembershipType
import Amazonka.Chime.Types.Identity
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details of a channel member.
--
-- /See:/ 'newChannelMembership' smart constructor.
data ChannelMembership = ChannelMembership'
  { -- | The time at which a channel membership was last updated.
    lastUpdatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The data of the channel member.
    member :: Prelude.Maybe Identity,
    -- | The membership type set for the channel member.
    type' :: Prelude.Maybe ChannelMembershipType,
    -- | The time at which the channel membership was created.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the member\'s channel.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the member who invited another member.
    invitedBy :: Prelude.Maybe Identity
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTimestamp', 'channelMembership_lastUpdatedTimestamp' - The time at which a channel membership was last updated.
--
-- 'member', 'channelMembership_member' - The data of the channel member.
--
-- 'type'', 'channelMembership_type' - The membership type set for the channel member.
--
-- 'createdTimestamp', 'channelMembership_createdTimestamp' - The time at which the channel membership was created.
--
-- 'channelArn', 'channelMembership_channelArn' - The ARN of the member\'s channel.
--
-- 'invitedBy', 'channelMembership_invitedBy' - The identifier of the member who invited another member.
newChannelMembership ::
  ChannelMembership
newChannelMembership =
  ChannelMembership'
    { lastUpdatedTimestamp =
        Prelude.Nothing,
      member = Prelude.Nothing,
      type' = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      channelArn = Prelude.Nothing,
      invitedBy = Prelude.Nothing
    }

-- | The time at which a channel membership was last updated.
channelMembership_lastUpdatedTimestamp :: Lens.Lens' ChannelMembership (Prelude.Maybe Prelude.UTCTime)
channelMembership_lastUpdatedTimestamp = Lens.lens (\ChannelMembership' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@ChannelMembership' {} a -> s {lastUpdatedTimestamp = a} :: ChannelMembership) Prelude.. Lens.mapping Core._Time

-- | The data of the channel member.
channelMembership_member :: Lens.Lens' ChannelMembership (Prelude.Maybe Identity)
channelMembership_member = Lens.lens (\ChannelMembership' {member} -> member) (\s@ChannelMembership' {} a -> s {member = a} :: ChannelMembership)

-- | The membership type set for the channel member.
channelMembership_type :: Lens.Lens' ChannelMembership (Prelude.Maybe ChannelMembershipType)
channelMembership_type = Lens.lens (\ChannelMembership' {type'} -> type') (\s@ChannelMembership' {} a -> s {type' = a} :: ChannelMembership)

-- | The time at which the channel membership was created.
channelMembership_createdTimestamp :: Lens.Lens' ChannelMembership (Prelude.Maybe Prelude.UTCTime)
channelMembership_createdTimestamp = Lens.lens (\ChannelMembership' {createdTimestamp} -> createdTimestamp) (\s@ChannelMembership' {} a -> s {createdTimestamp = a} :: ChannelMembership) Prelude.. Lens.mapping Core._Time

-- | The ARN of the member\'s channel.
channelMembership_channelArn :: Lens.Lens' ChannelMembership (Prelude.Maybe Prelude.Text)
channelMembership_channelArn = Lens.lens (\ChannelMembership' {channelArn} -> channelArn) (\s@ChannelMembership' {} a -> s {channelArn = a} :: ChannelMembership)

-- | The identifier of the member who invited another member.
channelMembership_invitedBy :: Lens.Lens' ChannelMembership (Prelude.Maybe Identity)
channelMembership_invitedBy = Lens.lens (\ChannelMembership' {invitedBy} -> invitedBy) (\s@ChannelMembership' {} a -> s {invitedBy = a} :: ChannelMembership)

instance Core.FromJSON ChannelMembership where
  parseJSON =
    Core.withObject
      "ChannelMembership"
      ( \x ->
          ChannelMembership'
            Prelude.<$> (x Core..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Core..:? "Member")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "ChannelArn")
            Prelude.<*> (x Core..:? "InvitedBy")
      )

instance Prelude.Hashable ChannelMembership where
  hashWithSalt _salt ChannelMembership' {..} =
    _salt `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` member
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` invitedBy

instance Prelude.NFData ChannelMembership where
  rnf ChannelMembership' {..} =
    Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf member
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf invitedBy
