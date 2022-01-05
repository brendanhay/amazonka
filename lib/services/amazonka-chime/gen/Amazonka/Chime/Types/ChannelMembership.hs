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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.ChannelMembership where

import Amazonka.Chime.Types.ChannelMembershipType
import Amazonka.Chime.Types.Identity
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details of a channel member.
--
-- /See:/ 'newChannelMembership' smart constructor.
data ChannelMembership = ChannelMembership'
  { -- | The ARN of the member\'s channel.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The data of the channel member.
    member :: Prelude.Maybe Identity,
    -- | The membership type set for the channel member.
    type' :: Prelude.Maybe ChannelMembershipType,
    -- | The identifier of the member who invited another member.
    invitedBy :: Prelude.Maybe Identity,
    -- | The time at which the channel membership was created.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The time at which a channel membership was last updated.
    lastUpdatedTimestamp :: Prelude.Maybe Core.POSIX
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
-- 'channelArn', 'channelMembership_channelArn' - The ARN of the member\'s channel.
--
-- 'member', 'channelMembership_member' - The data of the channel member.
--
-- 'type'', 'channelMembership_type' - The membership type set for the channel member.
--
-- 'invitedBy', 'channelMembership_invitedBy' - The identifier of the member who invited another member.
--
-- 'createdTimestamp', 'channelMembership_createdTimestamp' - The time at which the channel membership was created.
--
-- 'lastUpdatedTimestamp', 'channelMembership_lastUpdatedTimestamp' - The time at which a channel membership was last updated.
newChannelMembership ::
  ChannelMembership
newChannelMembership =
  ChannelMembership'
    { channelArn = Prelude.Nothing,
      member = Prelude.Nothing,
      type' = Prelude.Nothing,
      invitedBy = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing
    }

-- | The ARN of the member\'s channel.
channelMembership_channelArn :: Lens.Lens' ChannelMembership (Prelude.Maybe Prelude.Text)
channelMembership_channelArn = Lens.lens (\ChannelMembership' {channelArn} -> channelArn) (\s@ChannelMembership' {} a -> s {channelArn = a} :: ChannelMembership)

-- | The data of the channel member.
channelMembership_member :: Lens.Lens' ChannelMembership (Prelude.Maybe Identity)
channelMembership_member = Lens.lens (\ChannelMembership' {member} -> member) (\s@ChannelMembership' {} a -> s {member = a} :: ChannelMembership)

-- | The membership type set for the channel member.
channelMembership_type :: Lens.Lens' ChannelMembership (Prelude.Maybe ChannelMembershipType)
channelMembership_type = Lens.lens (\ChannelMembership' {type'} -> type') (\s@ChannelMembership' {} a -> s {type' = a} :: ChannelMembership)

-- | The identifier of the member who invited another member.
channelMembership_invitedBy :: Lens.Lens' ChannelMembership (Prelude.Maybe Identity)
channelMembership_invitedBy = Lens.lens (\ChannelMembership' {invitedBy} -> invitedBy) (\s@ChannelMembership' {} a -> s {invitedBy = a} :: ChannelMembership)

-- | The time at which the channel membership was created.
channelMembership_createdTimestamp :: Lens.Lens' ChannelMembership (Prelude.Maybe Prelude.UTCTime)
channelMembership_createdTimestamp = Lens.lens (\ChannelMembership' {createdTimestamp} -> createdTimestamp) (\s@ChannelMembership' {} a -> s {createdTimestamp = a} :: ChannelMembership) Prelude.. Lens.mapping Core._Time

-- | The time at which a channel membership was last updated.
channelMembership_lastUpdatedTimestamp :: Lens.Lens' ChannelMembership (Prelude.Maybe Prelude.UTCTime)
channelMembership_lastUpdatedTimestamp = Lens.lens (\ChannelMembership' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@ChannelMembership' {} a -> s {lastUpdatedTimestamp = a} :: ChannelMembership) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ChannelMembership where
  parseJSON =
    Core.withObject
      "ChannelMembership"
      ( \x ->
          ChannelMembership'
            Prelude.<$> (x Core..:? "ChannelArn")
            Prelude.<*> (x Core..:? "Member")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "InvitedBy")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
            Prelude.<*> (x Core..:? "LastUpdatedTimestamp")
      )

instance Prelude.Hashable ChannelMembership where
  hashWithSalt _salt ChannelMembership' {..} =
    _salt `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` member
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` invitedBy
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` lastUpdatedTimestamp

instance Prelude.NFData ChannelMembership where
  rnf ChannelMembership' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf member
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf invitedBy
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
