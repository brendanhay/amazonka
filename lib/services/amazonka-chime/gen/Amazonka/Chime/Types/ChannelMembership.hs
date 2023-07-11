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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.ChannelMembership where

import Amazonka.Chime.Types.ChannelMembershipType
import Amazonka.Chime.Types.Identity
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of a channel member.
--
-- /See:/ 'newChannelMembership' smart constructor.
data ChannelMembership = ChannelMembership'
  { -- | The ARN of the member\'s channel.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the channel membership was created.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the member who invited another member.
    invitedBy :: Prelude.Maybe Identity,
    -- | The time at which a channel membership was last updated.
    lastUpdatedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The data of the channel member.
    member :: Prelude.Maybe Identity,
    -- | The membership type set for the channel member.
    type' :: Prelude.Maybe ChannelMembershipType
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
-- 'createdTimestamp', 'channelMembership_createdTimestamp' - The time at which the channel membership was created.
--
-- 'invitedBy', 'channelMembership_invitedBy' - The identifier of the member who invited another member.
--
-- 'lastUpdatedTimestamp', 'channelMembership_lastUpdatedTimestamp' - The time at which a channel membership was last updated.
--
-- 'member', 'channelMembership_member' - The data of the channel member.
--
-- 'type'', 'channelMembership_type' - The membership type set for the channel member.
newChannelMembership ::
  ChannelMembership
newChannelMembership =
  ChannelMembership'
    { channelArn = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      invitedBy = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      member = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The ARN of the member\'s channel.
channelMembership_channelArn :: Lens.Lens' ChannelMembership (Prelude.Maybe Prelude.Text)
channelMembership_channelArn = Lens.lens (\ChannelMembership' {channelArn} -> channelArn) (\s@ChannelMembership' {} a -> s {channelArn = a} :: ChannelMembership)

-- | The time at which the channel membership was created.
channelMembership_createdTimestamp :: Lens.Lens' ChannelMembership (Prelude.Maybe Prelude.UTCTime)
channelMembership_createdTimestamp = Lens.lens (\ChannelMembership' {createdTimestamp} -> createdTimestamp) (\s@ChannelMembership' {} a -> s {createdTimestamp = a} :: ChannelMembership) Prelude.. Lens.mapping Data._Time

-- | The identifier of the member who invited another member.
channelMembership_invitedBy :: Lens.Lens' ChannelMembership (Prelude.Maybe Identity)
channelMembership_invitedBy = Lens.lens (\ChannelMembership' {invitedBy} -> invitedBy) (\s@ChannelMembership' {} a -> s {invitedBy = a} :: ChannelMembership)

-- | The time at which a channel membership was last updated.
channelMembership_lastUpdatedTimestamp :: Lens.Lens' ChannelMembership (Prelude.Maybe Prelude.UTCTime)
channelMembership_lastUpdatedTimestamp = Lens.lens (\ChannelMembership' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@ChannelMembership' {} a -> s {lastUpdatedTimestamp = a} :: ChannelMembership) Prelude.. Lens.mapping Data._Time

-- | The data of the channel member.
channelMembership_member :: Lens.Lens' ChannelMembership (Prelude.Maybe Identity)
channelMembership_member = Lens.lens (\ChannelMembership' {member} -> member) (\s@ChannelMembership' {} a -> s {member = a} :: ChannelMembership)

-- | The membership type set for the channel member.
channelMembership_type :: Lens.Lens' ChannelMembership (Prelude.Maybe ChannelMembershipType)
channelMembership_type = Lens.lens (\ChannelMembership' {type'} -> type') (\s@ChannelMembership' {} a -> s {type' = a} :: ChannelMembership)

instance Data.FromJSON ChannelMembership where
  parseJSON =
    Data.withObject
      "ChannelMembership"
      ( \x ->
          ChannelMembership'
            Prelude.<$> (x Data..:? "ChannelArn")
            Prelude.<*> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "InvitedBy")
            Prelude.<*> (x Data..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Data..:? "Member")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable ChannelMembership where
  hashWithSalt _salt ChannelMembership' {..} =
    _salt
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` invitedBy
      `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` member
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ChannelMembership where
  rnf ChannelMembership' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf invitedBy
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf member
      `Prelude.seq` Prelude.rnf type'
