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
-- Module      : Amazonka.Chime.Types.BatchChannelMemberships
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.BatchChannelMemberships where

import Amazonka.Chime.Types.ChannelMembershipType
import Amazonka.Chime.Types.Identity
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The membership information, including member ARNs, the channel ARN, and
-- membership types.
--
-- /See:/ 'newBatchChannelMemberships' smart constructor.
data BatchChannelMemberships = BatchChannelMemberships'
  { -- | The membership types set for the channel users.
    type' :: Prelude.Maybe ChannelMembershipType,
    -- | The users successfully added to the request.
    members :: Prelude.Maybe [Identity],
    -- | The ARN of the channel to which you\'re adding users.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the member who invited another member.
    invitedBy :: Prelude.Maybe Identity
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchChannelMemberships' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'batchChannelMemberships_type' - The membership types set for the channel users.
--
-- 'members', 'batchChannelMemberships_members' - The users successfully added to the request.
--
-- 'channelArn', 'batchChannelMemberships_channelArn' - The ARN of the channel to which you\'re adding users.
--
-- 'invitedBy', 'batchChannelMemberships_invitedBy' - The identifier of the member who invited another member.
newBatchChannelMemberships ::
  BatchChannelMemberships
newBatchChannelMemberships =
  BatchChannelMemberships'
    { type' = Prelude.Nothing,
      members = Prelude.Nothing,
      channelArn = Prelude.Nothing,
      invitedBy = Prelude.Nothing
    }

-- | The membership types set for the channel users.
batchChannelMemberships_type :: Lens.Lens' BatchChannelMemberships (Prelude.Maybe ChannelMembershipType)
batchChannelMemberships_type = Lens.lens (\BatchChannelMemberships' {type'} -> type') (\s@BatchChannelMemberships' {} a -> s {type' = a} :: BatchChannelMemberships)

-- | The users successfully added to the request.
batchChannelMemberships_members :: Lens.Lens' BatchChannelMemberships (Prelude.Maybe [Identity])
batchChannelMemberships_members = Lens.lens (\BatchChannelMemberships' {members} -> members) (\s@BatchChannelMemberships' {} a -> s {members = a} :: BatchChannelMemberships) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the channel to which you\'re adding users.
batchChannelMemberships_channelArn :: Lens.Lens' BatchChannelMemberships (Prelude.Maybe Prelude.Text)
batchChannelMemberships_channelArn = Lens.lens (\BatchChannelMemberships' {channelArn} -> channelArn) (\s@BatchChannelMemberships' {} a -> s {channelArn = a} :: BatchChannelMemberships)

-- | The identifier of the member who invited another member.
batchChannelMemberships_invitedBy :: Lens.Lens' BatchChannelMemberships (Prelude.Maybe Identity)
batchChannelMemberships_invitedBy = Lens.lens (\BatchChannelMemberships' {invitedBy} -> invitedBy) (\s@BatchChannelMemberships' {} a -> s {invitedBy = a} :: BatchChannelMemberships)

instance Core.FromJSON BatchChannelMemberships where
  parseJSON =
    Core.withObject
      "BatchChannelMemberships"
      ( \x ->
          BatchChannelMemberships'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Members" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ChannelArn")
            Prelude.<*> (x Core..:? "InvitedBy")
      )

instance Prelude.Hashable BatchChannelMemberships where
  hashWithSalt _salt BatchChannelMemberships' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` members
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` invitedBy

instance Prelude.NFData BatchChannelMemberships where
  rnf BatchChannelMemberships' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf members
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf invitedBy
