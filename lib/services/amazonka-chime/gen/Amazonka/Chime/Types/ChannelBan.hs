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
-- Module      : Amazonka.Chime.Types.ChannelBan
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.ChannelBan where

import Amazonka.Chime.Types.Identity
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of a channel ban.
--
-- /See:/ 'newChannelBan' smart constructor.
data ChannelBan = ChannelBan'
  { -- | The member being banned from the channel.
    member :: Prelude.Maybe Identity,
    -- | The time at which the ban was created.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the channel from which a member is being banned.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The @AppInstanceUser@ who created the ban.
    createdBy :: Prelude.Maybe Identity
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelBan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'member', 'channelBan_member' - The member being banned from the channel.
--
-- 'createdTimestamp', 'channelBan_createdTimestamp' - The time at which the ban was created.
--
-- 'channelArn', 'channelBan_channelArn' - The ARN of the channel from which a member is being banned.
--
-- 'createdBy', 'channelBan_createdBy' - The @AppInstanceUser@ who created the ban.
newChannelBan ::
  ChannelBan
newChannelBan =
  ChannelBan'
    { member = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      channelArn = Prelude.Nothing,
      createdBy = Prelude.Nothing
    }

-- | The member being banned from the channel.
channelBan_member :: Lens.Lens' ChannelBan (Prelude.Maybe Identity)
channelBan_member = Lens.lens (\ChannelBan' {member} -> member) (\s@ChannelBan' {} a -> s {member = a} :: ChannelBan)

-- | The time at which the ban was created.
channelBan_createdTimestamp :: Lens.Lens' ChannelBan (Prelude.Maybe Prelude.UTCTime)
channelBan_createdTimestamp = Lens.lens (\ChannelBan' {createdTimestamp} -> createdTimestamp) (\s@ChannelBan' {} a -> s {createdTimestamp = a} :: ChannelBan) Prelude.. Lens.mapping Data._Time

-- | The ARN of the channel from which a member is being banned.
channelBan_channelArn :: Lens.Lens' ChannelBan (Prelude.Maybe Prelude.Text)
channelBan_channelArn = Lens.lens (\ChannelBan' {channelArn} -> channelArn) (\s@ChannelBan' {} a -> s {channelArn = a} :: ChannelBan)

-- | The @AppInstanceUser@ who created the ban.
channelBan_createdBy :: Lens.Lens' ChannelBan (Prelude.Maybe Identity)
channelBan_createdBy = Lens.lens (\ChannelBan' {createdBy} -> createdBy) (\s@ChannelBan' {} a -> s {createdBy = a} :: ChannelBan)

instance Data.FromJSON ChannelBan where
  parseJSON =
    Data.withObject
      "ChannelBan"
      ( \x ->
          ChannelBan'
            Prelude.<$> (x Data..:? "Member")
            Prelude.<*> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "ChannelArn")
            Prelude.<*> (x Data..:? "CreatedBy")
      )

instance Prelude.Hashable ChannelBan where
  hashWithSalt _salt ChannelBan' {..} =
    _salt `Prelude.hashWithSalt` member
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` createdBy

instance Prelude.NFData ChannelBan where
  rnf ChannelBan' {..} =
    Prelude.rnf member
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf createdBy
