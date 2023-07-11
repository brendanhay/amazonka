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
-- Module      : Amazonka.Chime.Types.ChannelSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.ChannelSummary where

import Amazonka.Chime.Types.ChannelMode
import Amazonka.Chime.Types.ChannelPrivacy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary of the details of a @Channel@.
--
-- /See:/ 'newChannelSummary' smart constructor.
data ChannelSummary = ChannelSummary'
  { -- | The ARN of the channel.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the last message in a channel was sent.
    lastMessageTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The metadata of the channel.
    metadata :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The mode of the channel.
    mode :: Prelude.Maybe ChannelMode,
    -- | The name of the channel.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The privacy setting of the channel.
    privacy :: Prelude.Maybe ChannelPrivacy
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'channelSummary_channelArn' - The ARN of the channel.
--
-- 'lastMessageTimestamp', 'channelSummary_lastMessageTimestamp' - The time at which the last message in a channel was sent.
--
-- 'metadata', 'channelSummary_metadata' - The metadata of the channel.
--
-- 'mode', 'channelSummary_mode' - The mode of the channel.
--
-- 'name', 'channelSummary_name' - The name of the channel.
--
-- 'privacy', 'channelSummary_privacy' - The privacy setting of the channel.
newChannelSummary ::
  ChannelSummary
newChannelSummary =
  ChannelSummary'
    { channelArn = Prelude.Nothing,
      lastMessageTimestamp = Prelude.Nothing,
      metadata = Prelude.Nothing,
      mode = Prelude.Nothing,
      name = Prelude.Nothing,
      privacy = Prelude.Nothing
    }

-- | The ARN of the channel.
channelSummary_channelArn :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_channelArn = Lens.lens (\ChannelSummary' {channelArn} -> channelArn) (\s@ChannelSummary' {} a -> s {channelArn = a} :: ChannelSummary)

-- | The time at which the last message in a channel was sent.
channelSummary_lastMessageTimestamp :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.UTCTime)
channelSummary_lastMessageTimestamp = Lens.lens (\ChannelSummary' {lastMessageTimestamp} -> lastMessageTimestamp) (\s@ChannelSummary' {} a -> s {lastMessageTimestamp = a} :: ChannelSummary) Prelude.. Lens.mapping Data._Time

-- | The metadata of the channel.
channelSummary_metadata :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_metadata = Lens.lens (\ChannelSummary' {metadata} -> metadata) (\s@ChannelSummary' {} a -> s {metadata = a} :: ChannelSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The mode of the channel.
channelSummary_mode :: Lens.Lens' ChannelSummary (Prelude.Maybe ChannelMode)
channelSummary_mode = Lens.lens (\ChannelSummary' {mode} -> mode) (\s@ChannelSummary' {} a -> s {mode = a} :: ChannelSummary)

-- | The name of the channel.
channelSummary_name :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_name = Lens.lens (\ChannelSummary' {name} -> name) (\s@ChannelSummary' {} a -> s {name = a} :: ChannelSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The privacy setting of the channel.
channelSummary_privacy :: Lens.Lens' ChannelSummary (Prelude.Maybe ChannelPrivacy)
channelSummary_privacy = Lens.lens (\ChannelSummary' {privacy} -> privacy) (\s@ChannelSummary' {} a -> s {privacy = a} :: ChannelSummary)

instance Data.FromJSON ChannelSummary where
  parseJSON =
    Data.withObject
      "ChannelSummary"
      ( \x ->
          ChannelSummary'
            Prelude.<$> (x Data..:? "ChannelArn")
            Prelude.<*> (x Data..:? "LastMessageTimestamp")
            Prelude.<*> (x Data..:? "Metadata")
            Prelude.<*> (x Data..:? "Mode")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Privacy")
      )

instance Prelude.Hashable ChannelSummary where
  hashWithSalt _salt ChannelSummary' {..} =
    _salt
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` lastMessageTimestamp
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` privacy

instance Prelude.NFData ChannelSummary where
  rnf ChannelSummary' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf lastMessageTimestamp
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf privacy
