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
-- Module      : Amazonka.IoTAnalytics.Types.ChannelSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.ChannelSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.ChannelStatus
import Amazonka.IoTAnalytics.Types.ChannelStorageSummary
import qualified Amazonka.Prelude as Prelude

-- | A summary of information about a channel.
--
-- /See:/ 'newChannelSummary' smart constructor.
data ChannelSummary = ChannelSummary'
  { -- | The name of the channel.
    channelName :: Prelude.Maybe Prelude.Text,
    -- | Where channel data is stored.
    channelStorage :: Prelude.Maybe ChannelStorageSummary,
    -- | When the channel was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The last time when a new message arrived in the channel.
    --
    -- IoT Analytics updates this value at most once per minute for one
    -- channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
    --
    -- This feature only applies to messages that arrived in the data store
    -- after October 23, 2020.
    lastMessageArrivalTime :: Prelude.Maybe Data.POSIX,
    -- | The last time the channel was updated.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the channel.
    status :: Prelude.Maybe ChannelStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelName', 'channelSummary_channelName' - The name of the channel.
--
-- 'channelStorage', 'channelSummary_channelStorage' - Where channel data is stored.
--
-- 'creationTime', 'channelSummary_creationTime' - When the channel was created.
--
-- 'lastMessageArrivalTime', 'channelSummary_lastMessageArrivalTime' - The last time when a new message arrived in the channel.
--
-- IoT Analytics updates this value at most once per minute for one
-- channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
--
-- This feature only applies to messages that arrived in the data store
-- after October 23, 2020.
--
-- 'lastUpdateTime', 'channelSummary_lastUpdateTime' - The last time the channel was updated.
--
-- 'status', 'channelSummary_status' - The status of the channel.
newChannelSummary ::
  ChannelSummary
newChannelSummary =
  ChannelSummary'
    { channelName = Prelude.Nothing,
      channelStorage = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastMessageArrivalTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The name of the channel.
channelSummary_channelName :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_channelName = Lens.lens (\ChannelSummary' {channelName} -> channelName) (\s@ChannelSummary' {} a -> s {channelName = a} :: ChannelSummary)

-- | Where channel data is stored.
channelSummary_channelStorage :: Lens.Lens' ChannelSummary (Prelude.Maybe ChannelStorageSummary)
channelSummary_channelStorage = Lens.lens (\ChannelSummary' {channelStorage} -> channelStorage) (\s@ChannelSummary' {} a -> s {channelStorage = a} :: ChannelSummary)

-- | When the channel was created.
channelSummary_creationTime :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.UTCTime)
channelSummary_creationTime = Lens.lens (\ChannelSummary' {creationTime} -> creationTime) (\s@ChannelSummary' {} a -> s {creationTime = a} :: ChannelSummary) Prelude.. Lens.mapping Data._Time

-- | The last time when a new message arrived in the channel.
--
-- IoT Analytics updates this value at most once per minute for one
-- channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
--
-- This feature only applies to messages that arrived in the data store
-- after October 23, 2020.
channelSummary_lastMessageArrivalTime :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.UTCTime)
channelSummary_lastMessageArrivalTime = Lens.lens (\ChannelSummary' {lastMessageArrivalTime} -> lastMessageArrivalTime) (\s@ChannelSummary' {} a -> s {lastMessageArrivalTime = a} :: ChannelSummary) Prelude.. Lens.mapping Data._Time

-- | The last time the channel was updated.
channelSummary_lastUpdateTime :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.UTCTime)
channelSummary_lastUpdateTime = Lens.lens (\ChannelSummary' {lastUpdateTime} -> lastUpdateTime) (\s@ChannelSummary' {} a -> s {lastUpdateTime = a} :: ChannelSummary) Prelude.. Lens.mapping Data._Time

-- | The status of the channel.
channelSummary_status :: Lens.Lens' ChannelSummary (Prelude.Maybe ChannelStatus)
channelSummary_status = Lens.lens (\ChannelSummary' {status} -> status) (\s@ChannelSummary' {} a -> s {status = a} :: ChannelSummary)

instance Data.FromJSON ChannelSummary where
  parseJSON =
    Data.withObject
      "ChannelSummary"
      ( \x ->
          ChannelSummary'
            Prelude.<$> (x Data..:? "channelName")
            Prelude.<*> (x Data..:? "channelStorage")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "lastMessageArrivalTime")
            Prelude.<*> (x Data..:? "lastUpdateTime")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable ChannelSummary where
  hashWithSalt _salt ChannelSummary' {..} =
    _salt `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` channelStorage
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastMessageArrivalTime
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` status

instance Prelude.NFData ChannelSummary where
  rnf ChannelSummary' {..} =
    Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf channelStorage
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastMessageArrivalTime
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf status
