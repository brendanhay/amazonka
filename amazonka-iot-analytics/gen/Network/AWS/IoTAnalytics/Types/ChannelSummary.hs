{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ChannelSummary where

import Network.AWS.IoTAnalytics.Types.ChannelStatus
import Network.AWS.IoTAnalytics.Types.ChannelStorageSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A summary of information about a channel.
--
-- /See:/ 'newChannelSummary' smart constructor.
data ChannelSummary = ChannelSummary'
  { -- | The last time when a new message arrived in the channel.
    --
    -- AWS IoT Analytics updates this value at most once per minute for one
    -- channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
    --
    -- This feature only applies to messages that arrived in the data store
    -- after October 23, 2020.
    lastMessageArrivalTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the channel.
    channelName :: Prelude.Maybe Prelude.Text,
    -- | The status of the channel.
    status :: Prelude.Maybe ChannelStatus,
    -- | When the channel was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The last time the channel was updated.
    lastUpdateTime :: Prelude.Maybe Prelude.POSIX,
    -- | Where channel data is stored.
    channelStorage :: Prelude.Maybe ChannelStorageSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ChannelSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastMessageArrivalTime', 'channelSummary_lastMessageArrivalTime' - The last time when a new message arrived in the channel.
--
-- AWS IoT Analytics updates this value at most once per minute for one
-- channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
--
-- This feature only applies to messages that arrived in the data store
-- after October 23, 2020.
--
-- 'channelName', 'channelSummary_channelName' - The name of the channel.
--
-- 'status', 'channelSummary_status' - The status of the channel.
--
-- 'creationTime', 'channelSummary_creationTime' - When the channel was created.
--
-- 'lastUpdateTime', 'channelSummary_lastUpdateTime' - The last time the channel was updated.
--
-- 'channelStorage', 'channelSummary_channelStorage' - Where channel data is stored.
newChannelSummary ::
  ChannelSummary
newChannelSummary =
  ChannelSummary'
    { lastMessageArrivalTime =
        Prelude.Nothing,
      channelName = Prelude.Nothing,
      status = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      channelStorage = Prelude.Nothing
    }

-- | The last time when a new message arrived in the channel.
--
-- AWS IoT Analytics updates this value at most once per minute for one
-- channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
--
-- This feature only applies to messages that arrived in the data store
-- after October 23, 2020.
channelSummary_lastMessageArrivalTime :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.UTCTime)
channelSummary_lastMessageArrivalTime = Lens.lens (\ChannelSummary' {lastMessageArrivalTime} -> lastMessageArrivalTime) (\s@ChannelSummary' {} a -> s {lastMessageArrivalTime = a} :: ChannelSummary) Prelude.. Lens.mapping Prelude._Time

-- | The name of the channel.
channelSummary_channelName :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_channelName = Lens.lens (\ChannelSummary' {channelName} -> channelName) (\s@ChannelSummary' {} a -> s {channelName = a} :: ChannelSummary)

-- | The status of the channel.
channelSummary_status :: Lens.Lens' ChannelSummary (Prelude.Maybe ChannelStatus)
channelSummary_status = Lens.lens (\ChannelSummary' {status} -> status) (\s@ChannelSummary' {} a -> s {status = a} :: ChannelSummary)

-- | When the channel was created.
channelSummary_creationTime :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.UTCTime)
channelSummary_creationTime = Lens.lens (\ChannelSummary' {creationTime} -> creationTime) (\s@ChannelSummary' {} a -> s {creationTime = a} :: ChannelSummary) Prelude.. Lens.mapping Prelude._Time

-- | The last time the channel was updated.
channelSummary_lastUpdateTime :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.UTCTime)
channelSummary_lastUpdateTime = Lens.lens (\ChannelSummary' {lastUpdateTime} -> lastUpdateTime) (\s@ChannelSummary' {} a -> s {lastUpdateTime = a} :: ChannelSummary) Prelude.. Lens.mapping Prelude._Time

-- | Where channel data is stored.
channelSummary_channelStorage :: Lens.Lens' ChannelSummary (Prelude.Maybe ChannelStorageSummary)
channelSummary_channelStorage = Lens.lens (\ChannelSummary' {channelStorage} -> channelStorage) (\s@ChannelSummary' {} a -> s {channelStorage = a} :: ChannelSummary)

instance Prelude.FromJSON ChannelSummary where
  parseJSON =
    Prelude.withObject
      "ChannelSummary"
      ( \x ->
          ChannelSummary'
            Prelude.<$> (x Prelude..:? "lastMessageArrivalTime")
            Prelude.<*> (x Prelude..:? "channelName")
            Prelude.<*> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "creationTime")
            Prelude.<*> (x Prelude..:? "lastUpdateTime")
            Prelude.<*> (x Prelude..:? "channelStorage")
      )

instance Prelude.Hashable ChannelSummary

instance Prelude.NFData ChannelSummary
