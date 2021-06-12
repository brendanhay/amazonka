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

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.ChannelStatus
import Network.AWS.IoTAnalytics.Types.ChannelStorageSummary
import qualified Network.AWS.Lens as Lens

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
    lastMessageArrivalTime :: Core.Maybe Core.POSIX,
    -- | The name of the channel.
    channelName :: Core.Maybe Core.Text,
    -- | The status of the channel.
    status :: Core.Maybe ChannelStatus,
    -- | When the channel was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The last time the channel was updated.
    lastUpdateTime :: Core.Maybe Core.POSIX,
    -- | Where channel data is stored.
    channelStorage :: Core.Maybe ChannelStorageSummary
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      channelName = Core.Nothing,
      status = Core.Nothing,
      creationTime = Core.Nothing,
      lastUpdateTime = Core.Nothing,
      channelStorage = Core.Nothing
    }

-- | The last time when a new message arrived in the channel.
--
-- AWS IoT Analytics updates this value at most once per minute for one
-- channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
--
-- This feature only applies to messages that arrived in the data store
-- after October 23, 2020.
channelSummary_lastMessageArrivalTime :: Lens.Lens' ChannelSummary (Core.Maybe Core.UTCTime)
channelSummary_lastMessageArrivalTime = Lens.lens (\ChannelSummary' {lastMessageArrivalTime} -> lastMessageArrivalTime) (\s@ChannelSummary' {} a -> s {lastMessageArrivalTime = a} :: ChannelSummary) Core.. Lens.mapping Core._Time

-- | The name of the channel.
channelSummary_channelName :: Lens.Lens' ChannelSummary (Core.Maybe Core.Text)
channelSummary_channelName = Lens.lens (\ChannelSummary' {channelName} -> channelName) (\s@ChannelSummary' {} a -> s {channelName = a} :: ChannelSummary)

-- | The status of the channel.
channelSummary_status :: Lens.Lens' ChannelSummary (Core.Maybe ChannelStatus)
channelSummary_status = Lens.lens (\ChannelSummary' {status} -> status) (\s@ChannelSummary' {} a -> s {status = a} :: ChannelSummary)

-- | When the channel was created.
channelSummary_creationTime :: Lens.Lens' ChannelSummary (Core.Maybe Core.UTCTime)
channelSummary_creationTime = Lens.lens (\ChannelSummary' {creationTime} -> creationTime) (\s@ChannelSummary' {} a -> s {creationTime = a} :: ChannelSummary) Core.. Lens.mapping Core._Time

-- | The last time the channel was updated.
channelSummary_lastUpdateTime :: Lens.Lens' ChannelSummary (Core.Maybe Core.UTCTime)
channelSummary_lastUpdateTime = Lens.lens (\ChannelSummary' {lastUpdateTime} -> lastUpdateTime) (\s@ChannelSummary' {} a -> s {lastUpdateTime = a} :: ChannelSummary) Core.. Lens.mapping Core._Time

-- | Where channel data is stored.
channelSummary_channelStorage :: Lens.Lens' ChannelSummary (Core.Maybe ChannelStorageSummary)
channelSummary_channelStorage = Lens.lens (\ChannelSummary' {channelStorage} -> channelStorage) (\s@ChannelSummary' {} a -> s {channelStorage = a} :: ChannelSummary)

instance Core.FromJSON ChannelSummary where
  parseJSON =
    Core.withObject
      "ChannelSummary"
      ( \x ->
          ChannelSummary'
            Core.<$> (x Core..:? "lastMessageArrivalTime")
            Core.<*> (x Core..:? "channelName")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "creationTime")
            Core.<*> (x Core..:? "lastUpdateTime")
            Core.<*> (x Core..:? "channelStorage")
      )

instance Core.Hashable ChannelSummary

instance Core.NFData ChannelSummary
