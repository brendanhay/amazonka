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
-- Module      : Network.AWS.IoTAnalytics.Types.Channel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Channel where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.ChannelStatus
import Network.AWS.IoTAnalytics.Types.ChannelStorage
import Network.AWS.IoTAnalytics.Types.RetentionPeriod
import qualified Network.AWS.Lens as Lens

-- | A collection of data from an MQTT topic. Channels archive the raw,
-- unprocessed messages before publishing the data to a pipeline.
--
-- /See:/ 'newChannel' smart constructor.
data Channel = Channel'
  { -- | The last time when a new message arrived in the channel.
    --
    -- AWS IoT Analytics updates this value at most once per minute for one
    -- channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
    --
    -- This feature only applies to messages that arrived in the data store
    -- after October 23, 2020.
    lastMessageArrivalTime :: Core.Maybe Core.POSIX,
    -- | The status of the channel.
    status :: Core.Maybe ChannelStatus,
    -- | When the channel was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | When the channel was last updated.
    lastUpdateTime :: Core.Maybe Core.POSIX,
    -- | The ARN of the channel.
    arn :: Core.Maybe Core.Text,
    -- | The name of the channel.
    name :: Core.Maybe Core.Text,
    -- | How long, in days, message data is kept for the channel.
    retentionPeriod :: Core.Maybe RetentionPeriod,
    -- | Where channel data is stored. You can choose one of @serviceManagedS3@
    -- or @customerManagedS3@ storage. If not specified, the default is
    -- @serviceManagedS3@. You cannot change this storage option after the
    -- channel is created.
    storage :: Core.Maybe ChannelStorage
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Channel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastMessageArrivalTime', 'channel_lastMessageArrivalTime' - The last time when a new message arrived in the channel.
--
-- AWS IoT Analytics updates this value at most once per minute for one
-- channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
--
-- This feature only applies to messages that arrived in the data store
-- after October 23, 2020.
--
-- 'status', 'channel_status' - The status of the channel.
--
-- 'creationTime', 'channel_creationTime' - When the channel was created.
--
-- 'lastUpdateTime', 'channel_lastUpdateTime' - When the channel was last updated.
--
-- 'arn', 'channel_arn' - The ARN of the channel.
--
-- 'name', 'channel_name' - The name of the channel.
--
-- 'retentionPeriod', 'channel_retentionPeriod' - How long, in days, message data is kept for the channel.
--
-- 'storage', 'channel_storage' - Where channel data is stored. You can choose one of @serviceManagedS3@
-- or @customerManagedS3@ storage. If not specified, the default is
-- @serviceManagedS3@. You cannot change this storage option after the
-- channel is created.
newChannel ::
  Channel
newChannel =
  Channel'
    { lastMessageArrivalTime = Core.Nothing,
      status = Core.Nothing,
      creationTime = Core.Nothing,
      lastUpdateTime = Core.Nothing,
      arn = Core.Nothing,
      name = Core.Nothing,
      retentionPeriod = Core.Nothing,
      storage = Core.Nothing
    }

-- | The last time when a new message arrived in the channel.
--
-- AWS IoT Analytics updates this value at most once per minute for one
-- channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
--
-- This feature only applies to messages that arrived in the data store
-- after October 23, 2020.
channel_lastMessageArrivalTime :: Lens.Lens' Channel (Core.Maybe Core.UTCTime)
channel_lastMessageArrivalTime = Lens.lens (\Channel' {lastMessageArrivalTime} -> lastMessageArrivalTime) (\s@Channel' {} a -> s {lastMessageArrivalTime = a} :: Channel) Core.. Lens.mapping Core._Time

-- | The status of the channel.
channel_status :: Lens.Lens' Channel (Core.Maybe ChannelStatus)
channel_status = Lens.lens (\Channel' {status} -> status) (\s@Channel' {} a -> s {status = a} :: Channel)

-- | When the channel was created.
channel_creationTime :: Lens.Lens' Channel (Core.Maybe Core.UTCTime)
channel_creationTime = Lens.lens (\Channel' {creationTime} -> creationTime) (\s@Channel' {} a -> s {creationTime = a} :: Channel) Core.. Lens.mapping Core._Time

-- | When the channel was last updated.
channel_lastUpdateTime :: Lens.Lens' Channel (Core.Maybe Core.UTCTime)
channel_lastUpdateTime = Lens.lens (\Channel' {lastUpdateTime} -> lastUpdateTime) (\s@Channel' {} a -> s {lastUpdateTime = a} :: Channel) Core.. Lens.mapping Core._Time

-- | The ARN of the channel.
channel_arn :: Lens.Lens' Channel (Core.Maybe Core.Text)
channel_arn = Lens.lens (\Channel' {arn} -> arn) (\s@Channel' {} a -> s {arn = a} :: Channel)

-- | The name of the channel.
channel_name :: Lens.Lens' Channel (Core.Maybe Core.Text)
channel_name = Lens.lens (\Channel' {name} -> name) (\s@Channel' {} a -> s {name = a} :: Channel)

-- | How long, in days, message data is kept for the channel.
channel_retentionPeriod :: Lens.Lens' Channel (Core.Maybe RetentionPeriod)
channel_retentionPeriod = Lens.lens (\Channel' {retentionPeriod} -> retentionPeriod) (\s@Channel' {} a -> s {retentionPeriod = a} :: Channel)

-- | Where channel data is stored. You can choose one of @serviceManagedS3@
-- or @customerManagedS3@ storage. If not specified, the default is
-- @serviceManagedS3@. You cannot change this storage option after the
-- channel is created.
channel_storage :: Lens.Lens' Channel (Core.Maybe ChannelStorage)
channel_storage = Lens.lens (\Channel' {storage} -> storage) (\s@Channel' {} a -> s {storage = a} :: Channel)

instance Core.FromJSON Channel where
  parseJSON =
    Core.withObject
      "Channel"
      ( \x ->
          Channel'
            Core.<$> (x Core..:? "lastMessageArrivalTime")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "creationTime")
            Core.<*> (x Core..:? "lastUpdateTime")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "retentionPeriod")
            Core.<*> (x Core..:? "storage")
      )

instance Core.Hashable Channel

instance Core.NFData Channel
