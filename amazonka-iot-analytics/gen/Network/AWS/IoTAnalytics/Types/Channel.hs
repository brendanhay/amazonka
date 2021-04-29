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
-- Module      : Network.AWS.IoTAnalytics.Types.Channel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Channel where

import Network.AWS.IoTAnalytics.Types.ChannelStatus
import Network.AWS.IoTAnalytics.Types.ChannelStorage
import Network.AWS.IoTAnalytics.Types.RetentionPeriod
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    lastMessageArrivalTime :: Prelude.Maybe Prelude.POSIX,
    -- | The status of the channel.
    status :: Prelude.Maybe ChannelStatus,
    -- | When the channel was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | When the channel was last updated.
    lastUpdateTime :: Prelude.Maybe Prelude.POSIX,
    -- | The ARN of the channel.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the channel.
    name :: Prelude.Maybe Prelude.Text,
    -- | How long, in days, message data is kept for the channel.
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | Where channel data is stored. You can choose one of @serviceManagedS3@
    -- or @customerManagedS3@ storage. If not specified, the default is
    -- @serviceManagedS3@. You cannot change this storage option after the
    -- channel is created.
    storage :: Prelude.Maybe ChannelStorage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { lastMessageArrivalTime = Prelude.Nothing,
      status = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      name = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      storage = Prelude.Nothing
    }

-- | The last time when a new message arrived in the channel.
--
-- AWS IoT Analytics updates this value at most once per minute for one
-- channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
--
-- This feature only applies to messages that arrived in the data store
-- after October 23, 2020.
channel_lastMessageArrivalTime :: Lens.Lens' Channel (Prelude.Maybe Prelude.UTCTime)
channel_lastMessageArrivalTime = Lens.lens (\Channel' {lastMessageArrivalTime} -> lastMessageArrivalTime) (\s@Channel' {} a -> s {lastMessageArrivalTime = a} :: Channel) Prelude.. Lens.mapping Prelude._Time

-- | The status of the channel.
channel_status :: Lens.Lens' Channel (Prelude.Maybe ChannelStatus)
channel_status = Lens.lens (\Channel' {status} -> status) (\s@Channel' {} a -> s {status = a} :: Channel)

-- | When the channel was created.
channel_creationTime :: Lens.Lens' Channel (Prelude.Maybe Prelude.UTCTime)
channel_creationTime = Lens.lens (\Channel' {creationTime} -> creationTime) (\s@Channel' {} a -> s {creationTime = a} :: Channel) Prelude.. Lens.mapping Prelude._Time

-- | When the channel was last updated.
channel_lastUpdateTime :: Lens.Lens' Channel (Prelude.Maybe Prelude.UTCTime)
channel_lastUpdateTime = Lens.lens (\Channel' {lastUpdateTime} -> lastUpdateTime) (\s@Channel' {} a -> s {lastUpdateTime = a} :: Channel) Prelude.. Lens.mapping Prelude._Time

-- | The ARN of the channel.
channel_arn :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_arn = Lens.lens (\Channel' {arn} -> arn) (\s@Channel' {} a -> s {arn = a} :: Channel)

-- | The name of the channel.
channel_name :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_name = Lens.lens (\Channel' {name} -> name) (\s@Channel' {} a -> s {name = a} :: Channel)

-- | How long, in days, message data is kept for the channel.
channel_retentionPeriod :: Lens.Lens' Channel (Prelude.Maybe RetentionPeriod)
channel_retentionPeriod = Lens.lens (\Channel' {retentionPeriod} -> retentionPeriod) (\s@Channel' {} a -> s {retentionPeriod = a} :: Channel)

-- | Where channel data is stored. You can choose one of @serviceManagedS3@
-- or @customerManagedS3@ storage. If not specified, the default is
-- @serviceManagedS3@. You cannot change this storage option after the
-- channel is created.
channel_storage :: Lens.Lens' Channel (Prelude.Maybe ChannelStorage)
channel_storage = Lens.lens (\Channel' {storage} -> storage) (\s@Channel' {} a -> s {storage = a} :: Channel)

instance Prelude.FromJSON Channel where
  parseJSON =
    Prelude.withObject
      "Channel"
      ( \x ->
          Channel'
            Prelude.<$> (x Prelude..:? "lastMessageArrivalTime")
            Prelude.<*> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "creationTime")
            Prelude.<*> (x Prelude..:? "lastUpdateTime")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "retentionPeriod")
            Prelude.<*> (x Prelude..:? "storage")
      )

instance Prelude.Hashable Channel

instance Prelude.NFData Channel
