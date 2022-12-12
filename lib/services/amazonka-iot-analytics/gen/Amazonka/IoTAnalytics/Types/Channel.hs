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
-- Module      : Amazonka.IoTAnalytics.Types.Channel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.Channel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.ChannelStatus
import Amazonka.IoTAnalytics.Types.ChannelStorage
import Amazonka.IoTAnalytics.Types.RetentionPeriod
import qualified Amazonka.Prelude as Prelude

-- | A collection of data from an MQTT topic. Channels archive the raw,
-- unprocessed messages before publishing the data to a pipeline.
--
-- /See:/ 'newChannel' smart constructor.
data Channel = Channel'
  { -- | The ARN of the channel.
    arn :: Prelude.Maybe Prelude.Text,
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
    -- | When the channel was last updated.
    lastUpdateTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the channel.
    name :: Prelude.Maybe Prelude.Text,
    -- | How long, in days, message data is kept for the channel.
    retentionPeriod :: Prelude.Maybe RetentionPeriod,
    -- | The status of the channel.
    status :: Prelude.Maybe ChannelStatus,
    -- | Where channel data is stored. You can choose one of @serviceManagedS3@
    -- or @customerManagedS3@ storage. If not specified, the default is
    -- @serviceManagedS3@. You can\'t change this storage option after the
    -- channel is created.
    storage :: Prelude.Maybe ChannelStorage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Channel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'channel_arn' - The ARN of the channel.
--
-- 'creationTime', 'channel_creationTime' - When the channel was created.
--
-- 'lastMessageArrivalTime', 'channel_lastMessageArrivalTime' - The last time when a new message arrived in the channel.
--
-- IoT Analytics updates this value at most once per minute for one
-- channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
--
-- This feature only applies to messages that arrived in the data store
-- after October 23, 2020.
--
-- 'lastUpdateTime', 'channel_lastUpdateTime' - When the channel was last updated.
--
-- 'name', 'channel_name' - The name of the channel.
--
-- 'retentionPeriod', 'channel_retentionPeriod' - How long, in days, message data is kept for the channel.
--
-- 'status', 'channel_status' - The status of the channel.
--
-- 'storage', 'channel_storage' - Where channel data is stored. You can choose one of @serviceManagedS3@
-- or @customerManagedS3@ storage. If not specified, the default is
-- @serviceManagedS3@. You can\'t change this storage option after the
-- channel is created.
newChannel ::
  Channel
newChannel =
  Channel'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastMessageArrivalTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      name = Prelude.Nothing,
      retentionPeriod = Prelude.Nothing,
      status = Prelude.Nothing,
      storage = Prelude.Nothing
    }

-- | The ARN of the channel.
channel_arn :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_arn = Lens.lens (\Channel' {arn} -> arn) (\s@Channel' {} a -> s {arn = a} :: Channel)

-- | When the channel was created.
channel_creationTime :: Lens.Lens' Channel (Prelude.Maybe Prelude.UTCTime)
channel_creationTime = Lens.lens (\Channel' {creationTime} -> creationTime) (\s@Channel' {} a -> s {creationTime = a} :: Channel) Prelude.. Lens.mapping Data._Time

-- | The last time when a new message arrived in the channel.
--
-- IoT Analytics updates this value at most once per minute for one
-- channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
--
-- This feature only applies to messages that arrived in the data store
-- after October 23, 2020.
channel_lastMessageArrivalTime :: Lens.Lens' Channel (Prelude.Maybe Prelude.UTCTime)
channel_lastMessageArrivalTime = Lens.lens (\Channel' {lastMessageArrivalTime} -> lastMessageArrivalTime) (\s@Channel' {} a -> s {lastMessageArrivalTime = a} :: Channel) Prelude.. Lens.mapping Data._Time

-- | When the channel was last updated.
channel_lastUpdateTime :: Lens.Lens' Channel (Prelude.Maybe Prelude.UTCTime)
channel_lastUpdateTime = Lens.lens (\Channel' {lastUpdateTime} -> lastUpdateTime) (\s@Channel' {} a -> s {lastUpdateTime = a} :: Channel) Prelude.. Lens.mapping Data._Time

-- | The name of the channel.
channel_name :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_name = Lens.lens (\Channel' {name} -> name) (\s@Channel' {} a -> s {name = a} :: Channel)

-- | How long, in days, message data is kept for the channel.
channel_retentionPeriod :: Lens.Lens' Channel (Prelude.Maybe RetentionPeriod)
channel_retentionPeriod = Lens.lens (\Channel' {retentionPeriod} -> retentionPeriod) (\s@Channel' {} a -> s {retentionPeriod = a} :: Channel)

-- | The status of the channel.
channel_status :: Lens.Lens' Channel (Prelude.Maybe ChannelStatus)
channel_status = Lens.lens (\Channel' {status} -> status) (\s@Channel' {} a -> s {status = a} :: Channel)

-- | Where channel data is stored. You can choose one of @serviceManagedS3@
-- or @customerManagedS3@ storage. If not specified, the default is
-- @serviceManagedS3@. You can\'t change this storage option after the
-- channel is created.
channel_storage :: Lens.Lens' Channel (Prelude.Maybe ChannelStorage)
channel_storage = Lens.lens (\Channel' {storage} -> storage) (\s@Channel' {} a -> s {storage = a} :: Channel)

instance Data.FromJSON Channel where
  parseJSON =
    Data.withObject
      "Channel"
      ( \x ->
          Channel'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "lastMessageArrivalTime")
            Prelude.<*> (x Data..:? "lastUpdateTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "retentionPeriod")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "storage")
      )

instance Prelude.Hashable Channel where
  hashWithSalt _salt Channel' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastMessageArrivalTime
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` retentionPeriod
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` storage

instance Prelude.NFData Channel where
  rnf Channel' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastMessageArrivalTime
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf retentionPeriod
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf storage
