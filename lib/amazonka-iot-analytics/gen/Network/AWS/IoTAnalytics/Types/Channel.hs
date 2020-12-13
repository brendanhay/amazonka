{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.Channel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Channel
  ( Channel (..),

    -- * Smart constructor
    mkChannel,

    -- * Lenses
    cfCreationTime,
    cfStatus,
    cfLastMessageArrivalTime,
    cfArn,
    cfStorage,
    cfRetentionPeriod,
    cfName,
    cfLastUpdateTime,
  )
where

import Network.AWS.IoTAnalytics.Types.ChannelStatus
import Network.AWS.IoTAnalytics.Types.ChannelStorage
import Network.AWS.IoTAnalytics.Types.RetentionPeriod
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A collection of data from an MQTT topic. Channels archive the raw, unprocessed messages before publishing the data to a pipeline.
--
-- /See:/ 'mkChannel' smart constructor.
data Channel = Channel'
  { -- | When the channel was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The status of the channel.
    status :: Lude.Maybe ChannelStatus,
    -- | The last time when a new message arrived in the channel.
    --
    -- AWS IoT Analytics updates this value at most once per minute for one channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
    -- This feature only applies to messages that arrived in the data store after October 23, 2020.
    lastMessageArrivalTime :: Lude.Maybe Lude.Timestamp,
    -- | The ARN of the channel.
    arn :: Lude.Maybe Lude.Text,
    -- | Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
    storage :: Lude.Maybe ChannelStorage,
    -- | How long, in days, message data is kept for the channel.
    retentionPeriod :: Lude.Maybe RetentionPeriod,
    -- | The name of the channel.
    name :: Lude.Maybe Lude.Text,
    -- | When the channel was last updated.
    lastUpdateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Channel' with the minimum fields required to make a request.
--
-- * 'creationTime' - When the channel was created.
-- * 'status' - The status of the channel.
-- * 'lastMessageArrivalTime' - The last time when a new message arrived in the channel.
--
-- AWS IoT Analytics updates this value at most once per minute for one channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
-- This feature only applies to messages that arrived in the data store after October 23, 2020.
-- * 'arn' - The ARN of the channel.
-- * 'storage' - Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
-- * 'retentionPeriod' - How long, in days, message data is kept for the channel.
-- * 'name' - The name of the channel.
-- * 'lastUpdateTime' - When the channel was last updated.
mkChannel ::
  Channel
mkChannel =
  Channel'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      lastMessageArrivalTime = Lude.Nothing,
      arn = Lude.Nothing,
      storage = Lude.Nothing,
      retentionPeriod = Lude.Nothing,
      name = Lude.Nothing,
      lastUpdateTime = Lude.Nothing
    }

-- | When the channel was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfCreationTime :: Lens.Lens' Channel (Lude.Maybe Lude.Timestamp)
cfCreationTime = Lens.lens (creationTime :: Channel -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: Channel)
{-# DEPRECATED cfCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status of the channel.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfStatus :: Lens.Lens' Channel (Lude.Maybe ChannelStatus)
cfStatus = Lens.lens (status :: Channel -> Lude.Maybe ChannelStatus) (\s a -> s {status = a} :: Channel)
{-# DEPRECATED cfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The last time when a new message arrived in the channel.
--
-- AWS IoT Analytics updates this value at most once per minute for one channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
-- This feature only applies to messages that arrived in the data store after October 23, 2020.
--
-- /Note:/ Consider using 'lastMessageArrivalTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLastMessageArrivalTime :: Lens.Lens' Channel (Lude.Maybe Lude.Timestamp)
cfLastMessageArrivalTime = Lens.lens (lastMessageArrivalTime :: Channel -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastMessageArrivalTime = a} :: Channel)
{-# DEPRECATED cfLastMessageArrivalTime "Use generic-lens or generic-optics with 'lastMessageArrivalTime' instead." #-}

-- | The ARN of the channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfArn :: Lens.Lens' Channel (Lude.Maybe Lude.Text)
cfArn = Lens.lens (arn :: Channel -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Channel)
{-# DEPRECATED cfArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
--
-- /Note:/ Consider using 'storage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfStorage :: Lens.Lens' Channel (Lude.Maybe ChannelStorage)
cfStorage = Lens.lens (storage :: Channel -> Lude.Maybe ChannelStorage) (\s a -> s {storage = a} :: Channel)
{-# DEPRECATED cfStorage "Use generic-lens or generic-optics with 'storage' instead." #-}

-- | How long, in days, message data is kept for the channel.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfRetentionPeriod :: Lens.Lens' Channel (Lude.Maybe RetentionPeriod)
cfRetentionPeriod = Lens.lens (retentionPeriod :: Channel -> Lude.Maybe RetentionPeriod) (\s a -> s {retentionPeriod = a} :: Channel)
{-# DEPRECATED cfRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | The name of the channel.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfName :: Lens.Lens' Channel (Lude.Maybe Lude.Text)
cfName = Lens.lens (name :: Channel -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Channel)
{-# DEPRECATED cfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | When the channel was last updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLastUpdateTime :: Lens.Lens' Channel (Lude.Maybe Lude.Timestamp)
cfLastUpdateTime = Lens.lens (lastUpdateTime :: Channel -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: Channel)
{-# DEPRECATED cfLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

instance Lude.FromJSON Channel where
  parseJSON =
    Lude.withObject
      "Channel"
      ( \x ->
          Channel'
            Lude.<$> (x Lude..:? "creationTime")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "lastMessageArrivalTime")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "storage")
            Lude.<*> (x Lude..:? "retentionPeriod")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "lastUpdateTime")
      )
