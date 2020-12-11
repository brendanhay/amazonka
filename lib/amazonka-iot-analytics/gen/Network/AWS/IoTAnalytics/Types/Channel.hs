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
    cCreationTime,
    cStatus,
    cLastMessageArrivalTime,
    cArn,
    cStorage,
    cRetentionPeriod,
    cName,
    cLastUpdateTime,
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
  { creationTime :: Lude.Maybe Lude.Timestamp,
    status :: Lude.Maybe ChannelStatus,
    lastMessageArrivalTime :: Lude.Maybe Lude.Timestamp,
    arn :: Lude.Maybe Lude.Text,
    storage :: Lude.Maybe ChannelStorage,
    retentionPeriod :: Lude.Maybe RetentionPeriod,
    name :: Lude.Maybe Lude.Text,
    lastUpdateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Channel' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the channel.
-- * 'creationTime' - When the channel was created.
-- * 'lastMessageArrivalTime' - The last time when a new message arrived in the channel.
--
-- AWS IoT Analytics updates this value at most once per minute for one channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
-- This feature only applies to messages that arrived in the data store after October 23, 2020.
-- * 'lastUpdateTime' - When the channel was last updated.
-- * 'name' - The name of the channel.
-- * 'retentionPeriod' - How long, in days, message data is kept for the channel.
-- * 'status' - The status of the channel.
-- * 'storage' - Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
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
cCreationTime :: Lens.Lens' Channel (Lude.Maybe Lude.Timestamp)
cCreationTime = Lens.lens (creationTime :: Channel -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: Channel)
{-# DEPRECATED cCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status of the channel.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatus :: Lens.Lens' Channel (Lude.Maybe ChannelStatus)
cStatus = Lens.lens (status :: Channel -> Lude.Maybe ChannelStatus) (\s a -> s {status = a} :: Channel)
{-# DEPRECATED cStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The last time when a new message arrived in the channel.
--
-- AWS IoT Analytics updates this value at most once per minute for one channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
-- This feature only applies to messages that arrived in the data store after October 23, 2020.
--
-- /Note:/ Consider using 'lastMessageArrivalTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLastMessageArrivalTime :: Lens.Lens' Channel (Lude.Maybe Lude.Timestamp)
cLastMessageArrivalTime = Lens.lens (lastMessageArrivalTime :: Channel -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastMessageArrivalTime = a} :: Channel)
{-# DEPRECATED cLastMessageArrivalTime "Use generic-lens or generic-optics with 'lastMessageArrivalTime' instead." #-}

-- | The ARN of the channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cArn :: Lens.Lens' Channel (Lude.Maybe Lude.Text)
cArn = Lens.lens (arn :: Channel -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Channel)
{-# DEPRECATED cArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
--
-- /Note:/ Consider using 'storage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStorage :: Lens.Lens' Channel (Lude.Maybe ChannelStorage)
cStorage = Lens.lens (storage :: Channel -> Lude.Maybe ChannelStorage) (\s a -> s {storage = a} :: Channel)
{-# DEPRECATED cStorage "Use generic-lens or generic-optics with 'storage' instead." #-}

-- | How long, in days, message data is kept for the channel.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRetentionPeriod :: Lens.Lens' Channel (Lude.Maybe RetentionPeriod)
cRetentionPeriod = Lens.lens (retentionPeriod :: Channel -> Lude.Maybe RetentionPeriod) (\s a -> s {retentionPeriod = a} :: Channel)
{-# DEPRECATED cRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | The name of the channel.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Channel (Lude.Maybe Lude.Text)
cName = Lens.lens (name :: Channel -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Channel)
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | When the channel was last updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLastUpdateTime :: Lens.Lens' Channel (Lude.Maybe Lude.Timestamp)
cLastUpdateTime = Lens.lens (lastUpdateTime :: Channel -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: Channel)
{-# DEPRECATED cLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

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
