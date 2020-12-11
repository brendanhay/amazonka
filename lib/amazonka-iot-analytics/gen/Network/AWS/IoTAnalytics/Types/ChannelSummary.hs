-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ChannelSummary
  ( ChannelSummary (..),

    -- * Smart constructor
    mkChannelSummary,

    -- * Lenses
    csCreationTime,
    csStatus,
    csLastMessageArrivalTime,
    csChannelName,
    csChannelStorage,
    csLastUpdateTime,
  )
where

import Network.AWS.IoTAnalytics.Types.ChannelStatus
import Network.AWS.IoTAnalytics.Types.ChannelStorageSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A summary of information about a channel.
--
-- /See:/ 'mkChannelSummary' smart constructor.
data ChannelSummary = ChannelSummary'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    status :: Lude.Maybe ChannelStatus,
    lastMessageArrivalTime :: Lude.Maybe Lude.Timestamp,
    channelName :: Lude.Maybe Lude.Text,
    channelStorage :: Lude.Maybe ChannelStorageSummary,
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

-- | Creates a value of 'ChannelSummary' with the minimum fields required to make a request.
--
-- * 'channelName' - The name of the channel.
-- * 'channelStorage' - Where channel data is stored.
-- * 'creationTime' - When the channel was created.
-- * 'lastMessageArrivalTime' - The last time when a new message arrived in the channel.
--
-- AWS IoT Analytics updates this value at most once per minute for one channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
-- This feature only applies to messages that arrived in the data store after October 23, 2020.
-- * 'lastUpdateTime' - The last time the channel was updated.
-- * 'status' - The status of the channel.
mkChannelSummary ::
  ChannelSummary
mkChannelSummary =
  ChannelSummary'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      lastMessageArrivalTime = Lude.Nothing,
      channelName = Lude.Nothing,
      channelStorage = Lude.Nothing,
      lastUpdateTime = Lude.Nothing
    }

-- | When the channel was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCreationTime :: Lens.Lens' ChannelSummary (Lude.Maybe Lude.Timestamp)
csCreationTime = Lens.lens (creationTime :: ChannelSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: ChannelSummary)
{-# DEPRECATED csCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status of the channel.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStatus :: Lens.Lens' ChannelSummary (Lude.Maybe ChannelStatus)
csStatus = Lens.lens (status :: ChannelSummary -> Lude.Maybe ChannelStatus) (\s a -> s {status = a} :: ChannelSummary)
{-# DEPRECATED csStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The last time when a new message arrived in the channel.
--
-- AWS IoT Analytics updates this value at most once per minute for one channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
-- This feature only applies to messages that arrived in the data store after October 23, 2020.
--
-- /Note:/ Consider using 'lastMessageArrivalTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csLastMessageArrivalTime :: Lens.Lens' ChannelSummary (Lude.Maybe Lude.Timestamp)
csLastMessageArrivalTime = Lens.lens (lastMessageArrivalTime :: ChannelSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastMessageArrivalTime = a} :: ChannelSummary)
{-# DEPRECATED csLastMessageArrivalTime "Use generic-lens or generic-optics with 'lastMessageArrivalTime' instead." #-}

-- | The name of the channel.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csChannelName :: Lens.Lens' ChannelSummary (Lude.Maybe Lude.Text)
csChannelName = Lens.lens (channelName :: ChannelSummary -> Lude.Maybe Lude.Text) (\s a -> s {channelName = a} :: ChannelSummary)
{-# DEPRECATED csChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

-- | Where channel data is stored.
--
-- /Note:/ Consider using 'channelStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csChannelStorage :: Lens.Lens' ChannelSummary (Lude.Maybe ChannelStorageSummary)
csChannelStorage = Lens.lens (channelStorage :: ChannelSummary -> Lude.Maybe ChannelStorageSummary) (\s a -> s {channelStorage = a} :: ChannelSummary)
{-# DEPRECATED csChannelStorage "Use generic-lens or generic-optics with 'channelStorage' instead." #-}

-- | The last time the channel was updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csLastUpdateTime :: Lens.Lens' ChannelSummary (Lude.Maybe Lude.Timestamp)
csLastUpdateTime = Lens.lens (lastUpdateTime :: ChannelSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: ChannelSummary)
{-# DEPRECATED csLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

instance Lude.FromJSON ChannelSummary where
  parseJSON =
    Lude.withObject
      "ChannelSummary"
      ( \x ->
          ChannelSummary'
            Lude.<$> (x Lude..:? "creationTime")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "lastMessageArrivalTime")
            Lude.<*> (x Lude..:? "channelName")
            Lude.<*> (x Lude..:? "channelStorage")
            Lude.<*> (x Lude..:? "lastUpdateTime")
      )
