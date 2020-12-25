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
    cArn,
    cCreationTime,
    cLastMessageArrivalTime,
    cLastUpdateTime,
    cName,
    cRetentionPeriod,
    cStatus,
    cStorage,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.Arn as Types
import qualified Network.AWS.IoTAnalytics.Types.ChannelStatus as Types
import qualified Network.AWS.IoTAnalytics.Types.ChannelStorage as Types
import qualified Network.AWS.IoTAnalytics.Types.Name as Types
import qualified Network.AWS.IoTAnalytics.Types.RetentionPeriod as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A collection of data from an MQTT topic. Channels archive the raw, unprocessed messages before publishing the data to a pipeline.
--
-- /See:/ 'mkChannel' smart constructor.
data Channel = Channel'
  { -- | The ARN of the channel.
    arn :: Core.Maybe Types.Arn,
    -- | When the channel was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The last time when a new message arrived in the channel.
    --
    -- AWS IoT Analytics updates this value at most once per minute for one channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
    -- This feature only applies to messages that arrived in the data store after October 23, 2020.
    lastMessageArrivalTime :: Core.Maybe Core.NominalDiffTime,
    -- | When the channel was last updated.
    lastUpdateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the channel.
    name :: Core.Maybe Types.Name,
    -- | How long, in days, message data is kept for the channel.
    retentionPeriod :: Core.Maybe Types.RetentionPeriod,
    -- | The status of the channel.
    status :: Core.Maybe Types.ChannelStatus,
    -- | Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
    storage :: Core.Maybe Types.ChannelStorage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Channel' value with any optional fields omitted.
mkChannel ::
  Channel
mkChannel =
  Channel'
    { arn = Core.Nothing,
      creationTime = Core.Nothing,
      lastMessageArrivalTime = Core.Nothing,
      lastUpdateTime = Core.Nothing,
      name = Core.Nothing,
      retentionPeriod = Core.Nothing,
      status = Core.Nothing,
      storage = Core.Nothing
    }

-- | The ARN of the channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cArn :: Lens.Lens' Channel (Core.Maybe Types.Arn)
cArn = Lens.field @"arn"
{-# DEPRECATED cArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | When the channel was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCreationTime :: Lens.Lens' Channel (Core.Maybe Core.NominalDiffTime)
cCreationTime = Lens.field @"creationTime"
{-# DEPRECATED cCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The last time when a new message arrived in the channel.
--
-- AWS IoT Analytics updates this value at most once per minute for one channel. Hence, the @lastMessageArrivalTime@ value is an approximation.
-- This feature only applies to messages that arrived in the data store after October 23, 2020.
--
-- /Note:/ Consider using 'lastMessageArrivalTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLastMessageArrivalTime :: Lens.Lens' Channel (Core.Maybe Core.NominalDiffTime)
cLastMessageArrivalTime = Lens.field @"lastMessageArrivalTime"
{-# DEPRECATED cLastMessageArrivalTime "Use generic-lens or generic-optics with 'lastMessageArrivalTime' instead." #-}

-- | When the channel was last updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cLastUpdateTime :: Lens.Lens' Channel (Core.Maybe Core.NominalDiffTime)
cLastUpdateTime = Lens.field @"lastUpdateTime"
{-# DEPRECATED cLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

-- | The name of the channel.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Channel (Core.Maybe Types.Name)
cName = Lens.field @"name"
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | How long, in days, message data is kept for the channel.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRetentionPeriod :: Lens.Lens' Channel (Core.Maybe Types.RetentionPeriod)
cRetentionPeriod = Lens.field @"retentionPeriod"
{-# DEPRECATED cRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | The status of the channel.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatus :: Lens.Lens' Channel (Core.Maybe Types.ChannelStatus)
cStatus = Lens.field @"status"
{-# DEPRECATED cStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
--
-- /Note:/ Consider using 'storage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStorage :: Lens.Lens' Channel (Core.Maybe Types.ChannelStorage)
cStorage = Lens.field @"storage"
{-# DEPRECATED cStorage "Use generic-lens or generic-optics with 'storage' instead." #-}

instance Core.FromJSON Channel where
  parseJSON =
    Core.withObject "Channel" Core.$
      \x ->
        Channel'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "creationTime")
          Core.<*> (x Core..:? "lastMessageArrivalTime")
          Core.<*> (x Core..:? "lastUpdateTime")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "retentionPeriod")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "storage")
