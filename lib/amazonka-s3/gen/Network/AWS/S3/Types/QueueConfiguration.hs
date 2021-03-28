{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.QueueConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.QueueConfiguration
  ( QueueConfiguration (..)
  -- * Smart constructor
  , mkQueueConfiguration
  -- * Lenses
  , qcQueueArn
  , qcEvents
  , qcFilter
  , qcId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.Event as Types
import qualified Network.AWS.S3.Types.Id as Types
import qualified Network.AWS.S3.Types.NotificationConfigurationFilter as Types
import qualified Network.AWS.S3.Types.QueueArn as Types

-- | Specifies the configuration for publishing messages to an Amazon Simple Queue Service (Amazon SQS) queue when Amazon S3 detects specified events.
--
-- /See:/ 'mkQueueConfiguration' smart constructor.
data QueueConfiguration = QueueConfiguration'
  { queueArn :: Types.QueueArn
    -- ^ The Amazon Resource Name (ARN) of the Amazon SQS queue to which Amazon S3 publishes a message when it detects events of the specified type.
  , events :: [Types.Event]
    -- ^ A collection of bucket events for which to send notifications
  , filter :: Core.Maybe Types.NotificationConfigurationFilter
  , id :: Core.Maybe Types.Id
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueueConfiguration' value with any optional fields omitted.
mkQueueConfiguration
    :: Types.QueueArn -- ^ 'queueArn'
    -> QueueConfiguration
mkQueueConfiguration queueArn
  = QueueConfiguration'{queueArn, events = Core.mempty,
                        filter = Core.Nothing, id = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the Amazon SQS queue to which Amazon S3 publishes a message when it detects events of the specified type.
--
-- /Note:/ Consider using 'queueArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qcQueueArn :: Lens.Lens' QueueConfiguration Types.QueueArn
qcQueueArn = Lens.field @"queueArn"
{-# INLINEABLE qcQueueArn #-}
{-# DEPRECATED queueArn "Use generic-lens or generic-optics with 'queueArn' instead"  #-}

-- | A collection of bucket events for which to send notifications
--
-- /Note:/ Consider using 'events' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qcEvents :: Lens.Lens' QueueConfiguration [Types.Event]
qcEvents = Lens.field @"events"
{-# INLINEABLE qcEvents #-}
{-# DEPRECATED events "Use generic-lens or generic-optics with 'events' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qcFilter :: Lens.Lens' QueueConfiguration (Core.Maybe Types.NotificationConfigurationFilter)
qcFilter = Lens.field @"filter"
{-# INLINEABLE qcFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qcId :: Lens.Lens' QueueConfiguration (Core.Maybe Types.Id)
qcId = Lens.field @"id"
{-# INLINEABLE qcId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToXML QueueConfiguration where
        toXML QueueConfiguration{..}
          = Core.toXMLElement "Queue" queueArn Core.<>
              Core.toXMLList "Event" events
              Core.<> Core.maybe Core.mempty (Core.toXMLElement "Filter") filter
              Core.<> Core.maybe Core.mempty (Core.toXMLElement "Id") id

instance Core.FromXML QueueConfiguration where
        parseXML x
          = QueueConfiguration' Core.<$>
              (x Core..@ "Queue") Core.<*> x Core..@ "Event" Core..@! Core.mempty
                Core.<*> x Core..@? "Filter"
                Core.<*> x Core..@? "Id"
