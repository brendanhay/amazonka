{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.Event
  ( Event (..)
  -- * Smart constructor
  , mkEvent
  -- * Lenses
  , eDate
  , eMessage
  , eSourceIdentifier
  , eSourceType
  ) where

import qualified Network.AWS.ElastiCache.Types.SourceType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a single occurrence of something interesting within the system. Some examples of events are creating a cluster, adding or removing a cache node, or rebooting a node.
--
-- /See:/ 'mkEvent' smart constructor.
data Event = Event'
  { date :: Core.Maybe Core.UTCTime
    -- ^ The date and time when the event occurred.
  , message :: Core.Maybe Core.Text
    -- ^ The text of the event.
  , sourceIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier for the source of the event. For example, if the event occurred at the cluster level, the identifier would be the name of the cluster.
  , sourceType :: Core.Maybe Types.SourceType
    -- ^ Specifies the origin of this event - a cluster, a parameter group, a security group, etc.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Event' value with any optional fields omitted.
mkEvent
    :: Event
mkEvent
  = Event'{date = Core.Nothing, message = Core.Nothing,
           sourceIdentifier = Core.Nothing, sourceType = Core.Nothing}

-- | The date and time when the event occurred.
--
-- /Note:/ Consider using 'date' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDate :: Lens.Lens' Event (Core.Maybe Core.UTCTime)
eDate = Lens.field @"date"
{-# INLINEABLE eDate #-}
{-# DEPRECATED date "Use generic-lens or generic-optics with 'date' instead"  #-}

-- | The text of the event.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eMessage :: Lens.Lens' Event (Core.Maybe Core.Text)
eMessage = Lens.field @"message"
{-# INLINEABLE eMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The identifier for the source of the event. For example, if the event occurred at the cluster level, the identifier would be the name of the cluster.
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceIdentifier :: Lens.Lens' Event (Core.Maybe Core.Text)
eSourceIdentifier = Lens.field @"sourceIdentifier"
{-# INLINEABLE eSourceIdentifier #-}
{-# DEPRECATED sourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead"  #-}

-- | Specifies the origin of this event - a cluster, a parameter group, a security group, etc.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSourceType :: Lens.Lens' Event (Core.Maybe Types.SourceType)
eSourceType = Lens.field @"sourceType"
{-# INLINEABLE eSourceType #-}
{-# DEPRECATED sourceType "Use generic-lens or generic-optics with 'sourceType' instead"  #-}

instance Core.FromXML Event where
        parseXML x
          = Event' Core.<$>
              (x Core..@? "Date") Core.<*> x Core..@? "Message" Core.<*>
                x Core..@? "SourceIdentifier"
                Core.<*> x Core..@? "SourceType"
