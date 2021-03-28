{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.RealtimeLogConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.RealtimeLogConfigs
  ( RealtimeLogConfigs (..)
  -- * Smart constructor
  , mkRealtimeLogConfigs
  -- * Lenses
  , rlcMaxItems
  , rlcIsTruncated
  , rlcMarker
  , rlcItems
  , rlcNextMarker
  ) where

import qualified Network.AWS.CloudFront.Types.RealtimeLogConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of real-time log configurations.
--
-- /See:/ 'mkRealtimeLogConfigs' smart constructor.
data RealtimeLogConfigs = RealtimeLogConfigs'
  { maxItems :: Core.Int
    -- ^ The maximum number of real-time log configurations requested.
  , isTruncated :: Core.Bool
    -- ^ A flag that indicates whether there are more real-time log configurations than are contained in this list.
  , marker :: Core.Text
    -- ^ This parameter indicates where this list of real-time log configurations begins. This list includes real-time log configurations that occur after the marker.
  , items :: Core.Maybe [Types.RealtimeLogConfig]
    -- ^ Contains the list of real-time log configurations.
  , nextMarker :: Core.Maybe Core.Text
    -- ^ If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing real-time log configurations where you left off. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RealtimeLogConfigs' value with any optional fields omitted.
mkRealtimeLogConfigs
    :: Core.Int -- ^ 'maxItems'
    -> Core.Bool -- ^ 'isTruncated'
    -> Core.Text -- ^ 'marker'
    -> RealtimeLogConfigs
mkRealtimeLogConfigs maxItems isTruncated marker
  = RealtimeLogConfigs'{maxItems, isTruncated, marker,
                        items = Core.Nothing, nextMarker = Core.Nothing}

-- | The maximum number of real-time log configurations requested.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcMaxItems :: Lens.Lens' RealtimeLogConfigs Core.Int
rlcMaxItems = Lens.field @"maxItems"
{-# INLINEABLE rlcMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | A flag that indicates whether there are more real-time log configurations than are contained in this list.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcIsTruncated :: Lens.Lens' RealtimeLogConfigs Core.Bool
rlcIsTruncated = Lens.field @"isTruncated"
{-# INLINEABLE rlcIsTruncated #-}
{-# DEPRECATED isTruncated "Use generic-lens or generic-optics with 'isTruncated' instead"  #-}

-- | This parameter indicates where this list of real-time log configurations begins. This list includes real-time log configurations that occur after the marker.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcMarker :: Lens.Lens' RealtimeLogConfigs Core.Text
rlcMarker = Lens.field @"marker"
{-# INLINEABLE rlcMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | Contains the list of real-time log configurations.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcItems :: Lens.Lens' RealtimeLogConfigs (Core.Maybe [Types.RealtimeLogConfig])
rlcItems = Lens.field @"items"
{-# INLINEABLE rlcItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing real-time log configurations where you left off. 
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcNextMarker :: Lens.Lens' RealtimeLogConfigs (Core.Maybe Core.Text)
rlcNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE rlcNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

instance Core.FromXML RealtimeLogConfigs where
        parseXML x
          = RealtimeLogConfigs' Core.<$>
              (x Core..@ "MaxItems") Core.<*> x Core..@ "IsTruncated" Core.<*>
                x Core..@ "Marker"
                Core.<*> x Core..@? "Items" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "NextMarker"
