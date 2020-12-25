{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.RealtimeLogConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.RealtimeLogConfigs
  ( RealtimeLogConfigs (..),

    -- * Smart constructor
    mkRealtimeLogConfigs,

    -- * Lenses
    rlcMaxItems,
    rlcIsTruncated,
    rlcMarker,
    rlcItems,
    rlcNextMarker,
  )
where

import qualified Network.AWS.CloudFront.Types.Marker as Types
import qualified Network.AWS.CloudFront.Types.NextMarker as Types
import qualified Network.AWS.CloudFront.Types.RealtimeLogConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of real-time log configurations.
--
-- /See:/ 'mkRealtimeLogConfigs' smart constructor.
data RealtimeLogConfigs = RealtimeLogConfigs'
  { -- | The maximum number of real-time log configurations requested.
    maxItems :: Core.Int,
    -- | A flag that indicates whether there are more real-time log configurations than are contained in this list.
    isTruncated :: Core.Bool,
    -- | This parameter indicates where this list of real-time log configurations begins. This list includes real-time log configurations that occur after the marker.
    marker :: Types.Marker,
    -- | Contains the list of real-time log configurations.
    items :: Core.Maybe [Types.RealtimeLogConfig],
    -- | If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing real-time log configurations where you left off.
    nextMarker :: Core.Maybe Types.NextMarker
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RealtimeLogConfigs' value with any optional fields omitted.
mkRealtimeLogConfigs ::
  -- | 'maxItems'
  Core.Int ->
  -- | 'isTruncated'
  Core.Bool ->
  -- | 'marker'
  Types.Marker ->
  RealtimeLogConfigs
mkRealtimeLogConfigs maxItems isTruncated marker =
  RealtimeLogConfigs'
    { maxItems,
      isTruncated,
      marker,
      items = Core.Nothing,
      nextMarker = Core.Nothing
    }

-- | The maximum number of real-time log configurations requested.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcMaxItems :: Lens.Lens' RealtimeLogConfigs Core.Int
rlcMaxItems = Lens.field @"maxItems"
{-# DEPRECATED rlcMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | A flag that indicates whether there are more real-time log configurations than are contained in this list.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcIsTruncated :: Lens.Lens' RealtimeLogConfigs Core.Bool
rlcIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED rlcIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | This parameter indicates where this list of real-time log configurations begins. This list includes real-time log configurations that occur after the marker.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcMarker :: Lens.Lens' RealtimeLogConfigs Types.Marker
rlcMarker = Lens.field @"marker"
{-# DEPRECATED rlcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Contains the list of real-time log configurations.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcItems :: Lens.Lens' RealtimeLogConfigs (Core.Maybe [Types.RealtimeLogConfig])
rlcItems = Lens.field @"items"
{-# DEPRECATED rlcItems "Use generic-lens or generic-optics with 'items' instead." #-}

-- | If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing real-time log configurations where you left off.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlcNextMarker :: Lens.Lens' RealtimeLogConfigs (Core.Maybe Types.NextMarker)
rlcNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED rlcNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

instance Core.FromXML RealtimeLogConfigs where
  parseXML x =
    RealtimeLogConfigs'
      Core.<$> (x Core..@ "MaxItems")
      Core.<*> (x Core..@ "IsTruncated")
      Core.<*> (x Core..@ "Marker")
      Core.<*> (x Core..@? "Items" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "NextMarker")
