{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentDimensions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.SegmentDimensions
  ( SegmentDimensions (..)
  -- * Smart constructor
  , mkSegmentDimensions
  -- * Lenses
  , sdAttributes
  , sdBehavior
  , sdDemographic
  , sdLocation
  , sdMetrics
  , sdUserAttributes
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.AttributeDimension as Types
import qualified Network.AWS.Pinpoint.Types.MetricDimension as Types
import qualified Network.AWS.Pinpoint.Types.SegmentBehaviors as Types
import qualified Network.AWS.Pinpoint.Types.SegmentDemographics as Types
import qualified Network.AWS.Pinpoint.Types.SegmentLocation as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the dimension settings for a segment.
--
-- /See:/ 'mkSegmentDimensions' smart constructor.
data SegmentDimensions = SegmentDimensions'
  { attributes :: Core.Maybe (Core.HashMap Core.Text Types.AttributeDimension)
    -- ^ One or more custom attributes to use as criteria for the segment.
  , behavior :: Core.Maybe Types.SegmentBehaviors
    -- ^ The behavior-based criteria, such as how recently users have used your app, for the segment.
  , demographic :: Core.Maybe Types.SegmentDemographics
    -- ^ The demographic-based criteria, such as device platform, for the segment.
  , location :: Core.Maybe Types.SegmentLocation
    -- ^ The location-based criteria, such as region or GPS coordinates, for the segment.
  , metrics :: Core.Maybe (Core.HashMap Core.Text Types.MetricDimension)
    -- ^ One or more custom metrics to use as criteria for the segment.
  , userAttributes :: Core.Maybe (Core.HashMap Core.Text Types.AttributeDimension)
    -- ^ One or more custom user attributes to use as criteria for the segment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SegmentDimensions' value with any optional fields omitted.
mkSegmentDimensions
    :: SegmentDimensions
mkSegmentDimensions
  = SegmentDimensions'{attributes = Core.Nothing,
                       behavior = Core.Nothing, demographic = Core.Nothing,
                       location = Core.Nothing, metrics = Core.Nothing,
                       userAttributes = Core.Nothing}

-- | One or more custom attributes to use as criteria for the segment.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdAttributes :: Lens.Lens' SegmentDimensions (Core.Maybe (Core.HashMap Core.Text Types.AttributeDimension))
sdAttributes = Lens.field @"attributes"
{-# INLINEABLE sdAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The behavior-based criteria, such as how recently users have used your app, for the segment.
--
-- /Note:/ Consider using 'behavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdBehavior :: Lens.Lens' SegmentDimensions (Core.Maybe Types.SegmentBehaviors)
sdBehavior = Lens.field @"behavior"
{-# INLINEABLE sdBehavior #-}
{-# DEPRECATED behavior "Use generic-lens or generic-optics with 'behavior' instead"  #-}

-- | The demographic-based criteria, such as device platform, for the segment.
--
-- /Note:/ Consider using 'demographic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdDemographic :: Lens.Lens' SegmentDimensions (Core.Maybe Types.SegmentDemographics)
sdDemographic = Lens.field @"demographic"
{-# INLINEABLE sdDemographic #-}
{-# DEPRECATED demographic "Use generic-lens or generic-optics with 'demographic' instead"  #-}

-- | The location-based criteria, such as region or GPS coordinates, for the segment.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdLocation :: Lens.Lens' SegmentDimensions (Core.Maybe Types.SegmentLocation)
sdLocation = Lens.field @"location"
{-# INLINEABLE sdLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | One or more custom metrics to use as criteria for the segment.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdMetrics :: Lens.Lens' SegmentDimensions (Core.Maybe (Core.HashMap Core.Text Types.MetricDimension))
sdMetrics = Lens.field @"metrics"
{-# INLINEABLE sdMetrics #-}
{-# DEPRECATED metrics "Use generic-lens or generic-optics with 'metrics' instead"  #-}

-- | One or more custom user attributes to use as criteria for the segment.
--
-- /Note:/ Consider using 'userAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdUserAttributes :: Lens.Lens' SegmentDimensions (Core.Maybe (Core.HashMap Core.Text Types.AttributeDimension))
sdUserAttributes = Lens.field @"userAttributes"
{-# INLINEABLE sdUserAttributes #-}
{-# DEPRECATED userAttributes "Use generic-lens or generic-optics with 'userAttributes' instead"  #-}

instance Core.FromJSON SegmentDimensions where
        toJSON SegmentDimensions{..}
          = Core.object
              (Core.catMaybes
                 [("Attributes" Core..=) Core.<$> attributes,
                  ("Behavior" Core..=) Core.<$> behavior,
                  ("Demographic" Core..=) Core.<$> demographic,
                  ("Location" Core..=) Core.<$> location,
                  ("Metrics" Core..=) Core.<$> metrics,
                  ("UserAttributes" Core..=) Core.<$> userAttributes])

instance Core.FromJSON SegmentDimensions where
        parseJSON
          = Core.withObject "SegmentDimensions" Core.$
              \ x ->
                SegmentDimensions' Core.<$>
                  (x Core..:? "Attributes") Core.<*> x Core..:? "Behavior" Core.<*>
                    x Core..:? "Demographic"
                    Core.<*> x Core..:? "Location"
                    Core.<*> x Core..:? "Metrics"
                    Core.<*> x Core..:? "UserAttributes"
