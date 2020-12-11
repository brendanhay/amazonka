-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentDimensions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentDimensions
  ( SegmentDimensions (..),

    -- * Smart constructor
    mkSegmentDimensions,

    -- * Lenses
    sdMetrics,
    sdLocation,
    sdDemographic,
    sdUserAttributes,
    sdBehavior,
    sdAttributes,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.AttributeDimension
import Network.AWS.Pinpoint.Types.MetricDimension
import Network.AWS.Pinpoint.Types.SegmentBehaviors
import Network.AWS.Pinpoint.Types.SegmentDemographics
import Network.AWS.Pinpoint.Types.SegmentLocation
import qualified Network.AWS.Prelude as Lude

-- | Specifies the dimension settings for a segment.
--
-- /See:/ 'mkSegmentDimensions' smart constructor.
data SegmentDimensions = SegmentDimensions'
  { metrics ::
      Lude.Maybe (Lude.HashMap Lude.Text (MetricDimension)),
    location :: Lude.Maybe SegmentLocation,
    demographic :: Lude.Maybe SegmentDemographics,
    userAttributes ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (AttributeDimension)),
    behavior :: Lude.Maybe SegmentBehaviors,
    attributes ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (AttributeDimension))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SegmentDimensions' with the minimum fields required to make a request.
--
-- * 'attributes' - One or more custom attributes to use as criteria for the segment.
-- * 'behavior' - The behavior-based criteria, such as how recently users have used your app, for the segment.
-- * 'demographic' - The demographic-based criteria, such as device platform, for the segment.
-- * 'location' - The location-based criteria, such as region or GPS coordinates, for the segment.
-- * 'metrics' - One or more custom metrics to use as criteria for the segment.
-- * 'userAttributes' - One or more custom user attributes to use as criteria for the segment.
mkSegmentDimensions ::
  SegmentDimensions
mkSegmentDimensions =
  SegmentDimensions'
    { metrics = Lude.Nothing,
      location = Lude.Nothing,
      demographic = Lude.Nothing,
      userAttributes = Lude.Nothing,
      behavior = Lude.Nothing,
      attributes = Lude.Nothing
    }

-- | One or more custom metrics to use as criteria for the segment.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdMetrics :: Lens.Lens' SegmentDimensions (Lude.Maybe (Lude.HashMap Lude.Text (MetricDimension)))
sdMetrics = Lens.lens (metrics :: SegmentDimensions -> Lude.Maybe (Lude.HashMap Lude.Text (MetricDimension))) (\s a -> s {metrics = a} :: SegmentDimensions)
{-# DEPRECATED sdMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The location-based criteria, such as region or GPS coordinates, for the segment.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdLocation :: Lens.Lens' SegmentDimensions (Lude.Maybe SegmentLocation)
sdLocation = Lens.lens (location :: SegmentDimensions -> Lude.Maybe SegmentLocation) (\s a -> s {location = a} :: SegmentDimensions)
{-# DEPRECATED sdLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The demographic-based criteria, such as device platform, for the segment.
--
-- /Note:/ Consider using 'demographic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdDemographic :: Lens.Lens' SegmentDimensions (Lude.Maybe SegmentDemographics)
sdDemographic = Lens.lens (demographic :: SegmentDimensions -> Lude.Maybe SegmentDemographics) (\s a -> s {demographic = a} :: SegmentDimensions)
{-# DEPRECATED sdDemographic "Use generic-lens or generic-optics with 'demographic' instead." #-}

-- | One or more custom user attributes to use as criteria for the segment.
--
-- /Note:/ Consider using 'userAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdUserAttributes :: Lens.Lens' SegmentDimensions (Lude.Maybe (Lude.HashMap Lude.Text (AttributeDimension)))
sdUserAttributes = Lens.lens (userAttributes :: SegmentDimensions -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeDimension))) (\s a -> s {userAttributes = a} :: SegmentDimensions)
{-# DEPRECATED sdUserAttributes "Use generic-lens or generic-optics with 'userAttributes' instead." #-}

-- | The behavior-based criteria, such as how recently users have used your app, for the segment.
--
-- /Note:/ Consider using 'behavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdBehavior :: Lens.Lens' SegmentDimensions (Lude.Maybe SegmentBehaviors)
sdBehavior = Lens.lens (behavior :: SegmentDimensions -> Lude.Maybe SegmentBehaviors) (\s a -> s {behavior = a} :: SegmentDimensions)
{-# DEPRECATED sdBehavior "Use generic-lens or generic-optics with 'behavior' instead." #-}

-- | One or more custom attributes to use as criteria for the segment.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdAttributes :: Lens.Lens' SegmentDimensions (Lude.Maybe (Lude.HashMap Lude.Text (AttributeDimension)))
sdAttributes = Lens.lens (attributes :: SegmentDimensions -> Lude.Maybe (Lude.HashMap Lude.Text (AttributeDimension))) (\s a -> s {attributes = a} :: SegmentDimensions)
{-# DEPRECATED sdAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.FromJSON SegmentDimensions where
  parseJSON =
    Lude.withObject
      "SegmentDimensions"
      ( \x ->
          SegmentDimensions'
            Lude.<$> (x Lude..:? "Metrics" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Location")
            Lude.<*> (x Lude..:? "Demographic")
            Lude.<*> (x Lude..:? "UserAttributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Behavior")
            Lude.<*> (x Lude..:? "Attributes" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON SegmentDimensions where
  toJSON SegmentDimensions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Metrics" Lude..=) Lude.<$> metrics,
            ("Location" Lude..=) Lude.<$> location,
            ("Demographic" Lude..=) Lude.<$> demographic,
            ("UserAttributes" Lude..=) Lude.<$> userAttributes,
            ("Behavior" Lude..=) Lude.<$> behavior,
            ("Attributes" Lude..=) Lude.<$> attributes
          ]
      )
