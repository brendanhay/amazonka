{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentDimensions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentDimensions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.AttributeDimension
import Network.AWS.Pinpoint.Types.MetricDimension
import Network.AWS.Pinpoint.Types.SegmentBehaviors
import Network.AWS.Pinpoint.Types.SegmentDemographics
import Network.AWS.Pinpoint.Types.SegmentLocation

-- | Specifies the dimension settings for a segment.
--
-- /See:/ 'newSegmentDimensions' smart constructor.
data SegmentDimensions = SegmentDimensions'
  { -- | The demographic-based criteria, such as device platform, for the
    -- segment.
    demographic :: Core.Maybe SegmentDemographics,
    -- | One or more custom attributes to use as criteria for the segment.
    attributes :: Core.Maybe (Core.HashMap Core.Text AttributeDimension),
    -- | One or more custom metrics to use as criteria for the segment.
    metrics :: Core.Maybe (Core.HashMap Core.Text MetricDimension),
    -- | The behavior-based criteria, such as how recently users have used your
    -- app, for the segment.
    behavior :: Core.Maybe SegmentBehaviors,
    -- | One or more custom user attributes to use as criteria for the segment.
    userAttributes :: Core.Maybe (Core.HashMap Core.Text AttributeDimension),
    -- | The location-based criteria, such as region or GPS coordinates, for the
    -- segment.
    location :: Core.Maybe SegmentLocation
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SegmentDimensions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'demographic', 'segmentDimensions_demographic' - The demographic-based criteria, such as device platform, for the
-- segment.
--
-- 'attributes', 'segmentDimensions_attributes' - One or more custom attributes to use as criteria for the segment.
--
-- 'metrics', 'segmentDimensions_metrics' - One or more custom metrics to use as criteria for the segment.
--
-- 'behavior', 'segmentDimensions_behavior' - The behavior-based criteria, such as how recently users have used your
-- app, for the segment.
--
-- 'userAttributes', 'segmentDimensions_userAttributes' - One or more custom user attributes to use as criteria for the segment.
--
-- 'location', 'segmentDimensions_location' - The location-based criteria, such as region or GPS coordinates, for the
-- segment.
newSegmentDimensions ::
  SegmentDimensions
newSegmentDimensions =
  SegmentDimensions'
    { demographic = Core.Nothing,
      attributes = Core.Nothing,
      metrics = Core.Nothing,
      behavior = Core.Nothing,
      userAttributes = Core.Nothing,
      location = Core.Nothing
    }

-- | The demographic-based criteria, such as device platform, for the
-- segment.
segmentDimensions_demographic :: Lens.Lens' SegmentDimensions (Core.Maybe SegmentDemographics)
segmentDimensions_demographic = Lens.lens (\SegmentDimensions' {demographic} -> demographic) (\s@SegmentDimensions' {} a -> s {demographic = a} :: SegmentDimensions)

-- | One or more custom attributes to use as criteria for the segment.
segmentDimensions_attributes :: Lens.Lens' SegmentDimensions (Core.Maybe (Core.HashMap Core.Text AttributeDimension))
segmentDimensions_attributes = Lens.lens (\SegmentDimensions' {attributes} -> attributes) (\s@SegmentDimensions' {} a -> s {attributes = a} :: SegmentDimensions) Core.. Lens.mapping Lens._Coerce

-- | One or more custom metrics to use as criteria for the segment.
segmentDimensions_metrics :: Lens.Lens' SegmentDimensions (Core.Maybe (Core.HashMap Core.Text MetricDimension))
segmentDimensions_metrics = Lens.lens (\SegmentDimensions' {metrics} -> metrics) (\s@SegmentDimensions' {} a -> s {metrics = a} :: SegmentDimensions) Core.. Lens.mapping Lens._Coerce

-- | The behavior-based criteria, such as how recently users have used your
-- app, for the segment.
segmentDimensions_behavior :: Lens.Lens' SegmentDimensions (Core.Maybe SegmentBehaviors)
segmentDimensions_behavior = Lens.lens (\SegmentDimensions' {behavior} -> behavior) (\s@SegmentDimensions' {} a -> s {behavior = a} :: SegmentDimensions)

-- | One or more custom user attributes to use as criteria for the segment.
segmentDimensions_userAttributes :: Lens.Lens' SegmentDimensions (Core.Maybe (Core.HashMap Core.Text AttributeDimension))
segmentDimensions_userAttributes = Lens.lens (\SegmentDimensions' {userAttributes} -> userAttributes) (\s@SegmentDimensions' {} a -> s {userAttributes = a} :: SegmentDimensions) Core.. Lens.mapping Lens._Coerce

-- | The location-based criteria, such as region or GPS coordinates, for the
-- segment.
segmentDimensions_location :: Lens.Lens' SegmentDimensions (Core.Maybe SegmentLocation)
segmentDimensions_location = Lens.lens (\SegmentDimensions' {location} -> location) (\s@SegmentDimensions' {} a -> s {location = a} :: SegmentDimensions)

instance Core.FromJSON SegmentDimensions where
  parseJSON =
    Core.withObject
      "SegmentDimensions"
      ( \x ->
          SegmentDimensions'
            Core.<$> (x Core..:? "Demographic")
            Core.<*> (x Core..:? "Attributes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Metrics" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Behavior")
            Core.<*> (x Core..:? "UserAttributes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Location")
      )

instance Core.Hashable SegmentDimensions

instance Core.NFData SegmentDimensions

instance Core.ToJSON SegmentDimensions where
  toJSON SegmentDimensions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Demographic" Core..=) Core.<$> demographic,
            ("Attributes" Core..=) Core.<$> attributes,
            ("Metrics" Core..=) Core.<$> metrics,
            ("Behavior" Core..=) Core.<$> behavior,
            ("UserAttributes" Core..=) Core.<$> userAttributes,
            ("Location" Core..=) Core.<$> location
          ]
      )
