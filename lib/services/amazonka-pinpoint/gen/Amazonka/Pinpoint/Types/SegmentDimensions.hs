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
-- Module      : Amazonka.Pinpoint.Types.SegmentDimensions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.SegmentDimensions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.AttributeDimension
import Amazonka.Pinpoint.Types.MetricDimension
import Amazonka.Pinpoint.Types.SegmentBehaviors
import Amazonka.Pinpoint.Types.SegmentDemographics
import Amazonka.Pinpoint.Types.SegmentLocation
import qualified Amazonka.Prelude as Prelude

-- | Specifies the dimension settings for a segment.
--
-- /See:/ 'newSegmentDimensions' smart constructor.
data SegmentDimensions = SegmentDimensions'
  { -- | The demographic-based criteria, such as device platform, for the
    -- segment.
    demographic :: Prelude.Maybe SegmentDemographics,
    -- | One or more custom metrics to use as criteria for the segment.
    metrics :: Prelude.Maybe (Prelude.HashMap Prelude.Text MetricDimension),
    -- | One or more custom user attributes to use as criteria for the segment.
    userAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeDimension),
    -- | The location-based criteria, such as region or GPS coordinates, for the
    -- segment.
    location :: Prelude.Maybe SegmentLocation,
    -- | One or more custom attributes to use as criteria for the segment.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeDimension),
    -- | The behavior-based criteria, such as how recently users have used your
    -- app, for the segment.
    behavior :: Prelude.Maybe SegmentBehaviors
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'metrics', 'segmentDimensions_metrics' - One or more custom metrics to use as criteria for the segment.
--
-- 'userAttributes', 'segmentDimensions_userAttributes' - One or more custom user attributes to use as criteria for the segment.
--
-- 'location', 'segmentDimensions_location' - The location-based criteria, such as region or GPS coordinates, for the
-- segment.
--
-- 'attributes', 'segmentDimensions_attributes' - One or more custom attributes to use as criteria for the segment.
--
-- 'behavior', 'segmentDimensions_behavior' - The behavior-based criteria, such as how recently users have used your
-- app, for the segment.
newSegmentDimensions ::
  SegmentDimensions
newSegmentDimensions =
  SegmentDimensions'
    { demographic = Prelude.Nothing,
      metrics = Prelude.Nothing,
      userAttributes = Prelude.Nothing,
      location = Prelude.Nothing,
      attributes = Prelude.Nothing,
      behavior = Prelude.Nothing
    }

-- | The demographic-based criteria, such as device platform, for the
-- segment.
segmentDimensions_demographic :: Lens.Lens' SegmentDimensions (Prelude.Maybe SegmentDemographics)
segmentDimensions_demographic = Lens.lens (\SegmentDimensions' {demographic} -> demographic) (\s@SegmentDimensions' {} a -> s {demographic = a} :: SegmentDimensions)

-- | One or more custom metrics to use as criteria for the segment.
segmentDimensions_metrics :: Lens.Lens' SegmentDimensions (Prelude.Maybe (Prelude.HashMap Prelude.Text MetricDimension))
segmentDimensions_metrics = Lens.lens (\SegmentDimensions' {metrics} -> metrics) (\s@SegmentDimensions' {} a -> s {metrics = a} :: SegmentDimensions) Prelude.. Lens.mapping Lens.coerced

-- | One or more custom user attributes to use as criteria for the segment.
segmentDimensions_userAttributes :: Lens.Lens' SegmentDimensions (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeDimension))
segmentDimensions_userAttributes = Lens.lens (\SegmentDimensions' {userAttributes} -> userAttributes) (\s@SegmentDimensions' {} a -> s {userAttributes = a} :: SegmentDimensions) Prelude.. Lens.mapping Lens.coerced

-- | The location-based criteria, such as region or GPS coordinates, for the
-- segment.
segmentDimensions_location :: Lens.Lens' SegmentDimensions (Prelude.Maybe SegmentLocation)
segmentDimensions_location = Lens.lens (\SegmentDimensions' {location} -> location) (\s@SegmentDimensions' {} a -> s {location = a} :: SegmentDimensions)

-- | One or more custom attributes to use as criteria for the segment.
segmentDimensions_attributes :: Lens.Lens' SegmentDimensions (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeDimension))
segmentDimensions_attributes = Lens.lens (\SegmentDimensions' {attributes} -> attributes) (\s@SegmentDimensions' {} a -> s {attributes = a} :: SegmentDimensions) Prelude.. Lens.mapping Lens.coerced

-- | The behavior-based criteria, such as how recently users have used your
-- app, for the segment.
segmentDimensions_behavior :: Lens.Lens' SegmentDimensions (Prelude.Maybe SegmentBehaviors)
segmentDimensions_behavior = Lens.lens (\SegmentDimensions' {behavior} -> behavior) (\s@SegmentDimensions' {} a -> s {behavior = a} :: SegmentDimensions)

instance Core.FromJSON SegmentDimensions where
  parseJSON =
    Core.withObject
      "SegmentDimensions"
      ( \x ->
          SegmentDimensions'
            Prelude.<$> (x Core..:? "Demographic")
            Prelude.<*> (x Core..:? "Metrics" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "UserAttributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Location")
            Prelude.<*> (x Core..:? "Attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Behavior")
      )

instance Prelude.Hashable SegmentDimensions where
  hashWithSalt _salt SegmentDimensions' {..} =
    _salt `Prelude.hashWithSalt` demographic
      `Prelude.hashWithSalt` metrics
      `Prelude.hashWithSalt` userAttributes
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` behavior

instance Prelude.NFData SegmentDimensions where
  rnf SegmentDimensions' {..} =
    Prelude.rnf demographic
      `Prelude.seq` Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf userAttributes
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf behavior

instance Core.ToJSON SegmentDimensions where
  toJSON SegmentDimensions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Demographic" Core..=) Prelude.<$> demographic,
            ("Metrics" Core..=) Prelude.<$> metrics,
            ("UserAttributes" Core..=)
              Prelude.<$> userAttributes,
            ("Location" Core..=) Prelude.<$> location,
            ("Attributes" Core..=) Prelude.<$> attributes,
            ("Behavior" Core..=) Prelude.<$> behavior
          ]
      )
