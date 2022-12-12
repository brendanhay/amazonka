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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.SegmentDimensions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { -- | One or more custom attributes to use as criteria for the segment.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeDimension),
    -- | The behavior-based criteria, such as how recently users have used your
    -- app, for the segment.
    behavior :: Prelude.Maybe SegmentBehaviors,
    -- | The demographic-based criteria, such as device platform, for the
    -- segment.
    demographic :: Prelude.Maybe SegmentDemographics,
    -- | The location-based criteria, such as region or GPS coordinates, for the
    -- segment.
    location :: Prelude.Maybe SegmentLocation,
    -- | One or more custom metrics to use as criteria for the segment.
    metrics :: Prelude.Maybe (Prelude.HashMap Prelude.Text MetricDimension),
    -- | One or more custom user attributes to use as criteria for the segment.
    userAttributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeDimension)
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
-- 'attributes', 'segmentDimensions_attributes' - One or more custom attributes to use as criteria for the segment.
--
-- 'behavior', 'segmentDimensions_behavior' - The behavior-based criteria, such as how recently users have used your
-- app, for the segment.
--
-- 'demographic', 'segmentDimensions_demographic' - The demographic-based criteria, such as device platform, for the
-- segment.
--
-- 'location', 'segmentDimensions_location' - The location-based criteria, such as region or GPS coordinates, for the
-- segment.
--
-- 'metrics', 'segmentDimensions_metrics' - One or more custom metrics to use as criteria for the segment.
--
-- 'userAttributes', 'segmentDimensions_userAttributes' - One or more custom user attributes to use as criteria for the segment.
newSegmentDimensions ::
  SegmentDimensions
newSegmentDimensions =
  SegmentDimensions'
    { attributes = Prelude.Nothing,
      behavior = Prelude.Nothing,
      demographic = Prelude.Nothing,
      location = Prelude.Nothing,
      metrics = Prelude.Nothing,
      userAttributes = Prelude.Nothing
    }

-- | One or more custom attributes to use as criteria for the segment.
segmentDimensions_attributes :: Lens.Lens' SegmentDimensions (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeDimension))
segmentDimensions_attributes = Lens.lens (\SegmentDimensions' {attributes} -> attributes) (\s@SegmentDimensions' {} a -> s {attributes = a} :: SegmentDimensions) Prelude.. Lens.mapping Lens.coerced

-- | The behavior-based criteria, such as how recently users have used your
-- app, for the segment.
segmentDimensions_behavior :: Lens.Lens' SegmentDimensions (Prelude.Maybe SegmentBehaviors)
segmentDimensions_behavior = Lens.lens (\SegmentDimensions' {behavior} -> behavior) (\s@SegmentDimensions' {} a -> s {behavior = a} :: SegmentDimensions)

-- | The demographic-based criteria, such as device platform, for the
-- segment.
segmentDimensions_demographic :: Lens.Lens' SegmentDimensions (Prelude.Maybe SegmentDemographics)
segmentDimensions_demographic = Lens.lens (\SegmentDimensions' {demographic} -> demographic) (\s@SegmentDimensions' {} a -> s {demographic = a} :: SegmentDimensions)

-- | The location-based criteria, such as region or GPS coordinates, for the
-- segment.
segmentDimensions_location :: Lens.Lens' SegmentDimensions (Prelude.Maybe SegmentLocation)
segmentDimensions_location = Lens.lens (\SegmentDimensions' {location} -> location) (\s@SegmentDimensions' {} a -> s {location = a} :: SegmentDimensions)

-- | One or more custom metrics to use as criteria for the segment.
segmentDimensions_metrics :: Lens.Lens' SegmentDimensions (Prelude.Maybe (Prelude.HashMap Prelude.Text MetricDimension))
segmentDimensions_metrics = Lens.lens (\SegmentDimensions' {metrics} -> metrics) (\s@SegmentDimensions' {} a -> s {metrics = a} :: SegmentDimensions) Prelude.. Lens.mapping Lens.coerced

-- | One or more custom user attributes to use as criteria for the segment.
segmentDimensions_userAttributes :: Lens.Lens' SegmentDimensions (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeDimension))
segmentDimensions_userAttributes = Lens.lens (\SegmentDimensions' {userAttributes} -> userAttributes) (\s@SegmentDimensions' {} a -> s {userAttributes = a} :: SegmentDimensions) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SegmentDimensions where
  parseJSON =
    Data.withObject
      "SegmentDimensions"
      ( \x ->
          SegmentDimensions'
            Prelude.<$> (x Data..:? "Attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Behavior")
            Prelude.<*> (x Data..:? "Demographic")
            Prelude.<*> (x Data..:? "Location")
            Prelude.<*> (x Data..:? "Metrics" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "UserAttributes"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SegmentDimensions where
  hashWithSalt _salt SegmentDimensions' {..} =
    _salt `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` behavior
      `Prelude.hashWithSalt` demographic
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` metrics
      `Prelude.hashWithSalt` userAttributes

instance Prelude.NFData SegmentDimensions where
  rnf SegmentDimensions' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf behavior
      `Prelude.seq` Prelude.rnf demographic
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf userAttributes

instance Data.ToJSON SegmentDimensions where
  toJSON SegmentDimensions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Attributes" Data..=) Prelude.<$> attributes,
            ("Behavior" Data..=) Prelude.<$> behavior,
            ("Demographic" Data..=) Prelude.<$> demographic,
            ("Location" Data..=) Prelude.<$> location,
            ("Metrics" Data..=) Prelude.<$> metrics,
            ("UserAttributes" Data..=)
              Prelude.<$> userAttributes
          ]
      )
