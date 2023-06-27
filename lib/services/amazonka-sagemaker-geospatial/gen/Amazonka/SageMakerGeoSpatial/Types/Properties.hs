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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.Properties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.Properties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Properties associated with the Item.
--
-- /See:/ 'newProperties' smart constructor.
data Properties = Properties'
  { -- | Estimate of cloud cover.
    eoCloudCover :: Prelude.Maybe Prelude.Double,
    -- | Land cloud cover for Landsat Data Collection.
    landsatCloudCoverLand :: Prelude.Maybe Prelude.Double,
    -- | Platform property. Platform refers to the unique name of the specific
    -- platform the instrument is attached to. For satellites it is the name of
    -- the satellite, eg. landsat-8 (Landsat-8), sentinel-2a.
    platform :: Prelude.Maybe Prelude.Text,
    -- | The angle from the sensor between nadir (straight down) and the scene
    -- center. Measured in degrees (0-90).
    viewOffNadir :: Prelude.Maybe Prelude.Double,
    -- | The sun azimuth angle. From the scene center point on the ground, this
    -- is the angle between truth north and the sun. Measured clockwise in
    -- degrees (0-360).
    viewSunAzimuth :: Prelude.Maybe Prelude.Double,
    -- | The sun elevation angle. The angle from the tangent of the scene center
    -- point to the sun. Measured from the horizon in degrees (-90-90).
    -- Negative values indicate the sun is below the horizon, e.g. sun
    -- elevation of -10° means the data was captured during
    -- <https://www.timeanddate.com/astronomy/different-types-twilight.html nautical twilight>.
    viewSunElevation :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Properties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eoCloudCover', 'properties_eoCloudCover' - Estimate of cloud cover.
--
-- 'landsatCloudCoverLand', 'properties_landsatCloudCoverLand' - Land cloud cover for Landsat Data Collection.
--
-- 'platform', 'properties_platform' - Platform property. Platform refers to the unique name of the specific
-- platform the instrument is attached to. For satellites it is the name of
-- the satellite, eg. landsat-8 (Landsat-8), sentinel-2a.
--
-- 'viewOffNadir', 'properties_viewOffNadir' - The angle from the sensor between nadir (straight down) and the scene
-- center. Measured in degrees (0-90).
--
-- 'viewSunAzimuth', 'properties_viewSunAzimuth' - The sun azimuth angle. From the scene center point on the ground, this
-- is the angle between truth north and the sun. Measured clockwise in
-- degrees (0-360).
--
-- 'viewSunElevation', 'properties_viewSunElevation' - The sun elevation angle. The angle from the tangent of the scene center
-- point to the sun. Measured from the horizon in degrees (-90-90).
-- Negative values indicate the sun is below the horizon, e.g. sun
-- elevation of -10° means the data was captured during
-- <https://www.timeanddate.com/astronomy/different-types-twilight.html nautical twilight>.
newProperties ::
  Properties
newProperties =
  Properties'
    { eoCloudCover = Prelude.Nothing,
      landsatCloudCoverLand = Prelude.Nothing,
      platform = Prelude.Nothing,
      viewOffNadir = Prelude.Nothing,
      viewSunAzimuth = Prelude.Nothing,
      viewSunElevation = Prelude.Nothing
    }

-- | Estimate of cloud cover.
properties_eoCloudCover :: Lens.Lens' Properties (Prelude.Maybe Prelude.Double)
properties_eoCloudCover = Lens.lens (\Properties' {eoCloudCover} -> eoCloudCover) (\s@Properties' {} a -> s {eoCloudCover = a} :: Properties)

-- | Land cloud cover for Landsat Data Collection.
properties_landsatCloudCoverLand :: Lens.Lens' Properties (Prelude.Maybe Prelude.Double)
properties_landsatCloudCoverLand = Lens.lens (\Properties' {landsatCloudCoverLand} -> landsatCloudCoverLand) (\s@Properties' {} a -> s {landsatCloudCoverLand = a} :: Properties)

-- | Platform property. Platform refers to the unique name of the specific
-- platform the instrument is attached to. For satellites it is the name of
-- the satellite, eg. landsat-8 (Landsat-8), sentinel-2a.
properties_platform :: Lens.Lens' Properties (Prelude.Maybe Prelude.Text)
properties_platform = Lens.lens (\Properties' {platform} -> platform) (\s@Properties' {} a -> s {platform = a} :: Properties)

-- | The angle from the sensor between nadir (straight down) and the scene
-- center. Measured in degrees (0-90).
properties_viewOffNadir :: Lens.Lens' Properties (Prelude.Maybe Prelude.Double)
properties_viewOffNadir = Lens.lens (\Properties' {viewOffNadir} -> viewOffNadir) (\s@Properties' {} a -> s {viewOffNadir = a} :: Properties)

-- | The sun azimuth angle. From the scene center point on the ground, this
-- is the angle between truth north and the sun. Measured clockwise in
-- degrees (0-360).
properties_viewSunAzimuth :: Lens.Lens' Properties (Prelude.Maybe Prelude.Double)
properties_viewSunAzimuth = Lens.lens (\Properties' {viewSunAzimuth} -> viewSunAzimuth) (\s@Properties' {} a -> s {viewSunAzimuth = a} :: Properties)

-- | The sun elevation angle. The angle from the tangent of the scene center
-- point to the sun. Measured from the horizon in degrees (-90-90).
-- Negative values indicate the sun is below the horizon, e.g. sun
-- elevation of -10° means the data was captured during
-- <https://www.timeanddate.com/astronomy/different-types-twilight.html nautical twilight>.
properties_viewSunElevation :: Lens.Lens' Properties (Prelude.Maybe Prelude.Double)
properties_viewSunElevation = Lens.lens (\Properties' {viewSunElevation} -> viewSunElevation) (\s@Properties' {} a -> s {viewSunElevation = a} :: Properties)

instance Data.FromJSON Properties where
  parseJSON =
    Data.withObject
      "Properties"
      ( \x ->
          Properties'
            Prelude.<$> (x Data..:? "EoCloudCover")
            Prelude.<*> (x Data..:? "LandsatCloudCoverLand")
            Prelude.<*> (x Data..:? "Platform")
            Prelude.<*> (x Data..:? "ViewOffNadir")
            Prelude.<*> (x Data..:? "ViewSunAzimuth")
            Prelude.<*> (x Data..:? "ViewSunElevation")
      )

instance Prelude.Hashable Properties where
  hashWithSalt _salt Properties' {..} =
    _salt
      `Prelude.hashWithSalt` eoCloudCover
      `Prelude.hashWithSalt` landsatCloudCoverLand
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` viewOffNadir
      `Prelude.hashWithSalt` viewSunAzimuth
      `Prelude.hashWithSalt` viewSunElevation

instance Prelude.NFData Properties where
  rnf Properties' {..} =
    Prelude.rnf eoCloudCover
      `Prelude.seq` Prelude.rnf landsatCloudCoverLand
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf viewOffNadir
      `Prelude.seq` Prelude.rnf viewSunAzimuth
      `Prelude.seq` Prelude.rnf viewSunElevation
