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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.Property
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.Property where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.EoCloudCoverInput
import Amazonka.SageMakerGeoSpatial.Types.LandsatCloudCoverLandInput
import Amazonka.SageMakerGeoSpatial.Types.PlatformInput
import Amazonka.SageMakerGeoSpatial.Types.ViewOffNadirInput
import Amazonka.SageMakerGeoSpatial.Types.ViewSunAzimuthInput
import Amazonka.SageMakerGeoSpatial.Types.ViewSunElevationInput

-- | Represents a single searchable property to search on.
--
-- /See:/ 'newProperty' smart constructor.
data Property = Property'
  { -- | The structure representing EoCloudCover property filter containing a
    -- lower bound and upper bound.
    eoCloudCover :: Prelude.Maybe EoCloudCoverInput,
    -- | The structure representing Land Cloud Cover property filter for Landsat
    -- collection containing a lower bound and upper bound.
    landsatCloudCoverLand :: Prelude.Maybe LandsatCloudCoverLandInput,
    -- | The structure representing Platform property filter consisting of value
    -- and comparison operator.
    platform :: Prelude.Maybe PlatformInput,
    -- | The structure representing ViewOffNadir property filter containing a
    -- lower bound and upper bound.
    viewOffNadir :: Prelude.Maybe ViewOffNadirInput,
    -- | The structure representing ViewSunAzimuth property filter containing a
    -- lower bound and upper bound.
    viewSunAzimuth :: Prelude.Maybe ViewSunAzimuthInput,
    -- | The structure representing ViewSunElevation property filter containing a
    -- lower bound and upper bound.
    viewSunElevation :: Prelude.Maybe ViewSunElevationInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Property' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eoCloudCover', 'property_eoCloudCover' - The structure representing EoCloudCover property filter containing a
-- lower bound and upper bound.
--
-- 'landsatCloudCoverLand', 'property_landsatCloudCoverLand' - The structure representing Land Cloud Cover property filter for Landsat
-- collection containing a lower bound and upper bound.
--
-- 'platform', 'property_platform' - The structure representing Platform property filter consisting of value
-- and comparison operator.
--
-- 'viewOffNadir', 'property_viewOffNadir' - The structure representing ViewOffNadir property filter containing a
-- lower bound and upper bound.
--
-- 'viewSunAzimuth', 'property_viewSunAzimuth' - The structure representing ViewSunAzimuth property filter containing a
-- lower bound and upper bound.
--
-- 'viewSunElevation', 'property_viewSunElevation' - The structure representing ViewSunElevation property filter containing a
-- lower bound and upper bound.
newProperty ::
  Property
newProperty =
  Property'
    { eoCloudCover = Prelude.Nothing,
      landsatCloudCoverLand = Prelude.Nothing,
      platform = Prelude.Nothing,
      viewOffNadir = Prelude.Nothing,
      viewSunAzimuth = Prelude.Nothing,
      viewSunElevation = Prelude.Nothing
    }

-- | The structure representing EoCloudCover property filter containing a
-- lower bound and upper bound.
property_eoCloudCover :: Lens.Lens' Property (Prelude.Maybe EoCloudCoverInput)
property_eoCloudCover = Lens.lens (\Property' {eoCloudCover} -> eoCloudCover) (\s@Property' {} a -> s {eoCloudCover = a} :: Property)

-- | The structure representing Land Cloud Cover property filter for Landsat
-- collection containing a lower bound and upper bound.
property_landsatCloudCoverLand :: Lens.Lens' Property (Prelude.Maybe LandsatCloudCoverLandInput)
property_landsatCloudCoverLand = Lens.lens (\Property' {landsatCloudCoverLand} -> landsatCloudCoverLand) (\s@Property' {} a -> s {landsatCloudCoverLand = a} :: Property)

-- | The structure representing Platform property filter consisting of value
-- and comparison operator.
property_platform :: Lens.Lens' Property (Prelude.Maybe PlatformInput)
property_platform = Lens.lens (\Property' {platform} -> platform) (\s@Property' {} a -> s {platform = a} :: Property)

-- | The structure representing ViewOffNadir property filter containing a
-- lower bound and upper bound.
property_viewOffNadir :: Lens.Lens' Property (Prelude.Maybe ViewOffNadirInput)
property_viewOffNadir = Lens.lens (\Property' {viewOffNadir} -> viewOffNadir) (\s@Property' {} a -> s {viewOffNadir = a} :: Property)

-- | The structure representing ViewSunAzimuth property filter containing a
-- lower bound and upper bound.
property_viewSunAzimuth :: Lens.Lens' Property (Prelude.Maybe ViewSunAzimuthInput)
property_viewSunAzimuth = Lens.lens (\Property' {viewSunAzimuth} -> viewSunAzimuth) (\s@Property' {} a -> s {viewSunAzimuth = a} :: Property)

-- | The structure representing ViewSunElevation property filter containing a
-- lower bound and upper bound.
property_viewSunElevation :: Lens.Lens' Property (Prelude.Maybe ViewSunElevationInput)
property_viewSunElevation = Lens.lens (\Property' {viewSunElevation} -> viewSunElevation) (\s@Property' {} a -> s {viewSunElevation = a} :: Property)

instance Data.FromJSON Property where
  parseJSON =
    Data.withObject
      "Property"
      ( \x ->
          Property'
            Prelude.<$> (x Data..:? "EoCloudCover")
            Prelude.<*> (x Data..:? "LandsatCloudCoverLand")
            Prelude.<*> (x Data..:? "Platform")
            Prelude.<*> (x Data..:? "ViewOffNadir")
            Prelude.<*> (x Data..:? "ViewSunAzimuth")
            Prelude.<*> (x Data..:? "ViewSunElevation")
      )

instance Prelude.Hashable Property where
  hashWithSalt _salt Property' {..} =
    _salt
      `Prelude.hashWithSalt` eoCloudCover
      `Prelude.hashWithSalt` landsatCloudCoverLand
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` viewOffNadir
      `Prelude.hashWithSalt` viewSunAzimuth
      `Prelude.hashWithSalt` viewSunElevation

instance Prelude.NFData Property where
  rnf Property' {..} =
    Prelude.rnf eoCloudCover
      `Prelude.seq` Prelude.rnf landsatCloudCoverLand
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf viewOffNadir
      `Prelude.seq` Prelude.rnf viewSunAzimuth
      `Prelude.seq` Prelude.rnf viewSunElevation

instance Data.ToJSON Property where
  toJSON Property' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EoCloudCover" Data..=) Prelude.<$> eoCloudCover,
            ("LandsatCloudCoverLand" Data..=)
              Prelude.<$> landsatCloudCoverLand,
            ("Platform" Data..=) Prelude.<$> platform,
            ("ViewOffNadir" Data..=) Prelude.<$> viewOffNadir,
            ("ViewSunAzimuth" Data..=)
              Prelude.<$> viewSunAzimuth,
            ("ViewSunElevation" Data..=)
              Prelude.<$> viewSunElevation
          ]
      )
