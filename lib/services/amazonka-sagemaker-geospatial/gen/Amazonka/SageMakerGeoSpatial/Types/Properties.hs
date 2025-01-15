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
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.Properties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newProperties' smart constructor.
data Properties = Properties'
  { eoCloudCover :: Prelude.Maybe Prelude.Double,
    landsatCloudCoverLand :: Prelude.Maybe Prelude.Double,
    platform :: Prelude.Maybe Prelude.Text,
    viewOffNadir :: Prelude.Maybe Prelude.Double,
    viewSunAzimuth :: Prelude.Maybe Prelude.Double,
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
-- 'eoCloudCover', 'properties_eoCloudCover' -
--
-- 'landsatCloudCoverLand', 'properties_landsatCloudCoverLand' -
--
-- 'platform', 'properties_platform' -
--
-- 'viewOffNadir', 'properties_viewOffNadir' -
--
-- 'viewSunAzimuth', 'properties_viewSunAzimuth' -
--
-- 'viewSunElevation', 'properties_viewSunElevation' -
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

properties_eoCloudCover :: Lens.Lens' Properties (Prelude.Maybe Prelude.Double)
properties_eoCloudCover = Lens.lens (\Properties' {eoCloudCover} -> eoCloudCover) (\s@Properties' {} a -> s {eoCloudCover = a} :: Properties)

properties_landsatCloudCoverLand :: Lens.Lens' Properties (Prelude.Maybe Prelude.Double)
properties_landsatCloudCoverLand = Lens.lens (\Properties' {landsatCloudCoverLand} -> landsatCloudCoverLand) (\s@Properties' {} a -> s {landsatCloudCoverLand = a} :: Properties)

properties_platform :: Lens.Lens' Properties (Prelude.Maybe Prelude.Text)
properties_platform = Lens.lens (\Properties' {platform} -> platform) (\s@Properties' {} a -> s {platform = a} :: Properties)

properties_viewOffNadir :: Lens.Lens' Properties (Prelude.Maybe Prelude.Double)
properties_viewOffNadir = Lens.lens (\Properties' {viewOffNadir} -> viewOffNadir) (\s@Properties' {} a -> s {viewOffNadir = a} :: Properties)

properties_viewSunAzimuth :: Lens.Lens' Properties (Prelude.Maybe Prelude.Double)
properties_viewSunAzimuth = Lens.lens (\Properties' {viewSunAzimuth} -> viewSunAzimuth) (\s@Properties' {} a -> s {viewSunAzimuth = a} :: Properties)

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
    Prelude.rnf eoCloudCover `Prelude.seq`
      Prelude.rnf landsatCloudCoverLand `Prelude.seq`
        Prelude.rnf platform `Prelude.seq`
          Prelude.rnf viewOffNadir `Prelude.seq`
            Prelude.rnf viewSunAzimuth `Prelude.seq`
              Prelude.rnf viewSunElevation
