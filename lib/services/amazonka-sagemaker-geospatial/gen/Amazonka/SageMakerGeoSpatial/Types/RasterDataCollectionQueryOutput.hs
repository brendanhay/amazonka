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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.RasterDataCollectionQueryOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.RasterDataCollectionQueryOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.AreaOfInterest
import Amazonka.SageMakerGeoSpatial.Types.PropertyFilters
import Amazonka.SageMakerGeoSpatial.Types.TimeRangeFilterOutput

-- | The output structure contains the Raster Data Collection Query input
-- along with some additional metadata.
--
-- /See:/ 'newRasterDataCollectionQueryOutput' smart constructor.
data RasterDataCollectionQueryOutput = RasterDataCollectionQueryOutput'
  { -- | The Area of Interest used in the search.
    areaOfInterest :: Prelude.Maybe AreaOfInterest,
    -- | Property filters used in the search.
    propertyFilters :: Prelude.Maybe PropertyFilters,
    -- | The ARN of the Raster Data Collection against which the search is done.
    rasterDataCollectionArn :: Prelude.Text,
    -- | The name of the raster data collection.
    rasterDataCollectionName :: Prelude.Text,
    -- | The TimeRange filter used in the search.
    timeRangeFilter :: Data.Sensitive TimeRangeFilterOutput
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RasterDataCollectionQueryOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'areaOfInterest', 'rasterDataCollectionQueryOutput_areaOfInterest' - The Area of Interest used in the search.
--
-- 'propertyFilters', 'rasterDataCollectionQueryOutput_propertyFilters' - Property filters used in the search.
--
-- 'rasterDataCollectionArn', 'rasterDataCollectionQueryOutput_rasterDataCollectionArn' - The ARN of the Raster Data Collection against which the search is done.
--
-- 'rasterDataCollectionName', 'rasterDataCollectionQueryOutput_rasterDataCollectionName' - The name of the raster data collection.
--
-- 'timeRangeFilter', 'rasterDataCollectionQueryOutput_timeRangeFilter' - The TimeRange filter used in the search.
newRasterDataCollectionQueryOutput ::
  -- | 'rasterDataCollectionArn'
  Prelude.Text ->
  -- | 'rasterDataCollectionName'
  Prelude.Text ->
  -- | 'timeRangeFilter'
  TimeRangeFilterOutput ->
  RasterDataCollectionQueryOutput
newRasterDataCollectionQueryOutput
  pRasterDataCollectionArn_
  pRasterDataCollectionName_
  pTimeRangeFilter_ =
    RasterDataCollectionQueryOutput'
      { areaOfInterest =
          Prelude.Nothing,
        propertyFilters = Prelude.Nothing,
        rasterDataCollectionArn =
          pRasterDataCollectionArn_,
        rasterDataCollectionName =
          pRasterDataCollectionName_,
        timeRangeFilter =
          Data._Sensitive Lens.# pTimeRangeFilter_
      }

-- | The Area of Interest used in the search.
rasterDataCollectionQueryOutput_areaOfInterest :: Lens.Lens' RasterDataCollectionQueryOutput (Prelude.Maybe AreaOfInterest)
rasterDataCollectionQueryOutput_areaOfInterest = Lens.lens (\RasterDataCollectionQueryOutput' {areaOfInterest} -> areaOfInterest) (\s@RasterDataCollectionQueryOutput' {} a -> s {areaOfInterest = a} :: RasterDataCollectionQueryOutput)

-- | Property filters used in the search.
rasterDataCollectionQueryOutput_propertyFilters :: Lens.Lens' RasterDataCollectionQueryOutput (Prelude.Maybe PropertyFilters)
rasterDataCollectionQueryOutput_propertyFilters = Lens.lens (\RasterDataCollectionQueryOutput' {propertyFilters} -> propertyFilters) (\s@RasterDataCollectionQueryOutput' {} a -> s {propertyFilters = a} :: RasterDataCollectionQueryOutput)

-- | The ARN of the Raster Data Collection against which the search is done.
rasterDataCollectionQueryOutput_rasterDataCollectionArn :: Lens.Lens' RasterDataCollectionQueryOutput Prelude.Text
rasterDataCollectionQueryOutput_rasterDataCollectionArn = Lens.lens (\RasterDataCollectionQueryOutput' {rasterDataCollectionArn} -> rasterDataCollectionArn) (\s@RasterDataCollectionQueryOutput' {} a -> s {rasterDataCollectionArn = a} :: RasterDataCollectionQueryOutput)

-- | The name of the raster data collection.
rasterDataCollectionQueryOutput_rasterDataCollectionName :: Lens.Lens' RasterDataCollectionQueryOutput Prelude.Text
rasterDataCollectionQueryOutput_rasterDataCollectionName = Lens.lens (\RasterDataCollectionQueryOutput' {rasterDataCollectionName} -> rasterDataCollectionName) (\s@RasterDataCollectionQueryOutput' {} a -> s {rasterDataCollectionName = a} :: RasterDataCollectionQueryOutput)

-- | The TimeRange filter used in the search.
rasterDataCollectionQueryOutput_timeRangeFilter :: Lens.Lens' RasterDataCollectionQueryOutput TimeRangeFilterOutput
rasterDataCollectionQueryOutput_timeRangeFilter = Lens.lens (\RasterDataCollectionQueryOutput' {timeRangeFilter} -> timeRangeFilter) (\s@RasterDataCollectionQueryOutput' {} a -> s {timeRangeFilter = a} :: RasterDataCollectionQueryOutput) Prelude.. Data._Sensitive

instance
  Data.FromJSON
    RasterDataCollectionQueryOutput
  where
  parseJSON =
    Data.withObject
      "RasterDataCollectionQueryOutput"
      ( \x ->
          RasterDataCollectionQueryOutput'
            Prelude.<$> (x Data..:? "AreaOfInterest")
            Prelude.<*> (x Data..:? "PropertyFilters")
            Prelude.<*> (x Data..: "RasterDataCollectionArn")
            Prelude.<*> (x Data..: "RasterDataCollectionName")
            Prelude.<*> (x Data..: "TimeRangeFilter")
      )

instance
  Prelude.Hashable
    RasterDataCollectionQueryOutput
  where
  hashWithSalt
    _salt
    RasterDataCollectionQueryOutput' {..} =
      _salt
        `Prelude.hashWithSalt` areaOfInterest
        `Prelude.hashWithSalt` propertyFilters
        `Prelude.hashWithSalt` rasterDataCollectionArn
        `Prelude.hashWithSalt` rasterDataCollectionName
        `Prelude.hashWithSalt` timeRangeFilter

instance
  Prelude.NFData
    RasterDataCollectionQueryOutput
  where
  rnf RasterDataCollectionQueryOutput' {..} =
    Prelude.rnf areaOfInterest
      `Prelude.seq` Prelude.rnf propertyFilters
      `Prelude.seq` Prelude.rnf rasterDataCollectionArn
      `Prelude.seq` Prelude.rnf rasterDataCollectionName
      `Prelude.seq` Prelude.rnf timeRangeFilter
