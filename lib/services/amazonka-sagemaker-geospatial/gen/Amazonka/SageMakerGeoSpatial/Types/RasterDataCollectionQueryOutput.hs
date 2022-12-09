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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
import Amazonka.SageMakerGeoSpatial.Types.TimeRangeFilterInput

-- |
--
-- /See:/ 'newRasterDataCollectionQueryOutput' smart constructor.
data RasterDataCollectionQueryOutput = RasterDataCollectionQueryOutput'
  { areaOfInterest :: Prelude.Maybe AreaOfInterest,
    propertyFilters :: Prelude.Maybe PropertyFilters,
    rasterDataCollectionArn :: Prelude.Text,
    -- | The name of the raster data collection.
    rasterDataCollectionName :: Prelude.Text,
    timeRangeFilter :: Data.Sensitive TimeRangeFilterInput
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
-- 'areaOfInterest', 'rasterDataCollectionQueryOutput_areaOfInterest' -
--
-- 'propertyFilters', 'rasterDataCollectionQueryOutput_propertyFilters' -
--
-- 'rasterDataCollectionArn', 'rasterDataCollectionQueryOutput_rasterDataCollectionArn' -
--
-- 'rasterDataCollectionName', 'rasterDataCollectionQueryOutput_rasterDataCollectionName' - The name of the raster data collection.
--
-- 'timeRangeFilter', 'rasterDataCollectionQueryOutput_timeRangeFilter' -
newRasterDataCollectionQueryOutput ::
  -- | 'rasterDataCollectionArn'
  Prelude.Text ->
  -- | 'rasterDataCollectionName'
  Prelude.Text ->
  -- | 'timeRangeFilter'
  TimeRangeFilterInput ->
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

-- |
rasterDataCollectionQueryOutput_areaOfInterest :: Lens.Lens' RasterDataCollectionQueryOutput (Prelude.Maybe AreaOfInterest)
rasterDataCollectionQueryOutput_areaOfInterest = Lens.lens (\RasterDataCollectionQueryOutput' {areaOfInterest} -> areaOfInterest) (\s@RasterDataCollectionQueryOutput' {} a -> s {areaOfInterest = a} :: RasterDataCollectionQueryOutput)

-- |
rasterDataCollectionQueryOutput_propertyFilters :: Lens.Lens' RasterDataCollectionQueryOutput (Prelude.Maybe PropertyFilters)
rasterDataCollectionQueryOutput_propertyFilters = Lens.lens (\RasterDataCollectionQueryOutput' {propertyFilters} -> propertyFilters) (\s@RasterDataCollectionQueryOutput' {} a -> s {propertyFilters = a} :: RasterDataCollectionQueryOutput)

-- |
rasterDataCollectionQueryOutput_rasterDataCollectionArn :: Lens.Lens' RasterDataCollectionQueryOutput Prelude.Text
rasterDataCollectionQueryOutput_rasterDataCollectionArn = Lens.lens (\RasterDataCollectionQueryOutput' {rasterDataCollectionArn} -> rasterDataCollectionArn) (\s@RasterDataCollectionQueryOutput' {} a -> s {rasterDataCollectionArn = a} :: RasterDataCollectionQueryOutput)

-- | The name of the raster data collection.
rasterDataCollectionQueryOutput_rasterDataCollectionName :: Lens.Lens' RasterDataCollectionQueryOutput Prelude.Text
rasterDataCollectionQueryOutput_rasterDataCollectionName = Lens.lens (\RasterDataCollectionQueryOutput' {rasterDataCollectionName} -> rasterDataCollectionName) (\s@RasterDataCollectionQueryOutput' {} a -> s {rasterDataCollectionName = a} :: RasterDataCollectionQueryOutput)

-- |
rasterDataCollectionQueryOutput_timeRangeFilter :: Lens.Lens' RasterDataCollectionQueryOutput TimeRangeFilterInput
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
      _salt `Prelude.hashWithSalt` areaOfInterest
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
