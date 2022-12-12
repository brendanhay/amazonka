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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.RasterDataCollectionQueryInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.RasterDataCollectionQueryInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.AreaOfInterest
import Amazonka.SageMakerGeoSpatial.Types.PropertyFilters
import Amazonka.SageMakerGeoSpatial.Types.TimeRangeFilterInput

-- |
--
-- /See:/ 'newRasterDataCollectionQueryInput' smart constructor.
data RasterDataCollectionQueryInput = RasterDataCollectionQueryInput'
  { -- | The area of interest being queried for the raster data collection.
    areaOfInterest :: Prelude.Maybe AreaOfInterest,
    propertyFilters :: Prelude.Maybe PropertyFilters,
    -- | The Amazon Resource Name (ARN) of the raster data collection.
    rasterDataCollectionArn :: Prelude.Text,
    timeRangeFilter :: Data.Sensitive TimeRangeFilterInput
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RasterDataCollectionQueryInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'areaOfInterest', 'rasterDataCollectionQueryInput_areaOfInterest' - The area of interest being queried for the raster data collection.
--
-- 'propertyFilters', 'rasterDataCollectionQueryInput_propertyFilters' -
--
-- 'rasterDataCollectionArn', 'rasterDataCollectionQueryInput_rasterDataCollectionArn' - The Amazon Resource Name (ARN) of the raster data collection.
--
-- 'timeRangeFilter', 'rasterDataCollectionQueryInput_timeRangeFilter' -
newRasterDataCollectionQueryInput ::
  -- | 'rasterDataCollectionArn'
  Prelude.Text ->
  -- | 'timeRangeFilter'
  TimeRangeFilterInput ->
  RasterDataCollectionQueryInput
newRasterDataCollectionQueryInput
  pRasterDataCollectionArn_
  pTimeRangeFilter_ =
    RasterDataCollectionQueryInput'
      { areaOfInterest =
          Prelude.Nothing,
        propertyFilters = Prelude.Nothing,
        rasterDataCollectionArn =
          pRasterDataCollectionArn_,
        timeRangeFilter =
          Data._Sensitive Lens.# pTimeRangeFilter_
      }

-- | The area of interest being queried for the raster data collection.
rasterDataCollectionQueryInput_areaOfInterest :: Lens.Lens' RasterDataCollectionQueryInput (Prelude.Maybe AreaOfInterest)
rasterDataCollectionQueryInput_areaOfInterest = Lens.lens (\RasterDataCollectionQueryInput' {areaOfInterest} -> areaOfInterest) (\s@RasterDataCollectionQueryInput' {} a -> s {areaOfInterest = a} :: RasterDataCollectionQueryInput)

-- |
rasterDataCollectionQueryInput_propertyFilters :: Lens.Lens' RasterDataCollectionQueryInput (Prelude.Maybe PropertyFilters)
rasterDataCollectionQueryInput_propertyFilters = Lens.lens (\RasterDataCollectionQueryInput' {propertyFilters} -> propertyFilters) (\s@RasterDataCollectionQueryInput' {} a -> s {propertyFilters = a} :: RasterDataCollectionQueryInput)

-- | The Amazon Resource Name (ARN) of the raster data collection.
rasterDataCollectionQueryInput_rasterDataCollectionArn :: Lens.Lens' RasterDataCollectionQueryInput Prelude.Text
rasterDataCollectionQueryInput_rasterDataCollectionArn = Lens.lens (\RasterDataCollectionQueryInput' {rasterDataCollectionArn} -> rasterDataCollectionArn) (\s@RasterDataCollectionQueryInput' {} a -> s {rasterDataCollectionArn = a} :: RasterDataCollectionQueryInput)

-- |
rasterDataCollectionQueryInput_timeRangeFilter :: Lens.Lens' RasterDataCollectionQueryInput TimeRangeFilterInput
rasterDataCollectionQueryInput_timeRangeFilter = Lens.lens (\RasterDataCollectionQueryInput' {timeRangeFilter} -> timeRangeFilter) (\s@RasterDataCollectionQueryInput' {} a -> s {timeRangeFilter = a} :: RasterDataCollectionQueryInput) Prelude.. Data._Sensitive

instance
  Prelude.Hashable
    RasterDataCollectionQueryInput
  where
  hashWithSalt
    _salt
    RasterDataCollectionQueryInput' {..} =
      _salt `Prelude.hashWithSalt` areaOfInterest
        `Prelude.hashWithSalt` propertyFilters
        `Prelude.hashWithSalt` rasterDataCollectionArn
        `Prelude.hashWithSalt` timeRangeFilter

instance
  Prelude.NFData
    RasterDataCollectionQueryInput
  where
  rnf RasterDataCollectionQueryInput' {..} =
    Prelude.rnf areaOfInterest
      `Prelude.seq` Prelude.rnf propertyFilters
      `Prelude.seq` Prelude.rnf rasterDataCollectionArn
      `Prelude.seq` Prelude.rnf timeRangeFilter

instance Data.ToJSON RasterDataCollectionQueryInput where
  toJSON RasterDataCollectionQueryInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AreaOfInterest" Data..=)
              Prelude.<$> areaOfInterest,
            ("PropertyFilters" Data..=)
              Prelude.<$> propertyFilters,
            Prelude.Just
              ( "RasterDataCollectionArn"
                  Data..= rasterDataCollectionArn
              ),
            Prelude.Just
              ("TimeRangeFilter" Data..= timeRangeFilter)
          ]
      )
