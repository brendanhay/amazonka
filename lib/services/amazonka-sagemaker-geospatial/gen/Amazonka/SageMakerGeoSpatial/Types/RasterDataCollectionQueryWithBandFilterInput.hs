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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.RasterDataCollectionQueryWithBandFilterInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.RasterDataCollectionQueryWithBandFilterInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.AreaOfInterest
import Amazonka.SageMakerGeoSpatial.Types.PropertyFilters
import Amazonka.SageMakerGeoSpatial.Types.TimeRangeFilterInput

-- | This is a RasterDataCollectionQueryInput containing AreaOfInterest, Time
-- Range filter and Property filters.
--
-- /See:/ 'newRasterDataCollectionQueryWithBandFilterInput' smart constructor.
data RasterDataCollectionQueryWithBandFilterInput = RasterDataCollectionQueryWithBandFilterInput'
  { areaOfInterest :: Prelude.Maybe AreaOfInterest,
    bandFilter :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    propertyFilters :: Prelude.Maybe PropertyFilters,
    timeRangeFilter :: Data.Sensitive TimeRangeFilterInput
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RasterDataCollectionQueryWithBandFilterInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'areaOfInterest', 'rasterDataCollectionQueryWithBandFilterInput_areaOfInterest' -
--
-- 'bandFilter', 'rasterDataCollectionQueryWithBandFilterInput_bandFilter' -
--
-- 'propertyFilters', 'rasterDataCollectionQueryWithBandFilterInput_propertyFilters' -
--
-- 'timeRangeFilter', 'rasterDataCollectionQueryWithBandFilterInput_timeRangeFilter' -
newRasterDataCollectionQueryWithBandFilterInput ::
  -- | 'timeRangeFilter'
  TimeRangeFilterInput ->
  RasterDataCollectionQueryWithBandFilterInput
newRasterDataCollectionQueryWithBandFilterInput
  pTimeRangeFilter_ =
    RasterDataCollectionQueryWithBandFilterInput'
      { areaOfInterest =
          Prelude.Nothing,
        bandFilter = Prelude.Nothing,
        propertyFilters =
          Prelude.Nothing,
        timeRangeFilter =
          Data._Sensitive
            Lens.# pTimeRangeFilter_
      }

rasterDataCollectionQueryWithBandFilterInput_areaOfInterest :: Lens.Lens' RasterDataCollectionQueryWithBandFilterInput (Prelude.Maybe AreaOfInterest)
rasterDataCollectionQueryWithBandFilterInput_areaOfInterest = Lens.lens (\RasterDataCollectionQueryWithBandFilterInput' {areaOfInterest} -> areaOfInterest) (\s@RasterDataCollectionQueryWithBandFilterInput' {} a -> s {areaOfInterest = a} :: RasterDataCollectionQueryWithBandFilterInput)

rasterDataCollectionQueryWithBandFilterInput_bandFilter :: Lens.Lens' RasterDataCollectionQueryWithBandFilterInput (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
rasterDataCollectionQueryWithBandFilterInput_bandFilter = Lens.lens (\RasterDataCollectionQueryWithBandFilterInput' {bandFilter} -> bandFilter) (\s@RasterDataCollectionQueryWithBandFilterInput' {} a -> s {bandFilter = a} :: RasterDataCollectionQueryWithBandFilterInput) Prelude.. Lens.mapping Lens.coerced

rasterDataCollectionQueryWithBandFilterInput_propertyFilters :: Lens.Lens' RasterDataCollectionQueryWithBandFilterInput (Prelude.Maybe PropertyFilters)
rasterDataCollectionQueryWithBandFilterInput_propertyFilters = Lens.lens (\RasterDataCollectionQueryWithBandFilterInput' {propertyFilters} -> propertyFilters) (\s@RasterDataCollectionQueryWithBandFilterInput' {} a -> s {propertyFilters = a} :: RasterDataCollectionQueryWithBandFilterInput)

rasterDataCollectionQueryWithBandFilterInput_timeRangeFilter :: Lens.Lens' RasterDataCollectionQueryWithBandFilterInput TimeRangeFilterInput
rasterDataCollectionQueryWithBandFilterInput_timeRangeFilter = Lens.lens (\RasterDataCollectionQueryWithBandFilterInput' {timeRangeFilter} -> timeRangeFilter) (\s@RasterDataCollectionQueryWithBandFilterInput' {} a -> s {timeRangeFilter = a} :: RasterDataCollectionQueryWithBandFilterInput) Prelude.. Data._Sensitive

instance
  Prelude.Hashable
    RasterDataCollectionQueryWithBandFilterInput
  where
  hashWithSalt
    _salt
    RasterDataCollectionQueryWithBandFilterInput' {..} =
      _salt
        `Prelude.hashWithSalt` areaOfInterest
        `Prelude.hashWithSalt` bandFilter
        `Prelude.hashWithSalt` propertyFilters
        `Prelude.hashWithSalt` timeRangeFilter

instance
  Prelude.NFData
    RasterDataCollectionQueryWithBandFilterInput
  where
  rnf RasterDataCollectionQueryWithBandFilterInput' {..} =
    Prelude.rnf areaOfInterest `Prelude.seq`
      Prelude.rnf bandFilter `Prelude.seq`
        Prelude.rnf propertyFilters `Prelude.seq`
          Prelude.rnf timeRangeFilter

instance
  Data.ToJSON
    RasterDataCollectionQueryWithBandFilterInput
  where
  toJSON
    RasterDataCollectionQueryWithBandFilterInput' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("AreaOfInterest" Data..=)
                Prelude.<$> areaOfInterest,
              ("BandFilter" Data..=) Prelude.<$> bandFilter,
              ("PropertyFilters" Data..=)
                Prelude.<$> propertyFilters,
              Prelude.Just
                ("TimeRangeFilter" Data..= timeRangeFilter)
            ]
        )
