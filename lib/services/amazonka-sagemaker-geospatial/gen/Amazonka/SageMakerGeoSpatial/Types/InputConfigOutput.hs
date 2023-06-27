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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.InputConfigOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.InputConfigOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.RasterDataCollectionQueryOutput

-- | The InputConfig for an EarthObservationJob response.
--
-- /See:/ 'newInputConfigOutput' smart constructor.
data InputConfigOutput = InputConfigOutput'
  { -- | The Amazon Resource Name (ARN) of the previous Earth Observation job.
    previousEarthObservationJobArn :: Prelude.Maybe Prelude.Text,
    -- | The structure representing the RasterDataCollection Query consisting of
    -- the Area of Interest, RasterDataCollectionArn, RasterDataCollectionName,
    -- TimeRange, and Property Filters.
    rasterDataCollectionQuery :: Prelude.Maybe RasterDataCollectionQueryOutput
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputConfigOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'previousEarthObservationJobArn', 'inputConfigOutput_previousEarthObservationJobArn' - The Amazon Resource Name (ARN) of the previous Earth Observation job.
--
-- 'rasterDataCollectionQuery', 'inputConfigOutput_rasterDataCollectionQuery' - The structure representing the RasterDataCollection Query consisting of
-- the Area of Interest, RasterDataCollectionArn, RasterDataCollectionName,
-- TimeRange, and Property Filters.
newInputConfigOutput ::
  InputConfigOutput
newInputConfigOutput =
  InputConfigOutput'
    { previousEarthObservationJobArn =
        Prelude.Nothing,
      rasterDataCollectionQuery = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the previous Earth Observation job.
inputConfigOutput_previousEarthObservationJobArn :: Lens.Lens' InputConfigOutput (Prelude.Maybe Prelude.Text)
inputConfigOutput_previousEarthObservationJobArn = Lens.lens (\InputConfigOutput' {previousEarthObservationJobArn} -> previousEarthObservationJobArn) (\s@InputConfigOutput' {} a -> s {previousEarthObservationJobArn = a} :: InputConfigOutput)

-- | The structure representing the RasterDataCollection Query consisting of
-- the Area of Interest, RasterDataCollectionArn, RasterDataCollectionName,
-- TimeRange, and Property Filters.
inputConfigOutput_rasterDataCollectionQuery :: Lens.Lens' InputConfigOutput (Prelude.Maybe RasterDataCollectionQueryOutput)
inputConfigOutput_rasterDataCollectionQuery = Lens.lens (\InputConfigOutput' {rasterDataCollectionQuery} -> rasterDataCollectionQuery) (\s@InputConfigOutput' {} a -> s {rasterDataCollectionQuery = a} :: InputConfigOutput)

instance Data.FromJSON InputConfigOutput where
  parseJSON =
    Data.withObject
      "InputConfigOutput"
      ( \x ->
          InputConfigOutput'
            Prelude.<$> (x Data..:? "PreviousEarthObservationJobArn")
            Prelude.<*> (x Data..:? "RasterDataCollectionQuery")
      )

instance Prelude.Hashable InputConfigOutput where
  hashWithSalt _salt InputConfigOutput' {..} =
    _salt
      `Prelude.hashWithSalt` previousEarthObservationJobArn
      `Prelude.hashWithSalt` rasterDataCollectionQuery

instance Prelude.NFData InputConfigOutput where
  rnf InputConfigOutput' {..} =
    Prelude.rnf previousEarthObservationJobArn
      `Prelude.seq` Prelude.rnf rasterDataCollectionQuery
