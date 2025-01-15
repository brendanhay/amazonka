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
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.InputConfigOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.EojDataSourceConfigInput
import Amazonka.SageMakerGeoSpatial.Types.RasterDataCollectionQueryOutput

-- | The InputConfig for an EarthObservationJob response.
--
-- /See:/ 'newInputConfigOutput' smart constructor.
data InputConfigOutput = InputConfigOutput'
  { -- | The location of the input data.
    dataSourceConfig :: Prelude.Maybe EojDataSourceConfigInput,
    -- | The Amazon Resource Name (ARN) of the previous Earth Observation job.
    previousEarthObservationJobArn :: Prelude.Maybe Prelude.Text,
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
-- 'dataSourceConfig', 'inputConfigOutput_dataSourceConfig' - The location of the input data.
--
-- 'previousEarthObservationJobArn', 'inputConfigOutput_previousEarthObservationJobArn' - The Amazon Resource Name (ARN) of the previous Earth Observation job.
--
-- 'rasterDataCollectionQuery', 'inputConfigOutput_rasterDataCollectionQuery' -
newInputConfigOutput ::
  InputConfigOutput
newInputConfigOutput =
  InputConfigOutput'
    { dataSourceConfig =
        Prelude.Nothing,
      previousEarthObservationJobArn = Prelude.Nothing,
      rasterDataCollectionQuery = Prelude.Nothing
    }

-- | The location of the input data.
inputConfigOutput_dataSourceConfig :: Lens.Lens' InputConfigOutput (Prelude.Maybe EojDataSourceConfigInput)
inputConfigOutput_dataSourceConfig = Lens.lens (\InputConfigOutput' {dataSourceConfig} -> dataSourceConfig) (\s@InputConfigOutput' {} a -> s {dataSourceConfig = a} :: InputConfigOutput)

-- | The Amazon Resource Name (ARN) of the previous Earth Observation job.
inputConfigOutput_previousEarthObservationJobArn :: Lens.Lens' InputConfigOutput (Prelude.Maybe Prelude.Text)
inputConfigOutput_previousEarthObservationJobArn = Lens.lens (\InputConfigOutput' {previousEarthObservationJobArn} -> previousEarthObservationJobArn) (\s@InputConfigOutput' {} a -> s {previousEarthObservationJobArn = a} :: InputConfigOutput)

inputConfigOutput_rasterDataCollectionQuery :: Lens.Lens' InputConfigOutput (Prelude.Maybe RasterDataCollectionQueryOutput)
inputConfigOutput_rasterDataCollectionQuery = Lens.lens (\InputConfigOutput' {rasterDataCollectionQuery} -> rasterDataCollectionQuery) (\s@InputConfigOutput' {} a -> s {rasterDataCollectionQuery = a} :: InputConfigOutput)

instance Data.FromJSON InputConfigOutput where
  parseJSON =
    Data.withObject
      "InputConfigOutput"
      ( \x ->
          InputConfigOutput'
            Prelude.<$> (x Data..:? "DataSourceConfig")
            Prelude.<*> (x Data..:? "PreviousEarthObservationJobArn")
            Prelude.<*> (x Data..:? "RasterDataCollectionQuery")
      )

instance Prelude.Hashable InputConfigOutput where
  hashWithSalt _salt InputConfigOutput' {..} =
    _salt
      `Prelude.hashWithSalt` dataSourceConfig
      `Prelude.hashWithSalt` previousEarthObservationJobArn
      `Prelude.hashWithSalt` rasterDataCollectionQuery

instance Prelude.NFData InputConfigOutput where
  rnf InputConfigOutput' {..} =
    Prelude.rnf dataSourceConfig `Prelude.seq`
      Prelude.rnf previousEarthObservationJobArn `Prelude.seq`
        Prelude.rnf rasterDataCollectionQuery
