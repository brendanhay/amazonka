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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.InputConfigInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.InputConfigInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.RasterDataCollectionQueryInput

-- | Input configuration information.
--
-- /See:/ 'newInputConfigInput' smart constructor.
data InputConfigInput = InputConfigInput'
  { -- | The Amazon Resource Name (ARN) of the previous Earth Observation job.
    previousEarthObservationJobArn :: Prelude.Maybe Prelude.Text,
    -- | The structure representing the RasterDataCollection Query consisting of
    -- the Area of Interest, RasterDataCollectionArn,TimeRange and Property
    -- Filters.
    rasterDataCollectionQuery :: Prelude.Maybe RasterDataCollectionQueryInput
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputConfigInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'previousEarthObservationJobArn', 'inputConfigInput_previousEarthObservationJobArn' - The Amazon Resource Name (ARN) of the previous Earth Observation job.
--
-- 'rasterDataCollectionQuery', 'inputConfigInput_rasterDataCollectionQuery' - The structure representing the RasterDataCollection Query consisting of
-- the Area of Interest, RasterDataCollectionArn,TimeRange and Property
-- Filters.
newInputConfigInput ::
  InputConfigInput
newInputConfigInput =
  InputConfigInput'
    { previousEarthObservationJobArn =
        Prelude.Nothing,
      rasterDataCollectionQuery = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the previous Earth Observation job.
inputConfigInput_previousEarthObservationJobArn :: Lens.Lens' InputConfigInput (Prelude.Maybe Prelude.Text)
inputConfigInput_previousEarthObservationJobArn = Lens.lens (\InputConfigInput' {previousEarthObservationJobArn} -> previousEarthObservationJobArn) (\s@InputConfigInput' {} a -> s {previousEarthObservationJobArn = a} :: InputConfigInput)

-- | The structure representing the RasterDataCollection Query consisting of
-- the Area of Interest, RasterDataCollectionArn,TimeRange and Property
-- Filters.
inputConfigInput_rasterDataCollectionQuery :: Lens.Lens' InputConfigInput (Prelude.Maybe RasterDataCollectionQueryInput)
inputConfigInput_rasterDataCollectionQuery = Lens.lens (\InputConfigInput' {rasterDataCollectionQuery} -> rasterDataCollectionQuery) (\s@InputConfigInput' {} a -> s {rasterDataCollectionQuery = a} :: InputConfigInput)

instance Prelude.Hashable InputConfigInput where
  hashWithSalt _salt InputConfigInput' {..} =
    _salt
      `Prelude.hashWithSalt` previousEarthObservationJobArn
      `Prelude.hashWithSalt` rasterDataCollectionQuery

instance Prelude.NFData InputConfigInput where
  rnf InputConfigInput' {..} =
    Prelude.rnf previousEarthObservationJobArn
      `Prelude.seq` Prelude.rnf rasterDataCollectionQuery

instance Data.ToJSON InputConfigInput where
  toJSON InputConfigInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PreviousEarthObservationJobArn" Data..=)
              Prelude.<$> previousEarthObservationJobArn,
            ("RasterDataCollectionQuery" Data..=)
              Prelude.<$> rasterDataCollectionQuery
          ]
      )
