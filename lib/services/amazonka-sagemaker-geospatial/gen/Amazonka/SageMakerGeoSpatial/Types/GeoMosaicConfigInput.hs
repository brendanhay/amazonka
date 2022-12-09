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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.GeoMosaicConfigInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.GeoMosaicConfigInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.AlgorithmNameGeoMosaic

-- | Input configuration information for the geomosaic.
--
-- /See:/ 'newGeoMosaicConfigInput' smart constructor.
data GeoMosaicConfigInput = GeoMosaicConfigInput'
  { -- | The name of the algorithm being used for geomosaic.
    algorithmName :: Prelude.Maybe AlgorithmNameGeoMosaic,
    -- | The target bands for geomosaic.
    targetBands :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeoMosaicConfigInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithmName', 'geoMosaicConfigInput_algorithmName' - The name of the algorithm being used for geomosaic.
--
-- 'targetBands', 'geoMosaicConfigInput_targetBands' - The target bands for geomosaic.
newGeoMosaicConfigInput ::
  GeoMosaicConfigInput
newGeoMosaicConfigInput =
  GeoMosaicConfigInput'
    { algorithmName =
        Prelude.Nothing,
      targetBands = Prelude.Nothing
    }

-- | The name of the algorithm being used for geomosaic.
geoMosaicConfigInput_algorithmName :: Lens.Lens' GeoMosaicConfigInput (Prelude.Maybe AlgorithmNameGeoMosaic)
geoMosaicConfigInput_algorithmName = Lens.lens (\GeoMosaicConfigInput' {algorithmName} -> algorithmName) (\s@GeoMosaicConfigInput' {} a -> s {algorithmName = a} :: GeoMosaicConfigInput)

-- | The target bands for geomosaic.
geoMosaicConfigInput_targetBands :: Lens.Lens' GeoMosaicConfigInput (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
geoMosaicConfigInput_targetBands = Lens.lens (\GeoMosaicConfigInput' {targetBands} -> targetBands) (\s@GeoMosaicConfigInput' {} a -> s {targetBands = a} :: GeoMosaicConfigInput) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON GeoMosaicConfigInput where
  parseJSON =
    Data.withObject
      "GeoMosaicConfigInput"
      ( \x ->
          GeoMosaicConfigInput'
            Prelude.<$> (x Data..:? "AlgorithmName")
            Prelude.<*> (x Data..:? "TargetBands")
      )

instance Prelude.Hashable GeoMosaicConfigInput where
  hashWithSalt _salt GeoMosaicConfigInput' {..} =
    _salt `Prelude.hashWithSalt` algorithmName
      `Prelude.hashWithSalt` targetBands

instance Prelude.NFData GeoMosaicConfigInput where
  rnf GeoMosaicConfigInput' {..} =
    Prelude.rnf algorithmName
      `Prelude.seq` Prelude.rnf targetBands

instance Data.ToJSON GeoMosaicConfigInput where
  toJSON GeoMosaicConfigInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AlgorithmName" Data..=) Prelude.<$> algorithmName,
            ("TargetBands" Data..=) Prelude.<$> targetBands
          ]
      )
