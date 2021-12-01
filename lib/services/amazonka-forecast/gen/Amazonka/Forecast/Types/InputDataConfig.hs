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
-- Module      : Amazonka.Forecast.Types.InputDataConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.InputDataConfig where

import qualified Amazonka.Core as Core
import Amazonka.Forecast.Types.SupplementaryFeature
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The data used to train a predictor. The data includes a dataset group
-- and any supplementary features. You specify this object in the
-- CreatePredictor request.
--
-- /See:/ 'newInputDataConfig' smart constructor.
data InputDataConfig = InputDataConfig'
  { -- | An array of supplementary features. The only supported feature is a
    -- holiday calendar.
    supplementaryFeatures :: Prelude.Maybe (Prelude.NonEmpty SupplementaryFeature),
    -- | The Amazon Resource Name (ARN) of the dataset group.
    datasetGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'supplementaryFeatures', 'inputDataConfig_supplementaryFeatures' - An array of supplementary features. The only supported feature is a
-- holiday calendar.
--
-- 'datasetGroupArn', 'inputDataConfig_datasetGroupArn' - The Amazon Resource Name (ARN) of the dataset group.
newInputDataConfig ::
  -- | 'datasetGroupArn'
  Prelude.Text ->
  InputDataConfig
newInputDataConfig pDatasetGroupArn_ =
  InputDataConfig'
    { supplementaryFeatures =
        Prelude.Nothing,
      datasetGroupArn = pDatasetGroupArn_
    }

-- | An array of supplementary features. The only supported feature is a
-- holiday calendar.
inputDataConfig_supplementaryFeatures :: Lens.Lens' InputDataConfig (Prelude.Maybe (Prelude.NonEmpty SupplementaryFeature))
inputDataConfig_supplementaryFeatures = Lens.lens (\InputDataConfig' {supplementaryFeatures} -> supplementaryFeatures) (\s@InputDataConfig' {} a -> s {supplementaryFeatures = a} :: InputDataConfig) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the dataset group.
inputDataConfig_datasetGroupArn :: Lens.Lens' InputDataConfig Prelude.Text
inputDataConfig_datasetGroupArn = Lens.lens (\InputDataConfig' {datasetGroupArn} -> datasetGroupArn) (\s@InputDataConfig' {} a -> s {datasetGroupArn = a} :: InputDataConfig)

instance Core.FromJSON InputDataConfig where
  parseJSON =
    Core.withObject
      "InputDataConfig"
      ( \x ->
          InputDataConfig'
            Prelude.<$> (x Core..:? "SupplementaryFeatures")
            Prelude.<*> (x Core..: "DatasetGroupArn")
      )

instance Prelude.Hashable InputDataConfig where
  hashWithSalt salt' InputDataConfig' {..} =
    salt' `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` supplementaryFeatures

instance Prelude.NFData InputDataConfig where
  rnf InputDataConfig' {..} =
    Prelude.rnf supplementaryFeatures
      `Prelude.seq` Prelude.rnf datasetGroupArn

instance Core.ToJSON InputDataConfig where
  toJSON InputDataConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SupplementaryFeatures" Core..=)
              Prelude.<$> supplementaryFeatures,
            Prelude.Just
              ("DatasetGroupArn" Core..= datasetGroupArn)
          ]
      )
