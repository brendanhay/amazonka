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
-- Module      : Amazonka.Forecast.Types.FeaturizationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.FeaturizationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.Featurization
import qualified Amazonka.Prelude as Prelude

-- | This object belongs to the CreatePredictor operation. If you created
-- your predictor with CreateAutoPredictor, see AttributeConfig.
--
-- In a CreatePredictor operation, the specified algorithm trains a model
-- using the specified dataset group. You can optionally tell the operation
-- to modify data fields prior to training a model. These modifications are
-- referred to as /featurization/.
--
-- You define featurization using the @FeaturizationConfig@ object. You
-- specify an array of transformations, one for each field that you want to
-- featurize. You then include the @FeaturizationConfig@ object in your
-- @CreatePredictor@ request. Amazon Forecast applies the featurization to
-- the @TARGET_TIME_SERIES@ and @RELATED_TIME_SERIES@ datasets before model
-- training.
--
-- You can create multiple featurization configurations. For example, you
-- might call the @CreatePredictor@ operation twice by specifying different
-- featurization configurations.
--
-- /See:/ 'newFeaturizationConfig' smart constructor.
data FeaturizationConfig = FeaturizationConfig'
  { -- | An array of featurization (transformation) information for the fields of
    -- a dataset.
    featurizations :: Prelude.Maybe (Prelude.NonEmpty Featurization),
    -- | An array of dimension (field) names that specify how to group the
    -- generated forecast.
    --
    -- For example, suppose that you are generating a forecast for item sales
    -- across all of your stores, and your dataset contains a @store_id@ field.
    -- If you want the sales forecast for each item by store, you would specify
    -- @store_id@ as the dimension.
    --
    -- All forecast dimensions specified in the @TARGET_TIME_SERIES@ dataset
    -- don\'t need to be specified in the @CreatePredictor@ request. All
    -- forecast dimensions specified in the @RELATED_TIME_SERIES@ dataset must
    -- be specified in the @CreatePredictor@ request.
    forecastDimensions :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The frequency of predictions in a forecast.
    --
    -- Valid intervals are Y (Year), M (Month), W (Week), D (Day), H (Hour),
    -- 30min (30 minutes), 15min (15 minutes), 10min (10 minutes), 5min (5
    -- minutes), and 1min (1 minute). For example, \"Y\" indicates every year
    -- and \"5min\" indicates every five minutes.
    --
    -- The frequency must be greater than or equal to the TARGET_TIME_SERIES
    -- dataset frequency.
    --
    -- When a RELATED_TIME_SERIES dataset is provided, the frequency must be
    -- equal to the RELATED_TIME_SERIES dataset frequency.
    forecastFrequency :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FeaturizationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featurizations', 'featurizationConfig_featurizations' - An array of featurization (transformation) information for the fields of
-- a dataset.
--
-- 'forecastDimensions', 'featurizationConfig_forecastDimensions' - An array of dimension (field) names that specify how to group the
-- generated forecast.
--
-- For example, suppose that you are generating a forecast for item sales
-- across all of your stores, and your dataset contains a @store_id@ field.
-- If you want the sales forecast for each item by store, you would specify
-- @store_id@ as the dimension.
--
-- All forecast dimensions specified in the @TARGET_TIME_SERIES@ dataset
-- don\'t need to be specified in the @CreatePredictor@ request. All
-- forecast dimensions specified in the @RELATED_TIME_SERIES@ dataset must
-- be specified in the @CreatePredictor@ request.
--
-- 'forecastFrequency', 'featurizationConfig_forecastFrequency' - The frequency of predictions in a forecast.
--
-- Valid intervals are Y (Year), M (Month), W (Week), D (Day), H (Hour),
-- 30min (30 minutes), 15min (15 minutes), 10min (10 minutes), 5min (5
-- minutes), and 1min (1 minute). For example, \"Y\" indicates every year
-- and \"5min\" indicates every five minutes.
--
-- The frequency must be greater than or equal to the TARGET_TIME_SERIES
-- dataset frequency.
--
-- When a RELATED_TIME_SERIES dataset is provided, the frequency must be
-- equal to the RELATED_TIME_SERIES dataset frequency.
newFeaturizationConfig ::
  -- | 'forecastFrequency'
  Prelude.Text ->
  FeaturizationConfig
newFeaturizationConfig pForecastFrequency_ =
  FeaturizationConfig'
    { featurizations =
        Prelude.Nothing,
      forecastDimensions = Prelude.Nothing,
      forecastFrequency = pForecastFrequency_
    }

-- | An array of featurization (transformation) information for the fields of
-- a dataset.
featurizationConfig_featurizations :: Lens.Lens' FeaturizationConfig (Prelude.Maybe (Prelude.NonEmpty Featurization))
featurizationConfig_featurizations = Lens.lens (\FeaturizationConfig' {featurizations} -> featurizations) (\s@FeaturizationConfig' {} a -> s {featurizations = a} :: FeaturizationConfig) Prelude.. Lens.mapping Lens.coerced

-- | An array of dimension (field) names that specify how to group the
-- generated forecast.
--
-- For example, suppose that you are generating a forecast for item sales
-- across all of your stores, and your dataset contains a @store_id@ field.
-- If you want the sales forecast for each item by store, you would specify
-- @store_id@ as the dimension.
--
-- All forecast dimensions specified in the @TARGET_TIME_SERIES@ dataset
-- don\'t need to be specified in the @CreatePredictor@ request. All
-- forecast dimensions specified in the @RELATED_TIME_SERIES@ dataset must
-- be specified in the @CreatePredictor@ request.
featurizationConfig_forecastDimensions :: Lens.Lens' FeaturizationConfig (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
featurizationConfig_forecastDimensions = Lens.lens (\FeaturizationConfig' {forecastDimensions} -> forecastDimensions) (\s@FeaturizationConfig' {} a -> s {forecastDimensions = a} :: FeaturizationConfig) Prelude.. Lens.mapping Lens.coerced

-- | The frequency of predictions in a forecast.
--
-- Valid intervals are Y (Year), M (Month), W (Week), D (Day), H (Hour),
-- 30min (30 minutes), 15min (15 minutes), 10min (10 minutes), 5min (5
-- minutes), and 1min (1 minute). For example, \"Y\" indicates every year
-- and \"5min\" indicates every five minutes.
--
-- The frequency must be greater than or equal to the TARGET_TIME_SERIES
-- dataset frequency.
--
-- When a RELATED_TIME_SERIES dataset is provided, the frequency must be
-- equal to the RELATED_TIME_SERIES dataset frequency.
featurizationConfig_forecastFrequency :: Lens.Lens' FeaturizationConfig Prelude.Text
featurizationConfig_forecastFrequency = Lens.lens (\FeaturizationConfig' {forecastFrequency} -> forecastFrequency) (\s@FeaturizationConfig' {} a -> s {forecastFrequency = a} :: FeaturizationConfig)

instance Data.FromJSON FeaturizationConfig where
  parseJSON =
    Data.withObject
      "FeaturizationConfig"
      ( \x ->
          FeaturizationConfig'
            Prelude.<$> (x Data..:? "Featurizations")
            Prelude.<*> (x Data..:? "ForecastDimensions")
            Prelude.<*> (x Data..: "ForecastFrequency")
      )

instance Prelude.Hashable FeaturizationConfig where
  hashWithSalt _salt FeaturizationConfig' {..} =
    _salt
      `Prelude.hashWithSalt` featurizations
      `Prelude.hashWithSalt` forecastDimensions
      `Prelude.hashWithSalt` forecastFrequency

instance Prelude.NFData FeaturizationConfig where
  rnf FeaturizationConfig' {..} =
    Prelude.rnf featurizations
      `Prelude.seq` Prelude.rnf forecastDimensions
      `Prelude.seq` Prelude.rnf forecastFrequency

instance Data.ToJSON FeaturizationConfig where
  toJSON FeaturizationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Featurizations" Data..=)
              Prelude.<$> featurizations,
            ("ForecastDimensions" Data..=)
              Prelude.<$> forecastDimensions,
            Prelude.Just
              ("ForecastFrequency" Data..= forecastFrequency)
          ]
      )
