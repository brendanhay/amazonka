{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Forecast.CreateAutoPredictor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Forecast predictor.
--
-- Amazon Forecast creates predictors with AutoPredictor, which involves
-- applying the optimal combination of algorithms to each time series in
-- your datasets. You can use CreateAutoPredictor to create new predictors
-- or upgrade\/retrain existing predictors.
--
-- __Creating new predictors__
--
-- The following parameters are required when creating a new predictor:
--
-- -   @PredictorName@ - A unique name for the predictor.
--
-- -   @DatasetGroupArn@ - The ARN of the dataset group used to train the
--     predictor.
--
-- -   @ForecastFrequency@ - The granularity of your forecasts (hourly,
--     daily, weekly, etc).
--
-- -   @ForecastHorizon@ - The number of time-steps that the model
--     predicts. The forecast horizon is also called the prediction length.
--
-- When creating a new predictor, do not specify a value for
-- @ReferencePredictorArn@.
--
-- __Upgrading and retraining predictors__
--
-- The following parameters are required when retraining or upgrading a
-- predictor:
--
-- -   @PredictorName@ - A unique name for the predictor.
--
-- -   @ReferencePredictorArn@ - The ARN of the predictor to retrain or
--     upgrade.
--
-- When upgrading or retraining a predictor, only specify values for the
-- @ReferencePredictorArn@ and @PredictorName@.
module Amazonka.Forecast.CreateAutoPredictor
  ( -- * Creating a Request
    CreateAutoPredictor (..),
    newCreateAutoPredictor,

    -- * Request Lenses
    createAutoPredictor_dataConfig,
    createAutoPredictor_encryptionConfig,
    createAutoPredictor_explainPredictor,
    createAutoPredictor_forecastDimensions,
    createAutoPredictor_forecastFrequency,
    createAutoPredictor_forecastHorizon,
    createAutoPredictor_forecastTypes,
    createAutoPredictor_monitorConfig,
    createAutoPredictor_optimizationMetric,
    createAutoPredictor_referencePredictorArn,
    createAutoPredictor_tags,
    createAutoPredictor_timeAlignmentBoundary,
    createAutoPredictor_predictorName,

    -- * Destructuring the Response
    CreateAutoPredictorResponse (..),
    newCreateAutoPredictorResponse,

    -- * Response Lenses
    createAutoPredictorResponse_predictorArn,
    createAutoPredictorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAutoPredictor' smart constructor.
data CreateAutoPredictor = CreateAutoPredictor'
  { -- | The data configuration for your dataset group and any additional
    -- datasets.
    dataConfig :: Prelude.Maybe DataConfig,
    encryptionConfig :: Prelude.Maybe EncryptionConfig,
    -- | Create an Explainability resource for the predictor.
    explainPredictor :: Prelude.Maybe Prelude.Bool,
    -- | An array of dimension (field) names that specify how to group the
    -- generated forecast.
    --
    -- For example, if you are generating forecasts for item sales across all
    -- your stores, and your dataset contains a @store_id@ field, you would
    -- specify @store_id@ as a dimension to group sales forecasts for each
    -- store.
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
    forecastFrequency :: Prelude.Maybe Prelude.Text,
    -- | The number of time-steps that the model predicts. The forecast horizon
    -- is also called the prediction length.
    --
    -- The maximum forecast horizon is the lesser of 500 time-steps or 1\/4 of
    -- the TARGET_TIME_SERIES dataset length. If you are retraining an existing
    -- AutoPredictor, then the maximum forecast horizon is the lesser of 500
    -- time-steps or 1\/3 of the TARGET_TIME_SERIES dataset length.
    --
    -- If you are upgrading to an AutoPredictor or retraining an existing
    -- AutoPredictor, you cannot update the forecast horizon parameter. You can
    -- meet this requirement by providing longer time-series in the dataset.
    forecastHorizon :: Prelude.Maybe Prelude.Int,
    -- | The forecast types used to train a predictor. You can specify up to five
    -- forecast types. Forecast types can be quantiles from 0.01 to 0.99, by
    -- increments of 0.01 or higher. You can also specify the mean forecast
    -- with @mean@.
    forecastTypes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The configuration details for predictor monitoring. Provide a name for
    -- the monitor resource to enable predictor monitoring.
    --
    -- Predictor monitoring allows you to see how your predictor\'s performance
    -- changes over time. For more information, see
    -- <https://docs.aws.amazon.com/forecast/latest/dg/predictor-monitoring.html Predictor Monitoring>.
    monitorConfig :: Prelude.Maybe MonitorConfig,
    -- | The accuracy metric used to optimize the predictor.
    optimizationMetric :: Prelude.Maybe OptimizationMetric,
    -- | The ARN of the predictor to retrain or upgrade. This parameter is only
    -- used when retraining or upgrading a predictor. When creating a new
    -- predictor, do not specify a value for this parameter.
    --
    -- When upgrading or retraining a predictor, only specify values for the
    -- @ReferencePredictorArn@ and @PredictorName@. The value for
    -- @PredictorName@ must be a unique predictor name.
    referencePredictorArn :: Prelude.Maybe Prelude.Text,
    -- | Optional metadata to help you categorize and organize your predictors.
    -- Each tag consists of a key and an optional value, both of which you
    -- define. Tag keys and values are case sensitive.
    --
    -- The following restrictions apply to tags:
    --
    -- -   For each resource, each tag key must be unique and each tag key must
    --     have one value.
    --
    -- -   Maximum number of tags per resource: 50.
    --
    -- -   Maximum key length: 128 Unicode characters in UTF-8.
    --
    -- -   Maximum value length: 256 Unicode characters in UTF-8.
    --
    -- -   Accepted characters: all letters and numbers, spaces representable
    --     in UTF-8, and + - = . _ : \/ \@. If your tagging schema is used
    --     across other services and resources, the character restrictions of
    --     those services also apply.
    --
    -- -   Key prefixes cannot include any upper or lowercase combination of
    --     @aws:@ or @AWS:@. Values can have this prefix. If a tag value has
    --     @aws@ as its prefix but the key does not, Forecast considers it to
    --     be a user tag and will count against the limit of 50 tags. Tags with
    --     only the key prefix of @aws@ do not count against your tags per
    --     resource limit. You cannot edit or delete tag keys with this prefix.
    tags :: Prelude.Maybe [Tag],
    -- | The time boundary Forecast uses to align and aggregate any data that
    -- doesn\'t align with your forecast frequency. Provide the unit of time
    -- and the time boundary as a key value pair. For more information on
    -- specifying a time boundary, see
    -- <https://docs.aws.amazon.com/forecast/latest/dg/data-aggregation.html#specifying-time-boundary Specifying a Time Boundary>.
    -- If you don\'t provide a time boundary, Forecast uses a set of
    -- <https://docs.aws.amazon.com/forecast/latest/dg/data-aggregation.html#default-time-boundaries Default Time Boundaries>.
    timeAlignmentBoundary :: Prelude.Maybe TimeAlignmentBoundary,
    -- | A unique name for the predictor
    predictorName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAutoPredictor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataConfig', 'createAutoPredictor_dataConfig' - The data configuration for your dataset group and any additional
-- datasets.
--
-- 'encryptionConfig', 'createAutoPredictor_encryptionConfig' - Undocumented member.
--
-- 'explainPredictor', 'createAutoPredictor_explainPredictor' - Create an Explainability resource for the predictor.
--
-- 'forecastDimensions', 'createAutoPredictor_forecastDimensions' - An array of dimension (field) names that specify how to group the
-- generated forecast.
--
-- For example, if you are generating forecasts for item sales across all
-- your stores, and your dataset contains a @store_id@ field, you would
-- specify @store_id@ as a dimension to group sales forecasts for each
-- store.
--
-- 'forecastFrequency', 'createAutoPredictor_forecastFrequency' - The frequency of predictions in a forecast.
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
--
-- 'forecastHorizon', 'createAutoPredictor_forecastHorizon' - The number of time-steps that the model predicts. The forecast horizon
-- is also called the prediction length.
--
-- The maximum forecast horizon is the lesser of 500 time-steps or 1\/4 of
-- the TARGET_TIME_SERIES dataset length. If you are retraining an existing
-- AutoPredictor, then the maximum forecast horizon is the lesser of 500
-- time-steps or 1\/3 of the TARGET_TIME_SERIES dataset length.
--
-- If you are upgrading to an AutoPredictor or retraining an existing
-- AutoPredictor, you cannot update the forecast horizon parameter. You can
-- meet this requirement by providing longer time-series in the dataset.
--
-- 'forecastTypes', 'createAutoPredictor_forecastTypes' - The forecast types used to train a predictor. You can specify up to five
-- forecast types. Forecast types can be quantiles from 0.01 to 0.99, by
-- increments of 0.01 or higher. You can also specify the mean forecast
-- with @mean@.
--
-- 'monitorConfig', 'createAutoPredictor_monitorConfig' - The configuration details for predictor monitoring. Provide a name for
-- the monitor resource to enable predictor monitoring.
--
-- Predictor monitoring allows you to see how your predictor\'s performance
-- changes over time. For more information, see
-- <https://docs.aws.amazon.com/forecast/latest/dg/predictor-monitoring.html Predictor Monitoring>.
--
-- 'optimizationMetric', 'createAutoPredictor_optimizationMetric' - The accuracy metric used to optimize the predictor.
--
-- 'referencePredictorArn', 'createAutoPredictor_referencePredictorArn' - The ARN of the predictor to retrain or upgrade. This parameter is only
-- used when retraining or upgrading a predictor. When creating a new
-- predictor, do not specify a value for this parameter.
--
-- When upgrading or retraining a predictor, only specify values for the
-- @ReferencePredictorArn@ and @PredictorName@. The value for
-- @PredictorName@ must be a unique predictor name.
--
-- 'tags', 'createAutoPredictor_tags' - Optional metadata to help you categorize and organize your predictors.
-- Each tag consists of a key and an optional value, both of which you
-- define. Tag keys and values are case sensitive.
--
-- The following restrictions apply to tags:
--
-- -   For each resource, each tag key must be unique and each tag key must
--     have one value.
--
-- -   Maximum number of tags per resource: 50.
--
-- -   Maximum key length: 128 Unicode characters in UTF-8.
--
-- -   Maximum value length: 256 Unicode characters in UTF-8.
--
-- -   Accepted characters: all letters and numbers, spaces representable
--     in UTF-8, and + - = . _ : \/ \@. If your tagging schema is used
--     across other services and resources, the character restrictions of
--     those services also apply.
--
-- -   Key prefixes cannot include any upper or lowercase combination of
--     @aws:@ or @AWS:@. Values can have this prefix. If a tag value has
--     @aws@ as its prefix but the key does not, Forecast considers it to
--     be a user tag and will count against the limit of 50 tags. Tags with
--     only the key prefix of @aws@ do not count against your tags per
--     resource limit. You cannot edit or delete tag keys with this prefix.
--
-- 'timeAlignmentBoundary', 'createAutoPredictor_timeAlignmentBoundary' - The time boundary Forecast uses to align and aggregate any data that
-- doesn\'t align with your forecast frequency. Provide the unit of time
-- and the time boundary as a key value pair. For more information on
-- specifying a time boundary, see
-- <https://docs.aws.amazon.com/forecast/latest/dg/data-aggregation.html#specifying-time-boundary Specifying a Time Boundary>.
-- If you don\'t provide a time boundary, Forecast uses a set of
-- <https://docs.aws.amazon.com/forecast/latest/dg/data-aggregation.html#default-time-boundaries Default Time Boundaries>.
--
-- 'predictorName', 'createAutoPredictor_predictorName' - A unique name for the predictor
newCreateAutoPredictor ::
  -- | 'predictorName'
  Prelude.Text ->
  CreateAutoPredictor
newCreateAutoPredictor pPredictorName_ =
  CreateAutoPredictor'
    { dataConfig = Prelude.Nothing,
      encryptionConfig = Prelude.Nothing,
      explainPredictor = Prelude.Nothing,
      forecastDimensions = Prelude.Nothing,
      forecastFrequency = Prelude.Nothing,
      forecastHorizon = Prelude.Nothing,
      forecastTypes = Prelude.Nothing,
      monitorConfig = Prelude.Nothing,
      optimizationMetric = Prelude.Nothing,
      referencePredictorArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      timeAlignmentBoundary = Prelude.Nothing,
      predictorName = pPredictorName_
    }

-- | The data configuration for your dataset group and any additional
-- datasets.
createAutoPredictor_dataConfig :: Lens.Lens' CreateAutoPredictor (Prelude.Maybe DataConfig)
createAutoPredictor_dataConfig = Lens.lens (\CreateAutoPredictor' {dataConfig} -> dataConfig) (\s@CreateAutoPredictor' {} a -> s {dataConfig = a} :: CreateAutoPredictor)

-- | Undocumented member.
createAutoPredictor_encryptionConfig :: Lens.Lens' CreateAutoPredictor (Prelude.Maybe EncryptionConfig)
createAutoPredictor_encryptionConfig = Lens.lens (\CreateAutoPredictor' {encryptionConfig} -> encryptionConfig) (\s@CreateAutoPredictor' {} a -> s {encryptionConfig = a} :: CreateAutoPredictor)

-- | Create an Explainability resource for the predictor.
createAutoPredictor_explainPredictor :: Lens.Lens' CreateAutoPredictor (Prelude.Maybe Prelude.Bool)
createAutoPredictor_explainPredictor = Lens.lens (\CreateAutoPredictor' {explainPredictor} -> explainPredictor) (\s@CreateAutoPredictor' {} a -> s {explainPredictor = a} :: CreateAutoPredictor)

-- | An array of dimension (field) names that specify how to group the
-- generated forecast.
--
-- For example, if you are generating forecasts for item sales across all
-- your stores, and your dataset contains a @store_id@ field, you would
-- specify @store_id@ as a dimension to group sales forecasts for each
-- store.
createAutoPredictor_forecastDimensions :: Lens.Lens' CreateAutoPredictor (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createAutoPredictor_forecastDimensions = Lens.lens (\CreateAutoPredictor' {forecastDimensions} -> forecastDimensions) (\s@CreateAutoPredictor' {} a -> s {forecastDimensions = a} :: CreateAutoPredictor) Prelude.. Lens.mapping Lens.coerced

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
createAutoPredictor_forecastFrequency :: Lens.Lens' CreateAutoPredictor (Prelude.Maybe Prelude.Text)
createAutoPredictor_forecastFrequency = Lens.lens (\CreateAutoPredictor' {forecastFrequency} -> forecastFrequency) (\s@CreateAutoPredictor' {} a -> s {forecastFrequency = a} :: CreateAutoPredictor)

-- | The number of time-steps that the model predicts. The forecast horizon
-- is also called the prediction length.
--
-- The maximum forecast horizon is the lesser of 500 time-steps or 1\/4 of
-- the TARGET_TIME_SERIES dataset length. If you are retraining an existing
-- AutoPredictor, then the maximum forecast horizon is the lesser of 500
-- time-steps or 1\/3 of the TARGET_TIME_SERIES dataset length.
--
-- If you are upgrading to an AutoPredictor or retraining an existing
-- AutoPredictor, you cannot update the forecast horizon parameter. You can
-- meet this requirement by providing longer time-series in the dataset.
createAutoPredictor_forecastHorizon :: Lens.Lens' CreateAutoPredictor (Prelude.Maybe Prelude.Int)
createAutoPredictor_forecastHorizon = Lens.lens (\CreateAutoPredictor' {forecastHorizon} -> forecastHorizon) (\s@CreateAutoPredictor' {} a -> s {forecastHorizon = a} :: CreateAutoPredictor)

-- | The forecast types used to train a predictor. You can specify up to five
-- forecast types. Forecast types can be quantiles from 0.01 to 0.99, by
-- increments of 0.01 or higher. You can also specify the mean forecast
-- with @mean@.
createAutoPredictor_forecastTypes :: Lens.Lens' CreateAutoPredictor (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createAutoPredictor_forecastTypes = Lens.lens (\CreateAutoPredictor' {forecastTypes} -> forecastTypes) (\s@CreateAutoPredictor' {} a -> s {forecastTypes = a} :: CreateAutoPredictor) Prelude.. Lens.mapping Lens.coerced

-- | The configuration details for predictor monitoring. Provide a name for
-- the monitor resource to enable predictor monitoring.
--
-- Predictor monitoring allows you to see how your predictor\'s performance
-- changes over time. For more information, see
-- <https://docs.aws.amazon.com/forecast/latest/dg/predictor-monitoring.html Predictor Monitoring>.
createAutoPredictor_monitorConfig :: Lens.Lens' CreateAutoPredictor (Prelude.Maybe MonitorConfig)
createAutoPredictor_monitorConfig = Lens.lens (\CreateAutoPredictor' {monitorConfig} -> monitorConfig) (\s@CreateAutoPredictor' {} a -> s {monitorConfig = a} :: CreateAutoPredictor)

-- | The accuracy metric used to optimize the predictor.
createAutoPredictor_optimizationMetric :: Lens.Lens' CreateAutoPredictor (Prelude.Maybe OptimizationMetric)
createAutoPredictor_optimizationMetric = Lens.lens (\CreateAutoPredictor' {optimizationMetric} -> optimizationMetric) (\s@CreateAutoPredictor' {} a -> s {optimizationMetric = a} :: CreateAutoPredictor)

-- | The ARN of the predictor to retrain or upgrade. This parameter is only
-- used when retraining or upgrading a predictor. When creating a new
-- predictor, do not specify a value for this parameter.
--
-- When upgrading or retraining a predictor, only specify values for the
-- @ReferencePredictorArn@ and @PredictorName@. The value for
-- @PredictorName@ must be a unique predictor name.
createAutoPredictor_referencePredictorArn :: Lens.Lens' CreateAutoPredictor (Prelude.Maybe Prelude.Text)
createAutoPredictor_referencePredictorArn = Lens.lens (\CreateAutoPredictor' {referencePredictorArn} -> referencePredictorArn) (\s@CreateAutoPredictor' {} a -> s {referencePredictorArn = a} :: CreateAutoPredictor)

-- | Optional metadata to help you categorize and organize your predictors.
-- Each tag consists of a key and an optional value, both of which you
-- define. Tag keys and values are case sensitive.
--
-- The following restrictions apply to tags:
--
-- -   For each resource, each tag key must be unique and each tag key must
--     have one value.
--
-- -   Maximum number of tags per resource: 50.
--
-- -   Maximum key length: 128 Unicode characters in UTF-8.
--
-- -   Maximum value length: 256 Unicode characters in UTF-8.
--
-- -   Accepted characters: all letters and numbers, spaces representable
--     in UTF-8, and + - = . _ : \/ \@. If your tagging schema is used
--     across other services and resources, the character restrictions of
--     those services also apply.
--
-- -   Key prefixes cannot include any upper or lowercase combination of
--     @aws:@ or @AWS:@. Values can have this prefix. If a tag value has
--     @aws@ as its prefix but the key does not, Forecast considers it to
--     be a user tag and will count against the limit of 50 tags. Tags with
--     only the key prefix of @aws@ do not count against your tags per
--     resource limit. You cannot edit or delete tag keys with this prefix.
createAutoPredictor_tags :: Lens.Lens' CreateAutoPredictor (Prelude.Maybe [Tag])
createAutoPredictor_tags = Lens.lens (\CreateAutoPredictor' {tags} -> tags) (\s@CreateAutoPredictor' {} a -> s {tags = a} :: CreateAutoPredictor) Prelude.. Lens.mapping Lens.coerced

-- | The time boundary Forecast uses to align and aggregate any data that
-- doesn\'t align with your forecast frequency. Provide the unit of time
-- and the time boundary as a key value pair. For more information on
-- specifying a time boundary, see
-- <https://docs.aws.amazon.com/forecast/latest/dg/data-aggregation.html#specifying-time-boundary Specifying a Time Boundary>.
-- If you don\'t provide a time boundary, Forecast uses a set of
-- <https://docs.aws.amazon.com/forecast/latest/dg/data-aggregation.html#default-time-boundaries Default Time Boundaries>.
createAutoPredictor_timeAlignmentBoundary :: Lens.Lens' CreateAutoPredictor (Prelude.Maybe TimeAlignmentBoundary)
createAutoPredictor_timeAlignmentBoundary = Lens.lens (\CreateAutoPredictor' {timeAlignmentBoundary} -> timeAlignmentBoundary) (\s@CreateAutoPredictor' {} a -> s {timeAlignmentBoundary = a} :: CreateAutoPredictor)

-- | A unique name for the predictor
createAutoPredictor_predictorName :: Lens.Lens' CreateAutoPredictor Prelude.Text
createAutoPredictor_predictorName = Lens.lens (\CreateAutoPredictor' {predictorName} -> predictorName) (\s@CreateAutoPredictor' {} a -> s {predictorName = a} :: CreateAutoPredictor)

instance Core.AWSRequest CreateAutoPredictor where
  type
    AWSResponse CreateAutoPredictor =
      CreateAutoPredictorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAutoPredictorResponse'
            Prelude.<$> (x Data..?> "PredictorArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAutoPredictor where
  hashWithSalt _salt CreateAutoPredictor' {..} =
    _salt
      `Prelude.hashWithSalt` dataConfig
      `Prelude.hashWithSalt` encryptionConfig
      `Prelude.hashWithSalt` explainPredictor
      `Prelude.hashWithSalt` forecastDimensions
      `Prelude.hashWithSalt` forecastFrequency
      `Prelude.hashWithSalt` forecastHorizon
      `Prelude.hashWithSalt` forecastTypes
      `Prelude.hashWithSalt` monitorConfig
      `Prelude.hashWithSalt` optimizationMetric
      `Prelude.hashWithSalt` referencePredictorArn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timeAlignmentBoundary
      `Prelude.hashWithSalt` predictorName

instance Prelude.NFData CreateAutoPredictor where
  rnf CreateAutoPredictor' {..} =
    Prelude.rnf dataConfig `Prelude.seq`
      Prelude.rnf encryptionConfig `Prelude.seq`
        Prelude.rnf explainPredictor `Prelude.seq`
          Prelude.rnf forecastDimensions `Prelude.seq`
            Prelude.rnf forecastFrequency `Prelude.seq`
              Prelude.rnf forecastHorizon `Prelude.seq`
                Prelude.rnf forecastTypes `Prelude.seq`
                  Prelude.rnf monitorConfig `Prelude.seq`
                    Prelude.rnf optimizationMetric `Prelude.seq`
                      Prelude.rnf referencePredictorArn `Prelude.seq`
                        Prelude.rnf tags `Prelude.seq`
                          Prelude.rnf timeAlignmentBoundary `Prelude.seq`
                            Prelude.rnf predictorName

instance Data.ToHeaders CreateAutoPredictor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.CreateAutoPredictor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAutoPredictor where
  toJSON CreateAutoPredictor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataConfig" Data..=) Prelude.<$> dataConfig,
            ("EncryptionConfig" Data..=)
              Prelude.<$> encryptionConfig,
            ("ExplainPredictor" Data..=)
              Prelude.<$> explainPredictor,
            ("ForecastDimensions" Data..=)
              Prelude.<$> forecastDimensions,
            ("ForecastFrequency" Data..=)
              Prelude.<$> forecastFrequency,
            ("ForecastHorizon" Data..=)
              Prelude.<$> forecastHorizon,
            ("ForecastTypes" Data..=) Prelude.<$> forecastTypes,
            ("MonitorConfig" Data..=) Prelude.<$> monitorConfig,
            ("OptimizationMetric" Data..=)
              Prelude.<$> optimizationMetric,
            ("ReferencePredictorArn" Data..=)
              Prelude.<$> referencePredictorArn,
            ("Tags" Data..=) Prelude.<$> tags,
            ("TimeAlignmentBoundary" Data..=)
              Prelude.<$> timeAlignmentBoundary,
            Prelude.Just
              ("PredictorName" Data..= predictorName)
          ]
      )

instance Data.ToPath CreateAutoPredictor where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateAutoPredictor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAutoPredictorResponse' smart constructor.
data CreateAutoPredictorResponse = CreateAutoPredictorResponse'
  { -- | The Amazon Resource Name (ARN) of the predictor.
    predictorArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAutoPredictorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'predictorArn', 'createAutoPredictorResponse_predictorArn' - The Amazon Resource Name (ARN) of the predictor.
--
-- 'httpStatus', 'createAutoPredictorResponse_httpStatus' - The response's http status code.
newCreateAutoPredictorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAutoPredictorResponse
newCreateAutoPredictorResponse pHttpStatus_ =
  CreateAutoPredictorResponse'
    { predictorArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the predictor.
createAutoPredictorResponse_predictorArn :: Lens.Lens' CreateAutoPredictorResponse (Prelude.Maybe Prelude.Text)
createAutoPredictorResponse_predictorArn = Lens.lens (\CreateAutoPredictorResponse' {predictorArn} -> predictorArn) (\s@CreateAutoPredictorResponse' {} a -> s {predictorArn = a} :: CreateAutoPredictorResponse)

-- | The response's http status code.
createAutoPredictorResponse_httpStatus :: Lens.Lens' CreateAutoPredictorResponse Prelude.Int
createAutoPredictorResponse_httpStatus = Lens.lens (\CreateAutoPredictorResponse' {httpStatus} -> httpStatus) (\s@CreateAutoPredictorResponse' {} a -> s {httpStatus = a} :: CreateAutoPredictorResponse)

instance Prelude.NFData CreateAutoPredictorResponse where
  rnf CreateAutoPredictorResponse' {..} =
    Prelude.rnf predictorArn `Prelude.seq`
      Prelude.rnf httpStatus
