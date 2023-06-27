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
-- Module      : Amazonka.Forecast.CreatePredictor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation creates a legacy predictor that does not include all the
-- predictor functionalities provided by Amazon Forecast. To create a
-- predictor that is compatible with all aspects of Forecast, use
-- CreateAutoPredictor.
--
-- Creates an Amazon Forecast predictor.
--
-- In the request, provide a dataset group and either specify an algorithm
-- or let Amazon Forecast choose an algorithm for you using AutoML. If you
-- specify an algorithm, you also can override algorithm-specific
-- hyperparameters.
--
-- Amazon Forecast uses the algorithm to train a predictor using the latest
-- version of the datasets in the specified dataset group. You can then
-- generate a forecast using the CreateForecast operation.
--
-- To see the evaluation metrics, use the GetAccuracyMetrics operation.
--
-- You can specify a featurization configuration to fill and aggregate the
-- data fields in the @TARGET_TIME_SERIES@ dataset to improve model
-- training. For more information, see FeaturizationConfig.
--
-- For RELATED_TIME_SERIES datasets, @CreatePredictor@ verifies that the
-- @DataFrequency@ specified when the dataset was created matches the
-- @ForecastFrequency@. TARGET_TIME_SERIES datasets don\'t have this
-- restriction. Amazon Forecast also verifies the delimiter and timestamp
-- format. For more information, see howitworks-datasets-groups.
--
-- By default, predictors are trained and evaluated at the 0.1 (P10), 0.5
-- (P50), and 0.9 (P90) quantiles. You can choose custom forecast types to
-- train and evaluate your predictor by setting the @ForecastTypes@.
--
-- __AutoML__
--
-- If you want Amazon Forecast to evaluate each algorithm and choose the
-- one that minimizes the @objective function@, set @PerformAutoML@ to
-- @true@. The @objective function@ is defined as the mean of the weighted
-- losses over the forecast types. By default, these are the p10, p50, and
-- p90 quantile losses. For more information, see EvaluationResult.
--
-- When AutoML is enabled, the following properties are disallowed:
--
-- -   @AlgorithmArn@
--
-- -   @HPOConfig@
--
-- -   @PerformHPO@
--
-- -   @TrainingParameters@
--
-- To get a list of all of your predictors, use the ListPredictors
-- operation.
--
-- Before you can use the predictor to create a forecast, the @Status@ of
-- the predictor must be @ACTIVE@, signifying that training has completed.
-- To get the status, use the DescribePredictor operation.
module Amazonka.Forecast.CreatePredictor
  ( -- * Creating a Request
    CreatePredictor (..),
    newCreatePredictor,

    -- * Request Lenses
    createPredictor_algorithmArn,
    createPredictor_autoMLOverrideStrategy,
    createPredictor_encryptionConfig,
    createPredictor_evaluationParameters,
    createPredictor_forecastTypes,
    createPredictor_hPOConfig,
    createPredictor_optimizationMetric,
    createPredictor_performAutoML,
    createPredictor_performHPO,
    createPredictor_tags,
    createPredictor_trainingParameters,
    createPredictor_predictorName,
    createPredictor_forecastHorizon,
    createPredictor_inputDataConfig,
    createPredictor_featurizationConfig,

    -- * Destructuring the Response
    CreatePredictorResponse (..),
    newCreatePredictorResponse,

    -- * Response Lenses
    createPredictorResponse_predictorArn,
    createPredictorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePredictor' smart constructor.
data CreatePredictor = CreatePredictor'
  { -- | The Amazon Resource Name (ARN) of the algorithm to use for model
    -- training. Required if @PerformAutoML@ is not set to @true@.
    --
    -- __Supported algorithms:__
    --
    -- -   @arn:aws:forecast:::algorithm\/ARIMA@
    --
    -- -   @arn:aws:forecast:::algorithm\/CNN-QR@
    --
    -- -   @arn:aws:forecast:::algorithm\/Deep_AR_Plus@
    --
    -- -   @arn:aws:forecast:::algorithm\/ETS@
    --
    -- -   @arn:aws:forecast:::algorithm\/NPTS@
    --
    -- -   @arn:aws:forecast:::algorithm\/Prophet@
    algorithmArn :: Prelude.Maybe Prelude.Text,
    -- | The @LatencyOptimized@ AutoML override strategy is only available in
    -- private beta. Contact Amazon Web Services Support or your account
    -- manager to learn more about access privileges.
    --
    -- Used to overide the default AutoML strategy, which is to optimize
    -- predictor accuracy. To apply an AutoML strategy that minimizes training
    -- time, use @LatencyOptimized@.
    --
    -- This parameter is only valid for predictors trained using AutoML.
    autoMLOverrideStrategy :: Prelude.Maybe AutoMLOverrideStrategy,
    -- | An Key Management Service (KMS) key and the Identity and Access
    -- Management (IAM) role that Amazon Forecast can assume to access the key.
    encryptionConfig :: Prelude.Maybe EncryptionConfig,
    -- | Used to override the default evaluation parameters of the specified
    -- algorithm. Amazon Forecast evaluates a predictor by splitting a dataset
    -- into training data and testing data. The evaluation parameters define
    -- how to perform the split and the number of iterations.
    evaluationParameters :: Prelude.Maybe EvaluationParameters,
    -- | Specifies the forecast types used to train a predictor. You can specify
    -- up to five forecast types. Forecast types can be quantiles from 0.01 to
    -- 0.99, by increments of 0.01 or higher. You can also specify the mean
    -- forecast with @mean@.
    --
    -- The default value is @[\"0.10\", \"0.50\", \"0.9\"]@.
    forecastTypes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Provides hyperparameter override values for the algorithm. If you don\'t
    -- provide this parameter, Amazon Forecast uses default values. The
    -- individual algorithms specify which hyperparameters support
    -- hyperparameter optimization (HPO). For more information, see
    -- aws-forecast-choosing-recipes.
    --
    -- If you included the @HPOConfig@ object, you must set @PerformHPO@ to
    -- true.
    hPOConfig :: Prelude.Maybe HyperParameterTuningJobConfig,
    -- | The accuracy metric used to optimize the predictor.
    optimizationMetric :: Prelude.Maybe OptimizationMetric,
    -- | Whether to perform AutoML. When Amazon Forecast performs AutoML, it
    -- evaluates the algorithms it provides and chooses the best algorithm and
    -- configuration for your training dataset.
    --
    -- The default value is @false@. In this case, you are required to specify
    -- an algorithm.
    --
    -- Set @PerformAutoML@ to @true@ to have Amazon Forecast perform AutoML.
    -- This is a good option if you aren\'t sure which algorithm is suitable
    -- for your training data. In this case, @PerformHPO@ must be false.
    performAutoML :: Prelude.Maybe Prelude.Bool,
    -- | Whether to perform hyperparameter optimization (HPO). HPO finds optimal
    -- hyperparameter values for your training data. The process of performing
    -- HPO is known as running a hyperparameter tuning job.
    --
    -- The default value is @false@. In this case, Amazon Forecast uses default
    -- hyperparameter values from the chosen algorithm.
    --
    -- To override the default values, set @PerformHPO@ to @true@ and,
    -- optionally, supply the HyperParameterTuningJobConfig object. The tuning
    -- job specifies a metric to optimize, which hyperparameters participate in
    -- tuning, and the valid range for each tunable hyperparameter. In this
    -- case, you are required to specify an algorithm and @PerformAutoML@ must
    -- be false.
    --
    -- The following algorithms support HPO:
    --
    -- -   DeepAR+
    --
    -- -   CNN-QR
    performHPO :: Prelude.Maybe Prelude.Bool,
    -- | The optional metadata that you apply to the predictor to help you
    -- categorize and organize them. Each tag consists of a key and an optional
    -- value, both of which you define.
    --
    -- The following basic restrictions apply to tags:
    --
    -- -   Maximum number of tags per resource - 50.
    --
    -- -   For each resource, each tag key must be unique, and each tag key can
    --     have only one value.
    --
    -- -   Maximum key length - 128 Unicode characters in UTF-8.
    --
    -- -   Maximum value length - 256 Unicode characters in UTF-8.
    --
    -- -   If your tagging schema is used across multiple services and
    --     resources, remember that other services may have restrictions on
    --     allowed characters. Generally allowed characters are: letters,
    --     numbers, and spaces representable in UTF-8, and the following
    --     characters: + - = . _ : \/ \@.
    --
    -- -   Tag keys and values are case sensitive.
    --
    -- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
    --     such as a prefix for keys as it is reserved for Amazon Web Services
    --     use. You cannot edit or delete tag keys with this prefix. Values can
    --     have this prefix. If a tag value has @aws@ as its prefix but the key
    --     does not, then Forecast considers it to be a user tag and will count
    --     against the limit of 50 tags. Tags with only the key prefix of @aws@
    --     do not count against your tags per resource limit.
    tags :: Prelude.Maybe [Tag],
    -- | The hyperparameters to override for model training. The hyperparameters
    -- that you can override are listed in the individual algorithms. For the
    -- list of supported algorithms, see aws-forecast-choosing-recipes.
    trainingParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A name for the predictor.
    predictorName :: Prelude.Text,
    -- | Specifies the number of time-steps that the model is trained to predict.
    -- The forecast horizon is also called the prediction length.
    --
    -- For example, if you configure a dataset for daily data collection (using
    -- the @DataFrequency@ parameter of the CreateDataset operation) and set
    -- the forecast horizon to 10, the model returns predictions for 10 days.
    --
    -- The maximum forecast horizon is the lesser of 500 time-steps or 1\/3 of
    -- the TARGET_TIME_SERIES dataset length.
    forecastHorizon :: Prelude.Int,
    -- | Describes the dataset group that contains the data to use to train the
    -- predictor.
    inputDataConfig :: InputDataConfig,
    -- | The featurization configuration.
    featurizationConfig :: FeaturizationConfig
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePredictor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithmArn', 'createPredictor_algorithmArn' - The Amazon Resource Name (ARN) of the algorithm to use for model
-- training. Required if @PerformAutoML@ is not set to @true@.
--
-- __Supported algorithms:__
--
-- -   @arn:aws:forecast:::algorithm\/ARIMA@
--
-- -   @arn:aws:forecast:::algorithm\/CNN-QR@
--
-- -   @arn:aws:forecast:::algorithm\/Deep_AR_Plus@
--
-- -   @arn:aws:forecast:::algorithm\/ETS@
--
-- -   @arn:aws:forecast:::algorithm\/NPTS@
--
-- -   @arn:aws:forecast:::algorithm\/Prophet@
--
-- 'autoMLOverrideStrategy', 'createPredictor_autoMLOverrideStrategy' - The @LatencyOptimized@ AutoML override strategy is only available in
-- private beta. Contact Amazon Web Services Support or your account
-- manager to learn more about access privileges.
--
-- Used to overide the default AutoML strategy, which is to optimize
-- predictor accuracy. To apply an AutoML strategy that minimizes training
-- time, use @LatencyOptimized@.
--
-- This parameter is only valid for predictors trained using AutoML.
--
-- 'encryptionConfig', 'createPredictor_encryptionConfig' - An Key Management Service (KMS) key and the Identity and Access
-- Management (IAM) role that Amazon Forecast can assume to access the key.
--
-- 'evaluationParameters', 'createPredictor_evaluationParameters' - Used to override the default evaluation parameters of the specified
-- algorithm. Amazon Forecast evaluates a predictor by splitting a dataset
-- into training data and testing data. The evaluation parameters define
-- how to perform the split and the number of iterations.
--
-- 'forecastTypes', 'createPredictor_forecastTypes' - Specifies the forecast types used to train a predictor. You can specify
-- up to five forecast types. Forecast types can be quantiles from 0.01 to
-- 0.99, by increments of 0.01 or higher. You can also specify the mean
-- forecast with @mean@.
--
-- The default value is @[\"0.10\", \"0.50\", \"0.9\"]@.
--
-- 'hPOConfig', 'createPredictor_hPOConfig' - Provides hyperparameter override values for the algorithm. If you don\'t
-- provide this parameter, Amazon Forecast uses default values. The
-- individual algorithms specify which hyperparameters support
-- hyperparameter optimization (HPO). For more information, see
-- aws-forecast-choosing-recipes.
--
-- If you included the @HPOConfig@ object, you must set @PerformHPO@ to
-- true.
--
-- 'optimizationMetric', 'createPredictor_optimizationMetric' - The accuracy metric used to optimize the predictor.
--
-- 'performAutoML', 'createPredictor_performAutoML' - Whether to perform AutoML. When Amazon Forecast performs AutoML, it
-- evaluates the algorithms it provides and chooses the best algorithm and
-- configuration for your training dataset.
--
-- The default value is @false@. In this case, you are required to specify
-- an algorithm.
--
-- Set @PerformAutoML@ to @true@ to have Amazon Forecast perform AutoML.
-- This is a good option if you aren\'t sure which algorithm is suitable
-- for your training data. In this case, @PerformHPO@ must be false.
--
-- 'performHPO', 'createPredictor_performHPO' - Whether to perform hyperparameter optimization (HPO). HPO finds optimal
-- hyperparameter values for your training data. The process of performing
-- HPO is known as running a hyperparameter tuning job.
--
-- The default value is @false@. In this case, Amazon Forecast uses default
-- hyperparameter values from the chosen algorithm.
--
-- To override the default values, set @PerformHPO@ to @true@ and,
-- optionally, supply the HyperParameterTuningJobConfig object. The tuning
-- job specifies a metric to optimize, which hyperparameters participate in
-- tuning, and the valid range for each tunable hyperparameter. In this
-- case, you are required to specify an algorithm and @PerformAutoML@ must
-- be false.
--
-- The following algorithms support HPO:
--
-- -   DeepAR+
--
-- -   CNN-QR
--
-- 'tags', 'createPredictor_tags' - The optional metadata that you apply to the predictor to help you
-- categorize and organize them. Each tag consists of a key and an optional
-- value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50.
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8.
--
-- -   Maximum value length - 256 Unicode characters in UTF-8.
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for keys as it is reserved for Amazon Web Services
--     use. You cannot edit or delete tag keys with this prefix. Values can
--     have this prefix. If a tag value has @aws@ as its prefix but the key
--     does not, then Forecast considers it to be a user tag and will count
--     against the limit of 50 tags. Tags with only the key prefix of @aws@
--     do not count against your tags per resource limit.
--
-- 'trainingParameters', 'createPredictor_trainingParameters' - The hyperparameters to override for model training. The hyperparameters
-- that you can override are listed in the individual algorithms. For the
-- list of supported algorithms, see aws-forecast-choosing-recipes.
--
-- 'predictorName', 'createPredictor_predictorName' - A name for the predictor.
--
-- 'forecastHorizon', 'createPredictor_forecastHorizon' - Specifies the number of time-steps that the model is trained to predict.
-- The forecast horizon is also called the prediction length.
--
-- For example, if you configure a dataset for daily data collection (using
-- the @DataFrequency@ parameter of the CreateDataset operation) and set
-- the forecast horizon to 10, the model returns predictions for 10 days.
--
-- The maximum forecast horizon is the lesser of 500 time-steps or 1\/3 of
-- the TARGET_TIME_SERIES dataset length.
--
-- 'inputDataConfig', 'createPredictor_inputDataConfig' - Describes the dataset group that contains the data to use to train the
-- predictor.
--
-- 'featurizationConfig', 'createPredictor_featurizationConfig' - The featurization configuration.
newCreatePredictor ::
  -- | 'predictorName'
  Prelude.Text ->
  -- | 'forecastHorizon'
  Prelude.Int ->
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'featurizationConfig'
  FeaturizationConfig ->
  CreatePredictor
newCreatePredictor
  pPredictorName_
  pForecastHorizon_
  pInputDataConfig_
  pFeaturizationConfig_ =
    CreatePredictor'
      { algorithmArn = Prelude.Nothing,
        autoMLOverrideStrategy = Prelude.Nothing,
        encryptionConfig = Prelude.Nothing,
        evaluationParameters = Prelude.Nothing,
        forecastTypes = Prelude.Nothing,
        hPOConfig = Prelude.Nothing,
        optimizationMetric = Prelude.Nothing,
        performAutoML = Prelude.Nothing,
        performHPO = Prelude.Nothing,
        tags = Prelude.Nothing,
        trainingParameters = Prelude.Nothing,
        predictorName = pPredictorName_,
        forecastHorizon = pForecastHorizon_,
        inputDataConfig = pInputDataConfig_,
        featurizationConfig = pFeaturizationConfig_
      }

-- | The Amazon Resource Name (ARN) of the algorithm to use for model
-- training. Required if @PerformAutoML@ is not set to @true@.
--
-- __Supported algorithms:__
--
-- -   @arn:aws:forecast:::algorithm\/ARIMA@
--
-- -   @arn:aws:forecast:::algorithm\/CNN-QR@
--
-- -   @arn:aws:forecast:::algorithm\/Deep_AR_Plus@
--
-- -   @arn:aws:forecast:::algorithm\/ETS@
--
-- -   @arn:aws:forecast:::algorithm\/NPTS@
--
-- -   @arn:aws:forecast:::algorithm\/Prophet@
createPredictor_algorithmArn :: Lens.Lens' CreatePredictor (Prelude.Maybe Prelude.Text)
createPredictor_algorithmArn = Lens.lens (\CreatePredictor' {algorithmArn} -> algorithmArn) (\s@CreatePredictor' {} a -> s {algorithmArn = a} :: CreatePredictor)

-- | The @LatencyOptimized@ AutoML override strategy is only available in
-- private beta. Contact Amazon Web Services Support or your account
-- manager to learn more about access privileges.
--
-- Used to overide the default AutoML strategy, which is to optimize
-- predictor accuracy. To apply an AutoML strategy that minimizes training
-- time, use @LatencyOptimized@.
--
-- This parameter is only valid for predictors trained using AutoML.
createPredictor_autoMLOverrideStrategy :: Lens.Lens' CreatePredictor (Prelude.Maybe AutoMLOverrideStrategy)
createPredictor_autoMLOverrideStrategy = Lens.lens (\CreatePredictor' {autoMLOverrideStrategy} -> autoMLOverrideStrategy) (\s@CreatePredictor' {} a -> s {autoMLOverrideStrategy = a} :: CreatePredictor)

-- | An Key Management Service (KMS) key and the Identity and Access
-- Management (IAM) role that Amazon Forecast can assume to access the key.
createPredictor_encryptionConfig :: Lens.Lens' CreatePredictor (Prelude.Maybe EncryptionConfig)
createPredictor_encryptionConfig = Lens.lens (\CreatePredictor' {encryptionConfig} -> encryptionConfig) (\s@CreatePredictor' {} a -> s {encryptionConfig = a} :: CreatePredictor)

-- | Used to override the default evaluation parameters of the specified
-- algorithm. Amazon Forecast evaluates a predictor by splitting a dataset
-- into training data and testing data. The evaluation parameters define
-- how to perform the split and the number of iterations.
createPredictor_evaluationParameters :: Lens.Lens' CreatePredictor (Prelude.Maybe EvaluationParameters)
createPredictor_evaluationParameters = Lens.lens (\CreatePredictor' {evaluationParameters} -> evaluationParameters) (\s@CreatePredictor' {} a -> s {evaluationParameters = a} :: CreatePredictor)

-- | Specifies the forecast types used to train a predictor. You can specify
-- up to five forecast types. Forecast types can be quantiles from 0.01 to
-- 0.99, by increments of 0.01 or higher. You can also specify the mean
-- forecast with @mean@.
--
-- The default value is @[\"0.10\", \"0.50\", \"0.9\"]@.
createPredictor_forecastTypes :: Lens.Lens' CreatePredictor (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createPredictor_forecastTypes = Lens.lens (\CreatePredictor' {forecastTypes} -> forecastTypes) (\s@CreatePredictor' {} a -> s {forecastTypes = a} :: CreatePredictor) Prelude.. Lens.mapping Lens.coerced

-- | Provides hyperparameter override values for the algorithm. If you don\'t
-- provide this parameter, Amazon Forecast uses default values. The
-- individual algorithms specify which hyperparameters support
-- hyperparameter optimization (HPO). For more information, see
-- aws-forecast-choosing-recipes.
--
-- If you included the @HPOConfig@ object, you must set @PerformHPO@ to
-- true.
createPredictor_hPOConfig :: Lens.Lens' CreatePredictor (Prelude.Maybe HyperParameterTuningJobConfig)
createPredictor_hPOConfig = Lens.lens (\CreatePredictor' {hPOConfig} -> hPOConfig) (\s@CreatePredictor' {} a -> s {hPOConfig = a} :: CreatePredictor)

-- | The accuracy metric used to optimize the predictor.
createPredictor_optimizationMetric :: Lens.Lens' CreatePredictor (Prelude.Maybe OptimizationMetric)
createPredictor_optimizationMetric = Lens.lens (\CreatePredictor' {optimizationMetric} -> optimizationMetric) (\s@CreatePredictor' {} a -> s {optimizationMetric = a} :: CreatePredictor)

-- | Whether to perform AutoML. When Amazon Forecast performs AutoML, it
-- evaluates the algorithms it provides and chooses the best algorithm and
-- configuration for your training dataset.
--
-- The default value is @false@. In this case, you are required to specify
-- an algorithm.
--
-- Set @PerformAutoML@ to @true@ to have Amazon Forecast perform AutoML.
-- This is a good option if you aren\'t sure which algorithm is suitable
-- for your training data. In this case, @PerformHPO@ must be false.
createPredictor_performAutoML :: Lens.Lens' CreatePredictor (Prelude.Maybe Prelude.Bool)
createPredictor_performAutoML = Lens.lens (\CreatePredictor' {performAutoML} -> performAutoML) (\s@CreatePredictor' {} a -> s {performAutoML = a} :: CreatePredictor)

-- | Whether to perform hyperparameter optimization (HPO). HPO finds optimal
-- hyperparameter values for your training data. The process of performing
-- HPO is known as running a hyperparameter tuning job.
--
-- The default value is @false@. In this case, Amazon Forecast uses default
-- hyperparameter values from the chosen algorithm.
--
-- To override the default values, set @PerformHPO@ to @true@ and,
-- optionally, supply the HyperParameterTuningJobConfig object. The tuning
-- job specifies a metric to optimize, which hyperparameters participate in
-- tuning, and the valid range for each tunable hyperparameter. In this
-- case, you are required to specify an algorithm and @PerformAutoML@ must
-- be false.
--
-- The following algorithms support HPO:
--
-- -   DeepAR+
--
-- -   CNN-QR
createPredictor_performHPO :: Lens.Lens' CreatePredictor (Prelude.Maybe Prelude.Bool)
createPredictor_performHPO = Lens.lens (\CreatePredictor' {performHPO} -> performHPO) (\s@CreatePredictor' {} a -> s {performHPO = a} :: CreatePredictor)

-- | The optional metadata that you apply to the predictor to help you
-- categorize and organize them. Each tag consists of a key and an optional
-- value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50.
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8.
--
-- -   Maximum value length - 256 Unicode characters in UTF-8.
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for keys as it is reserved for Amazon Web Services
--     use. You cannot edit or delete tag keys with this prefix. Values can
--     have this prefix. If a tag value has @aws@ as its prefix but the key
--     does not, then Forecast considers it to be a user tag and will count
--     against the limit of 50 tags. Tags with only the key prefix of @aws@
--     do not count against your tags per resource limit.
createPredictor_tags :: Lens.Lens' CreatePredictor (Prelude.Maybe [Tag])
createPredictor_tags = Lens.lens (\CreatePredictor' {tags} -> tags) (\s@CreatePredictor' {} a -> s {tags = a} :: CreatePredictor) Prelude.. Lens.mapping Lens.coerced

-- | The hyperparameters to override for model training. The hyperparameters
-- that you can override are listed in the individual algorithms. For the
-- list of supported algorithms, see aws-forecast-choosing-recipes.
createPredictor_trainingParameters :: Lens.Lens' CreatePredictor (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createPredictor_trainingParameters = Lens.lens (\CreatePredictor' {trainingParameters} -> trainingParameters) (\s@CreatePredictor' {} a -> s {trainingParameters = a} :: CreatePredictor) Prelude.. Lens.mapping Lens.coerced

-- | A name for the predictor.
createPredictor_predictorName :: Lens.Lens' CreatePredictor Prelude.Text
createPredictor_predictorName = Lens.lens (\CreatePredictor' {predictorName} -> predictorName) (\s@CreatePredictor' {} a -> s {predictorName = a} :: CreatePredictor)

-- | Specifies the number of time-steps that the model is trained to predict.
-- The forecast horizon is also called the prediction length.
--
-- For example, if you configure a dataset for daily data collection (using
-- the @DataFrequency@ parameter of the CreateDataset operation) and set
-- the forecast horizon to 10, the model returns predictions for 10 days.
--
-- The maximum forecast horizon is the lesser of 500 time-steps or 1\/3 of
-- the TARGET_TIME_SERIES dataset length.
createPredictor_forecastHorizon :: Lens.Lens' CreatePredictor Prelude.Int
createPredictor_forecastHorizon = Lens.lens (\CreatePredictor' {forecastHorizon} -> forecastHorizon) (\s@CreatePredictor' {} a -> s {forecastHorizon = a} :: CreatePredictor)

-- | Describes the dataset group that contains the data to use to train the
-- predictor.
createPredictor_inputDataConfig :: Lens.Lens' CreatePredictor InputDataConfig
createPredictor_inputDataConfig = Lens.lens (\CreatePredictor' {inputDataConfig} -> inputDataConfig) (\s@CreatePredictor' {} a -> s {inputDataConfig = a} :: CreatePredictor)

-- | The featurization configuration.
createPredictor_featurizationConfig :: Lens.Lens' CreatePredictor FeaturizationConfig
createPredictor_featurizationConfig = Lens.lens (\CreatePredictor' {featurizationConfig} -> featurizationConfig) (\s@CreatePredictor' {} a -> s {featurizationConfig = a} :: CreatePredictor)

instance Core.AWSRequest CreatePredictor where
  type
    AWSResponse CreatePredictor =
      CreatePredictorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePredictorResponse'
            Prelude.<$> (x Data..?> "PredictorArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePredictor where
  hashWithSalt _salt CreatePredictor' {..} =
    _salt
      `Prelude.hashWithSalt` algorithmArn
      `Prelude.hashWithSalt` autoMLOverrideStrategy
      `Prelude.hashWithSalt` encryptionConfig
      `Prelude.hashWithSalt` evaluationParameters
      `Prelude.hashWithSalt` forecastTypes
      `Prelude.hashWithSalt` hPOConfig
      `Prelude.hashWithSalt` optimizationMetric
      `Prelude.hashWithSalt` performAutoML
      `Prelude.hashWithSalt` performHPO
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` trainingParameters
      `Prelude.hashWithSalt` predictorName
      `Prelude.hashWithSalt` forecastHorizon
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` featurizationConfig

instance Prelude.NFData CreatePredictor where
  rnf CreatePredictor' {..} =
    Prelude.rnf algorithmArn
      `Prelude.seq` Prelude.rnf autoMLOverrideStrategy
      `Prelude.seq` Prelude.rnf encryptionConfig
      `Prelude.seq` Prelude.rnf evaluationParameters
      `Prelude.seq` Prelude.rnf forecastTypes
      `Prelude.seq` Prelude.rnf hPOConfig
      `Prelude.seq` Prelude.rnf optimizationMetric
      `Prelude.seq` Prelude.rnf performAutoML
      `Prelude.seq` Prelude.rnf performHPO
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf trainingParameters
      `Prelude.seq` Prelude.rnf predictorName
      `Prelude.seq` Prelude.rnf forecastHorizon
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf featurizationConfig

instance Data.ToHeaders CreatePredictor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.CreatePredictor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePredictor where
  toJSON CreatePredictor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AlgorithmArn" Data..=) Prelude.<$> algorithmArn,
            ("AutoMLOverrideStrategy" Data..=)
              Prelude.<$> autoMLOverrideStrategy,
            ("EncryptionConfig" Data..=)
              Prelude.<$> encryptionConfig,
            ("EvaluationParameters" Data..=)
              Prelude.<$> evaluationParameters,
            ("ForecastTypes" Data..=) Prelude.<$> forecastTypes,
            ("HPOConfig" Data..=) Prelude.<$> hPOConfig,
            ("OptimizationMetric" Data..=)
              Prelude.<$> optimizationMetric,
            ("PerformAutoML" Data..=) Prelude.<$> performAutoML,
            ("PerformHPO" Data..=) Prelude.<$> performHPO,
            ("Tags" Data..=) Prelude.<$> tags,
            ("TrainingParameters" Data..=)
              Prelude.<$> trainingParameters,
            Prelude.Just ("PredictorName" Data..= predictorName),
            Prelude.Just
              ("ForecastHorizon" Data..= forecastHorizon),
            Prelude.Just
              ("InputDataConfig" Data..= inputDataConfig),
            Prelude.Just
              ("FeaturizationConfig" Data..= featurizationConfig)
          ]
      )

instance Data.ToPath CreatePredictor where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePredictor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePredictorResponse' smart constructor.
data CreatePredictorResponse = CreatePredictorResponse'
  { -- | The Amazon Resource Name (ARN) of the predictor.
    predictorArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePredictorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'predictorArn', 'createPredictorResponse_predictorArn' - The Amazon Resource Name (ARN) of the predictor.
--
-- 'httpStatus', 'createPredictorResponse_httpStatus' - The response's http status code.
newCreatePredictorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePredictorResponse
newCreatePredictorResponse pHttpStatus_ =
  CreatePredictorResponse'
    { predictorArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the predictor.
createPredictorResponse_predictorArn :: Lens.Lens' CreatePredictorResponse (Prelude.Maybe Prelude.Text)
createPredictorResponse_predictorArn = Lens.lens (\CreatePredictorResponse' {predictorArn} -> predictorArn) (\s@CreatePredictorResponse' {} a -> s {predictorArn = a} :: CreatePredictorResponse)

-- | The response's http status code.
createPredictorResponse_httpStatus :: Lens.Lens' CreatePredictorResponse Prelude.Int
createPredictorResponse_httpStatus = Lens.lens (\CreatePredictorResponse' {httpStatus} -> httpStatus) (\s@CreatePredictorResponse' {} a -> s {httpStatus = a} :: CreatePredictorResponse)

instance Prelude.NFData CreatePredictorResponse where
  rnf CreatePredictorResponse' {..} =
    Prelude.rnf predictorArn
      `Prelude.seq` Prelude.rnf httpStatus
