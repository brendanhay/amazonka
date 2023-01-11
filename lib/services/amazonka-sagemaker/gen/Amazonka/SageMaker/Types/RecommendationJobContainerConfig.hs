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
-- Module      : Amazonka.SageMaker.Types.RecommendationJobContainerConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RecommendationJobContainerConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.RecommendationJobPayloadConfig

-- | Specifies mandatory fields for running an Inference Recommender job
-- directly in the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateInferenceRecommendationsJob.html CreateInferenceRecommendationsJob>
-- API. The fields specified in @ContainerConfig@ override the
-- corresponding fields in the model package. Use @ContainerConfig@ if you
-- want to specify these fields for the recommendation job but don\'t want
-- to edit them in your model package.
--
-- /See:/ 'newRecommendationJobContainerConfig' smart constructor.
data RecommendationJobContainerConfig = RecommendationJobContainerConfig'
  { -- | The machine learning domain of the model and its components.
    --
    -- Valid Values:
    -- @COMPUTER_VISION | NATURAL_LANGUAGE_PROCESSING | MACHINE_LEARNING@
    domain :: Prelude.Maybe Prelude.Text,
    -- | The machine learning framework of the container image.
    --
    -- Valid Values: @TENSORFLOW | PYTORCH | XGBOOST | SAGEMAKER-SCIKIT-LEARN@
    framework :: Prelude.Maybe Prelude.Text,
    -- | The framework version of the container image.
    frameworkVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of a pre-trained machine learning model benchmarked by Amazon
    -- SageMaker Inference Recommender that matches your model.
    --
    -- Valid Values:
    -- @efficientnetb7 | unet | xgboost | faster-rcnn-resnet101 | nasnetlarge | vgg16 | inception-v3 | mask-rcnn | sagemaker-scikit-learn | densenet201-gluon | resnet18v2-gluon | xception | densenet201 | yolov4 | resnet152 | bert-base-cased | xceptionV1-keras | resnet50 | retinanet@
    nearestModelName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the @SamplePayloadUrl@ and all other sample payload-related
    -- fields.
    payloadConfig :: Prelude.Maybe RecommendationJobPayloadConfig,
    -- | A list of the instance types that are used to generate inferences in
    -- real-time.
    supportedInstanceTypes :: Prelude.Maybe [Prelude.Text],
    -- | The machine learning task that the model accomplishes.
    --
    -- Valid Values:
    -- @IMAGE_CLASSIFICATION | OBJECT_DETECTION | TEXT_GENERATION | IMAGE_SEGMENTATION | FILL_MASK | CLASSIFICATION | REGRESSION | OTHER@
    task :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationJobContainerConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'recommendationJobContainerConfig_domain' - The machine learning domain of the model and its components.
--
-- Valid Values:
-- @COMPUTER_VISION | NATURAL_LANGUAGE_PROCESSING | MACHINE_LEARNING@
--
-- 'framework', 'recommendationJobContainerConfig_framework' - The machine learning framework of the container image.
--
-- Valid Values: @TENSORFLOW | PYTORCH | XGBOOST | SAGEMAKER-SCIKIT-LEARN@
--
-- 'frameworkVersion', 'recommendationJobContainerConfig_frameworkVersion' - The framework version of the container image.
--
-- 'nearestModelName', 'recommendationJobContainerConfig_nearestModelName' - The name of a pre-trained machine learning model benchmarked by Amazon
-- SageMaker Inference Recommender that matches your model.
--
-- Valid Values:
-- @efficientnetb7 | unet | xgboost | faster-rcnn-resnet101 | nasnetlarge | vgg16 | inception-v3 | mask-rcnn | sagemaker-scikit-learn | densenet201-gluon | resnet18v2-gluon | xception | densenet201 | yolov4 | resnet152 | bert-base-cased | xceptionV1-keras | resnet50 | retinanet@
--
-- 'payloadConfig', 'recommendationJobContainerConfig_payloadConfig' - Specifies the @SamplePayloadUrl@ and all other sample payload-related
-- fields.
--
-- 'supportedInstanceTypes', 'recommendationJobContainerConfig_supportedInstanceTypes' - A list of the instance types that are used to generate inferences in
-- real-time.
--
-- 'task', 'recommendationJobContainerConfig_task' - The machine learning task that the model accomplishes.
--
-- Valid Values:
-- @IMAGE_CLASSIFICATION | OBJECT_DETECTION | TEXT_GENERATION | IMAGE_SEGMENTATION | FILL_MASK | CLASSIFICATION | REGRESSION | OTHER@
newRecommendationJobContainerConfig ::
  RecommendationJobContainerConfig
newRecommendationJobContainerConfig =
  RecommendationJobContainerConfig'
    { domain =
        Prelude.Nothing,
      framework = Prelude.Nothing,
      frameworkVersion = Prelude.Nothing,
      nearestModelName = Prelude.Nothing,
      payloadConfig = Prelude.Nothing,
      supportedInstanceTypes = Prelude.Nothing,
      task = Prelude.Nothing
    }

-- | The machine learning domain of the model and its components.
--
-- Valid Values:
-- @COMPUTER_VISION | NATURAL_LANGUAGE_PROCESSING | MACHINE_LEARNING@
recommendationJobContainerConfig_domain :: Lens.Lens' RecommendationJobContainerConfig (Prelude.Maybe Prelude.Text)
recommendationJobContainerConfig_domain = Lens.lens (\RecommendationJobContainerConfig' {domain} -> domain) (\s@RecommendationJobContainerConfig' {} a -> s {domain = a} :: RecommendationJobContainerConfig)

-- | The machine learning framework of the container image.
--
-- Valid Values: @TENSORFLOW | PYTORCH | XGBOOST | SAGEMAKER-SCIKIT-LEARN@
recommendationJobContainerConfig_framework :: Lens.Lens' RecommendationJobContainerConfig (Prelude.Maybe Prelude.Text)
recommendationJobContainerConfig_framework = Lens.lens (\RecommendationJobContainerConfig' {framework} -> framework) (\s@RecommendationJobContainerConfig' {} a -> s {framework = a} :: RecommendationJobContainerConfig)

-- | The framework version of the container image.
recommendationJobContainerConfig_frameworkVersion :: Lens.Lens' RecommendationJobContainerConfig (Prelude.Maybe Prelude.Text)
recommendationJobContainerConfig_frameworkVersion = Lens.lens (\RecommendationJobContainerConfig' {frameworkVersion} -> frameworkVersion) (\s@RecommendationJobContainerConfig' {} a -> s {frameworkVersion = a} :: RecommendationJobContainerConfig)

-- | The name of a pre-trained machine learning model benchmarked by Amazon
-- SageMaker Inference Recommender that matches your model.
--
-- Valid Values:
-- @efficientnetb7 | unet | xgboost | faster-rcnn-resnet101 | nasnetlarge | vgg16 | inception-v3 | mask-rcnn | sagemaker-scikit-learn | densenet201-gluon | resnet18v2-gluon | xception | densenet201 | yolov4 | resnet152 | bert-base-cased | xceptionV1-keras | resnet50 | retinanet@
recommendationJobContainerConfig_nearestModelName :: Lens.Lens' RecommendationJobContainerConfig (Prelude.Maybe Prelude.Text)
recommendationJobContainerConfig_nearestModelName = Lens.lens (\RecommendationJobContainerConfig' {nearestModelName} -> nearestModelName) (\s@RecommendationJobContainerConfig' {} a -> s {nearestModelName = a} :: RecommendationJobContainerConfig)

-- | Specifies the @SamplePayloadUrl@ and all other sample payload-related
-- fields.
recommendationJobContainerConfig_payloadConfig :: Lens.Lens' RecommendationJobContainerConfig (Prelude.Maybe RecommendationJobPayloadConfig)
recommendationJobContainerConfig_payloadConfig = Lens.lens (\RecommendationJobContainerConfig' {payloadConfig} -> payloadConfig) (\s@RecommendationJobContainerConfig' {} a -> s {payloadConfig = a} :: RecommendationJobContainerConfig)

-- | A list of the instance types that are used to generate inferences in
-- real-time.
recommendationJobContainerConfig_supportedInstanceTypes :: Lens.Lens' RecommendationJobContainerConfig (Prelude.Maybe [Prelude.Text])
recommendationJobContainerConfig_supportedInstanceTypes = Lens.lens (\RecommendationJobContainerConfig' {supportedInstanceTypes} -> supportedInstanceTypes) (\s@RecommendationJobContainerConfig' {} a -> s {supportedInstanceTypes = a} :: RecommendationJobContainerConfig) Prelude.. Lens.mapping Lens.coerced

-- | The machine learning task that the model accomplishes.
--
-- Valid Values:
-- @IMAGE_CLASSIFICATION | OBJECT_DETECTION | TEXT_GENERATION | IMAGE_SEGMENTATION | FILL_MASK | CLASSIFICATION | REGRESSION | OTHER@
recommendationJobContainerConfig_task :: Lens.Lens' RecommendationJobContainerConfig (Prelude.Maybe Prelude.Text)
recommendationJobContainerConfig_task = Lens.lens (\RecommendationJobContainerConfig' {task} -> task) (\s@RecommendationJobContainerConfig' {} a -> s {task = a} :: RecommendationJobContainerConfig)

instance
  Data.FromJSON
    RecommendationJobContainerConfig
  where
  parseJSON =
    Data.withObject
      "RecommendationJobContainerConfig"
      ( \x ->
          RecommendationJobContainerConfig'
            Prelude.<$> (x Data..:? "Domain")
            Prelude.<*> (x Data..:? "Framework")
            Prelude.<*> (x Data..:? "FrameworkVersion")
            Prelude.<*> (x Data..:? "NearestModelName")
            Prelude.<*> (x Data..:? "PayloadConfig")
            Prelude.<*> ( x Data..:? "SupportedInstanceTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Task")
      )

instance
  Prelude.Hashable
    RecommendationJobContainerConfig
  where
  hashWithSalt
    _salt
    RecommendationJobContainerConfig' {..} =
      _salt `Prelude.hashWithSalt` domain
        `Prelude.hashWithSalt` framework
        `Prelude.hashWithSalt` frameworkVersion
        `Prelude.hashWithSalt` nearestModelName
        `Prelude.hashWithSalt` payloadConfig
        `Prelude.hashWithSalt` supportedInstanceTypes
        `Prelude.hashWithSalt` task

instance
  Prelude.NFData
    RecommendationJobContainerConfig
  where
  rnf RecommendationJobContainerConfig' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf framework
      `Prelude.seq` Prelude.rnf frameworkVersion
      `Prelude.seq` Prelude.rnf nearestModelName
      `Prelude.seq` Prelude.rnf payloadConfig
      `Prelude.seq` Prelude.rnf supportedInstanceTypes
      `Prelude.seq` Prelude.rnf task

instance Data.ToJSON RecommendationJobContainerConfig where
  toJSON RecommendationJobContainerConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Domain" Data..=) Prelude.<$> domain,
            ("Framework" Data..=) Prelude.<$> framework,
            ("FrameworkVersion" Data..=)
              Prelude.<$> frameworkVersion,
            ("NearestModelName" Data..=)
              Prelude.<$> nearestModelName,
            ("PayloadConfig" Data..=) Prelude.<$> payloadConfig,
            ("SupportedInstanceTypes" Data..=)
              Prelude.<$> supportedInstanceTypes,
            ("Task" Data..=) Prelude.<$> task
          ]
      )
