{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.LabelingJobAlgorithmsConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobAlgorithmsConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.LabelingJobResourceConfig

-- | Provides configuration information for auto-labeling of your data
-- objects. A @LabelingJobAlgorithmsConfig@ object must be supplied in
-- order to use auto-labeling.
--
-- /See:/ 'newLabelingJobAlgorithmsConfig' smart constructor.
data LabelingJobAlgorithmsConfig = LabelingJobAlgorithmsConfig'
  { -- | At the end of an auto-label job Ground Truth sends the Amazon Resource
    -- Name (ARN) of the final model used for auto-labeling. You can use this
    -- model as the starting point for subsequent similar jobs by providing the
    -- ARN of the model here.
    initialActiveLearningModelArn :: Prelude.Maybe Prelude.Text,
    -- | Provides configuration information for a labeling job.
    labelingJobResourceConfig :: Prelude.Maybe LabelingJobResourceConfig,
    -- | Specifies the Amazon Resource Name (ARN) of the algorithm used for
    -- auto-labeling. You must select one of the following ARNs:
    --
    -- -   /Image classification/
    --
    --     @arn:aws:sagemaker:region:027400017018:labeling-job-algorithm-specification\/image-classification@
    --
    -- -   /Text classification/
    --
    --     @arn:aws:sagemaker:region:027400017018:labeling-job-algorithm-specification\/text-classification@
    --
    -- -   /Object detection/
    --
    --     @arn:aws:sagemaker:region:027400017018:labeling-job-algorithm-specification\/object-detection@
    --
    -- -   /Semantic Segmentation/
    --
    --     @arn:aws:sagemaker:region:027400017018:labeling-job-algorithm-specification\/semantic-segmentation@
    labelingJobAlgorithmSpecificationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LabelingJobAlgorithmsConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'initialActiveLearningModelArn', 'labelingJobAlgorithmsConfig_initialActiveLearningModelArn' - At the end of an auto-label job Ground Truth sends the Amazon Resource
-- Name (ARN) of the final model used for auto-labeling. You can use this
-- model as the starting point for subsequent similar jobs by providing the
-- ARN of the model here.
--
-- 'labelingJobResourceConfig', 'labelingJobAlgorithmsConfig_labelingJobResourceConfig' - Provides configuration information for a labeling job.
--
-- 'labelingJobAlgorithmSpecificationArn', 'labelingJobAlgorithmsConfig_labelingJobAlgorithmSpecificationArn' - Specifies the Amazon Resource Name (ARN) of the algorithm used for
-- auto-labeling. You must select one of the following ARNs:
--
-- -   /Image classification/
--
--     @arn:aws:sagemaker:region:027400017018:labeling-job-algorithm-specification\/image-classification@
--
-- -   /Text classification/
--
--     @arn:aws:sagemaker:region:027400017018:labeling-job-algorithm-specification\/text-classification@
--
-- -   /Object detection/
--
--     @arn:aws:sagemaker:region:027400017018:labeling-job-algorithm-specification\/object-detection@
--
-- -   /Semantic Segmentation/
--
--     @arn:aws:sagemaker:region:027400017018:labeling-job-algorithm-specification\/semantic-segmentation@
newLabelingJobAlgorithmsConfig ::
  -- | 'labelingJobAlgorithmSpecificationArn'
  Prelude.Text ->
  LabelingJobAlgorithmsConfig
newLabelingJobAlgorithmsConfig
  pLabelingJobAlgorithmSpecificationArn_ =
    LabelingJobAlgorithmsConfig'
      { initialActiveLearningModelArn =
          Prelude.Nothing,
        labelingJobResourceConfig = Prelude.Nothing,
        labelingJobAlgorithmSpecificationArn =
          pLabelingJobAlgorithmSpecificationArn_
      }

-- | At the end of an auto-label job Ground Truth sends the Amazon Resource
-- Name (ARN) of the final model used for auto-labeling. You can use this
-- model as the starting point for subsequent similar jobs by providing the
-- ARN of the model here.
labelingJobAlgorithmsConfig_initialActiveLearningModelArn :: Lens.Lens' LabelingJobAlgorithmsConfig (Prelude.Maybe Prelude.Text)
labelingJobAlgorithmsConfig_initialActiveLearningModelArn = Lens.lens (\LabelingJobAlgorithmsConfig' {initialActiveLearningModelArn} -> initialActiveLearningModelArn) (\s@LabelingJobAlgorithmsConfig' {} a -> s {initialActiveLearningModelArn = a} :: LabelingJobAlgorithmsConfig)

-- | Provides configuration information for a labeling job.
labelingJobAlgorithmsConfig_labelingJobResourceConfig :: Lens.Lens' LabelingJobAlgorithmsConfig (Prelude.Maybe LabelingJobResourceConfig)
labelingJobAlgorithmsConfig_labelingJobResourceConfig = Lens.lens (\LabelingJobAlgorithmsConfig' {labelingJobResourceConfig} -> labelingJobResourceConfig) (\s@LabelingJobAlgorithmsConfig' {} a -> s {labelingJobResourceConfig = a} :: LabelingJobAlgorithmsConfig)

-- | Specifies the Amazon Resource Name (ARN) of the algorithm used for
-- auto-labeling. You must select one of the following ARNs:
--
-- -   /Image classification/
--
--     @arn:aws:sagemaker:region:027400017018:labeling-job-algorithm-specification\/image-classification@
--
-- -   /Text classification/
--
--     @arn:aws:sagemaker:region:027400017018:labeling-job-algorithm-specification\/text-classification@
--
-- -   /Object detection/
--
--     @arn:aws:sagemaker:region:027400017018:labeling-job-algorithm-specification\/object-detection@
--
-- -   /Semantic Segmentation/
--
--     @arn:aws:sagemaker:region:027400017018:labeling-job-algorithm-specification\/semantic-segmentation@
labelingJobAlgorithmsConfig_labelingJobAlgorithmSpecificationArn :: Lens.Lens' LabelingJobAlgorithmsConfig Prelude.Text
labelingJobAlgorithmsConfig_labelingJobAlgorithmSpecificationArn = Lens.lens (\LabelingJobAlgorithmsConfig' {labelingJobAlgorithmSpecificationArn} -> labelingJobAlgorithmSpecificationArn) (\s@LabelingJobAlgorithmsConfig' {} a -> s {labelingJobAlgorithmSpecificationArn = a} :: LabelingJobAlgorithmsConfig)

instance Prelude.FromJSON LabelingJobAlgorithmsConfig where
  parseJSON =
    Prelude.withObject
      "LabelingJobAlgorithmsConfig"
      ( \x ->
          LabelingJobAlgorithmsConfig'
            Prelude.<$> (x Prelude..:? "InitialActiveLearningModelArn")
            Prelude.<*> (x Prelude..:? "LabelingJobResourceConfig")
            Prelude.<*> ( x
                            Prelude..: "LabelingJobAlgorithmSpecificationArn"
                        )
      )

instance Prelude.Hashable LabelingJobAlgorithmsConfig

instance Prelude.NFData LabelingJobAlgorithmsConfig

instance Prelude.ToJSON LabelingJobAlgorithmsConfig where
  toJSON LabelingJobAlgorithmsConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("InitialActiveLearningModelArn" Prelude..=)
              Prelude.<$> initialActiveLearningModelArn,
            ("LabelingJobResourceConfig" Prelude..=)
              Prelude.<$> labelingJobResourceConfig,
            Prelude.Just
              ( "LabelingJobAlgorithmSpecificationArn"
                  Prelude..= labelingJobAlgorithmSpecificationArn
              )
          ]
      )
