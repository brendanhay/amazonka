{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobAlgorithmsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobAlgorithmsConfig
  ( LabelingJobAlgorithmsConfig (..),

    -- * Smart constructor
    mkLabelingJobAlgorithmsConfig,

    -- * Lenses
    ljacLabelingJobAlgorithmSpecificationARN,
    ljacLabelingJobResourceConfig,
    ljacInitialActiveLearningModelARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.LabelingJobResourceConfig

-- | Provides configuration information for auto-labeling of your data objects. A @LabelingJobAlgorithmsConfig@ object must be supplied in order to use auto-labeling.
--
-- /See:/ 'mkLabelingJobAlgorithmsConfig' smart constructor.
data LabelingJobAlgorithmsConfig = LabelingJobAlgorithmsConfig'
  { -- | Specifies the Amazon Resource Name (ARN) of the algorithm used for auto-labeling. You must select one of the following ARNs:
    --
    --
    --     * /Image classification/
    -- @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/image-classification@
    --
    --
    --     * /Text classification/
    -- @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/text-classification@
    --
    --
    --     * /Object detection/
    -- @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/object-detection@
    --
    --
    --     * /Semantic Segmentation/
    -- @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/semantic-segmentation@
    labelingJobAlgorithmSpecificationARN :: Lude.Text,
    -- | Provides configuration information for a labeling job.
    labelingJobResourceConfig :: Lude.Maybe LabelingJobResourceConfig,
    -- | At the end of an auto-label job Ground Truth sends the Amazon Resource Name (ARN) of the final model used for auto-labeling. You can use this model as the starting point for subsequent similar jobs by providing the ARN of the model here.
    initialActiveLearningModelARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LabelingJobAlgorithmsConfig' with the minimum fields required to make a request.
--
-- * 'labelingJobAlgorithmSpecificationARN' - Specifies the Amazon Resource Name (ARN) of the algorithm used for auto-labeling. You must select one of the following ARNs:
--
--
--     * /Image classification/
-- @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/image-classification@
--
--
--     * /Text classification/
-- @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/text-classification@
--
--
--     * /Object detection/
-- @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/object-detection@
--
--
--     * /Semantic Segmentation/
-- @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/semantic-segmentation@
--
--
-- * 'labelingJobResourceConfig' - Provides configuration information for a labeling job.
-- * 'initialActiveLearningModelARN' - At the end of an auto-label job Ground Truth sends the Amazon Resource Name (ARN) of the final model used for auto-labeling. You can use this model as the starting point for subsequent similar jobs by providing the ARN of the model here.
mkLabelingJobAlgorithmsConfig ::
  -- | 'labelingJobAlgorithmSpecificationARN'
  Lude.Text ->
  LabelingJobAlgorithmsConfig
mkLabelingJobAlgorithmsConfig
  pLabelingJobAlgorithmSpecificationARN_ =
    LabelingJobAlgorithmsConfig'
      { labelingJobAlgorithmSpecificationARN =
          pLabelingJobAlgorithmSpecificationARN_,
        labelingJobResourceConfig = Lude.Nothing,
        initialActiveLearningModelARN = Lude.Nothing
      }

-- | Specifies the Amazon Resource Name (ARN) of the algorithm used for auto-labeling. You must select one of the following ARNs:
--
--
--     * /Image classification/
-- @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/image-classification@
--
--
--     * /Text classification/
-- @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/text-classification@
--
--
--     * /Object detection/
-- @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/object-detection@
--
--
--     * /Semantic Segmentation/
-- @arn:aws:sagemaker:/region/ :027400017018:labeling-job-algorithm-specification/semantic-segmentation@
--
--
--
-- /Note:/ Consider using 'labelingJobAlgorithmSpecificationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljacLabelingJobAlgorithmSpecificationARN :: Lens.Lens' LabelingJobAlgorithmsConfig Lude.Text
ljacLabelingJobAlgorithmSpecificationARN = Lens.lens (labelingJobAlgorithmSpecificationARN :: LabelingJobAlgorithmsConfig -> Lude.Text) (\s a -> s {labelingJobAlgorithmSpecificationARN = a} :: LabelingJobAlgorithmsConfig)
{-# DEPRECATED ljacLabelingJobAlgorithmSpecificationARN "Use generic-lens or generic-optics with 'labelingJobAlgorithmSpecificationARN' instead." #-}

-- | Provides configuration information for a labeling job.
--
-- /Note:/ Consider using 'labelingJobResourceConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljacLabelingJobResourceConfig :: Lens.Lens' LabelingJobAlgorithmsConfig (Lude.Maybe LabelingJobResourceConfig)
ljacLabelingJobResourceConfig = Lens.lens (labelingJobResourceConfig :: LabelingJobAlgorithmsConfig -> Lude.Maybe LabelingJobResourceConfig) (\s a -> s {labelingJobResourceConfig = a} :: LabelingJobAlgorithmsConfig)
{-# DEPRECATED ljacLabelingJobResourceConfig "Use generic-lens or generic-optics with 'labelingJobResourceConfig' instead." #-}

-- | At the end of an auto-label job Ground Truth sends the Amazon Resource Name (ARN) of the final model used for auto-labeling. You can use this model as the starting point for subsequent similar jobs by providing the ARN of the model here.
--
-- /Note:/ Consider using 'initialActiveLearningModelARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljacInitialActiveLearningModelARN :: Lens.Lens' LabelingJobAlgorithmsConfig (Lude.Maybe Lude.Text)
ljacInitialActiveLearningModelARN = Lens.lens (initialActiveLearningModelARN :: LabelingJobAlgorithmsConfig -> Lude.Maybe Lude.Text) (\s a -> s {initialActiveLearningModelARN = a} :: LabelingJobAlgorithmsConfig)
{-# DEPRECATED ljacInitialActiveLearningModelARN "Use generic-lens or generic-optics with 'initialActiveLearningModelARN' instead." #-}

instance Lude.FromJSON LabelingJobAlgorithmsConfig where
  parseJSON =
    Lude.withObject
      "LabelingJobAlgorithmsConfig"
      ( \x ->
          LabelingJobAlgorithmsConfig'
            Lude.<$> (x Lude..: "LabelingJobAlgorithmSpecificationArn")
            Lude.<*> (x Lude..:? "LabelingJobResourceConfig")
            Lude.<*> (x Lude..:? "InitialActiveLearningModelArn")
      )

instance Lude.ToJSON LabelingJobAlgorithmsConfig where
  toJSON LabelingJobAlgorithmsConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "LabelingJobAlgorithmSpecificationArn"
                  Lude..= labelingJobAlgorithmSpecificationARN
              ),
            ("LabelingJobResourceConfig" Lude..=)
              Lude.<$> labelingJobResourceConfig,
            ("InitialActiveLearningModelArn" Lude..=)
              Lude.<$> initialActiveLearningModelARN
          ]
      )
