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
    ljacLabelingJobAlgorithmSpecificationArn,
    ljacInitialActiveLearningModelArn,
    ljacLabelingJobResourceConfig,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.InitialActiveLearningModelArn as Types
import qualified Network.AWS.SageMaker.Types.LabelingJobAlgorithmSpecificationArn as Types
import qualified Network.AWS.SageMaker.Types.LabelingJobResourceConfig as Types

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
    labelingJobAlgorithmSpecificationArn :: Types.LabelingJobAlgorithmSpecificationArn,
    -- | At the end of an auto-label job Ground Truth sends the Amazon Resource Name (ARN) of the final model used for auto-labeling. You can use this model as the starting point for subsequent similar jobs by providing the ARN of the model here.
    initialActiveLearningModelArn :: Core.Maybe Types.InitialActiveLearningModelArn,
    -- | Provides configuration information for a labeling job.
    labelingJobResourceConfig :: Core.Maybe Types.LabelingJobResourceConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LabelingJobAlgorithmsConfig' value with any optional fields omitted.
mkLabelingJobAlgorithmsConfig ::
  -- | 'labelingJobAlgorithmSpecificationArn'
  Types.LabelingJobAlgorithmSpecificationArn ->
  LabelingJobAlgorithmsConfig
mkLabelingJobAlgorithmsConfig labelingJobAlgorithmSpecificationArn =
  LabelingJobAlgorithmsConfig'
    { labelingJobAlgorithmSpecificationArn,
      initialActiveLearningModelArn = Core.Nothing,
      labelingJobResourceConfig = Core.Nothing
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
-- /Note:/ Consider using 'labelingJobAlgorithmSpecificationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljacLabelingJobAlgorithmSpecificationArn :: Lens.Lens' LabelingJobAlgorithmsConfig Types.LabelingJobAlgorithmSpecificationArn
ljacLabelingJobAlgorithmSpecificationArn = Lens.field @"labelingJobAlgorithmSpecificationArn"
{-# DEPRECATED ljacLabelingJobAlgorithmSpecificationArn "Use generic-lens or generic-optics with 'labelingJobAlgorithmSpecificationArn' instead." #-}

-- | At the end of an auto-label job Ground Truth sends the Amazon Resource Name (ARN) of the final model used for auto-labeling. You can use this model as the starting point for subsequent similar jobs by providing the ARN of the model here.
--
-- /Note:/ Consider using 'initialActiveLearningModelArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljacInitialActiveLearningModelArn :: Lens.Lens' LabelingJobAlgorithmsConfig (Core.Maybe Types.InitialActiveLearningModelArn)
ljacInitialActiveLearningModelArn = Lens.field @"initialActiveLearningModelArn"
{-# DEPRECATED ljacInitialActiveLearningModelArn "Use generic-lens or generic-optics with 'initialActiveLearningModelArn' instead." #-}

-- | Provides configuration information for a labeling job.
--
-- /Note:/ Consider using 'labelingJobResourceConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljacLabelingJobResourceConfig :: Lens.Lens' LabelingJobAlgorithmsConfig (Core.Maybe Types.LabelingJobResourceConfig)
ljacLabelingJobResourceConfig = Lens.field @"labelingJobResourceConfig"
{-# DEPRECATED ljacLabelingJobResourceConfig "Use generic-lens or generic-optics with 'labelingJobResourceConfig' instead." #-}

instance Core.FromJSON LabelingJobAlgorithmsConfig where
  toJSON LabelingJobAlgorithmsConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "LabelingJobAlgorithmSpecificationArn"
                  Core..= labelingJobAlgorithmSpecificationArn
              ),
            ("InitialActiveLearningModelArn" Core..=)
              Core.<$> initialActiveLearningModelArn,
            ("LabelingJobResourceConfig" Core..=)
              Core.<$> labelingJobResourceConfig
          ]
      )

instance Core.FromJSON LabelingJobAlgorithmsConfig where
  parseJSON =
    Core.withObject "LabelingJobAlgorithmsConfig" Core.$
      \x ->
        LabelingJobAlgorithmsConfig'
          Core.<$> (x Core..: "LabelingJobAlgorithmSpecificationArn")
          Core.<*> (x Core..:? "InitialActiveLearningModelArn")
          Core.<*> (x Core..:? "LabelingJobResourceConfig")
