{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobOutput
  ( LabelingJobOutput (..),

    -- * Smart constructor
    mkLabelingJobOutput,

    -- * Lenses
    ljoOutputDatasetS3Uri,
    ljoFinalActiveLearningModelArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ModelArn as Types
import qualified Network.AWS.SageMaker.Types.S3Uri as Types

-- | Specifies the location of the output produced by the labeling job.
--
-- /See:/ 'mkLabelingJobOutput' smart constructor.
data LabelingJobOutput = LabelingJobOutput'
  { -- | The Amazon S3 bucket location of the manifest file for labeled data.
    outputDatasetS3Uri :: Types.S3Uri,
    -- | The Amazon Resource Name (ARN) for the most recent Amazon SageMaker model trained as part of automated data labeling.
    finalActiveLearningModelArn :: Core.Maybe Types.ModelArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LabelingJobOutput' value with any optional fields omitted.
mkLabelingJobOutput ::
  -- | 'outputDatasetS3Uri'
  Types.S3Uri ->
  LabelingJobOutput
mkLabelingJobOutput outputDatasetS3Uri =
  LabelingJobOutput'
    { outputDatasetS3Uri,
      finalActiveLearningModelArn = Core.Nothing
    }

-- | The Amazon S3 bucket location of the manifest file for labeled data.
--
-- /Note:/ Consider using 'outputDatasetS3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljoOutputDatasetS3Uri :: Lens.Lens' LabelingJobOutput Types.S3Uri
ljoOutputDatasetS3Uri = Lens.field @"outputDatasetS3Uri"
{-# DEPRECATED ljoOutputDatasetS3Uri "Use generic-lens or generic-optics with 'outputDatasetS3Uri' instead." #-}

-- | The Amazon Resource Name (ARN) for the most recent Amazon SageMaker model trained as part of automated data labeling.
--
-- /Note:/ Consider using 'finalActiveLearningModelArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljoFinalActiveLearningModelArn :: Lens.Lens' LabelingJobOutput (Core.Maybe Types.ModelArn)
ljoFinalActiveLearningModelArn = Lens.field @"finalActiveLearningModelArn"
{-# DEPRECATED ljoFinalActiveLearningModelArn "Use generic-lens or generic-optics with 'finalActiveLearningModelArn' instead." #-}

instance Core.FromJSON LabelingJobOutput where
  parseJSON =
    Core.withObject "LabelingJobOutput" Core.$
      \x ->
        LabelingJobOutput'
          Core.<$> (x Core..: "OutputDatasetS3Uri")
          Core.<*> (x Core..:? "FinalActiveLearningModelArn")
