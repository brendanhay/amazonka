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
    ljoFinalActiveLearningModelARN,
    ljoOutputDatasetS3URI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the location of the output produced by the labeling job.
--
-- /See:/ 'mkLabelingJobOutput' smart constructor.
data LabelingJobOutput = LabelingJobOutput'
  { -- | The Amazon Resource Name (ARN) for the most recent Amazon SageMaker model trained as part of automated data labeling.
    finalActiveLearningModelARN :: Lude.Maybe Lude.Text,
    -- | The Amazon S3 bucket location of the manifest file for labeled data.
    outputDatasetS3URI :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LabelingJobOutput' with the minimum fields required to make a request.
--
-- * 'finalActiveLearningModelARN' - The Amazon Resource Name (ARN) for the most recent Amazon SageMaker model trained as part of automated data labeling.
-- * 'outputDatasetS3URI' - The Amazon S3 bucket location of the manifest file for labeled data.
mkLabelingJobOutput ::
  -- | 'outputDatasetS3URI'
  Lude.Text ->
  LabelingJobOutput
mkLabelingJobOutput pOutputDatasetS3URI_ =
  LabelingJobOutput'
    { finalActiveLearningModelARN = Lude.Nothing,
      outputDatasetS3URI = pOutputDatasetS3URI_
    }

-- | The Amazon Resource Name (ARN) for the most recent Amazon SageMaker model trained as part of automated data labeling.
--
-- /Note:/ Consider using 'finalActiveLearningModelARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljoFinalActiveLearningModelARN :: Lens.Lens' LabelingJobOutput (Lude.Maybe Lude.Text)
ljoFinalActiveLearningModelARN = Lens.lens (finalActiveLearningModelARN :: LabelingJobOutput -> Lude.Maybe Lude.Text) (\s a -> s {finalActiveLearningModelARN = a} :: LabelingJobOutput)
{-# DEPRECATED ljoFinalActiveLearningModelARN "Use generic-lens or generic-optics with 'finalActiveLearningModelARN' instead." #-}

-- | The Amazon S3 bucket location of the manifest file for labeled data.
--
-- /Note:/ Consider using 'outputDatasetS3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljoOutputDatasetS3URI :: Lens.Lens' LabelingJobOutput Lude.Text
ljoOutputDatasetS3URI = Lens.lens (outputDatasetS3URI :: LabelingJobOutput -> Lude.Text) (\s a -> s {outputDatasetS3URI = a} :: LabelingJobOutput)
{-# DEPRECATED ljoOutputDatasetS3URI "Use generic-lens or generic-optics with 'outputDatasetS3URI' instead." #-}

instance Lude.FromJSON LabelingJobOutput where
  parseJSON =
    Lude.withObject
      "LabelingJobOutput"
      ( \x ->
          LabelingJobOutput'
            Lude.<$> (x Lude..:? "FinalActiveLearningModelArn")
            Lude.<*> (x Lude..: "OutputDatasetS3Uri")
      )
