-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentSourceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentSourceDetail
  ( TrialComponentSourceDetail (..),

    -- * Smart constructor
    mkTrialComponentSourceDetail,

    -- * Lenses
    tcsdTrainingJob,
    tcsdSourceARN,
    tcsdProcessingJob,
    tcsdTransformJob,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ProcessingJob
import Network.AWS.SageMaker.Types.TrainingJob
import Network.AWS.SageMaker.Types.TransformJob

-- | Detailed information about the source of a trial component. Either @ProcessingJob@ or @TrainingJob@ is returned.
--
-- /See:/ 'mkTrialComponentSourceDetail' smart constructor.
data TrialComponentSourceDetail = TrialComponentSourceDetail'
  { trainingJob ::
      Lude.Maybe TrainingJob,
    sourceARN :: Lude.Maybe Lude.Text,
    processingJob ::
      Lude.Maybe ProcessingJob,
    transformJob ::
      Lude.Maybe TransformJob
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrialComponentSourceDetail' with the minimum fields required to make a request.
--
-- * 'processingJob' - Information about a processing job that's the source of a trial component.
-- * 'sourceARN' - The Amazon Resource Name (ARN) of the source.
-- * 'trainingJob' - Information about a training job that's the source of a trial component.
-- * 'transformJob' - Information about a transform job that's the source of a trial component.
mkTrialComponentSourceDetail ::
  TrialComponentSourceDetail
mkTrialComponentSourceDetail =
  TrialComponentSourceDetail'
    { trainingJob = Lude.Nothing,
      sourceARN = Lude.Nothing,
      processingJob = Lude.Nothing,
      transformJob = Lude.Nothing
    }

-- | Information about a training job that's the source of a trial component.
--
-- /Note:/ Consider using 'trainingJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsdTrainingJob :: Lens.Lens' TrialComponentSourceDetail (Lude.Maybe TrainingJob)
tcsdTrainingJob = Lens.lens (trainingJob :: TrialComponentSourceDetail -> Lude.Maybe TrainingJob) (\s a -> s {trainingJob = a} :: TrialComponentSourceDetail)
{-# DEPRECATED tcsdTrainingJob "Use generic-lens or generic-optics with 'trainingJob' instead." #-}

-- | The Amazon Resource Name (ARN) of the source.
--
-- /Note:/ Consider using 'sourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsdSourceARN :: Lens.Lens' TrialComponentSourceDetail (Lude.Maybe Lude.Text)
tcsdSourceARN = Lens.lens (sourceARN :: TrialComponentSourceDetail -> Lude.Maybe Lude.Text) (\s a -> s {sourceARN = a} :: TrialComponentSourceDetail)
{-# DEPRECATED tcsdSourceARN "Use generic-lens or generic-optics with 'sourceARN' instead." #-}

-- | Information about a processing job that's the source of a trial component.
--
-- /Note:/ Consider using 'processingJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsdProcessingJob :: Lens.Lens' TrialComponentSourceDetail (Lude.Maybe ProcessingJob)
tcsdProcessingJob = Lens.lens (processingJob :: TrialComponentSourceDetail -> Lude.Maybe ProcessingJob) (\s a -> s {processingJob = a} :: TrialComponentSourceDetail)
{-# DEPRECATED tcsdProcessingJob "Use generic-lens or generic-optics with 'processingJob' instead." #-}

-- | Information about a transform job that's the source of a trial component.
--
-- /Note:/ Consider using 'transformJob' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsdTransformJob :: Lens.Lens' TrialComponentSourceDetail (Lude.Maybe TransformJob)
tcsdTransformJob = Lens.lens (transformJob :: TrialComponentSourceDetail -> Lude.Maybe TransformJob) (\s a -> s {transformJob = a} :: TrialComponentSourceDetail)
{-# DEPRECATED tcsdTransformJob "Use generic-lens or generic-optics with 'transformJob' instead." #-}

instance Lude.FromJSON TrialComponentSourceDetail where
  parseJSON =
    Lude.withObject
      "TrialComponentSourceDetail"
      ( \x ->
          TrialComponentSourceDetail'
            Lude.<$> (x Lude..:? "TrainingJob")
            Lude.<*> (x Lude..:? "SourceArn")
            Lude.<*> (x Lude..:? "ProcessingJob")
            Lude.<*> (x Lude..:? "TransformJob")
      )
