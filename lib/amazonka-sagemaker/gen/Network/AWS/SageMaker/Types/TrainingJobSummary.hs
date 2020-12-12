{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrainingJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingJobSummary
  ( TrainingJobSummary (..),

    -- * Smart constructor
    mkTrainingJobSummary,

    -- * Lenses
    tjsjTrainingEndTime,
    tjsjLastModifiedTime,
    tjsjTrainingJobName,
    tjsjTrainingJobARN,
    tjsjCreationTime,
    tjsjTrainingJobStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.TrainingJobStatus

-- | Provides summary information about a training job.
--
-- /See:/ 'mkTrainingJobSummary' smart constructor.
data TrainingJobSummary = TrainingJobSummary'
  { trainingEndTime ::
      Lude.Maybe Lude.Timestamp,
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    trainingJobName :: Lude.Text,
    trainingJobARN :: Lude.Text,
    creationTime :: Lude.Timestamp,
    trainingJobStatus :: TrainingJobStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrainingJobSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp that shows when the training job was created.
-- * 'lastModifiedTime' - Timestamp when the training job was last modified.
-- * 'trainingEndTime' - A timestamp that shows when the training job ended. This field is set only if the training job has one of the terminal statuses (@Completed@ , @Failed@ , or @Stopped@ ).
-- * 'trainingJobARN' - The Amazon Resource Name (ARN) of the training job.
-- * 'trainingJobName' - The name of the training job that you want a summary for.
-- * 'trainingJobStatus' - The status of the training job.
mkTrainingJobSummary ::
  -- | 'trainingJobName'
  Lude.Text ->
  -- | 'trainingJobARN'
  Lude.Text ->
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'trainingJobStatus'
  TrainingJobStatus ->
  TrainingJobSummary
mkTrainingJobSummary
  pTrainingJobName_
  pTrainingJobARN_
  pCreationTime_
  pTrainingJobStatus_ =
    TrainingJobSummary'
      { trainingEndTime = Lude.Nothing,
        lastModifiedTime = Lude.Nothing,
        trainingJobName = pTrainingJobName_,
        trainingJobARN = pTrainingJobARN_,
        creationTime = pCreationTime_,
        trainingJobStatus = pTrainingJobStatus_
      }

-- | A timestamp that shows when the training job ended. This field is set only if the training job has one of the terminal statuses (@Completed@ , @Failed@ , or @Stopped@ ).
--
-- /Note:/ Consider using 'trainingEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsjTrainingEndTime :: Lens.Lens' TrainingJobSummary (Lude.Maybe Lude.Timestamp)
tjsjTrainingEndTime = Lens.lens (trainingEndTime :: TrainingJobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {trainingEndTime = a} :: TrainingJobSummary)
{-# DEPRECATED tjsjTrainingEndTime "Use generic-lens or generic-optics with 'trainingEndTime' instead." #-}

-- | Timestamp when the training job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsjLastModifiedTime :: Lens.Lens' TrainingJobSummary (Lude.Maybe Lude.Timestamp)
tjsjLastModifiedTime = Lens.lens (lastModifiedTime :: TrainingJobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: TrainingJobSummary)
{-# DEPRECATED tjsjLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the training job that you want a summary for.
--
-- /Note:/ Consider using 'trainingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsjTrainingJobName :: Lens.Lens' TrainingJobSummary Lude.Text
tjsjTrainingJobName = Lens.lens (trainingJobName :: TrainingJobSummary -> Lude.Text) (\s a -> s {trainingJobName = a} :: TrainingJobSummary)
{-# DEPRECATED tjsjTrainingJobName "Use generic-lens or generic-optics with 'trainingJobName' instead." #-}

-- | The Amazon Resource Name (ARN) of the training job.
--
-- /Note:/ Consider using 'trainingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsjTrainingJobARN :: Lens.Lens' TrainingJobSummary Lude.Text
tjsjTrainingJobARN = Lens.lens (trainingJobARN :: TrainingJobSummary -> Lude.Text) (\s a -> s {trainingJobARN = a} :: TrainingJobSummary)
{-# DEPRECATED tjsjTrainingJobARN "Use generic-lens or generic-optics with 'trainingJobARN' instead." #-}

-- | A timestamp that shows when the training job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsjCreationTime :: Lens.Lens' TrainingJobSummary Lude.Timestamp
tjsjCreationTime = Lens.lens (creationTime :: TrainingJobSummary -> Lude.Timestamp) (\s a -> s {creationTime = a} :: TrainingJobSummary)
{-# DEPRECATED tjsjCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status of the training job.
--
-- /Note:/ Consider using 'trainingJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsjTrainingJobStatus :: Lens.Lens' TrainingJobSummary TrainingJobStatus
tjsjTrainingJobStatus = Lens.lens (trainingJobStatus :: TrainingJobSummary -> TrainingJobStatus) (\s a -> s {trainingJobStatus = a} :: TrainingJobSummary)
{-# DEPRECATED tjsjTrainingJobStatus "Use generic-lens or generic-optics with 'trainingJobStatus' instead." #-}

instance Lude.FromJSON TrainingJobSummary where
  parseJSON =
    Lude.withObject
      "TrainingJobSummary"
      ( \x ->
          TrainingJobSummary'
            Lude.<$> (x Lude..:? "TrainingEndTime")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..: "TrainingJobName")
            Lude.<*> (x Lude..: "TrainingJobArn")
            Lude.<*> (x Lude..: "CreationTime")
            Lude.<*> (x Lude..: "TrainingJobStatus")
      )
