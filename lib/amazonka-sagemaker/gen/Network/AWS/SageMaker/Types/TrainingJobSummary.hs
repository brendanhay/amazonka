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
    tjsfCreationTime,
    tjsfTrainingEndTime,
    tjsfTrainingJobStatus,
    tjsfLastModifiedTime,
    tjsfTrainingJobARN,
    tjsfTrainingJobName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.TrainingJobStatus

-- | Provides summary information about a training job.
--
-- /See:/ 'mkTrainingJobSummary' smart constructor.
data TrainingJobSummary = TrainingJobSummary'
  { -- | A timestamp that shows when the training job was created.
    creationTime :: Lude.Timestamp,
    -- | A timestamp that shows when the training job ended. This field is set only if the training job has one of the terminal statuses (@Completed@ , @Failed@ , or @Stopped@ ).
    trainingEndTime :: Lude.Maybe Lude.Timestamp,
    -- | The status of the training job.
    trainingJobStatus :: TrainingJobStatus,
    -- | Timestamp when the training job was last modified.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobARN :: Lude.Text,
    -- | The name of the training job that you want a summary for.
    trainingJobName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrainingJobSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp that shows when the training job was created.
-- * 'trainingEndTime' - A timestamp that shows when the training job ended. This field is set only if the training job has one of the terminal statuses (@Completed@ , @Failed@ , or @Stopped@ ).
-- * 'trainingJobStatus' - The status of the training job.
-- * 'lastModifiedTime' - Timestamp when the training job was last modified.
-- * 'trainingJobARN' - The Amazon Resource Name (ARN) of the training job.
-- * 'trainingJobName' - The name of the training job that you want a summary for.
mkTrainingJobSummary ::
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'trainingJobStatus'
  TrainingJobStatus ->
  -- | 'trainingJobARN'
  Lude.Text ->
  -- | 'trainingJobName'
  Lude.Text ->
  TrainingJobSummary
mkTrainingJobSummary
  pCreationTime_
  pTrainingJobStatus_
  pTrainingJobARN_
  pTrainingJobName_ =
    TrainingJobSummary'
      { creationTime = pCreationTime_,
        trainingEndTime = Lude.Nothing,
        trainingJobStatus = pTrainingJobStatus_,
        lastModifiedTime = Lude.Nothing,
        trainingJobARN = pTrainingJobARN_,
        trainingJobName = pTrainingJobName_
      }

-- | A timestamp that shows when the training job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsfCreationTime :: Lens.Lens' TrainingJobSummary Lude.Timestamp
tjsfCreationTime = Lens.lens (creationTime :: TrainingJobSummary -> Lude.Timestamp) (\s a -> s {creationTime = a} :: TrainingJobSummary)
{-# DEPRECATED tjsfCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A timestamp that shows when the training job ended. This field is set only if the training job has one of the terminal statuses (@Completed@ , @Failed@ , or @Stopped@ ).
--
-- /Note:/ Consider using 'trainingEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsfTrainingEndTime :: Lens.Lens' TrainingJobSummary (Lude.Maybe Lude.Timestamp)
tjsfTrainingEndTime = Lens.lens (trainingEndTime :: TrainingJobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {trainingEndTime = a} :: TrainingJobSummary)
{-# DEPRECATED tjsfTrainingEndTime "Use generic-lens or generic-optics with 'trainingEndTime' instead." #-}

-- | The status of the training job.
--
-- /Note:/ Consider using 'trainingJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsfTrainingJobStatus :: Lens.Lens' TrainingJobSummary TrainingJobStatus
tjsfTrainingJobStatus = Lens.lens (trainingJobStatus :: TrainingJobSummary -> TrainingJobStatus) (\s a -> s {trainingJobStatus = a} :: TrainingJobSummary)
{-# DEPRECATED tjsfTrainingJobStatus "Use generic-lens or generic-optics with 'trainingJobStatus' instead." #-}

-- | Timestamp when the training job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsfLastModifiedTime :: Lens.Lens' TrainingJobSummary (Lude.Maybe Lude.Timestamp)
tjsfLastModifiedTime = Lens.lens (lastModifiedTime :: TrainingJobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: TrainingJobSummary)
{-# DEPRECATED tjsfLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the training job.
--
-- /Note:/ Consider using 'trainingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsfTrainingJobARN :: Lens.Lens' TrainingJobSummary Lude.Text
tjsfTrainingJobARN = Lens.lens (trainingJobARN :: TrainingJobSummary -> Lude.Text) (\s a -> s {trainingJobARN = a} :: TrainingJobSummary)
{-# DEPRECATED tjsfTrainingJobARN "Use generic-lens or generic-optics with 'trainingJobARN' instead." #-}

-- | The name of the training job that you want a summary for.
--
-- /Note:/ Consider using 'trainingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsfTrainingJobName :: Lens.Lens' TrainingJobSummary Lude.Text
tjsfTrainingJobName = Lens.lens (trainingJobName :: TrainingJobSummary -> Lude.Text) (\s a -> s {trainingJobName = a} :: TrainingJobSummary)
{-# DEPRECATED tjsfTrainingJobName "Use generic-lens or generic-optics with 'trainingJobName' instead." #-}

instance Lude.FromJSON TrainingJobSummary where
  parseJSON =
    Lude.withObject
      "TrainingJobSummary"
      ( \x ->
          TrainingJobSummary'
            Lude.<$> (x Lude..: "CreationTime")
            Lude.<*> (x Lude..:? "TrainingEndTime")
            Lude.<*> (x Lude..: "TrainingJobStatus")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..: "TrainingJobArn")
            Lude.<*> (x Lude..: "TrainingJobName")
      )
