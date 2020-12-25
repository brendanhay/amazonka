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
    tjsfTrainingJobName,
    tjsfTrainingJobArn,
    tjsfCreationTime,
    tjsfTrainingJobStatus,
    tjsfLastModifiedTime,
    tjsfTrainingEndTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.TrainingJobArn as Types
import qualified Network.AWS.SageMaker.Types.TrainingJobName as Types
import qualified Network.AWS.SageMaker.Types.TrainingJobStatus as Types

-- | Provides summary information about a training job.
--
-- /See:/ 'mkTrainingJobSummary' smart constructor.
data TrainingJobSummary = TrainingJobSummary'
  { -- | The name of the training job that you want a summary for.
    trainingJobName :: Types.TrainingJobName,
    -- | The Amazon Resource Name (ARN) of the training job.
    trainingJobArn :: Types.TrainingJobArn,
    -- | A timestamp that shows when the training job was created.
    creationTime :: Core.NominalDiffTime,
    -- | The status of the training job.
    trainingJobStatus :: Types.TrainingJobStatus,
    -- | Timestamp when the training job was last modified.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | A timestamp that shows when the training job ended. This field is set only if the training job has one of the terminal statuses (@Completed@ , @Failed@ , or @Stopped@ ).
    trainingEndTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TrainingJobSummary' value with any optional fields omitted.
mkTrainingJobSummary ::
  -- | 'trainingJobName'
  Types.TrainingJobName ->
  -- | 'trainingJobArn'
  Types.TrainingJobArn ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'trainingJobStatus'
  Types.TrainingJobStatus ->
  TrainingJobSummary
mkTrainingJobSummary
  trainingJobName
  trainingJobArn
  creationTime
  trainingJobStatus =
    TrainingJobSummary'
      { trainingJobName,
        trainingJobArn,
        creationTime,
        trainingJobStatus,
        lastModifiedTime = Core.Nothing,
        trainingEndTime = Core.Nothing
      }

-- | The name of the training job that you want a summary for.
--
-- /Note:/ Consider using 'trainingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsfTrainingJobName :: Lens.Lens' TrainingJobSummary Types.TrainingJobName
tjsfTrainingJobName = Lens.field @"trainingJobName"
{-# DEPRECATED tjsfTrainingJobName "Use generic-lens or generic-optics with 'trainingJobName' instead." #-}

-- | The Amazon Resource Name (ARN) of the training job.
--
-- /Note:/ Consider using 'trainingJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsfTrainingJobArn :: Lens.Lens' TrainingJobSummary Types.TrainingJobArn
tjsfTrainingJobArn = Lens.field @"trainingJobArn"
{-# DEPRECATED tjsfTrainingJobArn "Use generic-lens or generic-optics with 'trainingJobArn' instead." #-}

-- | A timestamp that shows when the training job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsfCreationTime :: Lens.Lens' TrainingJobSummary Core.NominalDiffTime
tjsfCreationTime = Lens.field @"creationTime"
{-# DEPRECATED tjsfCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status of the training job.
--
-- /Note:/ Consider using 'trainingJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsfTrainingJobStatus :: Lens.Lens' TrainingJobSummary Types.TrainingJobStatus
tjsfTrainingJobStatus = Lens.field @"trainingJobStatus"
{-# DEPRECATED tjsfTrainingJobStatus "Use generic-lens or generic-optics with 'trainingJobStatus' instead." #-}

-- | Timestamp when the training job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsfLastModifiedTime :: Lens.Lens' TrainingJobSummary (Core.Maybe Core.NominalDiffTime)
tjsfLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED tjsfLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | A timestamp that shows when the training job ended. This field is set only if the training job has one of the terminal statuses (@Completed@ , @Failed@ , or @Stopped@ ).
--
-- /Note:/ Consider using 'trainingEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsfTrainingEndTime :: Lens.Lens' TrainingJobSummary (Core.Maybe Core.NominalDiffTime)
tjsfTrainingEndTime = Lens.field @"trainingEndTime"
{-# DEPRECATED tjsfTrainingEndTime "Use generic-lens or generic-optics with 'trainingEndTime' instead." #-}

instance Core.FromJSON TrainingJobSummary where
  parseJSON =
    Core.withObject "TrainingJobSummary" Core.$
      \x ->
        TrainingJobSummary'
          Core.<$> (x Core..: "TrainingJobName")
          Core.<*> (x Core..: "TrainingJobArn")
          Core.<*> (x Core..: "CreationTime")
          Core.<*> (x Core..: "TrainingJobStatus")
          Core.<*> (x Core..:? "LastModifiedTime")
          Core.<*> (x Core..:? "TrainingEndTime")
