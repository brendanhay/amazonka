{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformJobSummary
  ( TransformJobSummary (..),

    -- * Smart constructor
    mkTransformJobSummary,

    -- * Lenses
    tjsCreationTime,
    tjsTransformJobName,
    tjsFailureReason,
    tjsLastModifiedTime,
    tjsTransformEndTime,
    tjsTransformJobStatus,
    tjsTransformJobARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.TransformJobStatus

-- | Provides a summary of a transform job. Multiple @TransformJobSummary@ objects are returned as a list after in response to a 'ListTransformJobs' call.
--
-- /See:/ 'mkTransformJobSummary' smart constructor.
data TransformJobSummary = TransformJobSummary'
  { -- | A timestamp that shows when the transform Job was created.
    creationTime :: Lude.Timestamp,
    -- | The name of the transform job.
    transformJobName :: Lude.Text,
    -- | If the transform job failed, the reason it failed.
    failureReason :: Lude.Maybe Lude.Text,
    -- | Indicates when the transform job was last modified.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | Indicates when the transform job ends on compute instances. For successful jobs and stopped jobs, this is the exact time recorded after the results are uploaded. For failed jobs, this is when Amazon SageMaker detected that the job failed.
    transformEndTime :: Lude.Maybe Lude.Timestamp,
    -- | The status of the transform job.
    transformJobStatus :: TransformJobStatus,
    -- | The Amazon Resource Name (ARN) of the transform job.
    transformJobARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransformJobSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp that shows when the transform Job was created.
-- * 'transformJobName' - The name of the transform job.
-- * 'failureReason' - If the transform job failed, the reason it failed.
-- * 'lastModifiedTime' - Indicates when the transform job was last modified.
-- * 'transformEndTime' - Indicates when the transform job ends on compute instances. For successful jobs and stopped jobs, this is the exact time recorded after the results are uploaded. For failed jobs, this is when Amazon SageMaker detected that the job failed.
-- * 'transformJobStatus' - The status of the transform job.
-- * 'transformJobARN' - The Amazon Resource Name (ARN) of the transform job.
mkTransformJobSummary ::
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'transformJobName'
  Lude.Text ->
  -- | 'transformJobStatus'
  TransformJobStatus ->
  -- | 'transformJobARN'
  Lude.Text ->
  TransformJobSummary
mkTransformJobSummary
  pCreationTime_
  pTransformJobName_
  pTransformJobStatus_
  pTransformJobARN_ =
    TransformJobSummary'
      { creationTime = pCreationTime_,
        transformJobName = pTransformJobName_,
        failureReason = Lude.Nothing,
        lastModifiedTime = Lude.Nothing,
        transformEndTime = Lude.Nothing,
        transformJobStatus = pTransformJobStatus_,
        transformJobARN = pTransformJobARN_
      }

-- | A timestamp that shows when the transform Job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsCreationTime :: Lens.Lens' TransformJobSummary Lude.Timestamp
tjsCreationTime = Lens.lens (creationTime :: TransformJobSummary -> Lude.Timestamp) (\s a -> s {creationTime = a} :: TransformJobSummary)
{-# DEPRECATED tjsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The name of the transform job.
--
-- /Note:/ Consider using 'transformJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsTransformJobName :: Lens.Lens' TransformJobSummary Lude.Text
tjsTransformJobName = Lens.lens (transformJobName :: TransformJobSummary -> Lude.Text) (\s a -> s {transformJobName = a} :: TransformJobSummary)
{-# DEPRECATED tjsTransformJobName "Use generic-lens or generic-optics with 'transformJobName' instead." #-}

-- | If the transform job failed, the reason it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsFailureReason :: Lens.Lens' TransformJobSummary (Lude.Maybe Lude.Text)
tjsFailureReason = Lens.lens (failureReason :: TransformJobSummary -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: TransformJobSummary)
{-# DEPRECATED tjsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | Indicates when the transform job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsLastModifiedTime :: Lens.Lens' TransformJobSummary (Lude.Maybe Lude.Timestamp)
tjsLastModifiedTime = Lens.lens (lastModifiedTime :: TransformJobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: TransformJobSummary)
{-# DEPRECATED tjsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | Indicates when the transform job ends on compute instances. For successful jobs and stopped jobs, this is the exact time recorded after the results are uploaded. For failed jobs, this is when Amazon SageMaker detected that the job failed.
--
-- /Note:/ Consider using 'transformEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsTransformEndTime :: Lens.Lens' TransformJobSummary (Lude.Maybe Lude.Timestamp)
tjsTransformEndTime = Lens.lens (transformEndTime :: TransformJobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {transformEndTime = a} :: TransformJobSummary)
{-# DEPRECATED tjsTransformEndTime "Use generic-lens or generic-optics with 'transformEndTime' instead." #-}

-- | The status of the transform job.
--
-- /Note:/ Consider using 'transformJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsTransformJobStatus :: Lens.Lens' TransformJobSummary TransformJobStatus
tjsTransformJobStatus = Lens.lens (transformJobStatus :: TransformJobSummary -> TransformJobStatus) (\s a -> s {transformJobStatus = a} :: TransformJobSummary)
{-# DEPRECATED tjsTransformJobStatus "Use generic-lens or generic-optics with 'transformJobStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the transform job.
--
-- /Note:/ Consider using 'transformJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjsTransformJobARN :: Lens.Lens' TransformJobSummary Lude.Text
tjsTransformJobARN = Lens.lens (transformJobARN :: TransformJobSummary -> Lude.Text) (\s a -> s {transformJobARN = a} :: TransformJobSummary)
{-# DEPRECATED tjsTransformJobARN "Use generic-lens or generic-optics with 'transformJobARN' instead." #-}

instance Lude.FromJSON TransformJobSummary where
  parseJSON =
    Lude.withObject
      "TransformJobSummary"
      ( \x ->
          TransformJobSummary'
            Lude.<$> (x Lude..: "CreationTime")
            Lude.<*> (x Lude..: "TransformJobName")
            Lude.<*> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "TransformEndTime")
            Lude.<*> (x Lude..: "TransformJobStatus")
            Lude.<*> (x Lude..: "TransformJobArn")
      )
