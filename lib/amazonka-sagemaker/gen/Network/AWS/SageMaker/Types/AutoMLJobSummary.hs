{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobSummary
  ( AutoMLJobSummary (..),

    -- * Smart constructor
    mkAutoMLJobSummary,

    -- * Lenses
    amljsCreationTime,
    amljsFailureReason,
    amljsAutoMLJobStatus,
    amljsLastModifiedTime,
    amljsAutoMLJobName,
    amljsAutoMLJobARN,
    amljsAutoMLJobSecondaryStatus,
    amljsEndTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.AutoMLJobSecondaryStatus
import Network.AWS.SageMaker.Types.AutoMLJobStatus

-- | Provides a summary about a job.
--
-- /See:/ 'mkAutoMLJobSummary' smart constructor.
data AutoMLJobSummary = AutoMLJobSummary'
  { -- | When the job was created.
    creationTime :: Lude.Timestamp,
    -- | The failure reason of a job.
    failureReason :: Lude.Maybe Lude.Text,
    -- | The job's status.
    autoMLJobStatus :: AutoMLJobStatus,
    -- | When the job was last modified.
    lastModifiedTime :: Lude.Timestamp,
    -- | The name of the object you are requesting.
    autoMLJobName :: Lude.Text,
    -- | The ARN of the job.
    autoMLJobARN :: Lude.Text,
    -- | The job's secondary status.
    autoMLJobSecondaryStatus :: AutoMLJobSecondaryStatus,
    -- | The end time of an AutoML job.
    endTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoMLJobSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - When the job was created.
-- * 'failureReason' - The failure reason of a job.
-- * 'autoMLJobStatus' - The job's status.
-- * 'lastModifiedTime' - When the job was last modified.
-- * 'autoMLJobName' - The name of the object you are requesting.
-- * 'autoMLJobARN' - The ARN of the job.
-- * 'autoMLJobSecondaryStatus' - The job's secondary status.
-- * 'endTime' - The end time of an AutoML job.
mkAutoMLJobSummary ::
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'autoMLJobStatus'
  AutoMLJobStatus ->
  -- | 'lastModifiedTime'
  Lude.Timestamp ->
  -- | 'autoMLJobName'
  Lude.Text ->
  -- | 'autoMLJobARN'
  Lude.Text ->
  -- | 'autoMLJobSecondaryStatus'
  AutoMLJobSecondaryStatus ->
  AutoMLJobSummary
mkAutoMLJobSummary
  pCreationTime_
  pAutoMLJobStatus_
  pLastModifiedTime_
  pAutoMLJobName_
  pAutoMLJobARN_
  pAutoMLJobSecondaryStatus_ =
    AutoMLJobSummary'
      { creationTime = pCreationTime_,
        failureReason = Lude.Nothing,
        autoMLJobStatus = pAutoMLJobStatus_,
        lastModifiedTime = pLastModifiedTime_,
        autoMLJobName = pAutoMLJobName_,
        autoMLJobARN = pAutoMLJobARN_,
        autoMLJobSecondaryStatus = pAutoMLJobSecondaryStatus_,
        endTime = Lude.Nothing
      }

-- | When the job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsCreationTime :: Lens.Lens' AutoMLJobSummary Lude.Timestamp
amljsCreationTime = Lens.lens (creationTime :: AutoMLJobSummary -> Lude.Timestamp) (\s a -> s {creationTime = a} :: AutoMLJobSummary)
{-# DEPRECATED amljsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The failure reason of a job.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsFailureReason :: Lens.Lens' AutoMLJobSummary (Lude.Maybe Lude.Text)
amljsFailureReason = Lens.lens (failureReason :: AutoMLJobSummary -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: AutoMLJobSummary)
{-# DEPRECATED amljsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The job's status.
--
-- /Note:/ Consider using 'autoMLJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsAutoMLJobStatus :: Lens.Lens' AutoMLJobSummary AutoMLJobStatus
amljsAutoMLJobStatus = Lens.lens (autoMLJobStatus :: AutoMLJobSummary -> AutoMLJobStatus) (\s a -> s {autoMLJobStatus = a} :: AutoMLJobSummary)
{-# DEPRECATED amljsAutoMLJobStatus "Use generic-lens or generic-optics with 'autoMLJobStatus' instead." #-}

-- | When the job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsLastModifiedTime :: Lens.Lens' AutoMLJobSummary Lude.Timestamp
amljsLastModifiedTime = Lens.lens (lastModifiedTime :: AutoMLJobSummary -> Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: AutoMLJobSummary)
{-# DEPRECATED amljsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the object you are requesting.
--
-- /Note:/ Consider using 'autoMLJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsAutoMLJobName :: Lens.Lens' AutoMLJobSummary Lude.Text
amljsAutoMLJobName = Lens.lens (autoMLJobName :: AutoMLJobSummary -> Lude.Text) (\s a -> s {autoMLJobName = a} :: AutoMLJobSummary)
{-# DEPRECATED amljsAutoMLJobName "Use generic-lens or generic-optics with 'autoMLJobName' instead." #-}

-- | The ARN of the job.
--
-- /Note:/ Consider using 'autoMLJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsAutoMLJobARN :: Lens.Lens' AutoMLJobSummary Lude.Text
amljsAutoMLJobARN = Lens.lens (autoMLJobARN :: AutoMLJobSummary -> Lude.Text) (\s a -> s {autoMLJobARN = a} :: AutoMLJobSummary)
{-# DEPRECATED amljsAutoMLJobARN "Use generic-lens or generic-optics with 'autoMLJobARN' instead." #-}

-- | The job's secondary status.
--
-- /Note:/ Consider using 'autoMLJobSecondaryStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsAutoMLJobSecondaryStatus :: Lens.Lens' AutoMLJobSummary AutoMLJobSecondaryStatus
amljsAutoMLJobSecondaryStatus = Lens.lens (autoMLJobSecondaryStatus :: AutoMLJobSummary -> AutoMLJobSecondaryStatus) (\s a -> s {autoMLJobSecondaryStatus = a} :: AutoMLJobSummary)
{-# DEPRECATED amljsAutoMLJobSecondaryStatus "Use generic-lens or generic-optics with 'autoMLJobSecondaryStatus' instead." #-}

-- | The end time of an AutoML job.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljsEndTime :: Lens.Lens' AutoMLJobSummary (Lude.Maybe Lude.Timestamp)
amljsEndTime = Lens.lens (endTime :: AutoMLJobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: AutoMLJobSummary)
{-# DEPRECATED amljsEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

instance Lude.FromJSON AutoMLJobSummary where
  parseJSON =
    Lude.withObject
      "AutoMLJobSummary"
      ( \x ->
          AutoMLJobSummary'
            Lude.<$> (x Lude..: "CreationTime")
            Lude.<*> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..: "AutoMLJobStatus")
            Lude.<*> (x Lude..: "LastModifiedTime")
            Lude.<*> (x Lude..: "AutoMLJobName")
            Lude.<*> (x Lude..: "AutoMLJobArn")
            Lude.<*> (x Lude..: "AutoMLJobSecondaryStatus")
            Lude.<*> (x Lude..:? "EndTime")
      )
