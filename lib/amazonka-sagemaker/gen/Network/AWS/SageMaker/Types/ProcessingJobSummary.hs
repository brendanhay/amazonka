{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingJobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingJobSummary
  ( ProcessingJobSummary (..),

    -- * Smart constructor
    mkProcessingJobSummary,

    -- * Lenses
    pjsCreationTime,
    pjsFailureReason,
    pjsProcessingJobName,
    pjsLastModifiedTime,
    pjsProcessingJobStatus,
    pjsExitMessage,
    pjsProcessingEndTime,
    pjsProcessingJobARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ProcessingJobStatus

-- | Summary of information about a processing job.
--
-- /See:/ 'mkProcessingJobSummary' smart constructor.
data ProcessingJobSummary = ProcessingJobSummary'
  { -- | The time at which the processing job was created.
    creationTime :: Lude.Timestamp,
    -- | A string, up to one KB in size, that contains the reason a processing job failed, if it failed.
    failureReason :: Lude.Maybe Lude.Text,
    -- | The name of the processing job.
    processingJobName :: Lude.Text,
    -- | A timestamp that indicates the last time the processing job was modified.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | The status of the processing job.
    processingJobStatus :: ProcessingJobStatus,
    -- | An optional string, up to one KB in size, that contains metadata from the processing container when the processing job exits.
    exitMessage :: Lude.Maybe Lude.Text,
    -- | The time at which the processing job completed.
    processingEndTime :: Lude.Maybe Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of the processing job..
    processingJobARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProcessingJobSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time at which the processing job was created.
-- * 'failureReason' - A string, up to one KB in size, that contains the reason a processing job failed, if it failed.
-- * 'processingJobName' - The name of the processing job.
-- * 'lastModifiedTime' - A timestamp that indicates the last time the processing job was modified.
-- * 'processingJobStatus' - The status of the processing job.
-- * 'exitMessage' - An optional string, up to one KB in size, that contains metadata from the processing container when the processing job exits.
-- * 'processingEndTime' - The time at which the processing job completed.
-- * 'processingJobARN' - The Amazon Resource Name (ARN) of the processing job..
mkProcessingJobSummary ::
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'processingJobName'
  Lude.Text ->
  -- | 'processingJobStatus'
  ProcessingJobStatus ->
  -- | 'processingJobARN'
  Lude.Text ->
  ProcessingJobSummary
mkProcessingJobSummary
  pCreationTime_
  pProcessingJobName_
  pProcessingJobStatus_
  pProcessingJobARN_ =
    ProcessingJobSummary'
      { creationTime = pCreationTime_,
        failureReason = Lude.Nothing,
        processingJobName = pProcessingJobName_,
        lastModifiedTime = Lude.Nothing,
        processingJobStatus = pProcessingJobStatus_,
        exitMessage = Lude.Nothing,
        processingEndTime = Lude.Nothing,
        processingJobARN = pProcessingJobARN_
      }

-- | The time at which the processing job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjsCreationTime :: Lens.Lens' ProcessingJobSummary Lude.Timestamp
pjsCreationTime = Lens.lens (creationTime :: ProcessingJobSummary -> Lude.Timestamp) (\s a -> s {creationTime = a} :: ProcessingJobSummary)
{-# DEPRECATED pjsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A string, up to one KB in size, that contains the reason a processing job failed, if it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjsFailureReason :: Lens.Lens' ProcessingJobSummary (Lude.Maybe Lude.Text)
pjsFailureReason = Lens.lens (failureReason :: ProcessingJobSummary -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: ProcessingJobSummary)
{-# DEPRECATED pjsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The name of the processing job.
--
-- /Note:/ Consider using 'processingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjsProcessingJobName :: Lens.Lens' ProcessingJobSummary Lude.Text
pjsProcessingJobName = Lens.lens (processingJobName :: ProcessingJobSummary -> Lude.Text) (\s a -> s {processingJobName = a} :: ProcessingJobSummary)
{-# DEPRECATED pjsProcessingJobName "Use generic-lens or generic-optics with 'processingJobName' instead." #-}

-- | A timestamp that indicates the last time the processing job was modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjsLastModifiedTime :: Lens.Lens' ProcessingJobSummary (Lude.Maybe Lude.Timestamp)
pjsLastModifiedTime = Lens.lens (lastModifiedTime :: ProcessingJobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: ProcessingJobSummary)
{-# DEPRECATED pjsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The status of the processing job.
--
-- /Note:/ Consider using 'processingJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjsProcessingJobStatus :: Lens.Lens' ProcessingJobSummary ProcessingJobStatus
pjsProcessingJobStatus = Lens.lens (processingJobStatus :: ProcessingJobSummary -> ProcessingJobStatus) (\s a -> s {processingJobStatus = a} :: ProcessingJobSummary)
{-# DEPRECATED pjsProcessingJobStatus "Use generic-lens or generic-optics with 'processingJobStatus' instead." #-}

-- | An optional string, up to one KB in size, that contains metadata from the processing container when the processing job exits.
--
-- /Note:/ Consider using 'exitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjsExitMessage :: Lens.Lens' ProcessingJobSummary (Lude.Maybe Lude.Text)
pjsExitMessage = Lens.lens (exitMessage :: ProcessingJobSummary -> Lude.Maybe Lude.Text) (\s a -> s {exitMessage = a} :: ProcessingJobSummary)
{-# DEPRECATED pjsExitMessage "Use generic-lens or generic-optics with 'exitMessage' instead." #-}

-- | The time at which the processing job completed.
--
-- /Note:/ Consider using 'processingEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjsProcessingEndTime :: Lens.Lens' ProcessingJobSummary (Lude.Maybe Lude.Timestamp)
pjsProcessingEndTime = Lens.lens (processingEndTime :: ProcessingJobSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {processingEndTime = a} :: ProcessingJobSummary)
{-# DEPRECATED pjsProcessingEndTime "Use generic-lens or generic-optics with 'processingEndTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the processing job..
--
-- /Note:/ Consider using 'processingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pjsProcessingJobARN :: Lens.Lens' ProcessingJobSummary Lude.Text
pjsProcessingJobARN = Lens.lens (processingJobARN :: ProcessingJobSummary -> Lude.Text) (\s a -> s {processingJobARN = a} :: ProcessingJobSummary)
{-# DEPRECATED pjsProcessingJobARN "Use generic-lens or generic-optics with 'processingJobARN' instead." #-}

instance Lude.FromJSON ProcessingJobSummary where
  parseJSON =
    Lude.withObject
      "ProcessingJobSummary"
      ( \x ->
          ProcessingJobSummary'
            Lude.<$> (x Lude..: "CreationTime")
            Lude.<*> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..: "ProcessingJobName")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..: "ProcessingJobStatus")
            Lude.<*> (x Lude..:? "ExitMessage")
            Lude.<*> (x Lude..:? "ProcessingEndTime")
            Lude.<*> (x Lude..: "ProcessingJobArn")
      )
