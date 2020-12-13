{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.MailboxExportJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.MailboxExportJob
  ( MailboxExportJob (..),

    -- * Smart constructor
    mkMailboxExportJob,

    -- * Lenses
    mejState,
    mejJobId,
    mejStartTime,
    mejEstimatedProgress,
    mejEndTime,
    mejS3Path,
    mejEntityId,
    mejDescription,
    mejS3BucketName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkMail.Types.MailboxExportJobState

-- | The details of a mailbox export job, including the user or resource ID associated with the mailbox and the S3 bucket that the mailbox contents are exported to.
--
-- /See:/ 'mkMailboxExportJob' smart constructor.
data MailboxExportJob = MailboxExportJob'
  { -- | The state of the mailbox export job.
    state :: Lude.Maybe MailboxExportJobState,
    -- | The identifier of the mailbox export job.
    jobId :: Lude.Maybe Lude.Text,
    -- | The mailbox export job start timestamp.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | The estimated progress of the mailbox export job, in percentage points.
    estimatedProgress :: Lude.Maybe Lude.Natural,
    -- | The mailbox export job end timestamp.
    endTime :: Lude.Maybe Lude.Timestamp,
    -- | The path to the S3 bucket and file that the mailbox export job exports to.
    s3Path :: Lude.Maybe Lude.Text,
    -- | The identifier of the user or resource associated with the mailbox.
    entityId :: Lude.Maybe Lude.Text,
    -- | The mailbox export job description.
    description :: Lude.Maybe Lude.Text,
    -- | The name of the S3 bucket.
    s3BucketName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MailboxExportJob' with the minimum fields required to make a request.
--
-- * 'state' - The state of the mailbox export job.
-- * 'jobId' - The identifier of the mailbox export job.
-- * 'startTime' - The mailbox export job start timestamp.
-- * 'estimatedProgress' - The estimated progress of the mailbox export job, in percentage points.
-- * 'endTime' - The mailbox export job end timestamp.
-- * 's3Path' - The path to the S3 bucket and file that the mailbox export job exports to.
-- * 'entityId' - The identifier of the user or resource associated with the mailbox.
-- * 'description' - The mailbox export job description.
-- * 's3BucketName' - The name of the S3 bucket.
mkMailboxExportJob ::
  MailboxExportJob
mkMailboxExportJob =
  MailboxExportJob'
    { state = Lude.Nothing,
      jobId = Lude.Nothing,
      startTime = Lude.Nothing,
      estimatedProgress = Lude.Nothing,
      endTime = Lude.Nothing,
      s3Path = Lude.Nothing,
      entityId = Lude.Nothing,
      description = Lude.Nothing,
      s3BucketName = Lude.Nothing
    }

-- | The state of the mailbox export job.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mejState :: Lens.Lens' MailboxExportJob (Lude.Maybe MailboxExportJobState)
mejState = Lens.lens (state :: MailboxExportJob -> Lude.Maybe MailboxExportJobState) (\s a -> s {state = a} :: MailboxExportJob)
{-# DEPRECATED mejState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The identifier of the mailbox export job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mejJobId :: Lens.Lens' MailboxExportJob (Lude.Maybe Lude.Text)
mejJobId = Lens.lens (jobId :: MailboxExportJob -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: MailboxExportJob)
{-# DEPRECATED mejJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The mailbox export job start timestamp.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mejStartTime :: Lens.Lens' MailboxExportJob (Lude.Maybe Lude.Timestamp)
mejStartTime = Lens.lens (startTime :: MailboxExportJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: MailboxExportJob)
{-# DEPRECATED mejStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The estimated progress of the mailbox export job, in percentage points.
--
-- /Note:/ Consider using 'estimatedProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mejEstimatedProgress :: Lens.Lens' MailboxExportJob (Lude.Maybe Lude.Natural)
mejEstimatedProgress = Lens.lens (estimatedProgress :: MailboxExportJob -> Lude.Maybe Lude.Natural) (\s a -> s {estimatedProgress = a} :: MailboxExportJob)
{-# DEPRECATED mejEstimatedProgress "Use generic-lens or generic-optics with 'estimatedProgress' instead." #-}

-- | The mailbox export job end timestamp.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mejEndTime :: Lens.Lens' MailboxExportJob (Lude.Maybe Lude.Timestamp)
mejEndTime = Lens.lens (endTime :: MailboxExportJob -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: MailboxExportJob)
{-# DEPRECATED mejEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The path to the S3 bucket and file that the mailbox export job exports to.
--
-- /Note:/ Consider using 's3Path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mejS3Path :: Lens.Lens' MailboxExportJob (Lude.Maybe Lude.Text)
mejS3Path = Lens.lens (s3Path :: MailboxExportJob -> Lude.Maybe Lude.Text) (\s a -> s {s3Path = a} :: MailboxExportJob)
{-# DEPRECATED mejS3Path "Use generic-lens or generic-optics with 's3Path' instead." #-}

-- | The identifier of the user or resource associated with the mailbox.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mejEntityId :: Lens.Lens' MailboxExportJob (Lude.Maybe Lude.Text)
mejEntityId = Lens.lens (entityId :: MailboxExportJob -> Lude.Maybe Lude.Text) (\s a -> s {entityId = a} :: MailboxExportJob)
{-# DEPRECATED mejEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

-- | The mailbox export job description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mejDescription :: Lens.Lens' MailboxExportJob (Lude.Maybe Lude.Text)
mejDescription = Lens.lens (description :: MailboxExportJob -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: MailboxExportJob)
{-# DEPRECATED mejDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the S3 bucket.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mejS3BucketName :: Lens.Lens' MailboxExportJob (Lude.Maybe Lude.Text)
mejS3BucketName = Lens.lens (s3BucketName :: MailboxExportJob -> Lude.Maybe Lude.Text) (\s a -> s {s3BucketName = a} :: MailboxExportJob)
{-# DEPRECATED mejS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

instance Lude.FromJSON MailboxExportJob where
  parseJSON =
    Lude.withObject
      "MailboxExportJob"
      ( \x ->
          MailboxExportJob'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "JobId")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "EstimatedProgress")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "S3Path")
            Lude.<*> (x Lude..:? "EntityId")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "S3BucketName")
      )
