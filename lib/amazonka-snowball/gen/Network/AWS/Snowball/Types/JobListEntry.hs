{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.JobListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.JobListEntry
  ( JobListEntry (..),

    -- * Smart constructor
    mkJobListEntry,

    -- * Lenses
    jleJobType,
    jleJobId,
    jleJobState,
    jleSnowballType,
    jleCreationDate,
    jleDescription,
    jleIsMaster,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Snowball.Types.JobState
import Network.AWS.Snowball.Types.JobType
import Network.AWS.Snowball.Types.SnowballType

-- | Each @JobListEntry@ object contains a job's state, a job's ID, and a value that indicates whether the job is a job part, in the case of an export job.
--
-- /See:/ 'mkJobListEntry' smart constructor.
data JobListEntry = JobListEntry'
  { jobType :: Lude.Maybe JobType,
    jobId :: Lude.Maybe Lude.Text,
    jobState :: Lude.Maybe JobState,
    snowballType :: Lude.Maybe SnowballType,
    creationDate :: Lude.Maybe Lude.Timestamp,
    description :: Lude.Maybe Lude.Text,
    isMaster :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobListEntry' with the minimum fields required to make a request.
--
-- * 'creationDate' - The creation date for this job.
-- * 'description' - The optional description of this specific job, for example @Important Photos 2016-08-11@ .
-- * 'isMaster' - A value that indicates that this job is a main job. A main job represents a successful request to create an export job. Main jobs aren't associated with any Snowballs. Instead, each main job will have at least one job part, and each job part is associated with a Snowball. It might take some time before the job parts associated with a particular main job are listed, because they are created after the main job is created.
-- * 'jobId' - The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
-- * 'jobState' - The current state of this job.
-- * 'jobType' - The type of job.
-- * 'snowballType' - The type of device used with this job.
mkJobListEntry ::
  JobListEntry
mkJobListEntry =
  JobListEntry'
    { jobType = Lude.Nothing,
      jobId = Lude.Nothing,
      jobState = Lude.Nothing,
      snowballType = Lude.Nothing,
      creationDate = Lude.Nothing,
      description = Lude.Nothing,
      isMaster = Lude.Nothing
    }

-- | The type of job.
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jleJobType :: Lens.Lens' JobListEntry (Lude.Maybe JobType)
jleJobType = Lens.lens (jobType :: JobListEntry -> Lude.Maybe JobType) (\s a -> s {jobType = a} :: JobListEntry)
{-# DEPRECATED jleJobType "Use generic-lens or generic-optics with 'jobType' instead." #-}

-- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jleJobId :: Lens.Lens' JobListEntry (Lude.Maybe Lude.Text)
jleJobId = Lens.lens (jobId :: JobListEntry -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: JobListEntry)
{-# DEPRECATED jleJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The current state of this job.
--
-- /Note:/ Consider using 'jobState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jleJobState :: Lens.Lens' JobListEntry (Lude.Maybe JobState)
jleJobState = Lens.lens (jobState :: JobListEntry -> Lude.Maybe JobState) (\s a -> s {jobState = a} :: JobListEntry)
{-# DEPRECATED jleJobState "Use generic-lens or generic-optics with 'jobState' instead." #-}

-- | The type of device used with this job.
--
-- /Note:/ Consider using 'snowballType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jleSnowballType :: Lens.Lens' JobListEntry (Lude.Maybe SnowballType)
jleSnowballType = Lens.lens (snowballType :: JobListEntry -> Lude.Maybe SnowballType) (\s a -> s {snowballType = a} :: JobListEntry)
{-# DEPRECATED jleSnowballType "Use generic-lens or generic-optics with 'snowballType' instead." #-}

-- | The creation date for this job.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jleCreationDate :: Lens.Lens' JobListEntry (Lude.Maybe Lude.Timestamp)
jleCreationDate = Lens.lens (creationDate :: JobListEntry -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: JobListEntry)
{-# DEPRECATED jleCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The optional description of this specific job, for example @Important Photos 2016-08-11@ .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jleDescription :: Lens.Lens' JobListEntry (Lude.Maybe Lude.Text)
jleDescription = Lens.lens (description :: JobListEntry -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: JobListEntry)
{-# DEPRECATED jleDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A value that indicates that this job is a main job. A main job represents a successful request to create an export job. Main jobs aren't associated with any Snowballs. Instead, each main job will have at least one job part, and each job part is associated with a Snowball. It might take some time before the job parts associated with a particular main job are listed, because they are created after the main job is created.
--
-- /Note:/ Consider using 'isMaster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jleIsMaster :: Lens.Lens' JobListEntry (Lude.Maybe Lude.Bool)
jleIsMaster = Lens.lens (isMaster :: JobListEntry -> Lude.Maybe Lude.Bool) (\s a -> s {isMaster = a} :: JobListEntry)
{-# DEPRECATED jleIsMaster "Use generic-lens or generic-optics with 'isMaster' instead." #-}

instance Lude.FromJSON JobListEntry where
  parseJSON =
    Lude.withObject
      "JobListEntry"
      ( \x ->
          JobListEntry'
            Lude.<$> (x Lude..:? "JobType")
            Lude.<*> (x Lude..:? "JobId")
            Lude.<*> (x Lude..:? "JobState")
            Lude.<*> (x Lude..:? "SnowballType")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "IsMaster")
      )
