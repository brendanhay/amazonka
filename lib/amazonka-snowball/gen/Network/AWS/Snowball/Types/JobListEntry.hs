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
    jleCreationDate,
    jleDescription,
    jleIsMaster,
    jleJobId,
    jleJobState,
    jleJobType,
    jleSnowballType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Snowball.Types.JobState as Types
import qualified Network.AWS.Snowball.Types.JobType as Types
import qualified Network.AWS.Snowball.Types.SnowballType as Types
import qualified Network.AWS.Snowball.Types.String as Types

-- | Each @JobListEntry@ object contains a job's state, a job's ID, and a value that indicates whether the job is a job part, in the case of an export job.
--
-- /See:/ 'mkJobListEntry' smart constructor.
data JobListEntry = JobListEntry'
  { -- | The creation date for this job.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | The optional description of this specific job, for example @Important Photos 2016-08-11@ .
    description :: Core.Maybe Types.String,
    -- | A value that indicates that this job is a main job. A main job represents a successful request to create an export job. Main jobs aren't associated with any Snowballs. Instead, each main job will have at least one job part, and each job part is associated with a Snowball. It might take some time before the job parts associated with a particular main job are listed, because they are created after the main job is created.
    isMaster :: Core.Maybe Core.Bool,
    -- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
    jobId :: Core.Maybe Types.String,
    -- | The current state of this job.
    jobState :: Core.Maybe Types.JobState,
    -- | The type of job.
    jobType :: Core.Maybe Types.JobType,
    -- | The type of device used with this job.
    snowballType :: Core.Maybe Types.SnowballType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'JobListEntry' value with any optional fields omitted.
mkJobListEntry ::
  JobListEntry
mkJobListEntry =
  JobListEntry'
    { creationDate = Core.Nothing,
      description = Core.Nothing,
      isMaster = Core.Nothing,
      jobId = Core.Nothing,
      jobState = Core.Nothing,
      jobType = Core.Nothing,
      snowballType = Core.Nothing
    }

-- | The creation date for this job.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jleCreationDate :: Lens.Lens' JobListEntry (Core.Maybe Core.NominalDiffTime)
jleCreationDate = Lens.field @"creationDate"
{-# DEPRECATED jleCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The optional description of this specific job, for example @Important Photos 2016-08-11@ .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jleDescription :: Lens.Lens' JobListEntry (Core.Maybe Types.String)
jleDescription = Lens.field @"description"
{-# DEPRECATED jleDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A value that indicates that this job is a main job. A main job represents a successful request to create an export job. Main jobs aren't associated with any Snowballs. Instead, each main job will have at least one job part, and each job part is associated with a Snowball. It might take some time before the job parts associated with a particular main job are listed, because they are created after the main job is created.
--
-- /Note:/ Consider using 'isMaster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jleIsMaster :: Lens.Lens' JobListEntry (Core.Maybe Core.Bool)
jleIsMaster = Lens.field @"isMaster"
{-# DEPRECATED jleIsMaster "Use generic-lens or generic-optics with 'isMaster' instead." #-}

-- | The automatically generated ID for a job, for example @JID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jleJobId :: Lens.Lens' JobListEntry (Core.Maybe Types.String)
jleJobId = Lens.field @"jobId"
{-# DEPRECATED jleJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The current state of this job.
--
-- /Note:/ Consider using 'jobState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jleJobState :: Lens.Lens' JobListEntry (Core.Maybe Types.JobState)
jleJobState = Lens.field @"jobState"
{-# DEPRECATED jleJobState "Use generic-lens or generic-optics with 'jobState' instead." #-}

-- | The type of job.
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jleJobType :: Lens.Lens' JobListEntry (Core.Maybe Types.JobType)
jleJobType = Lens.field @"jobType"
{-# DEPRECATED jleJobType "Use generic-lens or generic-optics with 'jobType' instead." #-}

-- | The type of device used with this job.
--
-- /Note:/ Consider using 'snowballType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jleSnowballType :: Lens.Lens' JobListEntry (Core.Maybe Types.SnowballType)
jleSnowballType = Lens.field @"snowballType"
{-# DEPRECATED jleSnowballType "Use generic-lens or generic-optics with 'snowballType' instead." #-}

instance Core.FromJSON JobListEntry where
  parseJSON =
    Core.withObject "JobListEntry" Core.$
      \x ->
        JobListEntry'
          Core.<$> (x Core..:? "CreationDate")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "IsMaster")
          Core.<*> (x Core..:? "JobId")
          Core.<*> (x Core..:? "JobState")
          Core.<*> (x Core..:? "JobType")
          Core.<*> (x Core..:? "SnowballType")
