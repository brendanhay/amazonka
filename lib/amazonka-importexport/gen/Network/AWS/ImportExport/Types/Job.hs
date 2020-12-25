{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.Types.Job
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ImportExport.Types.Job
  ( Job (..),

    -- * Smart constructor
    mkJob,

    -- * Lenses
    jCreationDate,
    jIsCanceled,
    jJobId,
    jJobType,
  )
where

import qualified Network.AWS.ImportExport.Types.JobId as Types
import qualified Network.AWS.ImportExport.Types.JobType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Representation of a job returned by the ListJobs operation.
--
-- /See:/ 'mkJob' smart constructor.
data Job = Job'
  { creationDate :: Core.UTCTime,
    isCanceled :: Core.Bool,
    jobId :: Types.JobId,
    jobType :: Types.JobType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Job' value with any optional fields omitted.
mkJob ::
  -- | 'creationDate'
  Core.UTCTime ->
  -- | 'isCanceled'
  Core.Bool ->
  -- | 'jobId'
  Types.JobId ->
  -- | 'jobType'
  Types.JobType ->
  Job
mkJob creationDate isCanceled jobId jobType =
  Job' {creationDate, isCanceled, jobId, jobType}

-- | Undocumented field.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jCreationDate :: Lens.Lens' Job Core.UTCTime
jCreationDate = Lens.field @"creationDate"
{-# DEPRECATED jCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'isCanceled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jIsCanceled :: Lens.Lens' Job Core.Bool
jIsCanceled = Lens.field @"isCanceled"
{-# DEPRECATED jIsCanceled "Use generic-lens or generic-optics with 'isCanceled' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jJobId :: Lens.Lens' Job Types.JobId
jJobId = Lens.field @"jobId"
{-# DEPRECATED jJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jJobType :: Lens.Lens' Job Types.JobType
jJobType = Lens.field @"jobType"
{-# DEPRECATED jJobType "Use generic-lens or generic-optics with 'jobType' instead." #-}

instance Core.FromXML Job where
  parseXML x =
    Job'
      Core.<$> (x Core..@ "CreationDate")
      Core.<*> (x Core..@ "IsCanceled")
      Core.<*> (x Core..@ "JobId")
      Core.<*> (x Core..@ "JobType")
