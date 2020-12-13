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
    jJobType,
    jJobId,
    jIsCanceled,
    jCreationDate,
  )
where

import Network.AWS.ImportExport.Types.JobType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Representation of a job returned by the ListJobs operation.
--
-- /See:/ 'mkJob' smart constructor.
data Job = Job'
  { jobType :: JobType,
    jobId :: Lude.Text,
    isCanceled :: Lude.Bool,
    creationDate :: Lude.DateTime
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Job' with the minimum fields required to make a request.
--
-- * 'jobType' -
-- * 'jobId' -
-- * 'isCanceled' -
-- * 'creationDate' -
mkJob ::
  -- | 'jobType'
  JobType ->
  -- | 'jobId'
  Lude.Text ->
  -- | 'isCanceled'
  Lude.Bool ->
  -- | 'creationDate'
  Lude.DateTime ->
  Job
mkJob pJobType_ pJobId_ pIsCanceled_ pCreationDate_ =
  Job'
    { jobType = pJobType_,
      jobId = pJobId_,
      isCanceled = pIsCanceled_,
      creationDate = pCreationDate_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jJobType :: Lens.Lens' Job JobType
jJobType = Lens.lens (jobType :: Job -> JobType) (\s a -> s {jobType = a} :: Job)
{-# DEPRECATED jJobType "Use generic-lens or generic-optics with 'jobType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jJobId :: Lens.Lens' Job Lude.Text
jJobId = Lens.lens (jobId :: Job -> Lude.Text) (\s a -> s {jobId = a} :: Job)
{-# DEPRECATED jJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'isCanceled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jIsCanceled :: Lens.Lens' Job Lude.Bool
jIsCanceled = Lens.lens (isCanceled :: Job -> Lude.Bool) (\s a -> s {isCanceled = a} :: Job)
{-# DEPRECATED jIsCanceled "Use generic-lens or generic-optics with 'isCanceled' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jCreationDate :: Lens.Lens' Job Lude.DateTime
jCreationDate = Lens.lens (creationDate :: Job -> Lude.DateTime) (\s a -> s {creationDate = a} :: Job)
{-# DEPRECATED jCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

instance Lude.FromXML Job where
  parseXML x =
    Job'
      Lude.<$> (x Lude..@ "JobType")
      Lude.<*> (x Lude..@ "JobId")
      Lude.<*> (x Lude..@ "IsCanceled")
      Lude.<*> (x Lude..@ "CreationDate")
