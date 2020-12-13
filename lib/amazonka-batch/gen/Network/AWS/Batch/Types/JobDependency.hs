{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobDependency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobDependency
  ( JobDependency (..),

    -- * Smart constructor
    mkJobDependency,

    -- * Lenses
    jJobId,
    jType,
  )
where

import Network.AWS.Batch.Types.ArrayJobDependency
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing an AWS Batch job dependency.
--
-- /See:/ 'mkJobDependency' smart constructor.
data JobDependency = JobDependency'
  { -- | The job ID of the AWS Batch job associated with this dependency.
    jobId :: Lude.Maybe Lude.Text,
    -- | The type of the job dependency.
    type' :: Lude.Maybe ArrayJobDependency
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobDependency' with the minimum fields required to make a request.
--
-- * 'jobId' - The job ID of the AWS Batch job associated with this dependency.
-- * 'type'' - The type of the job dependency.
mkJobDependency ::
  JobDependency
mkJobDependency =
  JobDependency' {jobId = Lude.Nothing, type' = Lude.Nothing}

-- | The job ID of the AWS Batch job associated with this dependency.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jJobId :: Lens.Lens' JobDependency (Lude.Maybe Lude.Text)
jJobId = Lens.lens (jobId :: JobDependency -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: JobDependency)
{-# DEPRECATED jJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The type of the job dependency.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jType :: Lens.Lens' JobDependency (Lude.Maybe ArrayJobDependency)
jType = Lens.lens (type' :: JobDependency -> Lude.Maybe ArrayJobDependency) (\s a -> s {type' = a} :: JobDependency)
{-# DEPRECATED jType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON JobDependency where
  parseJSON =
    Lude.withObject
      "JobDependency"
      ( \x ->
          JobDependency'
            Lude.<$> (x Lude..:? "jobId") Lude.<*> (x Lude..:? "type")
      )

instance Lude.ToJSON JobDependency where
  toJSON JobDependency' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("jobId" Lude..=) Lude.<$> jobId,
            ("type" Lude..=) Lude.<$> type'
          ]
      )
