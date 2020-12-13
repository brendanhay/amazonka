{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.JobDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.JobDetails
  ( JobDetails (..),

    -- * Smart constructor
    mkJobDetails,

    -- * Lenses
    jdData,
    jdAccountId,
    jdId,
  )
where

import Network.AWS.CodePipeline.Types.JobData
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about the details of a job.
--
-- /See:/ 'mkJobDetails' smart constructor.
data JobDetails = JobDetails'
  { -- | Represents other information about a job required for a job worker to complete the job.
    data' :: Lude.Maybe JobData,
    -- | The AWS account ID associated with the job.
    accountId :: Lude.Maybe Lude.Text,
    -- | The unique system-generated ID of the job.
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobDetails' with the minimum fields required to make a request.
--
-- * 'data'' - Represents other information about a job required for a job worker to complete the job.
-- * 'accountId' - The AWS account ID associated with the job.
-- * 'id' - The unique system-generated ID of the job.
mkJobDetails ::
  JobDetails
mkJobDetails =
  JobDetails'
    { data' = Lude.Nothing,
      accountId = Lude.Nothing,
      id = Lude.Nothing
    }

-- | Represents other information about a job required for a job worker to complete the job.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdData :: Lens.Lens' JobDetails (Lude.Maybe JobData)
jdData = Lens.lens (data' :: JobDetails -> Lude.Maybe JobData) (\s a -> s {data' = a} :: JobDetails)
{-# DEPRECATED jdData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | The AWS account ID associated with the job.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdAccountId :: Lens.Lens' JobDetails (Lude.Maybe Lude.Text)
jdAccountId = Lens.lens (accountId :: JobDetails -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: JobDetails)
{-# DEPRECATED jdAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The unique system-generated ID of the job.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdId :: Lens.Lens' JobDetails (Lude.Maybe Lude.Text)
jdId = Lens.lens (id :: JobDetails -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: JobDetails)
{-# DEPRECATED jdId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON JobDetails where
  parseJSON =
    Lude.withObject
      "JobDetails"
      ( \x ->
          JobDetails'
            Lude.<$> (x Lude..:? "data")
            Lude.<*> (x Lude..:? "accountId")
            Lude.<*> (x Lude..:? "id")
      )
