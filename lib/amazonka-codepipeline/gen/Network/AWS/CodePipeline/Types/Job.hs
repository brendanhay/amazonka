-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.Job
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.Job
  ( Job (..),

    -- * Smart constructor
    mkJob,

    -- * Lenses
    jData,
    jAccountId,
    jId,
    jNonce,
  )
where

import Network.AWS.CodePipeline.Types.JobData
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about a job.
--
-- /See:/ 'mkJob' smart constructor.
data Job = Job'
  { data' :: Lude.Maybe JobData,
    accountId :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    nonce :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Job' with the minimum fields required to make a request.
--
-- * 'accountId' - The ID of the AWS account to use when performing the job.
-- * 'data'' - Other data about a job.
-- * 'id' - The unique system-generated ID of the job.
-- * 'nonce' - A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Use this number in an 'AcknowledgeJob' request.
mkJob ::
  Job
mkJob =
  Job'
    { data' = Lude.Nothing,
      accountId = Lude.Nothing,
      id = Lude.Nothing,
      nonce = Lude.Nothing
    }

-- | Other data about a job.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jData :: Lens.Lens' Job (Lude.Maybe JobData)
jData = Lens.lens (data' :: Job -> Lude.Maybe JobData) (\s a -> s {data' = a} :: Job)
{-# DEPRECATED jData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | The ID of the AWS account to use when performing the job.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jAccountId :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jAccountId = Lens.lens (accountId :: Job -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: Job)
{-# DEPRECATED jAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The unique system-generated ID of the job.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jId :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jId = Lens.lens (id :: Job -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Job)
{-# DEPRECATED jId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Use this number in an 'AcknowledgeJob' request.
--
-- /Note:/ Consider using 'nonce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jNonce :: Lens.Lens' Job (Lude.Maybe Lude.Text)
jNonce = Lens.lens (nonce :: Job -> Lude.Maybe Lude.Text) (\s a -> s {nonce = a} :: Job)
{-# DEPRECATED jNonce "Use generic-lens or generic-optics with 'nonce' instead." #-}

instance Lude.FromJSON Job where
  parseJSON =
    Lude.withObject
      "Job"
      ( \x ->
          Job'
            Lude.<$> (x Lude..:? "data")
            Lude.<*> (x Lude..:? "accountId")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "nonce")
      )
