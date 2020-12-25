{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    jAccountId,
    jData,
    jId,
    jNonce,
  )
where

import qualified Network.AWS.CodePipeline.Types.AccountId as Types
import qualified Network.AWS.CodePipeline.Types.JobData as Types
import qualified Network.AWS.CodePipeline.Types.JobId as Types
import qualified Network.AWS.CodePipeline.Types.Nonce as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about a job.
--
-- /See:/ 'mkJob' smart constructor.
data Job = Job'
  { -- | The ID of the AWS account to use when performing the job.
    accountId :: Core.Maybe Types.AccountId,
    -- | Other data about a job.
    data' :: Core.Maybe Types.JobData,
    -- | The unique system-generated ID of the job.
    id :: Core.Maybe Types.JobId,
    -- | A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Use this number in an 'AcknowledgeJob' request.
    nonce :: Core.Maybe Types.Nonce
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Job' value with any optional fields omitted.
mkJob ::
  Job
mkJob =
  Job'
    { accountId = Core.Nothing,
      data' = Core.Nothing,
      id = Core.Nothing,
      nonce = Core.Nothing
    }

-- | The ID of the AWS account to use when performing the job.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jAccountId :: Lens.Lens' Job (Core.Maybe Types.AccountId)
jAccountId = Lens.field @"accountId"
{-# DEPRECATED jAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Other data about a job.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jData :: Lens.Lens' Job (Core.Maybe Types.JobData)
jData = Lens.field @"data'"
{-# DEPRECATED jData "Use generic-lens or generic-optics with 'data'' instead." #-}

-- | The unique system-generated ID of the job.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jId :: Lens.Lens' Job (Core.Maybe Types.JobId)
jId = Lens.field @"id"
{-# DEPRECATED jId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Use this number in an 'AcknowledgeJob' request.
--
-- /Note:/ Consider using 'nonce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jNonce :: Lens.Lens' Job (Core.Maybe Types.Nonce)
jNonce = Lens.field @"nonce"
{-# DEPRECATED jNonce "Use generic-lens or generic-optics with 'nonce' instead." #-}

instance Core.FromJSON Job where
  parseJSON =
    Core.withObject "Job" Core.$
      \x ->
        Job'
          Core.<$> (x Core..:? "accountId")
          Core.<*> (x Core..:? "data")
          Core.<*> (x Core..:? "id")
          Core.<*> (x Core..:? "nonce")
