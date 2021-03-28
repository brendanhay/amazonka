{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.JobDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.JobDetails
  ( JobDetails (..)
  -- * Smart constructor
  , mkJobDetails
  -- * Lenses
  , jdAccountId
  , jdData
  , jdId
  ) where

import qualified Network.AWS.CodePipeline.Types.AccountId as Types
import qualified Network.AWS.CodePipeline.Types.JobData as Types
import qualified Network.AWS.CodePipeline.Types.JobId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about the details of a job.
--
-- /See:/ 'mkJobDetails' smart constructor.
data JobDetails = JobDetails'
  { accountId :: Core.Maybe Types.AccountId
    -- ^ The AWS account ID associated with the job.
  , data' :: Core.Maybe Types.JobData
    -- ^ Represents other information about a job required for a job worker to complete the job. 
  , id :: Core.Maybe Types.JobId
    -- ^ The unique system-generated ID of the job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobDetails' value with any optional fields omitted.
mkJobDetails
    :: JobDetails
mkJobDetails
  = JobDetails'{accountId = Core.Nothing, data' = Core.Nothing,
                id = Core.Nothing}

-- | The AWS account ID associated with the job.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdAccountId :: Lens.Lens' JobDetails (Core.Maybe Types.AccountId)
jdAccountId = Lens.field @"accountId"
{-# INLINEABLE jdAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | Represents other information about a job required for a job worker to complete the job. 
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdData :: Lens.Lens' JobDetails (Core.Maybe Types.JobData)
jdData = Lens.field @"data'"
{-# INLINEABLE jdData #-}
{-# DEPRECATED data' "Use generic-lens or generic-optics with 'data'' instead"  #-}

-- | The unique system-generated ID of the job.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jdId :: Lens.Lens' JobDetails (Core.Maybe Types.JobId)
jdId = Lens.field @"id"
{-# INLINEABLE jdId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.FromJSON JobDetails where
        parseJSON
          = Core.withObject "JobDetails" Core.$
              \ x ->
                JobDetails' Core.<$>
                  (x Core..:? "accountId") Core.<*> x Core..:? "data" Core.<*>
                    x Core..:? "id"
