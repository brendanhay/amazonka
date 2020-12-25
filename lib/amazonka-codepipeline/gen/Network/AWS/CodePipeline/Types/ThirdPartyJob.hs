{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ThirdPartyJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ThirdPartyJob
  ( ThirdPartyJob (..),

    -- * Smart constructor
    mkThirdPartyJob,

    -- * Lenses
    tpjClientId,
    tpjJobId,
  )
where

import qualified Network.AWS.CodePipeline.Types.ClientId as Types
import qualified Network.AWS.CodePipeline.Types.JobId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A response to a @PollForThirdPartyJobs@ request returned by AWS CodePipeline when there is a job to be worked on by a partner action.
--
-- /See:/ 'mkThirdPartyJob' smart constructor.
data ThirdPartyJob = ThirdPartyJob'
  { -- | The @clientToken@ portion of the @clientId@ and @clientToken@ pair used to verify that the calling entity is allowed access to the job and its details.
    clientId :: Core.Maybe Types.ClientId,
    -- | The identifier used to identify the job in AWS CodePipeline.
    jobId :: Core.Maybe Types.JobId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ThirdPartyJob' value with any optional fields omitted.
mkThirdPartyJob ::
  ThirdPartyJob
mkThirdPartyJob =
  ThirdPartyJob' {clientId = Core.Nothing, jobId = Core.Nothing}

-- | The @clientToken@ portion of the @clientId@ and @clientToken@ pair used to verify that the calling entity is allowed access to the job and its details.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpjClientId :: Lens.Lens' ThirdPartyJob (Core.Maybe Types.ClientId)
tpjClientId = Lens.field @"clientId"
{-# DEPRECATED tpjClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The identifier used to identify the job in AWS CodePipeline.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpjJobId :: Lens.Lens' ThirdPartyJob (Core.Maybe Types.JobId)
tpjJobId = Lens.field @"jobId"
{-# DEPRECATED tpjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Core.FromJSON ThirdPartyJob where
  parseJSON =
    Core.withObject "ThirdPartyJob" Core.$
      \x ->
        ThirdPartyJob'
          Core.<$> (x Core..:? "clientId") Core.<*> (x Core..:? "jobId")
