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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A response to a @PollForThirdPartyJobs@ request returned by AWS CodePipeline when there is a job to be worked on by a partner action.
--
-- /See:/ 'mkThirdPartyJob' smart constructor.
data ThirdPartyJob = ThirdPartyJob'
  { -- | The @clientToken@ portion of the @clientId@ and @clientToken@ pair used to verify that the calling entity is allowed access to the job and its details.
    clientId :: Lude.Maybe Lude.Text,
    -- | The identifier used to identify the job in AWS CodePipeline.
    jobId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ThirdPartyJob' with the minimum fields required to make a request.
--
-- * 'clientId' - The @clientToken@ portion of the @clientId@ and @clientToken@ pair used to verify that the calling entity is allowed access to the job and its details.
-- * 'jobId' - The identifier used to identify the job in AWS CodePipeline.
mkThirdPartyJob ::
  ThirdPartyJob
mkThirdPartyJob =
  ThirdPartyJob' {clientId = Lude.Nothing, jobId = Lude.Nothing}

-- | The @clientToken@ portion of the @clientId@ and @clientToken@ pair used to verify that the calling entity is allowed access to the job and its details.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpjClientId :: Lens.Lens' ThirdPartyJob (Lude.Maybe Lude.Text)
tpjClientId = Lens.lens (clientId :: ThirdPartyJob -> Lude.Maybe Lude.Text) (\s a -> s {clientId = a} :: ThirdPartyJob)
{-# DEPRECATED tpjClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The identifier used to identify the job in AWS CodePipeline.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpjJobId :: Lens.Lens' ThirdPartyJob (Lude.Maybe Lude.Text)
tpjJobId = Lens.lens (jobId :: ThirdPartyJob -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: ThirdPartyJob)
{-# DEPRECATED tpjJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.FromJSON ThirdPartyJob where
  parseJSON =
    Lude.withObject
      "ThirdPartyJob"
      ( \x ->
          ThirdPartyJob'
            Lude.<$> (x Lude..:? "clientId") Lude.<*> (x Lude..:? "jobId")
      )
