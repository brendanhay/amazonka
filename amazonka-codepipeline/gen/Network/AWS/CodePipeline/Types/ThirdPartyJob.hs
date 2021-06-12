{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ThirdPartyJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ThirdPartyJob where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A response to a @PollForThirdPartyJobs@ request returned by AWS
-- CodePipeline when there is a job to be worked on by a partner action.
--
-- /See:/ 'newThirdPartyJob' smart constructor.
data ThirdPartyJob = ThirdPartyJob'
  { -- | The @clientToken@ portion of the @clientId@ and @clientToken@ pair used
    -- to verify that the calling entity is allowed access to the job and its
    -- details.
    clientId :: Core.Maybe Core.Text,
    -- | The identifier used to identify the job in AWS CodePipeline.
    jobId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ThirdPartyJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientId', 'thirdPartyJob_clientId' - The @clientToken@ portion of the @clientId@ and @clientToken@ pair used
-- to verify that the calling entity is allowed access to the job and its
-- details.
--
-- 'jobId', 'thirdPartyJob_jobId' - The identifier used to identify the job in AWS CodePipeline.
newThirdPartyJob ::
  ThirdPartyJob
newThirdPartyJob =
  ThirdPartyJob'
    { clientId = Core.Nothing,
      jobId = Core.Nothing
    }

-- | The @clientToken@ portion of the @clientId@ and @clientToken@ pair used
-- to verify that the calling entity is allowed access to the job and its
-- details.
thirdPartyJob_clientId :: Lens.Lens' ThirdPartyJob (Core.Maybe Core.Text)
thirdPartyJob_clientId = Lens.lens (\ThirdPartyJob' {clientId} -> clientId) (\s@ThirdPartyJob' {} a -> s {clientId = a} :: ThirdPartyJob)

-- | The identifier used to identify the job in AWS CodePipeline.
thirdPartyJob_jobId :: Lens.Lens' ThirdPartyJob (Core.Maybe Core.Text)
thirdPartyJob_jobId = Lens.lens (\ThirdPartyJob' {jobId} -> jobId) (\s@ThirdPartyJob' {} a -> s {jobId = a} :: ThirdPartyJob)

instance Core.FromJSON ThirdPartyJob where
  parseJSON =
    Core.withObject
      "ThirdPartyJob"
      ( \x ->
          ThirdPartyJob'
            Core.<$> (x Core..:? "clientId")
            Core.<*> (x Core..:? "jobId")
      )

instance Core.Hashable ThirdPartyJob

instance Core.NFData ThirdPartyJob
