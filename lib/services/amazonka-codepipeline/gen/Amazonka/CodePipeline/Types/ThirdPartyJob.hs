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
-- Module      : Amazonka.CodePipeline.Types.ThirdPartyJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ThirdPartyJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A response to a @PollForThirdPartyJobs@ request returned by CodePipeline
-- when there is a job to be worked on by a partner action.
--
-- /See:/ 'newThirdPartyJob' smart constructor.
data ThirdPartyJob = ThirdPartyJob'
  { -- | The @clientToken@ portion of the @clientId@ and @clientToken@ pair used
    -- to verify that the calling entity is allowed access to the job and its
    -- details.
    clientId :: Prelude.Maybe Prelude.Text,
    -- | The identifier used to identify the job in CodePipeline.
    jobId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'jobId', 'thirdPartyJob_jobId' - The identifier used to identify the job in CodePipeline.
newThirdPartyJob ::
  ThirdPartyJob
newThirdPartyJob =
  ThirdPartyJob'
    { clientId = Prelude.Nothing,
      jobId = Prelude.Nothing
    }

-- | The @clientToken@ portion of the @clientId@ and @clientToken@ pair used
-- to verify that the calling entity is allowed access to the job and its
-- details.
thirdPartyJob_clientId :: Lens.Lens' ThirdPartyJob (Prelude.Maybe Prelude.Text)
thirdPartyJob_clientId = Lens.lens (\ThirdPartyJob' {clientId} -> clientId) (\s@ThirdPartyJob' {} a -> s {clientId = a} :: ThirdPartyJob)

-- | The identifier used to identify the job in CodePipeline.
thirdPartyJob_jobId :: Lens.Lens' ThirdPartyJob (Prelude.Maybe Prelude.Text)
thirdPartyJob_jobId = Lens.lens (\ThirdPartyJob' {jobId} -> jobId) (\s@ThirdPartyJob' {} a -> s {jobId = a} :: ThirdPartyJob)

instance Data.FromJSON ThirdPartyJob where
  parseJSON =
    Data.withObject
      "ThirdPartyJob"
      ( \x ->
          ThirdPartyJob'
            Prelude.<$> (x Data..:? "clientId")
            Prelude.<*> (x Data..:? "jobId")
      )

instance Prelude.Hashable ThirdPartyJob where
  hashWithSalt _salt ThirdPartyJob' {..} =
    _salt
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData ThirdPartyJob where
  rnf ThirdPartyJob' {..} =
    Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf jobId
