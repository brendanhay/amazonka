{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElasticTranscoder.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticTranscoder.Lens
import Amazonka.ElasticTranscoder.ReadJob
import Amazonka.ElasticTranscoder.Types
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.ElasticTranscoder.ReadJob' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newJobComplete :: Core.Wait ReadJob
newJobComplete =
  Core.Wait
    { Core.name = "JobComplete",
      Core.attempts = 120,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "Complete"
            Core.AcceptSuccess
            ( readJobResponse_job
                Prelude.. job_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Canceled"
            Core.AcceptFailure
            ( readJobResponse_job
                Prelude.. job_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Error"
            Core.AcceptFailure
            ( readJobResponse_job
                Prelude.. job_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }
