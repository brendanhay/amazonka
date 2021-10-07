{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Waiters where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticTranscoder.Lens
import Network.AWS.ElasticTranscoder.ReadJob
import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Polls 'Network.AWS.ElasticTranscoder.ReadJob' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newJobComplete :: Core.Wait ReadJob
newJobComplete =
  Core.Wait
    { Core._waitName = "JobComplete",
      Core._waitAttempts = 120,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "Complete"
            Core.AcceptSuccess
            ( readJobResponse_job
                Prelude.. job'_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Canceled"
            Core.AcceptFailure
            ( readJobResponse_job
                Prelude.. job'_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Error"
            Core.AcceptFailure
            ( readJobResponse_job
                Prelude.. job'_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }
