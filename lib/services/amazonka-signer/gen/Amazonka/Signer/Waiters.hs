{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Signer.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Signer.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Signer.DescribeSigningJob
import Amazonka.Signer.Lens
import Amazonka.Signer.Types

-- | Polls 'Amazonka.Signer.DescribeSigningJob' every 20 seconds until a successful state is reached. An error is returned after 25 failed checks.
newSuccessfulSigningJob :: Core.Wait DescribeSigningJob
newSuccessfulSigningJob =
  Core.Wait
    { Core._waitName = "SuccessfulSigningJob",
      Core._waitAttempts = 25,
      Core._waitDelay = 20,
      Core._waitAcceptors =
        [ Core.matchAll
            "Succeeded"
            Core.AcceptSuccess
            ( describeSigningJobResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeSigningJobResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ResourceNotFoundException"
            Core.AcceptFailure
        ]
    }
