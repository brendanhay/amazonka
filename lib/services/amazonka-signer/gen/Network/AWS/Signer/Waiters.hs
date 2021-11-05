{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Signer.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Signer.Waiters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Signer.DescribeSigningJob
import Network.AWS.Signer.Lens
import Network.AWS.Signer.Types

-- | Polls 'Network.AWS.Signer.DescribeSigningJob' every 20 seconds until a successful state is reached. An error is returned after 25 failed checks.
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
