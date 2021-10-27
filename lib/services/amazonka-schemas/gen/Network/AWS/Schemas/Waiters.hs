{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Schemas.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Schemas.Waiters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Schemas.DescribeCodeBinding
import Network.AWS.Schemas.Lens
import Network.AWS.Schemas.Types

-- | Polls 'Network.AWS.Schemas.DescribeCodeBinding' every 2 seconds until a successful state is reached. An error is returned after 30 failed checks.
newCodeBindingExists :: Core.Wait DescribeCodeBinding
newCodeBindingExists =
  Core.Wait
    { Core._waitName = "CodeBindingExists",
      Core._waitAttempts = 30,
      Core._waitDelay = 2,
      Core._waitAcceptors =
        [ Core.matchAll
            "CREATE_COMPLETE"
            Core.AcceptSuccess
            ( describeCodeBindingResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "CREATE_IN_PROGRESS"
            Core.AcceptRetry
            ( describeCodeBindingResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( describeCodeBindingResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "NotFoundException"
            Core.AcceptFailure
        ]
    }
