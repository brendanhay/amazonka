{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Waiters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.DescribeChannel
import Network.AWS.MediaLive.DescribeInput
import Network.AWS.MediaLive.DescribeMultiplex
import Network.AWS.MediaLive.Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Prelude

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 5 seconds until a successful state is reached. An error is returned after 120 failed checks.
newMultiplexRunning :: Core.Wait DescribeMultiplex
newMultiplexRunning =
  Core.Wait
    { Core._waitName = "MultiplexRunning",
      Core._waitAttempts = 120,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchAll
            "RUNNING"
            Core.AcceptSuccess
            ( describeMultiplexResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "STARTING"
            Core.AcceptRetry
            ( describeMultiplexResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 5 seconds until a successful state is reached. An error is returned after 120 failed checks.
newChannelRunning :: Core.Wait DescribeChannel
newChannelRunning =
  Core.Wait
    { Core._waitName = "ChannelRunning",
      Core._waitAttempts = 120,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchAll
            "RUNNING"
            Core.AcceptSuccess
            ( describeChannelResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "STARTING"
            Core.AcceptRetry
            ( describeChannelResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 5 seconds until a successful state is reached. An error is returned after 84 failed checks.
newChannelDeleted :: Core.Wait DescribeChannel
newChannelDeleted =
  Core.Wait
    { Core._waitName = "ChannelDeleted",
      Core._waitAttempts = 84,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchAll
            "DELETED"
            Core.AcceptSuccess
            ( describeChannelResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "DELETING"
            Core.AcceptRetry
            ( describeChannelResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeInput' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newInputDeleted :: Core.Wait DescribeInput
newInputDeleted =
  Core.Wait
    { Core._waitName = "InputDeleted",
      Core._waitAttempts = 20,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchAll
            "DELETED"
            Core.AcceptSuccess
            ( describeInputResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "DELETING"
            Core.AcceptRetry
            ( describeInputResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeInput' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newInputAttached :: Core.Wait DescribeInput
newInputAttached =
  Core.Wait
    { Core._waitName = "InputAttached",
      Core._waitAttempts = 20,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchAll
            "ATTACHED"
            Core.AcceptSuccess
            ( describeInputResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "DETACHED"
            Core.AcceptRetry
            ( describeInputResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 5 seconds until a successful state is reached. An error is returned after 28 failed checks.
newMultiplexStopped :: Core.Wait DescribeMultiplex
newMultiplexStopped =
  Core.Wait
    { Core._waitName = "MultiplexStopped",
      Core._waitAttempts = 28,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchAll
            "IDLE"
            Core.AcceptSuccess
            ( describeMultiplexResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "STOPPING"
            Core.AcceptRetry
            ( describeMultiplexResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 3 seconds until a successful state is reached. An error is returned after 5 failed checks.
newChannelCreated :: Core.Wait DescribeChannel
newChannelCreated =
  Core.Wait
    { Core._waitName = "ChannelCreated",
      Core._waitAttempts = 5,
      Core._waitDelay = 3,
      Core._waitAcceptors =
        [ Core.matchAll
            "IDLE"
            Core.AcceptSuccess
            ( describeChannelResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "CREATING"
            Core.AcceptRetry
            ( describeChannelResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry,
          Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( describeChannelResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeInput' every 5 seconds until a successful state is reached. An error is returned after 84 failed checks.
newInputDetached :: Core.Wait DescribeInput
newInputDetached =
  Core.Wait
    { Core._waitName = "InputDetached",
      Core._waitAttempts = 84,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchAll
            "DETACHED"
            Core.AcceptSuccess
            ( describeInputResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "CREATING"
            Core.AcceptRetry
            ( describeInputResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "ATTACHED"
            Core.AcceptRetry
            ( describeInputResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 3 seconds until a successful state is reached. An error is returned after 5 failed checks.
newMultiplexCreated :: Core.Wait DescribeMultiplex
newMultiplexCreated =
  Core.Wait
    { Core._waitName = "MultiplexCreated",
      Core._waitAttempts = 5,
      Core._waitDelay = 3,
      Core._waitAcceptors =
        [ Core.matchAll
            "IDLE"
            Core.AcceptSuccess
            ( describeMultiplexResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "CREATING"
            Core.AcceptRetry
            ( describeMultiplexResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry,
          Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( describeMultiplexResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newMultiplexDeleted :: Core.Wait DescribeMultiplex
newMultiplexDeleted =
  Core.Wait
    { Core._waitName = "MultiplexDeleted",
      Core._waitAttempts = 20,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchAll
            "DELETED"
            Core.AcceptSuccess
            ( describeMultiplexResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "DELETING"
            Core.AcceptRetry
            ( describeMultiplexResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
newChannelStopped :: Core.Wait DescribeChannel
newChannelStopped =
  Core.Wait
    { Core._waitName = "ChannelStopped",
      Core._waitAttempts = 60,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchAll
            "IDLE"
            Core.AcceptSuccess
            ( describeChannelResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "STOPPING"
            Core.AcceptRetry
            ( describeChannelResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry
        ]
    }
