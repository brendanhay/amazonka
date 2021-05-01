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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.DescribeChannel
import Network.AWS.MediaLive.DescribeInput
import Network.AWS.MediaLive.DescribeMultiplex
import Network.AWS.MediaLive.Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 5 seconds until a successful state is reached. An error is returned after 120 failed checks.
newMultiplexRunning :: Waiter.Wait DescribeMultiplex
newMultiplexRunning =
  Waiter.Wait
    { Waiter._waitName = "MultiplexRunning",
      Waiter._waitAttempts = 120,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "RUNNING"
            Waiter.AcceptSuccess
            ( describeMultiplexResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "STARTING"
            Waiter.AcceptRetry
            ( describeMultiplexResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchStatus 500 Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 5 seconds until a successful state is reached. An error is returned after 120 failed checks.
newChannelRunning :: Waiter.Wait DescribeChannel
newChannelRunning =
  Waiter.Wait
    { Waiter._waitName = "ChannelRunning",
      Waiter._waitAttempts = 120,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "RUNNING"
            Waiter.AcceptSuccess
            ( describeChannelResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "STARTING"
            Waiter.AcceptRetry
            ( describeChannelResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchStatus 500 Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 5 seconds until a successful state is reached. An error is returned after 84 failed checks.
newChannelDeleted :: Waiter.Wait DescribeChannel
newChannelDeleted =
  Waiter.Wait
    { Waiter._waitName = "ChannelDeleted",
      Waiter._waitAttempts = 84,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "DELETED"
            Waiter.AcceptSuccess
            ( describeChannelResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "DELETING"
            Waiter.AcceptRetry
            ( describeChannelResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchStatus 500 Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeInput' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newInputDeleted :: Waiter.Wait DescribeInput
newInputDeleted =
  Waiter.Wait
    { Waiter._waitName = "InputDeleted",
      Waiter._waitAttempts = 20,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "DELETED"
            Waiter.AcceptSuccess
            ( describeInputResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "DELETING"
            Waiter.AcceptRetry
            ( describeInputResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchStatus 500 Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeInput' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newInputAttached :: Waiter.Wait DescribeInput
newInputAttached =
  Waiter.Wait
    { Waiter._waitName = "InputAttached",
      Waiter._waitAttempts = 20,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "ATTACHED"
            Waiter.AcceptSuccess
            ( describeInputResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "DETACHED"
            Waiter.AcceptRetry
            ( describeInputResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchStatus 500 Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 5 seconds until a successful state is reached. An error is returned after 28 failed checks.
newMultiplexStopped :: Waiter.Wait DescribeMultiplex
newMultiplexStopped =
  Waiter.Wait
    { Waiter._waitName = "MultiplexStopped",
      Waiter._waitAttempts = 28,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "IDLE"
            Waiter.AcceptSuccess
            ( describeMultiplexResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "STOPPING"
            Waiter.AcceptRetry
            ( describeMultiplexResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchStatus 500 Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 3 seconds until a successful state is reached. An error is returned after 5 failed checks.
newChannelCreated :: Waiter.Wait DescribeChannel
newChannelCreated =
  Waiter.Wait
    { Waiter._waitName = "ChannelCreated",
      Waiter._waitAttempts = 5,
      Waiter._waitDelay = 3,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "IDLE"
            Waiter.AcceptSuccess
            ( describeChannelResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "CREATING"
            Waiter.AcceptRetry
            ( describeChannelResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchStatus 500 Waiter.AcceptRetry,
          Waiter.matchAll
            "CREATE_FAILED"
            Waiter.AcceptFailure
            ( describeChannelResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 3 seconds until a successful state is reached. An error is returned after 5 failed checks.
newMultiplexCreated :: Waiter.Wait DescribeMultiplex
newMultiplexCreated =
  Waiter.Wait
    { Waiter._waitName = "MultiplexCreated",
      Waiter._waitAttempts = 5,
      Waiter._waitDelay = 3,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "IDLE"
            Waiter.AcceptSuccess
            ( describeMultiplexResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "CREATING"
            Waiter.AcceptRetry
            ( describeMultiplexResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchStatus 500 Waiter.AcceptRetry,
          Waiter.matchAll
            "CREATE_FAILED"
            Waiter.AcceptFailure
            ( describeMultiplexResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeInput' every 5 seconds until a successful state is reached. An error is returned after 84 failed checks.
newInputDetached :: Waiter.Wait DescribeInput
newInputDetached =
  Waiter.Wait
    { Waiter._waitName = "InputDetached",
      Waiter._waitAttempts = 84,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "DETACHED"
            Waiter.AcceptSuccess
            ( describeInputResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "CREATING"
            Waiter.AcceptRetry
            ( describeInputResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "ATTACHED"
            Waiter.AcceptRetry
            ( describeInputResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchStatus 500 Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newMultiplexDeleted :: Waiter.Wait DescribeMultiplex
newMultiplexDeleted =
  Waiter.Wait
    { Waiter._waitName = "MultiplexDeleted",
      Waiter._waitAttempts = 20,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "DELETED"
            Waiter.AcceptSuccess
            ( describeMultiplexResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "DELETING"
            Waiter.AcceptRetry
            ( describeMultiplexResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchStatus 500 Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
newChannelStopped :: Waiter.Wait DescribeChannel
newChannelStopped =
  Waiter.Wait
    { Waiter._waitName = "ChannelStopped",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "IDLE"
            Waiter.AcceptSuccess
            ( describeChannelResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "STOPPING"
            Waiter.AcceptRetry
            ( describeChannelResponse_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchStatus 500 Waiter.AcceptRetry
        ]
    }
