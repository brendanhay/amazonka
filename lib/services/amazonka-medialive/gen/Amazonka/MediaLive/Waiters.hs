{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaLive.Waiters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.DescribeChannel
import Amazonka.MediaLive.DescribeInput
import Amazonka.MediaLive.DescribeMultiplex
import Amazonka.MediaLive.Lens
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.MediaLive.DescribeChannel' every 3 seconds until a successful state is reached. An error is returned after 5 failed checks.
newChannelCreated :: Core.Wait DescribeChannel
newChannelCreated =
  Core.Wait
    { Core.name = "ChannelCreated",
      Core.attempts = 5,
      Core.delay = 3,
      Core.acceptors =
        [ Core.matchAll
            "IDLE"
            Core.AcceptSuccess
            ( describeChannelResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CREATING"
            Core.AcceptRetry
            ( describeChannelResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry,
          Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( describeChannelResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.MediaLive.DescribeChannel' every 5 seconds until a successful state is reached. An error is returned after 84 failed checks.
newChannelDeleted :: Core.Wait DescribeChannel
newChannelDeleted =
  Core.Wait
    { Core.name = "ChannelDeleted",
      Core.attempts = 84,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "DELETED"
            Core.AcceptSuccess
            ( describeChannelResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "DELETING"
            Core.AcceptRetry
            ( describeChannelResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.MediaLive.DescribeChannel' every 5 seconds until a successful state is reached. An error is returned after 120 failed checks.
newChannelRunning :: Core.Wait DescribeChannel
newChannelRunning =
  Core.Wait
    { Core.name = "ChannelRunning",
      Core.attempts = 120,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "RUNNING"
            Core.AcceptSuccess
            ( describeChannelResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "STARTING"
            Core.AcceptRetry
            ( describeChannelResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.MediaLive.DescribeChannel' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
newChannelStopped :: Core.Wait DescribeChannel
newChannelStopped =
  Core.Wait
    { Core.name = "ChannelStopped",
      Core.attempts = 60,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "IDLE"
            Core.AcceptSuccess
            ( describeChannelResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "STOPPING"
            Core.AcceptRetry
            ( describeChannelResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.MediaLive.DescribeInput' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newInputAttached :: Core.Wait DescribeInput
newInputAttached =
  Core.Wait
    { Core.name = "InputAttached",
      Core.attempts = 20,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "ATTACHED"
            Core.AcceptSuccess
            ( describeInputResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "DETACHED"
            Core.AcceptRetry
            ( describeInputResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.MediaLive.DescribeInput' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newInputDeleted :: Core.Wait DescribeInput
newInputDeleted =
  Core.Wait
    { Core.name = "InputDeleted",
      Core.attempts = 20,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "DELETED"
            Core.AcceptSuccess
            ( describeInputResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "DELETING"
            Core.AcceptRetry
            ( describeInputResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.MediaLive.DescribeInput' every 5 seconds until a successful state is reached. An error is returned after 84 failed checks.
newInputDetached :: Core.Wait DescribeInput
newInputDetached =
  Core.Wait
    { Core.name = "InputDetached",
      Core.attempts = 84,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "DETACHED"
            Core.AcceptSuccess
            ( describeInputResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CREATING"
            Core.AcceptRetry
            ( describeInputResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "ATTACHED"
            Core.AcceptRetry
            ( describeInputResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.MediaLive.DescribeMultiplex' every 3 seconds until a successful state is reached. An error is returned after 5 failed checks.
newMultiplexCreated :: Core.Wait DescribeMultiplex
newMultiplexCreated =
  Core.Wait
    { Core.name = "MultiplexCreated",
      Core.attempts = 5,
      Core.delay = 3,
      Core.acceptors =
        [ Core.matchAll
            "IDLE"
            Core.AcceptSuccess
            ( describeMultiplexResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CREATING"
            Core.AcceptRetry
            ( describeMultiplexResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry,
          Core.matchAll
            "CREATE_FAILED"
            Core.AcceptFailure
            ( describeMultiplexResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.MediaLive.DescribeMultiplex' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newMultiplexDeleted :: Core.Wait DescribeMultiplex
newMultiplexDeleted =
  Core.Wait
    { Core.name = "MultiplexDeleted",
      Core.attempts = 20,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "DELETED"
            Core.AcceptSuccess
            ( describeMultiplexResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "DELETING"
            Core.AcceptRetry
            ( describeMultiplexResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.MediaLive.DescribeMultiplex' every 5 seconds until a successful state is reached. An error is returned after 120 failed checks.
newMultiplexRunning :: Core.Wait DescribeMultiplex
newMultiplexRunning =
  Core.Wait
    { Core.name = "MultiplexRunning",
      Core.attempts = 120,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "RUNNING"
            Core.AcceptSuccess
            ( describeMultiplexResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "STARTING"
            Core.AcceptRetry
            ( describeMultiplexResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.MediaLive.DescribeMultiplex' every 5 seconds until a successful state is reached. An error is returned after 28 failed checks.
newMultiplexStopped :: Core.Wait DescribeMultiplex
newMultiplexStopped =
  Core.Wait
    { Core.name = "MultiplexStopped",
      Core.attempts = 28,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "IDLE"
            Core.AcceptSuccess
            ( describeMultiplexResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "STOPPING"
            Core.AcceptRetry
            ( describeMultiplexResponse_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry
        ]
    }
