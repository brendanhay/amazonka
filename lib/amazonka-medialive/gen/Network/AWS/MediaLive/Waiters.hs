{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Waiters
  ( -- * ChannelRunning
    mkChannelRunning,

    -- * InputAttached
    mkInputAttached,

    -- * MultiplexRunning
    mkMultiplexRunning,

    -- * MultiplexDeleted
    mkMultiplexDeleted,

    -- * InputDetached
    mkInputDetached,

    -- * InputDeleted
    mkInputDeleted,

    -- * ChannelStopped
    mkChannelStopped,

    -- * MultiplexCreated
    mkMultiplexCreated,

    -- * ChannelCreated
    mkChannelCreated,

    -- * ChannelDeleted
    mkChannelDeleted,

    -- * MultiplexStopped
    mkMultiplexStopped,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.DescribeChannel
import Network.AWS.MediaLive.DescribeInput
import Network.AWS.MediaLive.DescribeMultiplex
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 5 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkChannelRunning :: Wait.Wait DescribeChannel
mkChannelRunning =
  Wait.Wait
    { Wait._waitName = "ChannelRunning",
      Wait._waitAttempts = 120,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "RUNNING"
            Wait.AcceptSuccess
            (dcrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "STARTING"
            Wait.AcceptRetry
            (dcrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchStatus 500 Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeInput' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkInputAttached :: Wait.Wait DescribeInput
mkInputAttached =
  Wait.Wait
    { Wait._waitName = "InputAttached",
      Wait._waitAttempts = 20,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "ATTACHED"
            Wait.AcceptSuccess
            (difrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "DETACHED"
            Wait.AcceptRetry
            (difrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchStatus 500 Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 5 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkMultiplexRunning :: Wait.Wait DescribeMultiplex
mkMultiplexRunning =
  Wait.Wait
    { Wait._waitName = "MultiplexRunning",
      Wait._waitAttempts = 120,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "RUNNING"
            Wait.AcceptSuccess
            (dmrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "STARTING"
            Wait.AcceptRetry
            (dmrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchStatus 500 Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkMultiplexDeleted :: Wait.Wait DescribeMultiplex
mkMultiplexDeleted =
  Wait.Wait
    { Wait._waitName = "MultiplexDeleted",
      Wait._waitAttempts = 20,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "DELETED"
            Wait.AcceptSuccess
            (dmrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "DELETING"
            Wait.AcceptRetry
            (dmrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchStatus 500 Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeInput' every 5 seconds until a successful state is reached. An error is returned after 84 failed checks.
mkInputDetached :: Wait.Wait DescribeInput
mkInputDetached =
  Wait.Wait
    { Wait._waitName = "InputDetached",
      Wait._waitAttempts = 84,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "DETACHED"
            Wait.AcceptSuccess
            (difrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "CREATING"
            Wait.AcceptRetry
            (difrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "ATTACHED"
            Wait.AcceptRetry
            (difrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchStatus 500 Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeInput' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkInputDeleted :: Wait.Wait DescribeInput
mkInputDeleted =
  Wait.Wait
    { Wait._waitName = "InputDeleted",
      Wait._waitAttempts = 20,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "DELETED"
            Wait.AcceptSuccess
            (difrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "DELETING"
            Wait.AcceptRetry
            (difrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchStatus 500 Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkChannelStopped :: Wait.Wait DescribeChannel
mkChannelStopped =
  Wait.Wait
    { Wait._waitName = "ChannelStopped",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "IDLE"
            Wait.AcceptSuccess
            (dcrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "STOPPING"
            Wait.AcceptRetry
            (dcrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchStatus 500 Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 3 seconds until a successful state is reached. An error is returned after 5 failed checks.
mkMultiplexCreated :: Wait.Wait DescribeMultiplex
mkMultiplexCreated =
  Wait.Wait
    { Wait._waitName = "MultiplexCreated",
      Wait._waitAttempts = 5,
      Wait._waitDelay = 3,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "IDLE"
            Wait.AcceptSuccess
            (dmrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "CREATING"
            Wait.AcceptRetry
            (dmrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchStatus 500 Wait.AcceptRetry,
          Wait.matchAll
            "CREATE_FAILED"
            Wait.AcceptFailure
            (dmrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText)
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 3 seconds until a successful state is reached. An error is returned after 5 failed checks.
mkChannelCreated :: Wait.Wait DescribeChannel
mkChannelCreated =
  Wait.Wait
    { Wait._waitName = "ChannelCreated",
      Wait._waitAttempts = 5,
      Wait._waitDelay = 3,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "IDLE"
            Wait.AcceptSuccess
            (dcrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "CREATING"
            Wait.AcceptRetry
            (dcrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchStatus 500 Wait.AcceptRetry,
          Wait.matchAll
            "CREATE_FAILED"
            Wait.AcceptFailure
            (dcrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText)
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 5 seconds until a successful state is reached. An error is returned after 84 failed checks.
mkChannelDeleted :: Wait.Wait DescribeChannel
mkChannelDeleted =
  Wait.Wait
    { Wait._waitName = "ChannelDeleted",
      Wait._waitAttempts = 84,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "DELETED"
            Wait.AcceptSuccess
            (dcrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "DELETING"
            Wait.AcceptRetry
            (dcrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchStatus 500 Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 5 seconds until a successful state is reached. An error is returned after 28 failed checks.
mkMultiplexStopped :: Wait.Wait DescribeMultiplex
mkMultiplexStopped =
  Wait.Wait
    { Wait._waitName = "MultiplexStopped",
      Wait._waitAttempts = 28,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "IDLE"
            Wait.AcceptSuccess
            (dmrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "STOPPING"
            Wait.AcceptRetry
            (dmrsState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchStatus 500 Wait.AcceptRetry
        ]
    }
