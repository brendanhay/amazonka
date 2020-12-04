{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Waiters where

import Network.AWS.Lens
import Network.AWS.MediaLive.DescribeChannel
import Network.AWS.MediaLive.DescribeInput
import Network.AWS.MediaLive.DescribeMultiplex
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 5 seconds until a successful state is reached. An error is returned after 120 failed checks.
channelRunning :: Wait DescribeChannel
channelRunning =
  Wait
    { _waitName = "ChannelRunning",
      _waitAttempts = 120,
      _waitDelay = 5,
      _waitAcceptors =
        [ matchAll "RUNNING" AcceptSuccess (dcrsState . to toTextCI),
          matchAll "STARTING" AcceptRetry (dcrsState . to toTextCI),
          matchStatus 500 AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeInput' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
inputAttached :: Wait DescribeInput
inputAttached =
  Wait
    { _waitName = "InputAttached",
      _waitAttempts = 20,
      _waitDelay = 5,
      _waitAcceptors =
        [ matchAll "ATTACHED" AcceptSuccess (diirsState . to toTextCI),
          matchAll "DETACHED" AcceptRetry (diirsState . to toTextCI),
          matchStatus 500 AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 5 seconds until a successful state is reached. An error is returned after 120 failed checks.
multiplexRunning :: Wait DescribeMultiplex
multiplexRunning =
  Wait
    { _waitName = "MultiplexRunning",
      _waitAttempts = 120,
      _waitDelay = 5,
      _waitAcceptors =
        [ matchAll "RUNNING" AcceptSuccess (dmrsState . to toTextCI),
          matchAll "STARTING" AcceptRetry (dmrsState . to toTextCI),
          matchStatus 500 AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
multiplexDeleted :: Wait DescribeMultiplex
multiplexDeleted =
  Wait
    { _waitName = "MultiplexDeleted",
      _waitAttempts = 20,
      _waitDelay = 5,
      _waitAcceptors =
        [ matchAll "DELETED" AcceptSuccess (dmrsState . to toTextCI),
          matchAll "DELETING" AcceptRetry (dmrsState . to toTextCI),
          matchStatus 500 AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeInput' every 5 seconds until a successful state is reached. An error is returned after 84 failed checks.
inputDetached :: Wait DescribeInput
inputDetached =
  Wait
    { _waitName = "InputDetached",
      _waitAttempts = 84,
      _waitDelay = 5,
      _waitAcceptors =
        [ matchAll "DETACHED" AcceptSuccess (diirsState . to toTextCI),
          matchAll "CREATING" AcceptRetry (diirsState . to toTextCI),
          matchAll "ATTACHED" AcceptRetry (diirsState . to toTextCI),
          matchStatus 500 AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeInput' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
inputDeleted :: Wait DescribeInput
inputDeleted =
  Wait
    { _waitName = "InputDeleted",
      _waitAttempts = 20,
      _waitDelay = 5,
      _waitAcceptors =
        [ matchAll "DELETED" AcceptSuccess (diirsState . to toTextCI),
          matchAll "DELETING" AcceptRetry (diirsState . to toTextCI),
          matchStatus 500 AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
channelStopped :: Wait DescribeChannel
channelStopped =
  Wait
    { _waitName = "ChannelStopped",
      _waitAttempts = 60,
      _waitDelay = 5,
      _waitAcceptors =
        [ matchAll "IDLE" AcceptSuccess (dcrsState . to toTextCI),
          matchAll "STOPPING" AcceptRetry (dcrsState . to toTextCI),
          matchStatus 500 AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 3 seconds until a successful state is reached. An error is returned after 5 failed checks.
multiplexCreated :: Wait DescribeMultiplex
multiplexCreated =
  Wait
    { _waitName = "MultiplexCreated",
      _waitAttempts = 5,
      _waitDelay = 3,
      _waitAcceptors =
        [ matchAll "IDLE" AcceptSuccess (dmrsState . to toTextCI),
          matchAll "CREATING" AcceptRetry (dmrsState . to toTextCI),
          matchStatus 500 AcceptRetry,
          matchAll "CREATE_FAILED" AcceptFailure (dmrsState . to toTextCI)
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 3 seconds until a successful state is reached. An error is returned after 5 failed checks.
channelCreated :: Wait DescribeChannel
channelCreated =
  Wait
    { _waitName = "ChannelCreated",
      _waitAttempts = 5,
      _waitDelay = 3,
      _waitAcceptors =
        [ matchAll "IDLE" AcceptSuccess (dcrsState . to toTextCI),
          matchAll "CREATING" AcceptRetry (dcrsState . to toTextCI),
          matchStatus 500 AcceptRetry,
          matchAll "CREATE_FAILED" AcceptFailure (dcrsState . to toTextCI)
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 5 seconds until a successful state is reached. An error is returned after 84 failed checks.
channelDeleted :: Wait DescribeChannel
channelDeleted =
  Wait
    { _waitName = "ChannelDeleted",
      _waitAttempts = 84,
      _waitDelay = 5,
      _waitAcceptors =
        [ matchAll "DELETED" AcceptSuccess (dcrsState . to toTextCI),
          matchAll "DELETING" AcceptRetry (dcrsState . to toTextCI),
          matchStatus 500 AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 5 seconds until a successful state is reached. An error is returned after 28 failed checks.
multiplexStopped :: Wait DescribeMultiplex
multiplexStopped =
  Wait
    { _waitName = "MultiplexStopped",
      _waitAttempts = 28,
      _waitDelay = 5,
      _waitAcceptors =
        [ matchAll "IDLE" AcceptSuccess (dmrsState . to toTextCI),
          matchAll "STOPPING" AcceptRetry (dmrsState . to toTextCI),
          matchStatus 500 AcceptRetry
        ]
    }
