{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Waiters
  (
    -- * ChannelRunning
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
  ) where

import Network.AWS.MediaLive.DescribeChannel
import Network.AWS.MediaLive.DescribeInput
import Network.AWS.MediaLive.DescribeMultiplex
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 5 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkChannelRunning :: Waiter.Wait DescribeChannel
mkChannelRunning
  = Waiter.Wait{Waiter._waitName = "ChannelRunning",
                Waiter._waitAttempts = 120, Waiter._waitDelay = 5,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "RUNNING" Waiter.AcceptSuccess
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchAll "STARTING" Waiter.AcceptRetry
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchStatus 500 Waiter.AcceptRetry]}

-- | Polls 'Network.AWS.MediaLive.DescribeInput' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkInputAttached :: Waiter.Wait DescribeInput
mkInputAttached
  = Waiter.Wait{Waiter._waitName = "InputAttached",
                Waiter._waitAttempts = 20, Waiter._waitDelay = 5,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "ATTACHED" Waiter.AcceptSuccess
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchAll "DETACHED" Waiter.AcceptRetry
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchStatus 500 Waiter.AcceptRetry]}

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 5 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkMultiplexRunning :: Waiter.Wait DescribeMultiplex
mkMultiplexRunning
  = Waiter.Wait{Waiter._waitName = "MultiplexRunning",
                Waiter._waitAttempts = 120, Waiter._waitDelay = 5,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "RUNNING" Waiter.AcceptSuccess
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchAll "STARTING" Waiter.AcceptRetry
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchStatus 500 Waiter.AcceptRetry]}

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkMultiplexDeleted :: Waiter.Wait DescribeMultiplex
mkMultiplexDeleted
  = Waiter.Wait{Waiter._waitName = "MultiplexDeleted",
                Waiter._waitAttempts = 20, Waiter._waitDelay = 5,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "DELETED" Waiter.AcceptSuccess
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchAll "DELETING" Waiter.AcceptRetry
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchStatus 500 Waiter.AcceptRetry]}

-- | Polls 'Network.AWS.MediaLive.DescribeInput' every 5 seconds until a successful state is reached. An error is returned after 84 failed checks.
mkInputDetached :: Waiter.Wait DescribeInput
mkInputDetached
  = Waiter.Wait{Waiter._waitName = "InputDetached",
                Waiter._waitAttempts = 84, Waiter._waitDelay = 5,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "DETACHED" Waiter.AcceptSuccess
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchAll "CREATING" Waiter.AcceptRetry
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchAll "ATTACHED" Waiter.AcceptRetry
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchStatus 500 Waiter.AcceptRetry]}

-- | Polls 'Network.AWS.MediaLive.DescribeInput' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkInputDeleted :: Waiter.Wait DescribeInput
mkInputDeleted
  = Waiter.Wait{Waiter._waitName = "InputDeleted",
                Waiter._waitAttempts = 20, Waiter._waitDelay = 5,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "DELETED" Waiter.AcceptSuccess
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchAll "DELETING" Waiter.AcceptRetry
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchStatus 500 Waiter.AcceptRetry]}

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkChannelStopped :: Waiter.Wait DescribeChannel
mkChannelStopped
  = Waiter.Wait{Waiter._waitName = "ChannelStopped",
                Waiter._waitAttempts = 60, Waiter._waitDelay = 5,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "IDLE" Waiter.AcceptSuccess
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchAll "STOPPING" Waiter.AcceptRetry
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchStatus 500 Waiter.AcceptRetry]}

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 3 seconds until a successful state is reached. An error is returned after 5 failed checks.
mkMultiplexCreated :: Waiter.Wait DescribeMultiplex
mkMultiplexCreated
  = Waiter.Wait{Waiter._waitName = "MultiplexCreated",
                Waiter._waitAttempts = 5, Waiter._waitDelay = 3,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "IDLE" Waiter.AcceptSuccess
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchAll "CREATING" Waiter.AcceptRetry
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchStatus 500 Waiter.AcceptRetry,
                   Waiter.matchAll "CREATE_FAILED" Waiter.AcceptFailure
                     (Lens.field @"state" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 3 seconds until a successful state is reached. An error is returned after 5 failed checks.
mkChannelCreated :: Waiter.Wait DescribeChannel
mkChannelCreated
  = Waiter.Wait{Waiter._waitName = "ChannelCreated",
                Waiter._waitAttempts = 5, Waiter._waitDelay = 3,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "IDLE" Waiter.AcceptSuccess
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchAll "CREATING" Waiter.AcceptRetry
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchStatus 500 Waiter.AcceptRetry,
                   Waiter.matchAll "CREATE_FAILED" Waiter.AcceptFailure
                     (Lens.field @"state" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.MediaLive.DescribeChannel' every 5 seconds until a successful state is reached. An error is returned after 84 failed checks.
mkChannelDeleted :: Waiter.Wait DescribeChannel
mkChannelDeleted
  = Waiter.Wait{Waiter._waitName = "ChannelDeleted",
                Waiter._waitAttempts = 84, Waiter._waitDelay = 5,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "DELETED" Waiter.AcceptSuccess
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchAll "DELETING" Waiter.AcceptRetry
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchStatus 500 Waiter.AcceptRetry]}

-- | Polls 'Network.AWS.MediaLive.DescribeMultiplex' every 5 seconds until a successful state is reached. An error is returned after 28 failed checks.
mkMultiplexStopped :: Waiter.Wait DescribeMultiplex
mkMultiplexStopped
  = Waiter.Wait{Waiter._waitName = "MultiplexStopped",
                Waiter._waitAttempts = 28, Waiter._waitDelay = 5,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "IDLE" Waiter.AcceptSuccess
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchAll "STOPPING" Waiter.AcceptRetry
                     (Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchStatus 500 Waiter.AcceptRetry]}
