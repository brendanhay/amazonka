{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Waiters
  (
    -- * StepComplete
    mkStepComplete,
    -- * ClusterTerminated
    mkClusterTerminated,
    -- * ClusterRunning
    mkClusterRunning,
  ) where

import Network.AWS.EMR.DescribeCluster
import Network.AWS.EMR.DescribeStep
import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.EMR.DescribeStep' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkStepComplete :: Waiter.Wait DescribeStep
mkStepComplete
  = Waiter.Wait{Waiter._waitName = "StepComplete",
                Waiter._waitAttempts = 60, Waiter._waitDelay = 30,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "COMPLETED" Waiter.AcceptSuccess
                     (Lens.field @"step" Core.. Lens._Just Core..
                        Lens.field @"status" Core.. Lens._Just Core..
                          Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchAll "FAILED" Waiter.AcceptFailure
                     (Lens.field @"step" Core.. Lens._Just Core..
                        Lens.field @"status" Core.. Lens._Just Core..
                          Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchAll "CANCELLED" Waiter.AcceptFailure
                     (Lens.field @"step" Core.. Lens._Just Core..
                        Lens.field @"status" Core.. Lens._Just Core..
                          Lens.field @"state" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.EMR.DescribeCluster' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkClusterTerminated :: Waiter.Wait DescribeCluster
mkClusterTerminated
  = Waiter.Wait{Waiter._waitName = "ClusterTerminated",
                Waiter._waitAttempts = 60, Waiter._waitDelay = 30,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "TERMINATED" Waiter.AcceptSuccess
                     (Lens.field @"cluster" Core..
                        Lens.field @"status" Core.. Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchAll "TERMINATED_WITH_ERRORS" Waiter.AcceptFailure
                     (Lens.field @"cluster" Core..
                        Lens.field @"status" Core..
                          Lens.field @"state" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.EMR.DescribeCluster' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkClusterRunning :: Waiter.Wait DescribeCluster
mkClusterRunning
  = Waiter.Wait{Waiter._waitName = "ClusterRunning",
                Waiter._waitAttempts = 60, Waiter._waitDelay = 30,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "RUNNING" Waiter.AcceptSuccess
                     (Lens.field @"cluster" Core..
                        Lens.field @"status" Core.. Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchAll "WAITING" Waiter.AcceptSuccess
                     (Lens.field @"cluster" Core..
                        Lens.field @"status" Core.. Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchAll "TERMINATING" Waiter.AcceptFailure
                     (Lens.field @"cluster" Core..
                        Lens.field @"status" Core.. Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchAll "TERMINATED" Waiter.AcceptFailure
                     (Lens.field @"cluster" Core..
                        Lens.field @"status" Core.. Lens.field @"state" Core.. Lens._Just),
                   Waiter.matchAll "TERMINATED_WITH_ERRORS" Waiter.AcceptFailure
                     (Lens.field @"cluster" Core..
                        Lens.field @"status" Core..
                          Lens.field @"state" Core.. Lens._Just)]}
