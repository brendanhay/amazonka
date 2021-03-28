{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Waiters
  (
    -- * InstanceTerminated
    mkInstanceTerminated,
    -- * DeploymentSuccessful
    mkDeploymentSuccessful,
    -- * InstanceStopped
    mkInstanceStopped,
    -- * InstanceOnline
    mkInstanceOnline,
    -- * AppExists
    mkAppExists,
    -- * InstanceRegistered
    mkInstanceRegistered,
  ) where

import Network.AWS.OpsWorks.DescribeApps
import Network.AWS.OpsWorks.DescribeDeployments
import Network.AWS.OpsWorks.DescribeInstances
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceTerminated :: Waiter.Wait DescribeInstances
mkInstanceTerminated
  = Waiter.Wait{Waiter._waitName = "InstanceTerminated",
                Waiter._waitAttempts = 40, Waiter._waitDelay = 15,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "terminated" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchError "ResourceNotFoundException" Waiter.AcceptSuccess,
                   Waiter.matchAny "booting" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "online" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "pending" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "rebooting" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "requested" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "running_setup" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "setup_failed" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "start_failed" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.OpsWorks.DescribeDeployments' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkDeploymentSuccessful :: Waiter.Wait DescribeDeployments
mkDeploymentSuccessful
  = Waiter.Wait{Waiter._waitName = "DeploymentSuccessful",
                Waiter._waitAttempts = 40, Waiter._waitDelay = 15,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "successful" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"deployments" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "failed" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"deployments" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceStopped :: Waiter.Wait DescribeInstances
mkInstanceStopped
  = Waiter.Wait{Waiter._waitName = "InstanceStopped",
                Waiter._waitAttempts = 40, Waiter._waitDelay = 15,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "stopped" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "booting" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "pending" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "rebooting" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "requested" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "running_setup" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "setup_failed" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "start_failed" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "stop_failed" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceOnline :: Waiter.Wait DescribeInstances
mkInstanceOnline
  = Waiter.Wait{Waiter._waitName = "InstanceOnline",
                Waiter._waitAttempts = 40, Waiter._waitDelay = 15,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "online" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "setup_failed" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "shutting_down" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "start_failed" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "stopped" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "stopping" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "terminating" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "terminated" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "stop_failed" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.OpsWorks.DescribeApps' every 1 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkAppExists :: Waiter.Wait DescribeApps
mkAppExists
  = Waiter.Wait{Waiter._waitName = "AppExists",
                Waiter._waitAttempts = 40, Waiter._waitDelay = 1,
                Waiter._waitAcceptors =
                  [Waiter.matchStatus 200 Waiter.AcceptSuccess,
                   Waiter.matchStatus 400 Waiter.AcceptFailure]}

-- | Polls 'Network.AWS.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceRegistered :: Waiter.Wait DescribeInstances
mkInstanceRegistered
  = Waiter.Wait{Waiter._waitName = "InstanceRegistered",
                Waiter._waitAttempts = 40, Waiter._waitDelay = 15,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "registered" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "setup_failed" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "shutting_down" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "stopped" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "stopping" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "terminating" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "terminated" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "stop_failed" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"instances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just)]}
