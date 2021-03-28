{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Waiters
  (
    -- * EnvironmentExists
    mkEnvironmentExists,
    -- * EnvironmentUpdated
    mkEnvironmentUpdated,
    -- * EnvironmentTerminated
    mkEnvironmentTerminated,
  ) where

import Network.AWS.ElasticBeanstalk.DescribeEnvironments
import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.ElasticBeanstalk.DescribeEnvironments' every 20 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkEnvironmentExists :: Waiter.Wait DescribeEnvironments
mkEnvironmentExists
  = Waiter.Wait{Waiter._waitName = "EnvironmentExists",
                Waiter._waitAttempts = 20, Waiter._waitDelay = 20,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "Ready" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"environments" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAll "Launching" Waiter.AcceptRetry
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"environments" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.ElasticBeanstalk.DescribeEnvironments' every 20 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkEnvironmentUpdated :: Waiter.Wait DescribeEnvironments
mkEnvironmentUpdated
  = Waiter.Wait{Waiter._waitName = "EnvironmentUpdated",
                Waiter._waitAttempts = 20, Waiter._waitDelay = 20,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "Ready" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"environments" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAll "Updating" Waiter.AcceptRetry
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"environments" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.ElasticBeanstalk.DescribeEnvironments' every 20 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkEnvironmentTerminated :: Waiter.Wait DescribeEnvironments
mkEnvironmentTerminated
  = Waiter.Wait{Waiter._waitName = "EnvironmentTerminated",
                Waiter._waitAttempts = 20, Waiter._waitDelay = 20,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "Terminated" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"environments" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAll "Terminating" Waiter.AcceptRetry
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"environments" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just)]}
