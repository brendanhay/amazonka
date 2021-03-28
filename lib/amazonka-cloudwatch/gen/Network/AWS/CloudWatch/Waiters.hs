{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatch.Waiters
  (
    -- * CompositeAlarmExists
    mkCompositeAlarmExists,
    -- * AlarmExists
    mkAlarmExists,
  ) where

import Network.AWS.CloudWatch.DescribeAlarms
import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.CloudWatch.DescribeAlarms' every 5 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkCompositeAlarmExists :: Waiter.Wait DescribeAlarms
mkCompositeAlarmExists
  = Waiter.Wait{Waiter._waitName = "CompositeAlarmExists",
                Waiter._waitAttempts = 40, Waiter._waitDelay = 5,
                Waiter._waitAcceptors =
                  [Waiter.matchNonEmpty Core.True Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"compositeAlarms" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Core._isNonEmpty)]}

-- | Polls 'Network.AWS.CloudWatch.DescribeAlarms' every 5 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkAlarmExists :: Waiter.Wait DescribeAlarms
mkAlarmExists
  = Waiter.Wait{Waiter._waitName = "AlarmExists",
                Waiter._waitAttempts = 40, Waiter._waitDelay = 5,
                Waiter._waitAcceptors =
                  [Waiter.matchNonEmpty Core.True Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"metricAlarms" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Core._isNonEmpty)]}
