{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Waiters where

import Network.AWS.CloudWatch.DescribeAlarms
import Network.AWS.CloudWatch.Lens
import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.CloudWatch.DescribeAlarms' every 5 seconds until a successful state is reached. An error is returned after 40 failed checks.
newAlarmExists :: Waiter.Wait DescribeAlarms
newAlarmExists =
  Waiter.Wait
    { Waiter._waitName = "AlarmExists",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchNonEmpty
            Prelude.True
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeAlarmsResponse_metricAlarms
                        Prelude.. Lens._Just
                    )
                )
            )
        ]
    }

-- | Polls 'Network.AWS.CloudWatch.DescribeAlarms' every 5 seconds until a successful state is reached. An error is returned after 40 failed checks.
newCompositeAlarmExists :: Waiter.Wait DescribeAlarms
newCompositeAlarmExists =
  Waiter.Wait
    { Waiter._waitName =
        "CompositeAlarmExists",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchNonEmpty
            Prelude.True
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeAlarmsResponse_compositeAlarms
                        Prelude.. Lens._Just
                    )
                )
            )
        ]
    }
