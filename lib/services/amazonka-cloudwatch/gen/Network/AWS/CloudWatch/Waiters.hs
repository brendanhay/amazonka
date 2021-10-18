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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Polls 'Network.AWS.CloudWatch.DescribeAlarms' every 5 seconds until a successful state is reached. An error is returned after 40 failed checks.
newAlarmExists :: Core.Wait DescribeAlarms
newAlarmExists =
  Core.Wait
    { Core._waitName = "AlarmExists",
      Core._waitAttempts = 40,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchNonEmpty
            Prelude.True
            Core.AcceptSuccess
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
newCompositeAlarmExists :: Core.Wait DescribeAlarms
newCompositeAlarmExists =
  Core.Wait
    { Core._waitName = "CompositeAlarmExists",
      Core._waitAttempts = 40,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchNonEmpty
            Prelude.True
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeAlarmsResponse_compositeAlarms
                        Prelude.. Lens._Just
                    )
                )
            )
        ]
    }
