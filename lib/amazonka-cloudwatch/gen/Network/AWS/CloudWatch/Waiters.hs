{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Waiters
  ( -- * CompositeAlarmExists
    mkCompositeAlarmExists,

    -- * AlarmExists
    mkAlarmExists,
  )
where

import Network.AWS.CloudWatch.DescribeAlarms
import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.CloudWatch.DescribeAlarms' every 5 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkCompositeAlarmExists :: Wait.Wait DescribeAlarms
mkCompositeAlarmExists =
  Wait.Wait
    { Wait._waitName = "CompositeAlarmExists",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchNonEmpty
            Lude.True
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( darsCompositeAlarms Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
            )
        ]
    }

-- | Polls 'Network.AWS.CloudWatch.DescribeAlarms' every 5 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkAlarmExists :: Wait.Wait DescribeAlarms
mkAlarmExists =
  Wait.Wait
    { Wait._waitName = "AlarmExists",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchNonEmpty
            Lude.True
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (darsMetricAlarms Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
            )
        ]
    }
