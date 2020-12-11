{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Waiters
  ( -- * StreamExists
    mkStreamExists,

    -- * StreamNotExists
    mkStreamNotExists,
  )
where

import Network.AWS.Kinesis.DescribeStream
import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.Kinesis.DescribeStream' every 10 seconds until a successful state is reached. An error is returned after 18 failed checks.
mkStreamExists :: Wait.Wait DescribeStream
mkStreamExists =
  Wait.Wait
    { Wait._waitName = "StreamExists",
      Wait._waitAttempts = 18,
      Wait._waitDelay = 10,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "ACTIVE"
            Wait.AcceptSuccess
            ( dsrsStreamDescription Lude.. sdStreamStatus
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.Kinesis.DescribeStream' every 10 seconds until a successful state is reached. An error is returned after 18 failed checks.
mkStreamNotExists :: Wait.Wait DescribeStream
mkStreamNotExists =
  Wait.Wait
    { Wait._waitName = "StreamNotExists",
      Wait._waitAttempts = 18,
      Wait._waitDelay = 10,
      Wait._waitAcceptors =
        [Wait.matchError "ResourceNotFoundException" Wait.AcceptSuccess]
    }
