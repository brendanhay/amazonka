{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Waiters where

import Network.AWS.Kinesis.DescribeStream
import Network.AWS.Kinesis.Lens
import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.Kinesis.DescribeStream' every 10 seconds until a successful state is reached. An error is returned after 18 failed checks.
newStreamExists :: Waiter.Wait DescribeStream
newStreamExists =
  Waiter.Wait
    { Waiter._waitName = "StreamExists",
      Waiter._waitAttempts = 18,
      Waiter._waitDelay = 10,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "ACTIVE"
            Waiter.AcceptSuccess
            ( describeStreamResponse_streamDescription
                Prelude.. streamDescription_streamStatus
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.Kinesis.DescribeStream' every 10 seconds until a successful state is reached. An error is returned after 18 failed checks.
newStreamNotExists :: Waiter.Wait DescribeStream
newStreamNotExists =
  Waiter.Wait
    { Waiter._waitName = "StreamNotExists",
      Waiter._waitAttempts = 18,
      Waiter._waitDelay = 10,
      Waiter._waitAcceptors =
        [ Waiter.matchError
            "ResourceNotFoundException"
            Waiter.AcceptSuccess
        ]
    }
