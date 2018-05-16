{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Waiters where

import Network.AWS.Kinesis.DescribeStream
import Network.AWS.Kinesis.DescribeStream
import Network.AWS.Kinesis.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.Kinesis.DescribeStream' every 10 seconds until a successful state is reached. An error is returned after 18 failed checks.
streamExists :: Wait DescribeStream
streamExists =
  Wait
    { _waitName = "StreamExists"
    , _waitAttempts = 18
    , _waitDelay = 10
    , _waitAcceptors =
        [ matchAll
            "ACTIVE"
            AcceptSuccess
            (dsrsStreamDescription . sdStreamStatus . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.Kinesis.DescribeStream' every 10 seconds until a successful state is reached. An error is returned after 18 failed checks.
streamNotExists :: Wait DescribeStream
streamNotExists =
  Wait
    { _waitName = "StreamNotExists"
    , _waitAttempts = 18
    , _waitDelay = 10
    , _waitAcceptors = [matchError "ResourceNotFoundException" AcceptSuccess]
    }

