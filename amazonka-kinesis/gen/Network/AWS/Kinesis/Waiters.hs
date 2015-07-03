{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Kinesis.Waiters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.Kinesis.Waiters where

import           Network.AWS.Kinesis.DescribeStream
import           Network.AWS.Kinesis.Types
import           Network.AWS.Prelude
import           Network.AWS.Waiter

streamExists :: Wait DescribeStream
streamExists =
    Wait
    { _waitName = "StreamExists"
    , _waitAttempts = 18
    , _waitDelay = 10
    , _waitAcceptors = [ matchAll
                             "ACTIVE"
                             AcceptSuccess
                             (dsrStreamDescription . sdStreamStatus . to toText)]
    }
