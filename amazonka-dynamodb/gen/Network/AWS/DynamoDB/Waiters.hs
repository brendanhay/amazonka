{-# LANGUAGE TypeFamilies #-}

-- Module      : Network.AWS.DynamoDB.Waiters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.DynamoDB.Waiters where

import Network.AWS.DynamoDB.DescribeTable
import Network.AWS.DynamoDB.Types
import Network.AWS.Types

data TableExists = TableExists

instance AWSWaiter TableExists where
    type Rq TableExists = DescribeTable

    waiter TableExists x = Waiter
        { _waitDelay     = 20
        , _waitAttempts  = 25
        , _waitOperation = x
        , _waitAccept    = const False
        }

data TableNotExists = TableNotExists

instance AWSWaiter TableNotExists where
    type Rq TableNotExists = DescribeTable

    waiter TableNotExists x = Waiter
        { _waitDelay     = 20
        , _waitAttempts  = 25
        , _waitOperation = x
        , _waitAccept    = const False
        }
