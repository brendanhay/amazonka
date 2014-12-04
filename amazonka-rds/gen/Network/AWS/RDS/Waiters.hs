{-# LANGUAGE TypeFamilies #-}

-- Module      : Network.AWS.RDS.Waiters
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

module Network.AWS.RDS.Waiters where

import Network.AWS.RDS.DescribeDBInstances
import Network.AWS.RDS.Types
import Network.AWS.Types

data DBInstanceAvailable = DBInstanceAvailable

instance AWSWaiter DBInstanceAvailable where
    type Rq DBInstanceAvailable = DescribeDBInstances

    waiter DBInstanceAvailable x = Waiter
        { _waitDelay     = 30
        , _waitAttempts  = 60
        , _waitOperation = x
        , _waitAccept    = const False
        }

data DBInstanceDeleted = DBInstanceDeleted

instance AWSWaiter DBInstanceDeleted where
    type Rq DBInstanceDeleted = DescribeDBInstances

    waiter DBInstanceDeleted x = Waiter
        { _waitDelay     = 30
        , _waitAttempts  = 60
        , _waitOperation = x
        , _waitAccept    = const False
        }
