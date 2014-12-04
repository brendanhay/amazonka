{-# LANGUAGE TypeFamilies #-}

-- Module      : Network.AWS.Redshift.Waiters
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

module Network.AWS.Redshift.Waiters where

import Network.AWS.Redshift.DescribeClusterSnapshots
import Network.AWS.Redshift.DescribeClusters
import Network.AWS.Types

data ClusterAvailable = ClusterAvailable
    deriving (Show)

instance AWSWaiter ClusterAvailable where
    type Rq ClusterAvailable = DescribeClusters

    waiter ClusterAvailable = Waiter
        { _waitDelay     = 60
        , _waitAttempts  = 30
        , _waitAccept    = const True
        }

data ClusterDeleted = ClusterDeleted
    deriving (Show)

instance AWSWaiter ClusterDeleted where
    type Rq ClusterDeleted = DescribeClusters

    waiter ClusterDeleted = Waiter
        { _waitDelay     = 60
        , _waitAttempts  = 30
        , _waitAccept    = const True
        }

data SnapshotAvailable = SnapshotAvailable
    deriving (Show)

instance AWSWaiter SnapshotAvailable where
    type Rq SnapshotAvailable = DescribeClusterSnapshots

    waiter SnapshotAvailable = Waiter
        { _waitDelay     = 15
        , _waitAttempts  = 20
        , _waitAccept    = const True
        }
