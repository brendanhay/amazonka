{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

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
import Network.AWS.Waiter

clusterAvailable :: Wait DescribeClusters
clusterAvailable = Wait
    { _waitName      = "ClusterAvailable"
    , _waitAttempts  = 30
    , _waitDelay     = 60
    , _waitAcceptors =
        [ pathAll Success {"contents":["dcrClusters",{"contents":"cClusterStatus","type":"access"}],"type":"indexed"} "available"
        , pathAny Failure {"contents":["dcrClusters",{"contents":"cClusterStatus","type":"access"}],"type":"indexed"} "deleting"
        , error Retry null "ClusterNotFound"
        ]
    }

clusterDeleted :: Wait DescribeClusters
clusterDeleted = Wait
    { _waitName      = "ClusterDeleted"
    , _waitAttempts  = 30
    , _waitDelay     = 60
    , _waitAcceptors =
        [ error Success null "ClusterNotFound"
        , pathAny Failure {"contents":["dcrClusters",{"contents":"cClusterStatus","type":"access"}],"type":"indexed"} "creating"
        , pathAny Failure {"contents":["dcrClusters",{"contents":"cClusterStatus","type":"access"}],"type":"indexed"} "rebooting"
        ]
    }

snapshotAvailable :: Wait DescribeClusterSnapshots
snapshotAvailable = Wait
    { _waitName      = "SnapshotAvailable"
    , _waitAttempts  = 20
    , _waitDelay     = 15
    , _waitAcceptors =
        [ pathAll Success {"contents":["dcsrSnapshots",{"contents":"sStatus","type":"access"}],"type":"indexed"} "available"
        , pathAny Failure {"contents":["dcsrSnapshots",{"contents":"sStatus","type":"access"}],"type":"indexed"} "failed"
        , pathAny Failure {"contents":["dcsrSnapshots",{"contents":"sStatus","type":"access"}],"type":"indexed"} "deleted"
        ]
    }
