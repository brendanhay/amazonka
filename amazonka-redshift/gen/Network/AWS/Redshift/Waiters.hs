{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Redshift.Waiters
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
import Network.AWS.Redshift.Types
import Network.AWS.Waiters

clusterAvailable :: Wait DescribeClusters
clusterAvailable = Wait
    { _waitName      = "ClusterAvailable"
    , _waitAttempts  = 30
    , _waitDelay     = 60
    , _waitAcceptors =
        [ matchAll "available" AcceptSuccess
            (folding (concatOf dcrClusters) . cClusterStatus . _Just)
        , matchAny "deleting" AcceptFailure
            (folding (concatOf dcrClusters) . cClusterStatus . _Just)
        , matchError "ClusterNotFound" AcceptRetry
        ]
    }

clusterDeleted :: Wait DescribeClusters
clusterDeleted = Wait
    { _waitName      = "ClusterDeleted"
    , _waitAttempts  = 30
    , _waitDelay     = 60
    , _waitAcceptors =
        [ matchError "ClusterNotFound" AcceptSuccess
        , matchAny "creating" AcceptFailure
            (folding (concatOf dcrClusters) . cClusterStatus . _Just)
        , matchAny "rebooting" AcceptFailure
            (folding (concatOf dcrClusters) . cClusterStatus . _Just)
        ]
    }

snapshotAvailable :: Wait DescribeClusterSnapshots
snapshotAvailable = Wait
    { _waitName      = "SnapshotAvailable"
    , _waitAttempts  = 20
    , _waitDelay     = 15
    , _waitAcceptors =
        [ matchAll "available" AcceptSuccess
            (folding (concatOf dcsrSnapshots) . sStatus . _Just)
        , matchAny "failed" AcceptFailure
            (folding (concatOf dcsrSnapshots) . sStatus . _Just)
        , matchAny "deleted" AcceptFailure
            (folding (concatOf dcsrSnapshots) . sStatus . _Just)
        ]
    }
