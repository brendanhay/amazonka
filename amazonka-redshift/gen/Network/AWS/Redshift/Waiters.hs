{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Waiters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Waiters where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.DescribeClusters
import           Network.AWS.Redshift.DescribeClusters
import           Network.AWS.Redshift.DescribeClusterSnapshots
import           Network.AWS.Redshift.Types
import           Network.AWS.Waiter

clusterDeleted :: Wait DescribeClusters
clusterDeleted =
    Wait
    { _waitName = "ClusterDeleted"
    , _waitAttempts = 30
    , _waitDelay = 60
    , _waitAcceptors = [ matchError "ClusterNotFound" AcceptSuccess
                       , matchAny
                             "creating"
                             AcceptFailure
                             (folding (concatOf dcrClusters) .
                              cluClusterStatus . _Just . to toText)
                       , matchAny
                             "rebooting"
                             AcceptFailure
                             (folding (concatOf dcrClusters) .
                              cluClusterStatus . _Just . to toText)]
    }

snapshotAvailable :: Wait DescribeClusterSnapshots
snapshotAvailable =
    Wait
    { _waitName = "SnapshotAvailable"
    , _waitAttempts = 20
    , _waitDelay = 15
    , _waitAcceptors = [ matchAll
                             "available"
                             AcceptSuccess
                             (folding (concatOf descSnapshots) .
                              snaStatus . _Just . to toText)
                       , matchAny
                             "failed"
                             AcceptFailure
                             (folding (concatOf descSnapshots) .
                              snaStatus . _Just . to toText)
                       , matchAny
                             "deleted"
                             AcceptFailure
                             (folding (concatOf descSnapshots) .
                              snaStatus . _Just . to toText)]
    }

clusterAvailable :: Wait DescribeClusters
clusterAvailable =
    Wait
    { _waitName = "ClusterAvailable"
    , _waitAttempts = 30
    , _waitDelay = 60
    , _waitAcceptors = [ matchAll
                             "available"
                             AcceptSuccess
                             (folding (concatOf dcrClusters) .
                              cluClusterStatus . _Just . to toText)
                       , matchAny
                             "deleting"
                             AcceptFailure
                             (folding (concatOf dcrClusters) .
                              cluClusterStatus . _Just . to toText)
                       , matchError "ClusterNotFound" AcceptRetry]
    }
