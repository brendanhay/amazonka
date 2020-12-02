{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Waiters where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.DescribeClusters
import Network.AWS.Redshift.DescribeClusters
import Network.AWS.Redshift.DescribeClusters
import Network.AWS.Redshift.DescribeClusterSnapshots
import Network.AWS.Redshift.Types
import Network.AWS.Waiter

-- | Polls 'Network.AWS.Redshift.DescribeClusters' every 60 seconds until a successful state is reached. An error is returned after 30 failed checks.
clusterRestored :: Wait DescribeClusters
clusterRestored =
  Wait
    { _waitName = "ClusterRestored"
    , _waitAttempts = 30
    , _waitDelay = 60
    , _waitAcceptors =
        [ matchAll
            "completed"
            AcceptSuccess
            (folding (concatOf dcrsClusters) .
             cRestoreStatus . _Just . rsStatus . _Just . to toTextCI)
        , matchAny
            "deleting"
            AcceptFailure
            (folding (concatOf dcrsClusters) .
             cClusterStatus . _Just . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.Redshift.DescribeClusters' every 60 seconds until a successful state is reached. An error is returned after 30 failed checks.
clusterDeleted :: Wait DescribeClusters
clusterDeleted =
  Wait
    { _waitName = "ClusterDeleted"
    , _waitAttempts = 30
    , _waitDelay = 60
    , _waitAcceptors =
        [ matchError "ClusterNotFound" AcceptSuccess
        , matchAny
            "creating"
            AcceptFailure
            (folding (concatOf dcrsClusters) .
             cClusterStatus . _Just . to toTextCI)
        , matchAny
            "modifying"
            AcceptFailure
            (folding (concatOf dcrsClusters) .
             cClusterStatus . _Just . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.Redshift.DescribeClusterSnapshots' every 15 seconds until a successful state is reached. An error is returned after 20 failed checks.
snapshotAvailable :: Wait DescribeClusterSnapshots
snapshotAvailable =
  Wait
    { _waitName = "SnapshotAvailable"
    , _waitAttempts = 20
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "available"
            AcceptSuccess
            (folding (concatOf dcssrsSnapshots) . sStatus . _Just . to toTextCI)
        , matchAny
            "failed"
            AcceptFailure
            (folding (concatOf dcssrsSnapshots) . sStatus . _Just . to toTextCI)
        , matchAny
            "deleted"
            AcceptFailure
            (folding (concatOf dcssrsSnapshots) . sStatus . _Just . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.Redshift.DescribeClusters' every 60 seconds until a successful state is reached. An error is returned after 30 failed checks.
clusterAvailable :: Wait DescribeClusters
clusterAvailable =
  Wait
    { _waitName = "ClusterAvailable"
    , _waitAttempts = 30
    , _waitDelay = 60
    , _waitAcceptors =
        [ matchAll
            "available"
            AcceptSuccess
            (folding (concatOf dcrsClusters) .
             cClusterStatus . _Just . to toTextCI)
        , matchAny
            "deleting"
            AcceptFailure
            (folding (concatOf dcrsClusters) .
             cClusterStatus . _Just . to toTextCI)
        , matchError "ClusterNotFound" AcceptRetry
        ]
    }

