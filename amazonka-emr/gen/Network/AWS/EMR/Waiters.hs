{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Waiters where

import Network.AWS.EMR.DescribeCluster
import Network.AWS.EMR.DescribeCluster
import Network.AWS.EMR.DescribeStep
import Network.AWS.EMR.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.EMR.DescribeStep' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
stepComplete :: Wait DescribeStep
stepComplete =
  Wait
    { _waitName = "StepComplete"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchAll
            "COMPLETED"
            AcceptSuccess
            (dsrsStep . _Just . sStatus . _Just . ssState . _Just . to toTextCI)
        , matchAll
            "FAILED"
            AcceptFailure
            (dsrsStep . _Just . sStatus . _Just . ssState . _Just . to toTextCI)
        , matchAll
            "CANCELLED"
            AcceptFailure
            (dsrsStep . _Just . sStatus . _Just . ssState . _Just . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.EMR.DescribeCluster' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
clusterTerminated :: Wait DescribeCluster
clusterTerminated =
  Wait
    { _waitName = "ClusterTerminated"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchAll
            "TERMINATED"
            AcceptSuccess
            (dcrsCluster . cluStatus . csState . _Just . to toTextCI)
        , matchAll
            "TERMINATED_WITH_ERRORS"
            AcceptFailure
            (dcrsCluster . cluStatus . csState . _Just . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.EMR.DescribeCluster' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
clusterRunning :: Wait DescribeCluster
clusterRunning =
  Wait
    { _waitName = "ClusterRunning"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchAll
            "RUNNING"
            AcceptSuccess
            (dcrsCluster . cluStatus . csState . _Just . to toTextCI)
        , matchAll
            "WAITING"
            AcceptSuccess
            (dcrsCluster . cluStatus . csState . _Just . to toTextCI)
        , matchAll
            "TERMINATING"
            AcceptFailure
            (dcrsCluster . cluStatus . csState . _Just . to toTextCI)
        , matchAll
            "TERMINATED"
            AcceptFailure
            (dcrsCluster . cluStatus . csState . _Just . to toTextCI)
        , matchAll
            "TERMINATED_WITH_ERRORS"
            AcceptFailure
            (dcrsCluster . cluStatus . csState . _Just . to toTextCI)
        ]
    }

