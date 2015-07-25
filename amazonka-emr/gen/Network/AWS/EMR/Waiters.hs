{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Waiters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Waiters where

import           Network.AWS.EMR.DescribeCluster
import           Network.AWS.EMR.Types
import           Network.AWS.Prelude
import           Network.AWS.Waiter

clusterRunning :: Wait DescribeCluster
clusterRunning =
    Wait
    { _waitName = "ClusterRunning"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors = [ matchAll
                             "RUNNING"
                             AcceptSuccess
                             (dcrsCluster .
                              cluStatus . csState . _Just . to toText)
                       , matchAll
                             "WAITING"
                             AcceptSuccess
                             (dcrsCluster .
                              cluStatus . csState . _Just . to toText)
                       , matchAll
                             "TERMINATING"
                             AcceptFailure
                             (dcrsCluster .
                              cluStatus . csState . _Just . to toText)
                       , matchAll
                             "TERMINATED"
                             AcceptFailure
                             (dcrsCluster .
                              cluStatus . csState . _Just . to toText)
                       , matchAll
                             "TERMINATED_WITH_ERRORS"
                             AcceptFailure
                             (dcrsCluster .
                              cluStatus . csState . _Just . to toText)]
    }
