{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EMR.Waiters
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

module Network.AWS.EMR.Waiters where

import Network.AWS.EMR.DescribeCluster
import Network.AWS.EMR.Types
import Network.AWS.Waiters

clusterRunning :: Wait DescribeCluster
clusterRunning = Wait
    { _waitName      = "ClusterRunning"
    , _waitAttempts  = 60
    , _waitDelay     = 30
    , _waitAcceptors =
        [ matchAll CSRunning AcceptSuccess
            (dcrCluster . c1Status . csState . _Just)
        , matchAll CSWaiting AcceptSuccess
            (dcrCluster . c1Status . csState . _Just)
        , matchAll CSTerminating AcceptFailure
            (dcrCluster . c1Status . csState . _Just)
        , matchAll CSTerminated AcceptFailure
            (dcrCluster . c1Status . csState . _Just)
        , matchAll CSTerminatedWithErrors AcceptFailure
            (dcrCluster . c1Status . csState . _Just)
        ]
    }
