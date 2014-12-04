{-# LANGUAGE TypeFamilies #-}

-- Module      : Network.AWS.CloudFront.Waiters
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

module Network.AWS.CloudFront.Waiters where

import Network.AWS.CloudFront.GetDistribution
import Network.AWS.CloudFront.GetInvalidation
import Network.AWS.CloudFront.GetStreamingDistribution
import Network.AWS.CloudFront.Types
import Network.AWS.Types

data DistributionDeployed = DistributionDeployed

instance AWSWaiter DistributionDeployed where
    type Rq DistributionDeployed = GetDistribution

    waiter DistributionDeployed x = Waiter
        { _waitDelay     = 60
        , _waitAttempts  = 25
        , _waitOperation = x
        , _waitAccept    = const False
        }

data InvalidationCompleted = InvalidationCompleted

instance AWSWaiter InvalidationCompleted where
    type Rq InvalidationCompleted = GetInvalidation

    waiter InvalidationCompleted x = Waiter
        { _waitDelay     = 20
        , _waitAttempts  = 30
        , _waitOperation = x
        , _waitAccept    = const False
        }

data StreamingDistributionDeployed = StreamingDistributionDeployed

instance AWSWaiter StreamingDistributionDeployed where
    type Rq StreamingDistributionDeployed = GetStreamingDistribution

    waiter StreamingDistributionDeployed x = Waiter
        { _waitDelay     = 60
        , _waitAttempts  = 25
        , _waitOperation = x
        , _waitAccept    = const False
        }
