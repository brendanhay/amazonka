{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudFront.Waiters
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

module Network.AWS.CloudFront.Waiters where

import Network.AWS.CloudFront.GetDistribution
import Network.AWS.CloudFront.GetInvalidation
import Network.AWS.CloudFront.GetStreamingDistribution
import Network.AWS.CloudFront.Types
import Network.AWS.Prelude
import Network.AWS.Waiters

streamingDistributionDeployed :: Wait GetStreamingDistribution
streamingDistributionDeployed = Wait{_waitName = "StreamingDistributionDeployed", _waitAttempts = 25, _waitDelay = 60, _waitAcceptors = [matchAll "Deployed" AcceptSuccess (gsdrStreamingDistribution . _Just . sdStatus . to toText)]};

distributionDeployed :: Wait GetDistribution
distributionDeployed = Wait{_waitName = "DistributionDeployed", _waitAttempts = 25, _waitDelay = 60, _waitAcceptors = [matchAll "Deployed" AcceptSuccess (gdrDistribution . _Just . disStatus . to toText)]};

invalidationCompleted :: Wait GetInvalidation
invalidationCompleted = Wait{_waitName = "InvalidationCompleted", _waitAttempts = 60, _waitDelay = 20, _waitAcceptors = [matchAll "Completed" AcceptSuccess (girInvalidation . _Just . invStatus . to toText)]};
