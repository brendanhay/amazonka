{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Waiters where

import Network.AWS.CloudFront.GetDistribution
import Network.AWS.CloudFront.GetInvalidation
import Network.AWS.CloudFront.GetStreamingDistribution
import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.CloudFront.GetStreamingDistribution' every 60 seconds until a successful state is reached. An error is returned after 25 failed checks.
streamingDistributionDeployed :: Wait GetStreamingDistribution
streamingDistributionDeployed =
  Wait
    { _waitName = "StreamingDistributionDeployed"
    , _waitAttempts = 25
    , _waitDelay = 60
    , _waitAcceptors =
        [ matchAll
            "Deployed"
            AcceptSuccess
            (gsdrsStreamingDistribution . _Just . sdStatus . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.CloudFront.GetDistribution' every 60 seconds until a successful state is reached. An error is returned after 25 failed checks.
distributionDeployed :: Wait GetDistribution
distributionDeployed =
  Wait
    { _waitName = "DistributionDeployed"
    , _waitAttempts = 25
    , _waitDelay = 60
    , _waitAcceptors =
        [ matchAll
            "Deployed"
            AcceptSuccess
            (gdrsDistribution . _Just . dStatus . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.CloudFront.GetInvalidation' every 20 seconds until a successful state is reached. An error is returned after 30 failed checks.
invalidationCompleted :: Wait GetInvalidation
invalidationCompleted =
  Wait
    { _waitName = "InvalidationCompleted"
    , _waitAttempts = 30
    , _waitDelay = 20
    , _waitAcceptors =
        [ matchAll
            "Completed"
            AcceptSuccess
            (girsInvalidation . _Just . iStatus . to toTextCI)
        ]
    }

