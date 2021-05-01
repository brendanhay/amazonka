{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Waiters where

import Network.AWS.CloudFront.GetDistribution
import Network.AWS.CloudFront.GetInvalidation
import Network.AWS.CloudFront.GetStreamingDistribution
import Network.AWS.CloudFront.Lens
import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.CloudFront.GetInvalidation' every 20 seconds until a successful state is reached. An error is returned after 30 failed checks.
newInvalidationCompleted :: Waiter.Wait GetInvalidation
newInvalidationCompleted =
  Waiter.Wait
    { Waiter._waitName =
        "InvalidationCompleted",
      Waiter._waitAttempts = 30,
      Waiter._waitDelay = 20,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Completed"
            Waiter.AcceptSuccess
            ( getInvalidationResponse_invalidation
                Prelude.. Lens._Just
                Prelude.. invalidation_status
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.CloudFront.GetDistribution' every 60 seconds until a successful state is reached. An error is returned after 35 failed checks.
newDistributionDeployed :: Waiter.Wait GetDistribution
newDistributionDeployed =
  Waiter.Wait
    { Waiter._waitName =
        "DistributionDeployed",
      Waiter._waitAttempts = 35,
      Waiter._waitDelay = 60,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Deployed"
            Waiter.AcceptSuccess
            ( getDistributionResponse_distribution
                Prelude.. Lens._Just
                Prelude.. distribution_status
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.CloudFront.GetStreamingDistribution' every 60 seconds until a successful state is reached. An error is returned after 25 failed checks.
newStreamingDistributionDeployed :: Waiter.Wait GetStreamingDistribution
newStreamingDistributionDeployed =
  Waiter.Wait
    { Waiter._waitName =
        "StreamingDistributionDeployed",
      Waiter._waitAttempts = 25,
      Waiter._waitDelay = 60,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Deployed"
            Waiter.AcceptSuccess
            ( getStreamingDistributionResponse_streamingDistribution
                Prelude.. Lens._Just
                Prelude.. streamingDistribution_status
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }
