{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Waiters
  ( -- * StreamingDistributionDeployed
    mkStreamingDistributionDeployed,

    -- * DistributionDeployed
    mkDistributionDeployed,

    -- * InvalidationCompleted
    mkInvalidationCompleted,
  )
where

import Network.AWS.CloudFront.GetDistribution
import Network.AWS.CloudFront.GetInvalidation
import Network.AWS.CloudFront.GetStreamingDistribution
import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.CloudFront.GetStreamingDistribution' every 60 seconds until a successful state is reached. An error is returned after 25 failed checks.
mkStreamingDistributionDeployed :: Waiter.Wait GetStreamingDistribution
mkStreamingDistributionDeployed =
  Waiter.Wait
    { Waiter._waitName = "StreamingDistributionDeployed",
      Waiter._waitAttempts = 25,
      Waiter._waitDelay = 60,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Deployed"
            Waiter.AcceptSuccess
            ( Lens.field @"streamingDistribution" Core.. Lens._Just
                Core.. Lens.field @"status"
            )
        ]
    }

-- | Polls 'Network.AWS.CloudFront.GetDistribution' every 60 seconds until a successful state is reached. An error is returned after 35 failed checks.
mkDistributionDeployed :: Waiter.Wait GetDistribution
mkDistributionDeployed =
  Waiter.Wait
    { Waiter._waitName = "DistributionDeployed",
      Waiter._waitAttempts = 35,
      Waiter._waitDelay = 60,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Deployed"
            Waiter.AcceptSuccess
            ( Lens.field @"distribution" Core.. Lens._Just
                Core.. Lens.field @"status"
            )
        ]
    }

-- | Polls 'Network.AWS.CloudFront.GetInvalidation' every 20 seconds until a successful state is reached. An error is returned after 30 failed checks.
mkInvalidationCompleted :: Waiter.Wait GetInvalidation
mkInvalidationCompleted =
  Waiter.Wait
    { Waiter._waitName = "InvalidationCompleted",
      Waiter._waitAttempts = 30,
      Waiter._waitDelay = 20,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Completed"
            Waiter.AcceptSuccess
            ( Lens.field @"invalidation" Core.. Lens._Just
                Core.. Lens.field @"status"
            )
        ]
    }
