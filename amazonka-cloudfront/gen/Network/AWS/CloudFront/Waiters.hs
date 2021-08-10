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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Polls 'Network.AWS.CloudFront.GetInvalidation' every 20 seconds until a successful state is reached. An error is returned after 30 failed checks.
newInvalidationCompleted :: Core.Wait GetInvalidation
newInvalidationCompleted =
  Core.Wait
    { Core._waitName = "InvalidationCompleted",
      Core._waitAttempts = 30,
      Core._waitDelay = 20,
      Core._waitAcceptors =
        [ Core.matchAll
            "Completed"
            Core.AcceptSuccess
            ( getInvalidationResponse_invalidation
                Prelude.. Lens._Just
                Prelude.. invalidation_status
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.CloudFront.GetDistribution' every 60 seconds until a successful state is reached. An error is returned after 35 failed checks.
newDistributionDeployed :: Core.Wait GetDistribution
newDistributionDeployed =
  Core.Wait
    { Core._waitName = "DistributionDeployed",
      Core._waitAttempts = 35,
      Core._waitDelay = 60,
      Core._waitAcceptors =
        [ Core.matchAll
            "Deployed"
            Core.AcceptSuccess
            ( getDistributionResponse_distribution
                Prelude.. Lens._Just
                Prelude.. distribution_status
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.CloudFront.GetStreamingDistribution' every 60 seconds until a successful state is reached. An error is returned after 25 failed checks.
newStreamingDistributionDeployed :: Core.Wait GetStreamingDistribution
newStreamingDistributionDeployed =
  Core.Wait
    { Core._waitName =
        "StreamingDistributionDeployed",
      Core._waitAttempts = 25,
      Core._waitDelay = 60,
      Core._waitAcceptors =
        [ Core.matchAll
            "Deployed"
            Core.AcceptSuccess
            ( getStreamingDistributionResponse_streamingDistribution
                Prelude.. Lens._Just
                Prelude.. streamingDistribution_status
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }
