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
import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.CloudFront.GetStreamingDistribution' every 60 seconds until a successful state is reached. An error is returned after 25 failed checks.
mkStreamingDistributionDeployed :: Wait.Wait GetStreamingDistribution
mkStreamingDistributionDeployed =
  Wait.Wait
    { Wait._waitName = "StreamingDistributionDeployed",
      Wait._waitAttempts = 25,
      Wait._waitDelay = 60,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "Deployed"
            Wait.AcceptSuccess
            ( gsdrsStreamingDistribution Lude.. Lens._Just Lude.. sdStatus
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.CloudFront.GetDistribution' every 60 seconds until a successful state is reached. An error is returned after 35 failed checks.
mkDistributionDeployed :: Wait.Wait GetDistribution
mkDistributionDeployed =
  Wait.Wait
    { Wait._waitName = "DistributionDeployed",
      Wait._waitAttempts = 35,
      Wait._waitDelay = 60,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "Deployed"
            Wait.AcceptSuccess
            ( gdrsDistribution Lude.. Lens._Just Lude.. dStatus
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.CloudFront.GetInvalidation' every 20 seconds until a successful state is reached. An error is returned after 30 failed checks.
mkInvalidationCompleted :: Wait.Wait GetInvalidation
mkInvalidationCompleted =
  Wait.Wait
    { Wait._waitName = "InvalidationCompleted",
      Wait._waitAttempts = 30,
      Wait._waitDelay = 20,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "Completed"
            Wait.AcceptSuccess
            ( girsInvalidation Lude.. Lens._Just Lude.. iStatus
                Lude.. Lens.to Lude.toText
            )
        ]
    }
