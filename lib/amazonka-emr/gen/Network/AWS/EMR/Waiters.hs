{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Waiters
  ( -- * StepComplete
    mkStepComplete,

    -- * ClusterTerminated
    mkClusterTerminated,

    -- * ClusterRunning
    mkClusterRunning,
  )
where

import Network.AWS.EMR.DescribeCluster
import Network.AWS.EMR.DescribeStep
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.EMR.DescribeStep' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkStepComplete :: Wait.Wait DescribeStep
mkStepComplete =
  Wait.Wait
    { Wait._waitName = "StepComplete",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "COMPLETED"
            Wait.AcceptSuccess
            ( dsfrsStep Lude.. Lens._Just Lude.. sgStatus Lude.. Lens._Just
                Lude.. ssState
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAll
            "FAILED"
            Wait.AcceptFailure
            ( dsfrsStep Lude.. Lens._Just Lude.. sgStatus Lude.. Lens._Just
                Lude.. ssState
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAll
            "CANCELLED"
            Wait.AcceptFailure
            ( dsfrsStep Lude.. Lens._Just Lude.. sgStatus Lude.. Lens._Just
                Lude.. ssState
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.EMR.DescribeCluster' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkClusterTerminated :: Wait.Wait DescribeCluster
mkClusterTerminated =
  Wait.Wait
    { Wait._waitName = "ClusterTerminated",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "TERMINATED"
            Wait.AcceptSuccess
            ( dcrsCluster Lude.. cfStatus Lude.. csState Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAll
            "TERMINATED_WITH_ERRORS"
            Wait.AcceptFailure
            ( dcrsCluster Lude.. cfStatus Lude.. csState Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.EMR.DescribeCluster' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkClusterRunning :: Wait.Wait DescribeCluster
mkClusterRunning =
  Wait.Wait
    { Wait._waitName = "ClusterRunning",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "RUNNING"
            Wait.AcceptSuccess
            ( dcrsCluster Lude.. cfStatus Lude.. csState Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAll
            "WAITING"
            Wait.AcceptSuccess
            ( dcrsCluster Lude.. cfStatus Lude.. csState Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAll
            "TERMINATING"
            Wait.AcceptFailure
            ( dcrsCluster Lude.. cfStatus Lude.. csState Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAll
            "TERMINATED"
            Wait.AcceptFailure
            ( dcrsCluster Lude.. cfStatus Lude.. csState Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAll
            "TERMINATED_WITH_ERRORS"
            Wait.AcceptFailure
            ( dcrsCluster Lude.. cfStatus Lude.. csState Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }
