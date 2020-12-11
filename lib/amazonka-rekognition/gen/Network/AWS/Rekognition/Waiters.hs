{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Waiters
  ( -- * ProjectVersionRunning
    mkProjectVersionRunning,

    -- * ProjectVersionTrainingCompleted
    mkProjectVersionTrainingCompleted,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.DescribeProjectVersions
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.Rekognition.DescribeProjectVersions' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkProjectVersionRunning :: Wait.Wait DescribeProjectVersions
mkProjectVersionRunning =
  Wait.Wait
    { Wait._waitName = "ProjectVersionRunning",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "RUNNING"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( dpvrsProjectVersionDescriptions Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. pvdStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "FAILED"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( dpvrsProjectVersionDescriptions Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. pvdStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.Rekognition.DescribeProjectVersions' every 120 seconds until a successful state is reached. An error is returned after 360 failed checks.
mkProjectVersionTrainingCompleted :: Wait.Wait DescribeProjectVersions
mkProjectVersionTrainingCompleted =
  Wait.Wait
    { Wait._waitName = "ProjectVersionTrainingCompleted",
      Wait._waitAttempts = 360,
      Wait._waitDelay = 120,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "TRAINING_COMPLETED"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( dpvrsProjectVersionDescriptions Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. pvdStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "TRAINING_FAILED"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( dpvrsProjectVersionDescriptions Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. pvdStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }
