{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Waiters where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.DescribeProjectVersions
import Network.AWS.Rekognition.Lens
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.Rekognition.DescribeProjectVersions' every 120 seconds until a successful state is reached. An error is returned after 360 failed checks.
newProjectVersionTrainingCompleted :: Waiter.Wait DescribeProjectVersions
newProjectVersionTrainingCompleted =
  Waiter.Wait
    { Waiter._waitName =
        "ProjectVersionTrainingCompleted",
      Waiter._waitAttempts = 360,
      Waiter._waitDelay = 120,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "TRAINING_COMPLETED"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeProjectVersionsResponse_projectVersionDescriptions
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. projectVersionDescription_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "TRAINING_FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeProjectVersionsResponse_projectVersionDescriptions
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. projectVersionDescription_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.Rekognition.DescribeProjectVersions' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
newProjectVersionRunning :: Waiter.Wait DescribeProjectVersions
newProjectVersionRunning =
  Waiter.Wait
    { Waiter._waitName =
        "ProjectVersionRunning",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "RUNNING"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeProjectVersionsResponse_projectVersionDescriptions
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. projectVersionDescription_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeProjectVersionsResponse_projectVersionDescriptions
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. projectVersionDescription_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }
