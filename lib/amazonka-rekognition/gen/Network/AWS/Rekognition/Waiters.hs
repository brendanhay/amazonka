{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Waiters where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.DescribeProjectVersions
import Network.AWS.Rekognition.Types
import Network.AWS.Waiter

-- | Polls 'Network.AWS.Rekognition.DescribeProjectVersions' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
projectVersionRunning :: Wait DescribeProjectVersions
projectVersionRunning =
  Wait
    { _waitName = "ProjectVersionRunning",
      _waitAttempts = 40,
      _waitDelay = 30,
      _waitAcceptors =
        [ matchAll
            "RUNNING"
            AcceptSuccess
            ( folding (concatOf (dpvrsProjectVersionDescriptions . to toList))
                . pvdStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "FAILED"
            AcceptFailure
            ( folding (concatOf (dpvrsProjectVersionDescriptions . to toList))
                . pvdStatus
                . _Just
                . to toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.Rekognition.DescribeProjectVersions' every 120 seconds until a successful state is reached. An error is returned after 360 failed checks.
projectVersionTrainingCompleted :: Wait DescribeProjectVersions
projectVersionTrainingCompleted =
  Wait
    { _waitName = "ProjectVersionTrainingCompleted",
      _waitAttempts = 360,
      _waitDelay = 120,
      _waitAcceptors =
        [ matchAll
            "TRAINING_COMPLETED"
            AcceptSuccess
            ( folding (concatOf (dpvrsProjectVersionDescriptions . to toList))
                . pvdStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "TRAINING_FAILED"
            AcceptFailure
            ( folding (concatOf (dpvrsProjectVersionDescriptions . to toList))
                . pvdStatus
                . _Just
                . to toTextCI
            )
        ]
    }
