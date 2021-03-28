{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Waiters
  (
    -- * ProjectVersionRunning
    mkProjectVersionRunning,
    -- * ProjectVersionTrainingCompleted
    mkProjectVersionTrainingCompleted,
  ) where

import Network.AWS.Rekognition.DescribeProjectVersions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types as Types
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.Rekognition.DescribeProjectVersions' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkProjectVersionRunning :: Waiter.Wait DescribeProjectVersions
mkProjectVersionRunning
  = Waiter.Wait{Waiter._waitName = "ProjectVersionRunning",
                Waiter._waitAttempts = 40, Waiter._waitDelay = 30,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "RUNNING" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"projectVersionDescriptions" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "FAILED" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"projectVersionDescriptions" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.Rekognition.DescribeProjectVersions' every 120 seconds until a successful state is reached. An error is returned after 360 failed checks.
mkProjectVersionTrainingCompleted :: Waiter.Wait DescribeProjectVersions
mkProjectVersionTrainingCompleted
  = Waiter.Wait{Waiter._waitName = "ProjectVersionTrainingCompleted",
                Waiter._waitAttempts = 360, Waiter._waitDelay = 120,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "TRAINING_COMPLETED" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"projectVersionDescriptions" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "TRAINING_FAILED" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"projectVersionDescriptions" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just)]}
