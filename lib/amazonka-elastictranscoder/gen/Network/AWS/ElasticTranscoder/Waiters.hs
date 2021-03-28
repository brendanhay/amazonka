{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Waiters
  (
    -- * JobComplete
    mkJobComplete,
  ) where

import Network.AWS.ElasticTranscoder.ReadJob
import qualified Network.AWS.ElasticTranscoder.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.ElasticTranscoder.ReadJob' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkJobComplete :: Waiter.Wait ReadJob
mkJobComplete
  = Waiter.Wait{Waiter._waitName = "JobComplete",
                Waiter._waitAttempts = 120, Waiter._waitDelay = 30,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "Complete" Waiter.AcceptSuccess
                     (Lens.field @"job" Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAll "Canceled" Waiter.AcceptFailure
                     (Lens.field @"job" Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAll "Error" Waiter.AcceptFailure
                     (Lens.field @"job" Core.. Lens.field @"status" Core.. Lens._Just)]}
