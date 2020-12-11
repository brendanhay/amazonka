{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Waiters
  ( -- * JobComplete
    mkJobComplete,
  )
where

import Network.AWS.ElasticTranscoder.ReadJob
import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.ElasticTranscoder.ReadJob' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkJobComplete :: Wait.Wait ReadJob
mkJobComplete =
  Wait.Wait
    { Wait._waitName = "JobComplete",
      Wait._waitAttempts = 120,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "Complete"
            Wait.AcceptSuccess
            ( rjrsJob Lude.. jStatus Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAll
            "Canceled"
            Wait.AcceptFailure
            ( rjrsJob Lude.. jStatus Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAll
            "Error"
            Wait.AcceptFailure
            ( rjrsJob Lude.. jStatus Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }
