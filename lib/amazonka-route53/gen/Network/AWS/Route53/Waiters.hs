{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Waiters
  ( -- * ResourceRecordSetsChanged
    mkResourceRecordSetsChanged,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.GetChange
import Network.AWS.Route53.Types
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.Route53.GetChange' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkResourceRecordSetsChanged :: Wait.Wait GetChange
mkResourceRecordSetsChanged =
  Wait.Wait
    { Wait._waitName = "ResourceRecordSetsChanged",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "INSYNC"
            Wait.AcceptSuccess
            (gcrsChangeInfo Lude.. ciStatus Lude.. Lens.to Lude.toText)
        ]
    }
