{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Waiters
  ( -- * NodeAssociated
    mkNodeAssociated,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.DescribeNodeAssociationStatus
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.OpsWorksCM.DescribeNodeAssociationStatus' every 15 seconds until a successful state is reached. An error is returned after 15 failed checks.
mkNodeAssociated :: Wait.Wait DescribeNodeAssociationStatus
mkNodeAssociated =
  Wait.Wait
    { Wait._waitName = "NodeAssociated",
      Wait._waitAttempts = 15,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "SUCCESS"
            Wait.AcceptSuccess
            (dnasrsNodeAssociationStatus Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "FAILED"
            Wait.AcceptFailure
            (dnasrsNodeAssociationStatus Lude.. Lens.to Lude.toText)
        ]
    }
