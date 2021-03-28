{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorksCM.Waiters
  (
    -- * NodeAssociated
    mkNodeAssociated,
  ) where

import Network.AWS.OpsWorksCM.DescribeNodeAssociationStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorksCM.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.OpsWorksCM.DescribeNodeAssociationStatus' every 15 seconds until a successful state is reached. An error is returned after 15 failed checks.
mkNodeAssociated :: Waiter.Wait DescribeNodeAssociationStatus
mkNodeAssociated
  = Waiter.Wait{Waiter._waitName = "NodeAssociated",
                Waiter._waitAttempts = 15, Waiter._waitDelay = 15,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "SUCCESS" Waiter.AcceptSuccess
                     (Lens.field @"nodeAssociationStatus"),
                   Waiter.matchAll "FAILED" Waiter.AcceptFailure
                     (Lens.field @"nodeAssociationStatus")]}
