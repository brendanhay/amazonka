{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Waiters
  (
    -- * VaultNotExists
    mkVaultNotExists,
    -- * VaultExists
    mkVaultExists,
  ) where

import Network.AWS.Glacier.DescribeVault
import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.Glacier.DescribeVault' every 3 seconds until a successful state is reached. An error is returned after 15 failed checks.
mkVaultNotExists :: Waiter.Wait DescribeVault
mkVaultNotExists
  = Waiter.Wait{Waiter._waitName = "VaultNotExists",
                Waiter._waitAttempts = 15, Waiter._waitDelay = 3,
                Waiter._waitAcceptors =
                  [Waiter.matchStatus 200 Waiter.AcceptRetry,
                   Waiter.matchError "ResourceNotFoundException"
                     Waiter.AcceptSuccess]}

-- | Polls 'Network.AWS.Glacier.DescribeVault' every 3 seconds until a successful state is reached. An error is returned after 15 failed checks.
mkVaultExists :: Waiter.Wait DescribeVault
mkVaultExists
  = Waiter.Wait{Waiter._waitName = "VaultExists",
                Waiter._waitAttempts = 15, Waiter._waitDelay = 3,
                Waiter._waitAcceptors =
                  [Waiter.matchStatus 200 Waiter.AcceptSuccess,
                   Waiter.matchError "ResourceNotFoundException" Waiter.AcceptRetry]}
