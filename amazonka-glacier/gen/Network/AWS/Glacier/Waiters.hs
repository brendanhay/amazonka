{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Waiters where

import Network.AWS.Glacier.DescribeVault
import Network.AWS.Glacier.Lens
import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.Glacier.DescribeVault' every 3 seconds until a successful state is reached. An error is returned after 15 failed checks.
newVaultExists :: Waiter.Wait DescribeVault
newVaultExists =
  Waiter.Wait
    { Waiter._waitName = "VaultExists",
      Waiter._waitAttempts = 15,
      Waiter._waitDelay = 3,
      Waiter._waitAcceptors =
        [ Waiter.matchStatus 200 Waiter.AcceptSuccess,
          Waiter.matchError
            "ResourceNotFoundException"
            Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.Glacier.DescribeVault' every 3 seconds until a successful state is reached. An error is returned after 15 failed checks.
newVaultNotExists :: Waiter.Wait DescribeVault
newVaultNotExists =
  Waiter.Wait
    { Waiter._waitName = "VaultNotExists",
      Waiter._waitAttempts = 15,
      Waiter._waitDelay = 3,
      Waiter._waitAcceptors =
        [ Waiter.matchStatus 200 Waiter.AcceptRetry,
          Waiter.matchError
            "ResourceNotFoundException"
            Waiter.AcceptSuccess
        ]
    }
