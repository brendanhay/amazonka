{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Waiters
  ( -- * VaultNotExists
    mkVaultNotExists,

    -- * VaultExists
    mkVaultExists,
  )
where

import Network.AWS.Glacier.DescribeVault
import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.Glacier.DescribeVault' every 3 seconds until a successful state is reached. An error is returned after 15 failed checks.
mkVaultNotExists :: Wait.Wait DescribeVault
mkVaultNotExists =
  Wait.Wait
    { Wait._waitName = "VaultNotExists",
      Wait._waitAttempts = 15,
      Wait._waitDelay = 3,
      Wait._waitAcceptors =
        [ Wait.matchStatus 200 Wait.AcceptRetry,
          Wait.matchError "ResourceNotFoundException" Wait.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.Glacier.DescribeVault' every 3 seconds until a successful state is reached. An error is returned after 15 failed checks.
mkVaultExists :: Wait.Wait DescribeVault
mkVaultExists =
  Wait.Wait
    { Wait._waitName = "VaultExists",
      Wait._waitAttempts = 15,
      Wait._waitDelay = 3,
      Wait._waitAcceptors =
        [ Wait.matchStatus 200 Wait.AcceptSuccess,
          Wait.matchError "ResourceNotFoundException" Wait.AcceptRetry
        ]
    }
