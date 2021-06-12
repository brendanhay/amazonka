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

import qualified Network.AWS.Core as Core
import Network.AWS.Glacier.DescribeVault
import Network.AWS.Glacier.Lens
import Network.AWS.Glacier.Types
import qualified Network.AWS.Lens as Lens

-- | Polls 'Network.AWS.Glacier.DescribeVault' every 3 seconds until a successful state is reached. An error is returned after 15 failed checks.
newVaultExists :: Core.Wait DescribeVault
newVaultExists =
  Core.Wait
    { Core._waitName = "VaultExists",
      Core._waitAttempts = 15,
      Core._waitDelay = 3,
      Core._waitAcceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchError
            "ResourceNotFoundException"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.Glacier.DescribeVault' every 3 seconds until a successful state is reached. An error is returned after 15 failed checks.
newVaultNotExists :: Core.Wait DescribeVault
newVaultNotExists =
  Core.Wait
    { Core._waitName = "VaultNotExists",
      Core._waitAttempts = 15,
      Core._waitDelay = 3,
      Core._waitAcceptors =
        [ Core.matchStatus 200 Core.AcceptRetry,
          Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess
        ]
    }
