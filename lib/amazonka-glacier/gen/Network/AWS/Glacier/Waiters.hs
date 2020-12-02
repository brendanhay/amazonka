{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Waiters where

import Network.AWS.Glacier.DescribeVault
import Network.AWS.Glacier.DescribeVault
import Network.AWS.Glacier.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.Glacier.DescribeVault' every 3 seconds until a successful state is reached. An error is returned after 15 failed checks.
vaultNotExists :: Wait DescribeVault
vaultNotExists =
  Wait
    { _waitName = "VaultNotExists"
    , _waitAttempts = 15
    , _waitDelay = 3
    , _waitAcceptors =
        [ matchStatus 200 AcceptRetry
        , matchError "ResourceNotFoundException" AcceptSuccess
        ]
    }


-- | Polls 'Network.AWS.Glacier.DescribeVault' every 3 seconds until a successful state is reached. An error is returned after 15 failed checks.
vaultExists :: Wait DescribeVault
vaultExists =
  Wait
    { _waitName = "VaultExists"
    , _waitAttempts = 15
    , _waitDelay = 3
    , _waitAcceptors =
        [ matchStatus 200 AcceptSuccess
        , matchError "ResourceNotFoundException" AcceptRetry
        ]
    }

