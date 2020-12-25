{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Waiters
  ( -- * TableNotExists
    mkTableNotExists,

    -- * TableExists
    mkTableExists,
  )
where

import Network.AWS.DynamoDB.DescribeTable
import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.DynamoDB.DescribeTable' every 20 seconds until a successful state is reached. An error is returned after 25 failed checks.
mkTableNotExists :: Waiter.Wait DescribeTable
mkTableNotExists =
  Waiter.Wait
    { Waiter._waitName = "TableNotExists",
      Waiter._waitAttempts = 25,
      Waiter._waitDelay = 20,
      Waiter._waitAcceptors =
        [ Waiter.matchError
            "ResourceNotFoundException"
            Waiter.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.DynamoDB.DescribeTable' every 20 seconds until a successful state is reached. An error is returned after 25 failed checks.
mkTableExists :: Waiter.Wait DescribeTable
mkTableExists =
  Waiter.Wait
    { Waiter._waitName = "TableExists",
      Waiter._waitAttempts = 25,
      Waiter._waitDelay = 20,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "ACTIVE"
            Waiter.AcceptSuccess
            ( Lens.field @"table" Core.. Lens._Just
                Core.. Lens.field @"tableStatus"
                Core.. Lens._Just
            ),
          Waiter.matchError "ResourceNotFoundException" Waiter.AcceptRetry
        ]
    }
