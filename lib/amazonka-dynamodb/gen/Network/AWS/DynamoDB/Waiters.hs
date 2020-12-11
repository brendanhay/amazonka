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
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.DynamoDB.DescribeTable' every 20 seconds until a successful state is reached. An error is returned after 25 failed checks.
mkTableNotExists :: Wait.Wait DescribeTable
mkTableNotExists =
  Wait.Wait
    { Wait._waitName = "TableNotExists",
      Wait._waitAttempts = 25,
      Wait._waitDelay = 20,
      Wait._waitAcceptors =
        [Wait.matchError "ResourceNotFoundException" Wait.AcceptSuccess]
    }

-- | Polls 'Network.AWS.DynamoDB.DescribeTable' every 20 seconds until a successful state is reached. An error is returned after 25 failed checks.
mkTableExists :: Wait.Wait DescribeTable
mkTableExists =
  Wait.Wait
    { Wait._waitName = "TableExists",
      Wait._waitAttempts = 25,
      Wait._waitDelay = 20,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "ACTIVE"
            Wait.AcceptSuccess
            ( drsTable Lude.. Lens._Just Lude.. tdTableStatus Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError "ResourceNotFoundException" Wait.AcceptRetry
        ]
    }
