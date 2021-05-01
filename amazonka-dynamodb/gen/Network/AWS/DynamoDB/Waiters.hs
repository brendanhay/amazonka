{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Waiters where

import Network.AWS.DynamoDB.DescribeTable
import Network.AWS.DynamoDB.Lens
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.DynamoDB.DescribeTable' every 20 seconds until a successful state is reached. An error is returned after 25 failed checks.
newTableNotExists :: Waiter.Wait DescribeTable
newTableNotExists =
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
newTableExists :: Waiter.Wait DescribeTable
newTableExists =
  Waiter.Wait
    { Waiter._waitName = "TableExists",
      Waiter._waitAttempts = 25,
      Waiter._waitDelay = 20,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "ACTIVE"
            Waiter.AcceptSuccess
            ( describeTableResponse_table Prelude.. Lens._Just
                Prelude.. tableDescription_tableStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "ResourceNotFoundException"
            Waiter.AcceptRetry
        ]
    }
