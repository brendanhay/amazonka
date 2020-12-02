{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Waiters where

import Network.AWS.DynamoDB.DescribeTable
import Network.AWS.DynamoDB.DescribeTable
import Network.AWS.DynamoDB.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.DynamoDB.DescribeTable' every 20 seconds until a successful state is reached. An error is returned after 25 failed checks.
tableNotExists :: Wait DescribeTable
tableNotExists =
  Wait
    { _waitName = "TableNotExists"
    , _waitAttempts = 25
    , _waitDelay = 20
    , _waitAcceptors = [matchError "ResourceNotFoundException" AcceptSuccess]
    }


-- | Polls 'Network.AWS.DynamoDB.DescribeTable' every 20 seconds until a successful state is reached. An error is returned after 25 failed checks.
tableExists :: Wait DescribeTable
tableExists =
  Wait
    { _waitName = "TableExists"
    , _waitAttempts = 25
    , _waitDelay = 20
    , _waitAcceptors =
        [ matchAll
            "ACTIVE"
            AcceptSuccess
            (drsTable . _Just . tdTableStatus . _Just . to toTextCI)
        , matchError "ResourceNotFoundException" AcceptRetry
        ]
    }

