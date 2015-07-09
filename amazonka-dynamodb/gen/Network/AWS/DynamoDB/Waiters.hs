{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Waiters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Waiters where

import           Network.AWS.DynamoDB.DescribeTable
import           Network.AWS.DynamoDB.DescribeTable
import           Network.AWS.DynamoDB.Types
import           Network.AWS.Prelude
import           Network.AWS.Waiter

tableNotExists :: Wait DescribeTable
tableNotExists =
    Wait
    { _waitName = "TableNotExists"
    , _waitAttempts = 25
    , _waitDelay = 20
    , _waitAcceptors = [matchError "ResourceNotFoundException" AcceptSuccess]
    }

tableExists :: Wait DescribeTable
tableExists =
    Wait
    { _waitName = "TableExists"
    , _waitAttempts = 25
    , _waitDelay = 20
    , _waitAcceptors = [ matchAll
                             "ACTIVE"
                             AcceptSuccess
                             (desTable .
                              _Just . tdTableStatus . _Just . to toText)
                       , matchError "ResourceNotFoundException" AcceptRetry]
    }
