{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DynamoDB.Waiters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.DynamoDB.Waiters where

import Network.AWS.DynamoDB.DescribeTable
import Network.AWS.Types

tableExists :: Wait DescribeTable
tableExists = Wait
    { _waitName     = "TableExists"
    , _waitDelay    = 20
    , _waitAttempts = 25
    , _waitAccept   = const True
    }

tableNotExists :: Wait DescribeTable
tableNotExists = Wait
    { _waitName     = "TableNotExists"
    , _waitDelay    = 20
    , _waitAttempts = 25
    , _waitAccept   = const True
    }
