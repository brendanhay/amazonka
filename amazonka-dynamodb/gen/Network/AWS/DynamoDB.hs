-- Module      : Network.AWS.DynamoDB
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon DynamoDB is a fully managed NoSQL database service that provides
-- fast and predictable performance with seamless scalability. You can use
-- Amazon DynamoDB to create a database table that can store and retrieve any
-- amount of data, and serve any level of request traffic. Amazon DynamoDB
-- automatically spreads the data and traffic for the table over a sufficient
-- number of servers to handle the request capacity specified by the customer
-- and the amount of data stored, while maintaining consistent and fast
-- performance.
module Network.AWS.DynamoDB
    ( module Network.AWS.DynamoDB.BatchGetItem
    , module Network.AWS.DynamoDB.BatchWriteItem
    , module Network.AWS.DynamoDB.CreateTable
    , module Network.AWS.DynamoDB.DeleteItem
    , module Network.AWS.DynamoDB.DeleteTable
    , module Network.AWS.DynamoDB.DescribeTable
    , module Network.AWS.DynamoDB.GetItem
    , module Network.AWS.DynamoDB.ListTables
    , module Network.AWS.DynamoDB.PutItem
    , module Network.AWS.DynamoDB.Query
    , module Network.AWS.DynamoDB.Scan
    , module Network.AWS.DynamoDB.Types
    , module Network.AWS.DynamoDB.UpdateItem
    , module Network.AWS.DynamoDB.UpdateTable
    ) where

import Network.AWS.DynamoDB.BatchGetItem
import Network.AWS.DynamoDB.BatchWriteItem
import Network.AWS.DynamoDB.CreateTable
import Network.AWS.DynamoDB.DeleteItem
import Network.AWS.DynamoDB.DeleteTable
import Network.AWS.DynamoDB.DescribeTable
import Network.AWS.DynamoDB.GetItem
import Network.AWS.DynamoDB.ListTables
import Network.AWS.DynamoDB.PutItem
import Network.AWS.DynamoDB.Query
import Network.AWS.DynamoDB.Scan
import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.UpdateItem
import Network.AWS.DynamoDB.UpdateTable
