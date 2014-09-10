-- Module      : Network.AWS.SimpleDB
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon SimpleDB is a highly available and flexible non-relational data
-- store that offloads the work of database administration. Developers simply
-- store and query data items via web services requests and Amazon SimpleDB
-- does the rest. Unbound by the strict requirements of a relational database,
-- Amazon SimpleDB is optimized to provide high availability and flexibility,
-- with little or no administrative burden. Behind the scenes, Amazon SimpleDB
-- creates and manages multiple geographically distributed replicas of your
-- data automatically to enable high availability and data durability. The
-- service charges you only for the resources actually consumed in storing
-- your data and serving your requests. You can change your data model on the
-- fly, and data is automatically indexed for you. With Amazon SimpleDB, you
-- can focus on application development without worrying about infrastructure
-- provisioning, high availability, software maintenance, schema and index
-- management, or performance tuning.
module Network.AWS.SimpleDB
    ( module Network.AWS.SimpleDB.BatchDeleteAttributes
    , module Network.AWS.SimpleDB.BatchPutAttributes
    , module Network.AWS.SimpleDB.CreateDomain
    , module Network.AWS.SimpleDB.DeleteAttributes
    , module Network.AWS.SimpleDB.DeleteDomain
    , module Network.AWS.SimpleDB.DomainMetadata
    , module Network.AWS.SimpleDB.GetAttributes
    , module Network.AWS.SimpleDB.ListDomains
    , module Network.AWS.SimpleDB.PutAttributes
    , module Network.AWS.SimpleDB.Select
    , module Network.AWS.SimpleDB.Types
    ) where

import Network.AWS.SimpleDB.BatchDeleteAttributes
import Network.AWS.SimpleDB.BatchPutAttributes
import Network.AWS.SimpleDB.CreateDomain
import Network.AWS.SimpleDB.DeleteAttributes
import Network.AWS.SimpleDB.DeleteDomain
import Network.AWS.SimpleDB.DomainMetadata
import Network.AWS.SimpleDB.GetAttributes
import Network.AWS.SimpleDB.ListDomains
import Network.AWS.SimpleDB.PutAttributes
import Network.AWS.SimpleDB.Select
import Network.AWS.SimpleDB.Types
