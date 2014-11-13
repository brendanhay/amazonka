-- Module      : Network.AWS.SDB
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
module Network.AWS.SDB
    ( module Network.AWS.SDB.BatchDeleteAttributes
    , module Network.AWS.SDB.BatchPutAttributes
    , module Network.AWS.SDB.CreateDomain
    , module Network.AWS.SDB.DeleteAttributes
    , module Network.AWS.SDB.DeleteDomain
    , module Network.AWS.SDB.DomainMetadata
    , module Network.AWS.SDB.GetAttributes
    , module Network.AWS.SDB.ListDomains
    , module Network.AWS.SDB.PutAttributes
    , module Network.AWS.SDB.Select
    , module Network.AWS.SDB.Types
    ) where

import Network.AWS.SDB.BatchDeleteAttributes
import Network.AWS.SDB.BatchPutAttributes
import Network.AWS.SDB.CreateDomain
import Network.AWS.SDB.DeleteAttributes
import Network.AWS.SDB.DeleteDomain
import Network.AWS.SDB.DomainMetadata
import Network.AWS.SDB.GetAttributes
import Network.AWS.SDB.ListDomains
import Network.AWS.SDB.PutAttributes
import Network.AWS.SDB.Select
import Network.AWS.SDB.Types
