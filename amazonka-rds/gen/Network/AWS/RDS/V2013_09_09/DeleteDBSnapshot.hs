{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DeleteDBSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a DBSnapshot. If the snapshot is being copied, the copy operation
-- is terminated. The DBSnapshot must be in the available state to be deleted.
-- https://rds.amazon.com/ &DBSnapshotIdentifier=mydbsnapshot
-- &Version=2013-05-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-23T06%3A27%3A42.551Z &AWSAccessKeyId= &Signature= 3306
-- 2011-03-11T07:20:24.082Z mysql deleted us-east-1d general-public-license
-- 2010-07-16T00:06:59.107Z 60 simcoprod01 5.1.47 mysnapshot2 manual master
-- 627a43a1-8507-11e0-bd9b-a7b1ece36d51.
module Network.AWS.RDS.V2013_09_09.DeleteDBSnapshot
    (
    -- * Request
      DeleteDBSnapshot
    -- ** Request constructor
    , deleteDBSnapshot
    -- ** Request lenses
    , ddbsmDBSnapshotIdentifier

    -- * Response
    , DeleteDBSnapshotResponse
    -- ** Response lenses
    , dbsyDBSnapshot
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteDBSnapshot' request.
deleteDBSnapshot :: Text -- ^ 'ddbsmDBSnapshotIdentifier'
                 -> DeleteDBSnapshot
deleteDBSnapshot p1 = DeleteDBSnapshot
    { _ddbsmDBSnapshotIdentifier = p1
    }
{-# INLINE deleteDBSnapshot #-}

data DeleteDBSnapshot = DeleteDBSnapshot
    { _ddbsmDBSnapshotIdentifier :: Text
      -- ^ The DBSnapshot identifier. Constraints: Must be the name of an
      -- existing DB snapshot in the available state.
    } deriving (Show, Generic)

-- | The DBSnapshot identifier. Constraints: Must be the name of an existing DB
-- snapshot in the available state.
ddbsmDBSnapshotIdentifier :: Lens' DeleteDBSnapshot (Text)
ddbsmDBSnapshotIdentifier f x =
    f (_ddbsmDBSnapshotIdentifier x)
        <&> \y -> x { _ddbsmDBSnapshotIdentifier = y }
{-# INLINE ddbsmDBSnapshotIdentifier #-}

instance ToQuery DeleteDBSnapshot where
    toQuery = genericQuery def

data DeleteDBSnapshotResponse = DeleteDBSnapshotResponse
    { _dbsyDBSnapshot :: Maybe DBSnapshot
      -- ^ Contains the result of a successful invocation of the following
      -- actions: CreateDBSnapshot DeleteDBSnapshot This data type is used
      -- as a response element in the DescribeDBSnapshots action.
    } deriving (Show, Generic)

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBSnapshot DeleteDBSnapshot This data type is used as a response
-- element in the DescribeDBSnapshots action.
dbsyDBSnapshot :: Lens' DeleteDBSnapshotResponse (Maybe DBSnapshot)
dbsyDBSnapshot f x =
    f (_dbsyDBSnapshot x)
        <&> \y -> x { _dbsyDBSnapshot = y }
{-# INLINE dbsyDBSnapshot #-}

instance FromXML DeleteDBSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteDBSnapshot where
    type Sv DeleteDBSnapshot = RDS
    type Rs DeleteDBSnapshot = DeleteDBSnapshotResponse

    request = post "DeleteDBSnapshot"
    response _ = xmlResponse
