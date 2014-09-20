{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.DeleteTable
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteTable operation deletes a table and all of its items. After a
-- DeleteTable request, the specified table is in the DELETING state until
-- DynamoDB completes the deletion. If the table is in the ACTIVE state, you
-- can delete it. If a table is in CREATING or UPDATING states, then DynamoDB
-- returns a ResourceInUseException. If the specified table does not exist,
-- DynamoDB returns a ResourceNotFoundException. If table is already in the
-- DELETING state, no error is returned. DynamoDB might continue to accept
-- data read and write operations, such as GetItem and PutItem, on a table in
-- the DELETING state until the table deletion is complete. When you delete a
-- table, any indexes on that table are also deleted. Use the DescribeTable
-- API to check the status of the table. Delete a Table This example deletes
-- the Reply table. { "TableDescription": { "ItemCount": 0,
-- "ProvisionedThroughput": { "NumberOfDecreasesToday": 0,
-- "ReadCapacityUnits": 5, "WriteCapacityUnits": 5 }, "TableName": "Reply",
-- "TableSizeBytes": 0, "TableStatus": "DELETING" } }.
module Network.AWS.DynamoDB.DeleteTable
    (
    -- * Request
      DeleteTable
    -- ** Request constructor
    , deleteTable
    -- ** Request lenses
    , dtTableName

    -- * Response
    , DeleteTableResponse
    -- ** Response constructor
    , deleteTableResponse
    -- ** Response lenses
    , dtrTableDescription
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Represents the input of a DeleteTable operation.
newtype DeleteTable = DeleteTable
    { _dtTableName :: Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteTable' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TableName ::@ @Text@
--
deleteTable :: Text -- ^ 'dtTableName'
            -> DeleteTable
deleteTable p1 = DeleteTable
    { _dtTableName = p1
    }

-- | The name of the table to delete.
dtTableName :: Lens' DeleteTable Text
dtTableName = lens _dtTableName (\s a -> s { _dtTableName = a })

instance ToPath DeleteTable

instance ToQuery DeleteTable

instance ToHeaders DeleteTable

instance ToJSON DeleteTable

-- | Represents the output of a DeleteTable operation.
newtype DeleteTableResponse = DeleteTableResponse
    { _dtrTableDescription :: Maybe TableDescription
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteTableResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TableDescription ::@ @Maybe TableDescription@
--
deleteTableResponse :: DeleteTableResponse
deleteTableResponse = DeleteTableResponse
    { _dtrTableDescription = Nothing
    }

-- | Represents the properties of a table.
dtrTableDescription :: Lens' DeleteTableResponse (Maybe TableDescription)
dtrTableDescription =
    lens _dtrTableDescription (\s a -> s { _dtrTableDescription = a })

instance FromJSON DeleteTableResponse

instance AWSRequest DeleteTable where
    type Sv DeleteTable = DynamoDB
    type Rs DeleteTable = DeleteTableResponse

    request = get
    response _ = jsonResponse
