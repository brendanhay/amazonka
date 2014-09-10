{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns an array of all the tables associated with the current account and
-- endpoint. List Tables This example requests a list of tables, starting with
-- a table named comp2 and ending after three table names have been returned.
-- { "LastEvaluatedTableName": "Thread", "TableNames":
-- ["Forum","Reply","Thread"] }.
module Network.AWS.DynamoDB
    (
    -- * Request
      ListTables
    -- ** Request constructor
    , mkListTables
    -- ** Request lenses
    , ltExclusiveStartTableName
    , ltLimit

    -- * Response
    , ListTablesResponse
    -- ** Response constructor
    , mkListTablesResponse
    -- ** Response lenses
    , ltrTableNames
    , ltrLastEvaluatedTableName
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Represents the input of a ListTables operation.
data ListTables = ListTables
    { _ltExclusiveStartTableName :: !(Maybe Text)
    , _ltLimit :: !(Maybe Integer)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListTables' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ExclusiveStartTableName ::@ @Maybe Text@
--
-- * @Limit ::@ @Maybe Integer@
--
mkListTables :: ListTables
mkListTables = ListTables
    { _ltExclusiveStartTableName = Nothing
    , _ltLimit = Nothing
    }

-- | The name of the table that starts the list. If you already ran a ListTables
-- operation and received a LastEvaluatedTableName value in the response, use
-- that value here to continue the list.
ltExclusiveStartTableName :: Lens' ListTables (Maybe Text)
ltExclusiveStartTableName =
    lens _ltExclusiveStartTableName
         (\s a -> s { _ltExclusiveStartTableName = a })

-- | A maximum number of table names to return.
ltLimit :: Lens' ListTables (Maybe Integer)
ltLimit = lens _ltLimit (\s a -> s { _ltLimit = a })

instance ToPath ListTables

instance ToQuery ListTables

instance ToHeaders ListTables

instance ToJSON ListTables

-- | Represents the output of a ListTables operation.
data ListTablesResponse = ListTablesResponse
    { _ltrTableNames :: [Text]
    , _ltrLastEvaluatedTableName :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListTablesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TableNames ::@ @[Text]@
--
-- * @LastEvaluatedTableName ::@ @Maybe Text@
--
mkListTablesResponse :: ListTablesResponse
mkListTablesResponse = ListTablesResponse
    { _ltrTableNames = mempty
    , _ltrLastEvaluatedTableName = Nothing
    }

-- | The names of the tables associated with the current account at the current
-- endpoint.
ltrTableNames :: Lens' ListTablesResponse [Text]
ltrTableNames = lens _ltrTableNames (\s a -> s { _ltrTableNames = a })

-- | The name of the last table in the current list, only if some tables for the
-- account and endpoint have not been returned. This value does not exist in a
-- response if all table names are already returned. Use this value as the
-- ExclusiveStartTableName in a new request to continue the list until all the
-- table names are returned.
ltrLastEvaluatedTableName :: Lens' ListTablesResponse (Maybe Text)
ltrLastEvaluatedTableName =
    lens _ltrLastEvaluatedTableName
         (\s a -> s { _ltrLastEvaluatedTableName = a })

instance FromJSON ListTablesResponse

instance AWSRequest ListTables where
    type Sv ListTables = DynamoDB
    type Rs ListTables = ListTablesResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListTables where
    next rq rs = (\x -> rq & ltExclusiveStartTableName ?~ x)
        <$> (rs ^. ltrLastEvaluatedTableName)
