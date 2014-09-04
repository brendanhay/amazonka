{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.V2012_08_10.ListTables
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
module Network.AWS.DynamoDB.V2012_08_10.ListTables
    (
    -- * Request
      ListTables
    -- ** Request constructor
    , mkListTablesInput
    -- ** Request lenses
    , ltiExclusiveStartTableName
    , ltiLimit

    -- * Response
    , ListTablesResponse
    -- ** Response lenses
    , ltoTableNames
    , ltoLastEvaluatedTableName
    ) where

import           Network.AWS.DynamoDB.V2012_08_10.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListTables' request.
mkListTablesInput :: ListTables
mkListTablesInput = ListTables
    { _ltiExclusiveStartTableName = Nothing
    , _ltiLimit = Nothing
    }
{-# INLINE mkListTablesInput #-}

data ListTables = ListTables
    { _ltiExclusiveStartTableName :: Maybe Text
      -- ^ The name of the table that starts the list. If you already ran a
      -- ListTables operation and received a LastEvaluatedTableName value
      -- in the response, use that value here to continue the list.
    , _ltiLimit :: Maybe Integer
      -- ^ A maximum number of table names to return.
    } deriving (Show, Generic)

-- | The name of the table that starts the list. If you already ran a ListTables
-- operation and received a LastEvaluatedTableName value in the response, use
-- that value here to continue the list.
ltiExclusiveStartTableName :: Lens' ListTables (Maybe Text)
ltiExclusiveStartTableName = lens _ltiExclusiveStartTableName (\s a -> s { _ltiExclusiveStartTableName = a })
{-# INLINE ltiExclusiveStartTableName #-}

-- | A maximum number of table names to return.
ltiLimit :: Lens' ListTables (Maybe Integer)
ltiLimit = lens _ltiLimit (\s a -> s { _ltiLimit = a })
{-# INLINE ltiLimit #-}

instance ToPath ListTables

instance ToQuery ListTables

instance ToHeaders ListTables

instance ToJSON ListTables

data ListTablesResponse = ListTablesResponse
    { _ltoTableNames :: [Text]
      -- ^ The names of the tables associated with the current account at
      -- the current endpoint.
    , _ltoLastEvaluatedTableName :: Maybe Text
      -- ^ The name of the last table in the current list, only if some
      -- tables for the account and endpoint have not been returned. This
      -- value does not exist in a response if all table names are already
      -- returned. Use this value as the ExclusiveStartTableName in a new
      -- request to continue the list until all the table names are
      -- returned.
    } deriving (Show, Generic)

-- | The names of the tables associated with the current account at the current
-- endpoint.
ltoTableNames :: Lens' ListTablesResponse ([Text])
ltoTableNames = lens _ltoTableNames (\s a -> s { _ltoTableNames = a })
{-# INLINE ltoTableNames #-}

-- | The name of the last table in the current list, only if some tables for the
-- account and endpoint have not been returned. This value does not exist in a
-- response if all table names are already returned. Use this value as the
-- ExclusiveStartTableName in a new request to continue the list until all the
-- table names are returned.
ltoLastEvaluatedTableName :: Lens' ListTablesResponse (Maybe Text)
ltoLastEvaluatedTableName = lens _ltoLastEvaluatedTableName (\s a -> s { _ltoLastEvaluatedTableName = a })
{-# INLINE ltoLastEvaluatedTableName #-}

instance FromJSON ListTablesResponse

instance AWSRequest ListTables where
    type Sv ListTables = DynamoDB
    type Rs ListTables = ListTablesResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListTables where
    next rq rs = (\x -> rq { _ltiExclusiveStartTableName = Just x })
        <$> (_ltoLastEvaluatedTableName rs)
