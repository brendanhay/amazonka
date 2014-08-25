{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.DynamoDB.V2012_08_10.ListTables where

import           Network.AWS.DynamoDB.V2012_08_10.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'ListTables' request.
listTables :: ListTables
listTables = ListTables
    { _ltiLimit = Nothing
    , _ltiExclusiveStartTableName = Nothing
    }

data ListTables = ListTables
    { _ltiLimit :: Maybe Integer
      -- ^ A maximum number of table names to return.
    , _ltiExclusiveStartTableName :: Maybe Text
      -- ^ The name of the table that starts the list. If you already ran a
      -- ListTables operation and received a LastEvaluatedTableName value
      -- in the response, use that value here to continue the list.
    } deriving (Show, Generic)

makeLenses ''ListTables

instance ToPath ListTables

instance ToQuery ListTables

instance ToHeaders ListTables

instance ToJSON ListTables

data ListTablesResponse = ListTablesResponse
    { _ltoLastEvaluatedTableName :: Maybe Text
      -- ^ The name of the last table in the current list, only if some
      -- tables for the account and endpoint have not been returned. This
      -- value does not exist in a response if all table names are already
      -- returned. Use this value as the ExclusiveStartTableName in a new
      -- request to continue the list until all the table names are
      -- returned.
    , _ltoTableNames :: [Text]
      -- ^ The names of the tables associated with the current account at
      -- the current endpoint.
    } deriving (Show, Generic)

makeLenses ''ListTablesResponse

instance FromJSON ListTablesResponse

instance AWSRequest ListTables where
    type Sv ListTables = DynamoDB
    type Rs ListTables = ListTablesResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListTables where
    next rq rs = (\x -> rq { _ltiExclusiveStartTableName = Just x })
        <$> (_ltoLastEvaluatedTableName rs)
