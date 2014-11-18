{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DynamoDB.DescribeTable
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the table, including the current status of the
-- table, when it was created, the primary key schema, and any indexes on the
-- table.
--
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_DescribeTable.html>
module Network.AWS.DynamoDB.DescribeTable
    (
    -- * Request
      DescribeTable
    -- ** Request constructor
    , describeTable
    -- ** Request lenses
    , dt1TableName

    -- * Response
    , DescribeTableResponse
    -- ** Response constructor
    , describeTableResponse
    -- ** Response lenses
    , dtrTable
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DynamoDB.Types
import qualified GHC.Exts

newtype DescribeTable = DescribeTable
    { _dt1TableName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DescribeTable' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dt1TableName' @::@ 'Text'
--
describeTable :: Text -- ^ 'dt1TableName'
              -> DescribeTable
describeTable p1 = DescribeTable
    { _dt1TableName = p1
    }

-- | The name of the table to describe.
dt1TableName :: Lens' DescribeTable Text
dt1TableName = lens _dt1TableName (\s a -> s { _dt1TableName = a })

newtype DescribeTableResponse = DescribeTableResponse
    { _dtrTable :: Maybe TableDescription
    } deriving (Eq, Show, Generic)

-- | 'DescribeTableResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrTable' @::@ 'Maybe' 'TableDescription'
--
describeTableResponse :: DescribeTableResponse
describeTableResponse = DescribeTableResponse
    { _dtrTable = Nothing
    }

dtrTable :: Lens' DescribeTableResponse (Maybe TableDescription)
dtrTable = lens _dtrTable (\s a -> s { _dtrTable = a })

instance ToPath DescribeTable where
    toPath = const "/"

instance ToQuery DescribeTable where
    toQuery = const mempty

instance ToHeaders DescribeTable

instance ToJSON DescribeTable where
    toJSON DescribeTable{..} = object
        [ "TableName" .= _dt1TableName
        ]

instance AWSRequest DescribeTable where
    type Sv DescribeTable = DynamoDB
    type Rs DescribeTable = DescribeTableResponse

    request  = post "DescribeTable"
    response = jsonResponse

instance FromJSON DescribeTableResponse where
    parseJSON = withObject "DescribeTableResponse" $ \o -> DescribeTableResponse
        <$> o .:? "Table"
