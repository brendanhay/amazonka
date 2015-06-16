{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.DynamoDB.DescribeTable
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns information about the table, including the current status of the
-- table, when it was created, the primary key schema, and any indexes on
-- the table.
--
-- If you issue a DescribeTable request immediately after a CreateTable
-- request, DynamoDB might return a ResourceNotFoundException. This is
-- because DescribeTable uses an eventually consistent query, and the
-- metadata for your table might not be available at that moment. Wait for
-- a few seconds, and then try the DescribeTable request again.
--
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_DescribeTable.html>
module Network.AWS.DynamoDB.DescribeTable
    (
    -- * Request
      DescribeTable
    -- ** Request constructor
    , describeTable
    -- ** Request lenses
    , desTableName

    -- * Response
    , DescribeTableResponse
    -- ** Response constructor
    , describeTableResponse
    -- ** Response lenses
    , dtrTable
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.DynamoDB.Types

-- | /See:/ 'describeTable' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desTableName'
newtype DescribeTable = DescribeTable'{_desTableName :: Text} deriving (Eq, Read, Show)

-- | 'DescribeTable' smart constructor.
describeTable :: Text -> DescribeTable
describeTable pTableName = DescribeTable'{_desTableName = pTableName};

-- | The name of the table to describe.
desTableName :: Lens' DescribeTable Text
desTableName = lens _desTableName (\ s a -> s{_desTableName = a});

instance AWSRequest DescribeTable where
        type Sv DescribeTable = DynamoDB
        type Rs DescribeTable = DescribeTableResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTableResponse' <$> (x .?> "Table"))

instance ToHeaders DescribeTable where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.DescribeTable" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DescribeTable where
        toJSON DescribeTable'{..}
          = object ["TableName" .= _desTableName]

instance ToPath DescribeTable where
        toPath = const "/"

instance ToQuery DescribeTable where
        toQuery = const mempty

-- | /See:/ 'describeTableResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrTable'
newtype DescribeTableResponse = DescribeTableResponse'{_dtrTable :: Maybe TableDescription} deriving (Eq, Read, Show)

-- | 'DescribeTableResponse' smart constructor.
describeTableResponse :: DescribeTableResponse
describeTableResponse = DescribeTableResponse'{_dtrTable = Nothing};

-- | FIXME: Undocumented member.
dtrTable :: Lens' DescribeTableResponse (Maybe TableDescription)
dtrTable = lens _dtrTable (\ s a -> s{_dtrTable = a});
