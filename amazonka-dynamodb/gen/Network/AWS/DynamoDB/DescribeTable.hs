{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeTable
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the table, including the current status of the
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
    , desTable
    , desStatus
    ) where

import           Network.AWS.DynamoDB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DescribeTable/ operation.
--
-- /See:/ 'describeTable' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desTableName'
newtype DescribeTable = DescribeTable'
    { _desTableName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTable' smart constructor.
describeTable :: Text -> DescribeTable
describeTable pTableName =
    DescribeTable'
    { _desTableName = pTableName
    }

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
                 DescribeTableResponse' <$>
                   (x .?> "Table") <*> (pure (fromEnum s)))

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

-- | Represents the output of a /DescribeTable/ operation.
--
-- /See:/ 'describeTableResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desTable'
--
-- * 'desStatus'
data DescribeTableResponse = DescribeTableResponse'
    { _desTable  :: !(Maybe TableDescription)
    , _desStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTableResponse' smart constructor.
describeTableResponse :: Int -> DescribeTableResponse
describeTableResponse pStatus =
    DescribeTableResponse'
    { _desTable = Nothing
    , _desStatus = pStatus
    }

-- | FIXME: Undocumented member.
desTable :: Lens' DescribeTableResponse (Maybe TableDescription)
desTable = lens _desTable (\ s a -> s{_desTable = a});

-- | FIXME: Undocumented member.
desStatus :: Lens' DescribeTableResponse Int
desStatus = lens _desStatus (\ s a -> s{_desStatus = a});
