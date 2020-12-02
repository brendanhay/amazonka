{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeTable
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the table, including the current status of the table, when it was created, the primary key schema, and any indexes on the table.
--
--
module Network.AWS.DynamoDB.DescribeTable
    (
    -- * Creating a Request
      describeTable
    , DescribeTable
    -- * Request Lenses
    , dTableName

    -- * Destructuring the Response
    , describeTableResponse
    , DescribeTableResponse
    -- * Response Lenses
    , drsTable
    , drsResponseStatus
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @DescribeTable@ operation.
--
--
--
-- /See:/ 'describeTable' smart constructor.
newtype DescribeTable = DescribeTable'
  { _dTableName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dTableName' - The name of the table to describe.
describeTable
    :: Text -- ^ 'dTableName'
    -> DescribeTable
describeTable pTableName_ = DescribeTable' {_dTableName = pTableName_}


-- | The name of the table to describe.
dTableName :: Lens' DescribeTable Text
dTableName = lens _dTableName (\ s a -> s{_dTableName = a})

instance AWSRequest DescribeTable where
        type Rs DescribeTable = DescribeTableResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTableResponse' <$>
                   (x .?> "Table") <*> (pure (fromEnum s)))

instance Hashable DescribeTable where

instance NFData DescribeTable where

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
          = object
              (catMaybes [Just ("TableName" .= _dTableName)])

instance ToPath DescribeTable where
        toPath = const "/"

instance ToQuery DescribeTable where
        toQuery = const mempty

-- | Represents the output of a @DescribeTable@ operation.
--
--
--
-- /See:/ 'describeTableResponse' smart constructor.
data DescribeTableResponse = DescribeTableResponse'
  { _drsTable          :: !(Maybe TableDescription)
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTableResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsTable' - The properties of the table.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeTableResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeTableResponse
describeTableResponse pResponseStatus_ =
  DescribeTableResponse'
    {_drsTable = Nothing, _drsResponseStatus = pResponseStatus_}


-- | The properties of the table.
drsTable :: Lens' DescribeTableResponse (Maybe TableDescription)
drsTable = lens _drsTable (\ s a -> s{_drsTable = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeTableResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeTableResponse where
