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
-- Module      : Network.AWS.DynamoDB.DeleteTable
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The /DeleteTable/ operation deletes a table and all of its items. After
-- a /DeleteTable/ request, the specified table is in the 'DELETING' state
-- until DynamoDB completes the deletion. If the table is in the 'ACTIVE'
-- state, you can delete it. If a table is in 'CREATING' or 'UPDATING'
-- states, then DynamoDB returns a /ResourceInUseException/. If the
-- specified table does not exist, DynamoDB returns a
-- /ResourceNotFoundException/. If table is already in the 'DELETING'
-- state, no error is returned.
--
-- DynamoDB might continue to accept data read and write operations, such
-- as /GetItem/ and /PutItem/, on a table in the 'DELETING' state until the
-- table deletion is complete.
--
-- When you delete a table, any indexes on that table are also deleted.
--
-- If you have DynamoDB Streams enabled on the table, then the
-- corresponding stream on that table goes into the 'DISABLED' state, and
-- the stream is automatically deleted after 24 hours.
--
-- Use the /DescribeTable/ API to check the status of the table.
--
-- /See:/ <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_DeleteTable.html AWS API Reference> for DeleteTable.
module Network.AWS.DynamoDB.DeleteTable
    (
    -- * Creating a Request
      deleteTable
    , DeleteTable
    -- * Request Lenses
    , dtTableName

    -- * Destructuring the Response
    , deleteTableResponse
    , DeleteTableResponse
    -- * Response Lenses
    , dtrsTableDescription
    , dtrsStatus
    ) where

import           Network.AWS.DynamoDB.Types
import           Network.AWS.DynamoDB.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DeleteTable/ operation.
--
-- /See:/ 'deleteTable' smart constructor.
newtype DeleteTable = DeleteTable'
    { _dtTableName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteTable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtTableName'
deleteTable
    :: Text -- ^ 'dtTableName'
    -> DeleteTable
deleteTable pTableName_ =
    DeleteTable'
    { _dtTableName = pTableName_
    }

-- | The name of the table to delete.
dtTableName :: Lens' DeleteTable Text
dtTableName = lens _dtTableName (\ s a -> s{_dtTableName = a});

instance AWSRequest DeleteTable where
        type Rs DeleteTable = DeleteTableResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 DeleteTableResponse' <$>
                   (x .?> "TableDescription") <*> (pure (fromEnum s)))

instance ToHeaders DeleteTable where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.DeleteTable" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DeleteTable where
        toJSON DeleteTable'{..}
          = object
              (catMaybes [Just ("TableName" .= _dtTableName)])

instance ToPath DeleteTable where
        toPath = const "/"

instance ToQuery DeleteTable where
        toQuery = const mempty

-- | Represents the output of a /DeleteTable/ operation.
--
-- /See:/ 'deleteTableResponse' smart constructor.
data DeleteTableResponse = DeleteTableResponse'
    { _dtrsTableDescription :: !(Maybe TableDescription)
    , _dtrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteTableResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsTableDescription'
--
-- * 'dtrsStatus'
deleteTableResponse
    :: Int -- ^ 'dtrsStatus'
    -> DeleteTableResponse
deleteTableResponse pStatus_ =
    DeleteTableResponse'
    { _dtrsTableDescription = Nothing
    , _dtrsStatus = pStatus_
    }

-- | Undocumented member.
dtrsTableDescription :: Lens' DeleteTableResponse (Maybe TableDescription)
dtrsTableDescription = lens _dtrsTableDescription (\ s a -> s{_dtrsTableDescription = a});

-- | The response status code.
dtrsStatus :: Lens' DeleteTableResponse Int
dtrsStatus = lens _dtrsStatus (\ s a -> s{_dtrsStatus = a});
