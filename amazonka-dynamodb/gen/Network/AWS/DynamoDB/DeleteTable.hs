{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DeleteTable
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | The /DeleteTable/ operation deletes a table and all of its items. After
-- a /DeleteTable/ request, the specified table is in the @DELETING@ state
-- until DynamoDB completes the deletion. If the table is in the @ACTIVE@
-- state, you can delete it. If a table is in @CREATING@ or @UPDATING@
-- states, then DynamoDB returns a /ResourceInUseException/. If the
-- specified table does not exist, DynamoDB returns a
-- /ResourceNotFoundException/. If table is already in the @DELETING@
-- state, no error is returned.
--
-- DynamoDB might continue to accept data read and write operations, such
-- as /GetItem/ and /PutItem/, on a table in the @DELETING@ state until the
-- table deletion is complete.
--
-- When you delete a table, any indexes on that table are also deleted.
--
-- Use the /DescribeTable/ API to check the status of the table.
--
-- <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_DeleteTable.html>
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
    , dtrStatus
    ) where

import           Network.AWS.DynamoDB.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DeleteTable/ operation.
--
-- /See:/ 'deleteTable' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtTableName'
newtype DeleteTable = DeleteTable'
    { _dtTableName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTable' smart constructor.
deleteTable :: Text -> DeleteTable
deleteTable pTableName =
    DeleteTable'
    { _dtTableName = pTableName
    }

-- | The name of the table to delete.
dtTableName :: Lens' DeleteTable Text
dtTableName = lens _dtTableName (\ s a -> s{_dtTableName = a});

instance AWSRequest DeleteTable where
        type Sv DeleteTable = DynamoDB
        type Rs DeleteTable = DeleteTableResponse
        request = postJSON
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
          = object ["TableName" .= _dtTableName]

instance ToPath DeleteTable where
        toPath = const "/"

instance ToQuery DeleteTable where
        toQuery = const mempty

-- | Represents the output of a /DeleteTable/ operation.
--
-- /See:/ 'deleteTableResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrTableDescription'
--
-- * 'dtrStatus'
data DeleteTableResponse = DeleteTableResponse'
    { _dtrTableDescription :: !(Maybe TableDescription)
    , _dtrStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTableResponse' smart constructor.
deleteTableResponse :: Int -> DeleteTableResponse
deleteTableResponse pStatus =
    DeleteTableResponse'
    { _dtrTableDescription = Nothing
    , _dtrStatus = pStatus
    }

-- | FIXME: Undocumented member.
dtrTableDescription :: Lens' DeleteTableResponse (Maybe TableDescription)
dtrTableDescription = lens _dtrTableDescription (\ s a -> s{_dtrTableDescription = a});

-- | FIXME: Undocumented member.
dtrStatus :: Lens' DeleteTableResponse Int
dtrStatus = lens _dtrStatus (\ s a -> s{_dtrStatus = a});
