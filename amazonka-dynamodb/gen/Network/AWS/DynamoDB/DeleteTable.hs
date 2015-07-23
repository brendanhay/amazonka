{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DeleteTable
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /DeleteTable/ operation deletes a table and all of its items. After
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
-- If you have DynamoDB Streams enabled on the table, then the
-- corresponding stream on that table goes into the @DISABLED@ state, and
-- the stream is automatically deleted after 24 hours.
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
    , dtrqTableName

    -- * Response
    , DeleteTableResponse
    -- ** Response constructor
    , deleteTableResponse
    -- ** Response lenses
    , dtrsTableDescription
    , dtrsStatus
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
-- * 'dtrqTableName'
newtype DeleteTable = DeleteTable'
    { _dtrqTableName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTable' smart constructor.
deleteTable :: Text -> DeleteTable
deleteTable pTableName_ =
    DeleteTable'
    { _dtrqTableName = pTableName_
    }

-- | The name of the table to delete.
dtrqTableName :: Lens' DeleteTable Text
dtrqTableName = lens _dtrqTableName (\ s a -> s{_dtrqTableName = a});

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
          = object ["TableName" .= _dtrqTableName]

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
-- * 'dtrsTableDescription'
--
-- * 'dtrsStatus'
data DeleteTableResponse = DeleteTableResponse'
    { _dtrsTableDescription :: !(Maybe TableDescription)
    , _dtrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTableResponse' smart constructor.
deleteTableResponse :: Int -> DeleteTableResponse
deleteTableResponse pStatus_ =
    DeleteTableResponse'
    { _dtrsTableDescription = Nothing
    , _dtrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
dtrsTableDescription :: Lens' DeleteTableResponse (Maybe TableDescription)
dtrsTableDescription = lens _dtrsTableDescription (\ s a -> s{_dtrsTableDescription = a});

-- | FIXME: Undocumented member.
dtrsStatus :: Lens' DeleteTableResponse Int
dtrsStatus = lens _dtrsStatus (\ s a -> s{_dtrsStatus = a});
