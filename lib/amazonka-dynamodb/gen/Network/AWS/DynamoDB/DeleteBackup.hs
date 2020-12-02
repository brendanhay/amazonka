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
-- Module      : Network.AWS.DynamoDB.DeleteBackup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing backup of a table.
--
--
-- You can call @DeleteBackup@ at a maximum rate of 10 times per second.
--
module Network.AWS.DynamoDB.DeleteBackup
    (
    -- * Creating a Request
      deleteBackup
    , DeleteBackup
    -- * Request Lenses
    , dbBackupARN

    -- * Destructuring the Response
    , deleteBackupResponse
    , DeleteBackupResponse
    -- * Response Lenses
    , dbrsBackupDescription
    , dbrsResponseStatus
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteBackup' smart constructor.
newtype DeleteBackup = DeleteBackup'
  { _dbBackupARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBackup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbBackupARN' - The ARN associated with the backup.
deleteBackup
    :: Text -- ^ 'dbBackupARN'
    -> DeleteBackup
deleteBackup pBackupARN_ = DeleteBackup' {_dbBackupARN = pBackupARN_}


-- | The ARN associated with the backup.
dbBackupARN :: Lens' DeleteBackup Text
dbBackupARN = lens _dbBackupARN (\ s a -> s{_dbBackupARN = a})

instance AWSRequest DeleteBackup where
        type Rs DeleteBackup = DeleteBackupResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 DeleteBackupResponse' <$>
                   (x .?> "BackupDescription") <*> (pure (fromEnum s)))

instance Hashable DeleteBackup where

instance NFData DeleteBackup where

instance ToHeaders DeleteBackup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.DeleteBackup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DeleteBackup where
        toJSON DeleteBackup'{..}
          = object
              (catMaybes [Just ("BackupArn" .= _dbBackupARN)])

instance ToPath DeleteBackup where
        toPath = const "/"

instance ToQuery DeleteBackup where
        toQuery = const mempty

-- | /See:/ 'deleteBackupResponse' smart constructor.
data DeleteBackupResponse = DeleteBackupResponse'
  { _dbrsBackupDescription :: !(Maybe BackupDescription)
  , _dbrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBackupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbrsBackupDescription' - Contains the description of the backup created for the table.
--
-- * 'dbrsResponseStatus' - -- | The response status code.
deleteBackupResponse
    :: Int -- ^ 'dbrsResponseStatus'
    -> DeleteBackupResponse
deleteBackupResponse pResponseStatus_ =
  DeleteBackupResponse'
    {_dbrsBackupDescription = Nothing, _dbrsResponseStatus = pResponseStatus_}


-- | Contains the description of the backup created for the table.
dbrsBackupDescription :: Lens' DeleteBackupResponse (Maybe BackupDescription)
dbrsBackupDescription = lens _dbrsBackupDescription (\ s a -> s{_dbrsBackupDescription = a})

-- | -- | The response status code.
dbrsResponseStatus :: Lens' DeleteBackupResponse Int
dbrsResponseStatus = lens _dbrsResponseStatus (\ s a -> s{_dbrsResponseStatus = a})

instance NFData DeleteBackupResponse where
