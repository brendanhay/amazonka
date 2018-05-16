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
-- Module      : Network.AWS.DynamoDB.CreateBackup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a backup for an existing table.
--
--
-- Each time you create an On-Demand Backup, the entire table data is backed up. There is no limit to the number of on-demand backups that can be taken.
--
-- When you create an On-Demand Backup, a time marker of the request is cataloged, and the backup is created asynchronously, by applying all changes until the time of the request to the last full table snapshot. Backup requests are processed instantaneously and become available for restore within minutes.
--
-- You can call @CreateBackup@ at a maximum rate of 50 times per second.
--
-- All backups in DynamoDB work without consuming any provisioned throughput on the table.
--
-- If you submit a backup request on 2018-12-14 at 14:25:00, the backup is guaranteed to contain all data committed to the table up to 14:24:00, and data committed after 14:26:00 will not be. The backup may or may not contain data modifications made between 14:24:00 and 14:26:00. On-Demand Backup does not support causal consistency.
--
-- Along with data, the following are also included on the backups:
--
--     * Global secondary indexes (GSIs)
--
--     * Local secondary indexes (LSIs)
--
--     * Streams
--
--     * Provisioned read and write capacity
--
--
--
module Network.AWS.DynamoDB.CreateBackup
    (
    -- * Creating a Request
      createBackup
    , CreateBackup
    -- * Request Lenses
    , cbTableName
    , cbBackupName

    -- * Destructuring the Response
    , createBackupResponse
    , CreateBackupResponse
    -- * Response Lenses
    , cbrsBackupDetails
    , cbrsResponseStatus
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createBackup' smart constructor.
data CreateBackup = CreateBackup'
  { _cbTableName  :: !Text
  , _cbBackupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBackup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbTableName' - The name of the table.
--
-- * 'cbBackupName' - Specified name for the backup.
createBackup
    :: Text -- ^ 'cbTableName'
    -> Text -- ^ 'cbBackupName'
    -> CreateBackup
createBackup pTableName_ pBackupName_ =
  CreateBackup' {_cbTableName = pTableName_, _cbBackupName = pBackupName_}


-- | The name of the table.
cbTableName :: Lens' CreateBackup Text
cbTableName = lens _cbTableName (\ s a -> s{_cbTableName = a})

-- | Specified name for the backup.
cbBackupName :: Lens' CreateBackup Text
cbBackupName = lens _cbBackupName (\ s a -> s{_cbBackupName = a})

instance AWSRequest CreateBackup where
        type Rs CreateBackup = CreateBackupResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 CreateBackupResponse' <$>
                   (x .?> "BackupDetails") <*> (pure (fromEnum s)))

instance Hashable CreateBackup where

instance NFData CreateBackup where

instance ToHeaders CreateBackup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.CreateBackup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON CreateBackup where
        toJSON CreateBackup'{..}
          = object
              (catMaybes
                 [Just ("TableName" .= _cbTableName),
                  Just ("BackupName" .= _cbBackupName)])

instance ToPath CreateBackup where
        toPath = const "/"

instance ToQuery CreateBackup where
        toQuery = const mempty

-- | /See:/ 'createBackupResponse' smart constructor.
data CreateBackupResponse = CreateBackupResponse'
  { _cbrsBackupDetails  :: !(Maybe BackupDetails)
  , _cbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBackupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbrsBackupDetails' - Contains the details of the backup created for the table.
--
-- * 'cbrsResponseStatus' - -- | The response status code.
createBackupResponse
    :: Int -- ^ 'cbrsResponseStatus'
    -> CreateBackupResponse
createBackupResponse pResponseStatus_ =
  CreateBackupResponse'
    {_cbrsBackupDetails = Nothing, _cbrsResponseStatus = pResponseStatus_}


-- | Contains the details of the backup created for the table.
cbrsBackupDetails :: Lens' CreateBackupResponse (Maybe BackupDetails)
cbrsBackupDetails = lens _cbrsBackupDetails (\ s a -> s{_cbrsBackupDetails = a})

-- | -- | The response status code.
cbrsResponseStatus :: Lens' CreateBackupResponse Int
cbrsResponseStatus = lens _cbrsResponseStatus (\ s a -> s{_cbrsResponseStatus = a})

instance NFData CreateBackupResponse where
