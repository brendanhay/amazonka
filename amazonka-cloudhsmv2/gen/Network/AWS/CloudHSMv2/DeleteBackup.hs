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
-- Module      : Network.AWS.CloudHSMv2.DeleteBackup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified AWS CloudHSM backup. A backup can be restored up to 7 days after the DeleteBackup request. For more information on restoring a backup, see 'RestoreBackup'
--
--
module Network.AWS.CloudHSMv2.DeleteBackup
    (
    -- * Creating a Request
      deleteBackup
    , DeleteBackup
    -- * Request Lenses
    , dbBackupId

    -- * Destructuring the Response
    , deleteBackupResponse
    , DeleteBackupResponse
    -- * Response Lenses
    , dbrsBackup
    , dbrsResponseStatus
    ) where

import Network.AWS.CloudHSMv2.Types
import Network.AWS.CloudHSMv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteBackup' smart constructor.
newtype DeleteBackup = DeleteBackup'
  { _dbBackupId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBackup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbBackupId' - The ID of the backup to be deleted. To find the ID of a backup, use the 'DescribeBackups' operation.
deleteBackup
    :: Text -- ^ 'dbBackupId'
    -> DeleteBackup
deleteBackup pBackupId_ = DeleteBackup' {_dbBackupId = pBackupId_}


-- | The ID of the backup to be deleted. To find the ID of a backup, use the 'DescribeBackups' operation.
dbBackupId :: Lens' DeleteBackup Text
dbBackupId = lens _dbBackupId (\ s a -> s{_dbBackupId = a})

instance AWSRequest DeleteBackup where
        type Rs DeleteBackup = DeleteBackupResponse
        request = postJSON cloudHSMv2
        response
          = receiveJSON
              (\ s h x ->
                 DeleteBackupResponse' <$>
                   (x .?> "Backup") <*> (pure (fromEnum s)))

instance Hashable DeleteBackup where

instance NFData DeleteBackup where

instance ToHeaders DeleteBackup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("BaldrApiService.DeleteBackup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteBackup where
        toJSON DeleteBackup'{..}
          = object
              (catMaybes [Just ("BackupId" .= _dbBackupId)])

instance ToPath DeleteBackup where
        toPath = const "/"

instance ToQuery DeleteBackup where
        toQuery = const mempty

-- | /See:/ 'deleteBackupResponse' smart constructor.
data DeleteBackupResponse = DeleteBackupResponse'
  { _dbrsBackup         :: !(Maybe Backup)
  , _dbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteBackupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbrsBackup' - Information on the @Backup@ object deleted.
--
-- * 'dbrsResponseStatus' - -- | The response status code.
deleteBackupResponse
    :: Int -- ^ 'dbrsResponseStatus'
    -> DeleteBackupResponse
deleteBackupResponse pResponseStatus_ =
  DeleteBackupResponse'
    {_dbrsBackup = Nothing, _dbrsResponseStatus = pResponseStatus_}


-- | Information on the @Backup@ object deleted.
dbrsBackup :: Lens' DeleteBackupResponse (Maybe Backup)
dbrsBackup = lens _dbrsBackup (\ s a -> s{_dbrsBackup = a})

-- | -- | The response status code.
dbrsResponseStatus :: Lens' DeleteBackupResponse Int
dbrsResponseStatus = lens _dbrsResponseStatus (\ s a -> s{_dbrsResponseStatus = a})

instance NFData DeleteBackupResponse where
