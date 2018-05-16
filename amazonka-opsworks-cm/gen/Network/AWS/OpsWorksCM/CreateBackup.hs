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
-- Module      : Network.AWS.OpsWorksCM.CreateBackup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application-level backup of a server. While the server is in the @BACKING_UP@ state, the server cannot be changed, and no additional backup can be created.
--
--
-- Backups can be created for servers in @RUNNING@ , @HEALTHY@ , and @UNHEALTHY@ states. By default, you can create a maximum of 50 manual backups.
--
-- This operation is asynchronous.
--
-- A @LimitExceededException@ is thrown when the maximum number of manual backups is reached. An @InvalidStateException@ is thrown when the server is not in any of the following states: RUNNING, HEALTHY, or UNHEALTHY. A @ResourceNotFoundException@ is thrown when the server is not found. A @ValidationException@ is thrown when parameters of the request are not valid.
--
module Network.AWS.OpsWorksCM.CreateBackup
    (
    -- * Creating a Request
      createBackup
    , CreateBackup
    -- * Request Lenses
    , cbDescription
    , cbServerName

    -- * Destructuring the Response
    , createBackupResponse
    , CreateBackupResponse
    -- * Response Lenses
    , cbrsBackup
    , cbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorksCM.Types
import Network.AWS.OpsWorksCM.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createBackup' smart constructor.
data CreateBackup = CreateBackup'
  { _cbDescription :: !(Maybe Text)
  , _cbServerName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBackup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbDescription' - A user-defined description of the backup.
--
-- * 'cbServerName' - The name of the server that you want to back up.
createBackup
    :: Text -- ^ 'cbServerName'
    -> CreateBackup
createBackup pServerName_ =
  CreateBackup' {_cbDescription = Nothing, _cbServerName = pServerName_}


-- | A user-defined description of the backup.
cbDescription :: Lens' CreateBackup (Maybe Text)
cbDescription = lens _cbDescription (\ s a -> s{_cbDescription = a})

-- | The name of the server that you want to back up.
cbServerName :: Lens' CreateBackup Text
cbServerName = lens _cbServerName (\ s a -> s{_cbServerName = a})

instance AWSRequest CreateBackup where
        type Rs CreateBackup = CreateBackupResponse
        request = postJSON opsWorksCM
        response
          = receiveJSON
              (\ s h x ->
                 CreateBackupResponse' <$>
                   (x .?> "Backup") <*> (pure (fromEnum s)))

instance Hashable CreateBackup where

instance NFData CreateBackup where

instance ToHeaders CreateBackup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorksCM_V2016_11_01.CreateBackup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateBackup where
        toJSON CreateBackup'{..}
          = object
              (catMaybes
                 [("Description" .=) <$> _cbDescription,
                  Just ("ServerName" .= _cbServerName)])

instance ToPath CreateBackup where
        toPath = const "/"

instance ToQuery CreateBackup where
        toQuery = const mempty

-- | /See:/ 'createBackupResponse' smart constructor.
data CreateBackupResponse = CreateBackupResponse'
  { _cbrsBackup         :: !(Maybe Backup)
  , _cbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBackupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbrsBackup' - Backup created by request.
--
-- * 'cbrsResponseStatus' - -- | The response status code.
createBackupResponse
    :: Int -- ^ 'cbrsResponseStatus'
    -> CreateBackupResponse
createBackupResponse pResponseStatus_ =
  CreateBackupResponse'
    {_cbrsBackup = Nothing, _cbrsResponseStatus = pResponseStatus_}


-- | Backup created by request.
cbrsBackup :: Lens' CreateBackupResponse (Maybe Backup)
cbrsBackup = lens _cbrsBackup (\ s a -> s{_cbrsBackup = a})

-- | -- | The response status code.
cbrsResponseStatus :: Lens' CreateBackupResponse Int
cbrsResponseStatus = lens _cbrsResponseStatus (\ s a -> s{_cbrsResponseStatus = a})

instance NFData CreateBackupResponse where
