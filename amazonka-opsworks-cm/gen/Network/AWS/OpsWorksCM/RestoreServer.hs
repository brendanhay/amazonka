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
-- Module      : Network.AWS.OpsWorksCM.RestoreServer
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a backup to a server that is in a @CONNECTION_LOST@ , @HEALTHY@ , @RUNNING@ , @UNHEALTHY@ , or @TERMINATED@ state. When you run RestoreServer, the server's EC2 instance is deleted, and a new EC2 instance is configured. RestoreServer maintains the existing server endpoint, so configuration management of the server's client devices (nodes) should continue to work.
--
--
-- This operation is asynchronous.
--
-- An @InvalidStateException@ is thrown when the server is not in a valid state. A @ResourceNotFoundException@ is thrown when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid.
--
module Network.AWS.OpsWorksCM.RestoreServer
    (
    -- * Creating a Request
      restoreServer
    , RestoreServer
    -- * Request Lenses
    , rsKeyPair
    , rsInstanceType
    , rsBackupId
    , rsServerName

    -- * Destructuring the Response
    , restoreServerResponse
    , RestoreServerResponse
    -- * Response Lenses
    , rsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorksCM.Types
import Network.AWS.OpsWorksCM.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'restoreServer' smart constructor.
data RestoreServer = RestoreServer'
  { _rsKeyPair      :: !(Maybe Text)
  , _rsInstanceType :: !(Maybe Text)
  , _rsBackupId     :: !Text
  , _rsServerName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreServer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsKeyPair' - The name of the key pair to set on the new EC2 instance. This can be helpful if the administrator no longer has the SSH key.
--
-- * 'rsInstanceType' - The type of the instance to create. Valid values must be specified in the following format: @^([cm][34]|t2).*@ For example, @m4.large@ . Valid values are @t2.medium@ , @m4.large@ , and @m4.2xlarge@ . If you do not specify this parameter, RestoreServer uses the instance type from the specified backup.
--
-- * 'rsBackupId' - The ID of the backup that you want to use to restore a server.
--
-- * 'rsServerName' - The name of the server that you want to restore.
restoreServer
    :: Text -- ^ 'rsBackupId'
    -> Text -- ^ 'rsServerName'
    -> RestoreServer
restoreServer pBackupId_ pServerName_ =
  RestoreServer'
    { _rsKeyPair = Nothing
    , _rsInstanceType = Nothing
    , _rsBackupId = pBackupId_
    , _rsServerName = pServerName_
    }


-- | The name of the key pair to set on the new EC2 instance. This can be helpful if the administrator no longer has the SSH key.
rsKeyPair :: Lens' RestoreServer (Maybe Text)
rsKeyPair = lens _rsKeyPair (\ s a -> s{_rsKeyPair = a})

-- | The type of the instance to create. Valid values must be specified in the following format: @^([cm][34]|t2).*@ For example, @m4.large@ . Valid values are @t2.medium@ , @m4.large@ , and @m4.2xlarge@ . If you do not specify this parameter, RestoreServer uses the instance type from the specified backup.
rsInstanceType :: Lens' RestoreServer (Maybe Text)
rsInstanceType = lens _rsInstanceType (\ s a -> s{_rsInstanceType = a})

-- | The ID of the backup that you want to use to restore a server.
rsBackupId :: Lens' RestoreServer Text
rsBackupId = lens _rsBackupId (\ s a -> s{_rsBackupId = a})

-- | The name of the server that you want to restore.
rsServerName :: Lens' RestoreServer Text
rsServerName = lens _rsServerName (\ s a -> s{_rsServerName = a})

instance AWSRequest RestoreServer where
        type Rs RestoreServer = RestoreServerResponse
        request = postJSON opsWorksCM
        response
          = receiveEmpty
              (\ s h x ->
                 RestoreServerResponse' <$> (pure (fromEnum s)))

instance Hashable RestoreServer where

instance NFData RestoreServer where

instance ToHeaders RestoreServer where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorksCM_V2016_11_01.RestoreServer" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RestoreServer where
        toJSON RestoreServer'{..}
          = object
              (catMaybes
                 [("KeyPair" .=) <$> _rsKeyPair,
                  ("InstanceType" .=) <$> _rsInstanceType,
                  Just ("BackupId" .= _rsBackupId),
                  Just ("ServerName" .= _rsServerName)])

instance ToPath RestoreServer where
        toPath = const "/"

instance ToQuery RestoreServer where
        toQuery = const mempty

-- | /See:/ 'restoreServerResponse' smart constructor.
newtype RestoreServerResponse = RestoreServerResponse'
  { _rsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreServerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsrsResponseStatus' - -- | The response status code.
restoreServerResponse
    :: Int -- ^ 'rsrsResponseStatus'
    -> RestoreServerResponse
restoreServerResponse pResponseStatus_ =
  RestoreServerResponse' {_rsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
rsrsResponseStatus :: Lens' RestoreServerResponse Int
rsrsResponseStatus = lens _rsrsResponseStatus (\ s a -> s{_rsrsResponseStatus = a})

instance NFData RestoreServerResponse where
