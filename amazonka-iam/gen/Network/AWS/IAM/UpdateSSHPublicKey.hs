{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateSSHPublicKey
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Sets the status of the specified SSH public key to active or inactive.
-- SSH public keys that are inactive cannot be used for authentication.
-- This action can be used to disable a user\'s SSH public key as part of a
-- key rotation work flow.
--
-- The SSH public key affected by this action is used only for
-- authenticating the associated IAM user to an AWS CodeCommit repository.
-- For more information about using SSH keys to authenticate to an AWS
-- CodeCommit repository, see
-- <http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up AWS CodeCommit for SSH Connections>
-- in the /AWS CodeCommit User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateSSHPublicKey.html AWS API Reference> for UpdateSSHPublicKey.
module Network.AWS.IAM.UpdateSSHPublicKey
    (
    -- * Creating a Request
      UpdateSSHPublicKey
    , updateSSHPublicKey
    -- * Request Lenses
    , uspkUserName
    , uspkSSHPublicKeyId
    , uspkStatus

    -- * Destructuring the Response
    , UpdateSSHPublicKeyResponse
    , updateSSHPublicKeyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateSSHPublicKey' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uspkUserName'
--
-- * 'uspkSSHPublicKeyId'
--
-- * 'uspkStatus'
data UpdateSSHPublicKey = UpdateSSHPublicKey'
    { _uspkUserName       :: !Text
    , _uspkSSHPublicKeyId :: !Text
    , _uspkStatus         :: !StatusType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateSSHPublicKey' smart constructor.
updateSSHPublicKey :: Text -> Text -> StatusType -> UpdateSSHPublicKey
updateSSHPublicKey pUserName_ pSSHPublicKeyId_ pStatus_ =
    UpdateSSHPublicKey'
    { _uspkUserName = pUserName_
    , _uspkSSHPublicKeyId = pSSHPublicKeyId_
    , _uspkStatus = pStatus_
    }

-- | The name of the IAM user associated with the SSH public key.
uspkUserName :: Lens' UpdateSSHPublicKey Text
uspkUserName = lens _uspkUserName (\ s a -> s{_uspkUserName = a});

-- | The unique identifier for the SSH public key.
uspkSSHPublicKeyId :: Lens' UpdateSSHPublicKey Text
uspkSSHPublicKeyId = lens _uspkSSHPublicKeyId (\ s a -> s{_uspkSSHPublicKeyId = a});

-- | The status to assign to the SSH public key. @Active@ means the key can
-- be used for authentication with an AWS CodeCommit repository. @Inactive@
-- means the key cannot be used.
uspkStatus :: Lens' UpdateSSHPublicKey StatusType
uspkStatus = lens _uspkStatus (\ s a -> s{_uspkStatus = a});

instance AWSRequest UpdateSSHPublicKey where
        type Sv UpdateSSHPublicKey = IAM
        type Rs UpdateSSHPublicKey =
             UpdateSSHPublicKeyResponse
        request = postQuery
        response = receiveNull UpdateSSHPublicKeyResponse'

instance ToHeaders UpdateSSHPublicKey where
        toHeaders = const mempty

instance ToPath UpdateSSHPublicKey where
        toPath = const "/"

instance ToQuery UpdateSSHPublicKey where
        toQuery UpdateSSHPublicKey'{..}
          = mconcat
              ["Action" =: ("UpdateSSHPublicKey" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _uspkUserName,
               "SSHPublicKeyId" =: _uspkSSHPublicKeyId,
               "Status" =: _uspkStatus]

-- | /See:/ 'updateSSHPublicKeyResponse' smart constructor.
data UpdateSSHPublicKeyResponse =
    UpdateSSHPublicKeyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateSSHPublicKeyResponse' smart constructor.
updateSSHPublicKeyResponse :: UpdateSSHPublicKeyResponse
updateSSHPublicKeyResponse = UpdateSSHPublicKeyResponse'
