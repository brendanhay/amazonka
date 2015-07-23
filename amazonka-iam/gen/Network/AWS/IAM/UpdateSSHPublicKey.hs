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
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateSSHPublicKey.html>
module Network.AWS.IAM.UpdateSSHPublicKey
    (
    -- * Request
      UpdateSSHPublicKey
    -- ** Request constructor
    , updateSSHPublicKey
    -- ** Request lenses
    , uspkrqUserName
    , uspkrqSSHPublicKeyId
    , uspkrqStatus

    -- * Response
    , UpdateSSHPublicKeyResponse
    -- ** Response constructor
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
-- * 'uspkrqUserName'
--
-- * 'uspkrqSSHPublicKeyId'
--
-- * 'uspkrqStatus'
data UpdateSSHPublicKey = UpdateSSHPublicKey'
    { _uspkrqUserName       :: !Text
    , _uspkrqSSHPublicKeyId :: !Text
    , _uspkrqStatus         :: !StatusType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateSSHPublicKey' smart constructor.
updateSSHPublicKey :: Text -> Text -> StatusType -> UpdateSSHPublicKey
updateSSHPublicKey pUserName_ pSSHPublicKeyId_ pStatus_ =
    UpdateSSHPublicKey'
    { _uspkrqUserName = pUserName_
    , _uspkrqSSHPublicKeyId = pSSHPublicKeyId_
    , _uspkrqStatus = pStatus_
    }

-- | The name of the IAM user associated with the SSH public key.
uspkrqUserName :: Lens' UpdateSSHPublicKey Text
uspkrqUserName = lens _uspkrqUserName (\ s a -> s{_uspkrqUserName = a});

-- | The unique identifier for the SSH public key.
uspkrqSSHPublicKeyId :: Lens' UpdateSSHPublicKey Text
uspkrqSSHPublicKeyId = lens _uspkrqSSHPublicKeyId (\ s a -> s{_uspkrqSSHPublicKeyId = a});

-- | The status to assign to the SSH public key. @Active@ means the key can
-- be used for authentication with an AWS CodeCommit repository. @Inactive@
-- means the key cannot be used.
uspkrqStatus :: Lens' UpdateSSHPublicKey StatusType
uspkrqStatus = lens _uspkrqStatus (\ s a -> s{_uspkrqStatus = a});

instance AWSRequest UpdateSSHPublicKey where
        type Sv UpdateSSHPublicKey = IAM
        type Rs UpdateSSHPublicKey =
             UpdateSSHPublicKeyResponse
        request = post
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
               "UserName" =: _uspkrqUserName,
               "SSHPublicKeyId" =: _uspkrqSSHPublicKeyId,
               "Status" =: _uspkrqStatus]

-- | /See:/ 'updateSSHPublicKeyResponse' smart constructor.
data UpdateSSHPublicKeyResponse =
    UpdateSSHPublicKeyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateSSHPublicKeyResponse' smart constructor.
updateSSHPublicKeyResponse :: UpdateSSHPublicKeyResponse
updateSSHPublicKeyResponse = UpdateSSHPublicKeyResponse'
