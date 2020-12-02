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
-- Module      : Network.AWS.IAM.UpdateSSHPublicKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the status of an IAM user's SSH public key to active or inactive. SSH public keys that are inactive cannot be used for authentication. This operation can be used to disable a user's SSH public key as part of a key rotation work flow.
--
--
-- The SSH public key affected by this operation is used only for authenticating the associated IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up AWS CodeCommit for SSH Connections> in the /AWS CodeCommit User Guide/ .
--
module Network.AWS.IAM.UpdateSSHPublicKey
    (
    -- * Creating a Request
      updateSSHPublicKey
    , UpdateSSHPublicKey
    -- * Request Lenses
    , uspkUserName
    , uspkSSHPublicKeyId
    , uspkStatus

    -- * Destructuring the Response
    , updateSSHPublicKeyResponse
    , UpdateSSHPublicKeyResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateSSHPublicKey' smart constructor.
data UpdateSSHPublicKey = UpdateSSHPublicKey'
  { _uspkUserName       :: !Text
  , _uspkSSHPublicKeyId :: !Text
  , _uspkStatus         :: !StatusType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSSHPublicKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uspkUserName' - The name of the IAM user associated with the SSH public key. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'uspkSSHPublicKeyId' - The unique identifier for the SSH public key. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- * 'uspkStatus' - The status to assign to the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
updateSSHPublicKey
    :: Text -- ^ 'uspkUserName'
    -> Text -- ^ 'uspkSSHPublicKeyId'
    -> StatusType -- ^ 'uspkStatus'
    -> UpdateSSHPublicKey
updateSSHPublicKey pUserName_ pSSHPublicKeyId_ pStatus_ =
  UpdateSSHPublicKey'
    { _uspkUserName = pUserName_
    , _uspkSSHPublicKeyId = pSSHPublicKeyId_
    , _uspkStatus = pStatus_
    }


-- | The name of the IAM user associated with the SSH public key. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
uspkUserName :: Lens' UpdateSSHPublicKey Text
uspkUserName = lens _uspkUserName (\ s a -> s{_uspkUserName = a})

-- | The unique identifier for the SSH public key. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
uspkSSHPublicKeyId :: Lens' UpdateSSHPublicKey Text
uspkSSHPublicKeyId = lens _uspkSSHPublicKeyId (\ s a -> s{_uspkSSHPublicKeyId = a})

-- | The status to assign to the SSH public key. @Active@ means that the key can be used for authentication with an AWS CodeCommit repository. @Inactive@ means that the key cannot be used.
uspkStatus :: Lens' UpdateSSHPublicKey StatusType
uspkStatus = lens _uspkStatus (\ s a -> s{_uspkStatus = a})

instance AWSRequest UpdateSSHPublicKey where
        type Rs UpdateSSHPublicKey =
             UpdateSSHPublicKeyResponse
        request = postQuery iam
        response = receiveNull UpdateSSHPublicKeyResponse'

instance Hashable UpdateSSHPublicKey where

instance NFData UpdateSSHPublicKey where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSSHPublicKeyResponse' with the minimum fields required to make a request.
--
updateSSHPublicKeyResponse
    :: UpdateSSHPublicKeyResponse
updateSSHPublicKeyResponse = UpdateSSHPublicKeyResponse'


instance NFData UpdateSSHPublicKeyResponse where
