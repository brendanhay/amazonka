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
-- Module      : Network.AWS.IAM.DeleteSSHPublicKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified SSH public key.
--
--
-- The SSH public key deleted by this operation is used only for authenticating the associated IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up AWS CodeCommit for SSH Connections> in the /AWS CodeCommit User Guide/ .
--
module Network.AWS.IAM.DeleteSSHPublicKey
    (
    -- * Creating a Request
      deleteSSHPublicKey
    , DeleteSSHPublicKey
    -- * Request Lenses
    , dspkUserName
    , dspkSSHPublicKeyId

    -- * Destructuring the Response
    , deleteSSHPublicKeyResponse
    , DeleteSSHPublicKeyResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSSHPublicKey' smart constructor.
data DeleteSSHPublicKey = DeleteSSHPublicKey'
  { _dspkUserName       :: !Text
  , _dspkSSHPublicKeyId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSSHPublicKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dspkUserName' - The name of the IAM user associated with the SSH public key. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'dspkSSHPublicKeyId' - The unique identifier for the SSH public key. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
deleteSSHPublicKey
    :: Text -- ^ 'dspkUserName'
    -> Text -- ^ 'dspkSSHPublicKeyId'
    -> DeleteSSHPublicKey
deleteSSHPublicKey pUserName_ pSSHPublicKeyId_ =
  DeleteSSHPublicKey'
    {_dspkUserName = pUserName_, _dspkSSHPublicKeyId = pSSHPublicKeyId_}


-- | The name of the IAM user associated with the SSH public key. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
dspkUserName :: Lens' DeleteSSHPublicKey Text
dspkUserName = lens _dspkUserName (\ s a -> s{_dspkUserName = a})

-- | The unique identifier for the SSH public key. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
dspkSSHPublicKeyId :: Lens' DeleteSSHPublicKey Text
dspkSSHPublicKeyId = lens _dspkSSHPublicKeyId (\ s a -> s{_dspkSSHPublicKeyId = a})

instance AWSRequest DeleteSSHPublicKey where
        type Rs DeleteSSHPublicKey =
             DeleteSSHPublicKeyResponse
        request = postQuery iam
        response = receiveNull DeleteSSHPublicKeyResponse'

instance Hashable DeleteSSHPublicKey where

instance NFData DeleteSSHPublicKey where

instance ToHeaders DeleteSSHPublicKey where
        toHeaders = const mempty

instance ToPath DeleteSSHPublicKey where
        toPath = const "/"

instance ToQuery DeleteSSHPublicKey where
        toQuery DeleteSSHPublicKey'{..}
          = mconcat
              ["Action" =: ("DeleteSSHPublicKey" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _dspkUserName,
               "SSHPublicKeyId" =: _dspkSSHPublicKeyId]

-- | /See:/ 'deleteSSHPublicKeyResponse' smart constructor.
data DeleteSSHPublicKeyResponse =
  DeleteSSHPublicKeyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSSHPublicKeyResponse' with the minimum fields required to make a request.
--
deleteSSHPublicKeyResponse
    :: DeleteSSHPublicKeyResponse
deleteSSHPublicKeyResponse = DeleteSSHPublicKeyResponse'


instance NFData DeleteSSHPublicKeyResponse where
