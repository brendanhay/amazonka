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
-- Module      : Network.AWS.IAM.UploadSSHPublicKey
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads an SSH public key and associates it with the specified IAM user.
--
-- The SSH public key uploaded by this action can be used only for authenticating the associated IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up AWS CodeCommit for SSH Connections> in the /AWS CodeCommit User Guide/.
module Network.AWS.IAM.UploadSSHPublicKey
    (
    -- * Creating a Request
      uploadSSHPublicKey
    , UploadSSHPublicKey
    -- * Request Lenses
    , usshpkUserName
    , usshpkSSHPublicKeyBody

    -- * Destructuring the Response
    , uploadSSHPublicKeyResponse
    , UploadSSHPublicKeyResponse
    -- * Response Lenses
    , uspkrsSSHPublicKey
    , uspkrsResponseStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'uploadSSHPublicKey' smart constructor.
data UploadSSHPublicKey = UploadSSHPublicKey'
    { _usshpkUserName         :: !Text
    , _usshpkSSHPublicKeyBody :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UploadSSHPublicKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usshpkUserName'
--
-- * 'usshpkSSHPublicKeyBody'
uploadSSHPublicKey
    :: Text -- ^ 'usshpkUserName'
    -> Text -- ^ 'usshpkSSHPublicKeyBody'
    -> UploadSSHPublicKey
uploadSSHPublicKey pUserName_ pSSHPublicKeyBody_ =
    UploadSSHPublicKey'
    { _usshpkUserName = pUserName_
    , _usshpkSSHPublicKeyBody = pSSHPublicKeyBody_
    }

-- | The name of the IAM user to associate the SSH public key with.
usshpkUserName :: Lens' UploadSSHPublicKey Text
usshpkUserName = lens _usshpkUserName (\ s a -> s{_usshpkUserName = a});

-- | The SSH public key. The public key must be encoded in ssh-rsa format or PEM format.
usshpkSSHPublicKeyBody :: Lens' UploadSSHPublicKey Text
usshpkSSHPublicKeyBody = lens _usshpkSSHPublicKeyBody (\ s a -> s{_usshpkSSHPublicKeyBody = a});

instance AWSRequest UploadSSHPublicKey where
        type Rs UploadSSHPublicKey =
             UploadSSHPublicKeyResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "UploadSSHPublicKeyResult"
              (\ s h x ->
                 UploadSSHPublicKeyResponse' <$>
                   (x .@? "SSHPublicKey") <*> (pure (fromEnum s)))

instance Hashable UploadSSHPublicKey

instance NFData UploadSSHPublicKey

instance ToHeaders UploadSSHPublicKey where
        toHeaders = const mempty

instance ToPath UploadSSHPublicKey where
        toPath = const "/"

instance ToQuery UploadSSHPublicKey where
        toQuery UploadSSHPublicKey'{..}
          = mconcat
              ["Action" =: ("UploadSSHPublicKey" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _usshpkUserName,
               "SSHPublicKeyBody" =: _usshpkSSHPublicKeyBody]

-- | Contains the response to a successful < UploadSSHPublicKey> request.
--
-- /See:/ 'uploadSSHPublicKeyResponse' smart constructor.
data UploadSSHPublicKeyResponse = UploadSSHPublicKeyResponse'
    { _uspkrsSSHPublicKey   :: !(Maybe SSHPublicKey)
    , _uspkrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UploadSSHPublicKeyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uspkrsSSHPublicKey'
--
-- * 'uspkrsResponseStatus'
uploadSSHPublicKeyResponse
    :: Int -- ^ 'uspkrsResponseStatus'
    -> UploadSSHPublicKeyResponse
uploadSSHPublicKeyResponse pResponseStatus_ =
    UploadSSHPublicKeyResponse'
    { _uspkrsSSHPublicKey = Nothing
    , _uspkrsResponseStatus = pResponseStatus_
    }

-- | Contains information about the SSH public key.
uspkrsSSHPublicKey :: Lens' UploadSSHPublicKeyResponse (Maybe SSHPublicKey)
uspkrsSSHPublicKey = lens _uspkrsSSHPublicKey (\ s a -> s{_uspkrsSSHPublicKey = a});

-- | The response status code.
uspkrsResponseStatus :: Lens' UploadSSHPublicKeyResponse Int
uspkrsResponseStatus = lens _uspkrsResponseStatus (\ s a -> s{_uspkrsResponseStatus = a});

instance NFData UploadSSHPublicKeyResponse
