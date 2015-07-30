{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UploadSSHPublicKey
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Uploads an SSH public key and associates it with the specified IAM user.
--
-- The SSH public key uploaded by this action can be used only for
-- authenticating the associated IAM user to an AWS CodeCommit repository.
-- For more information about using SSH keys to authenticate to an AWS
-- CodeCommit repository, see
-- <http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up AWS CodeCommit for SSH Connections>
-- in the /AWS CodeCommit User Guide/.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UploadSSHPublicKey.html>
module Network.AWS.IAM.UploadSSHPublicKey
    (
    -- * Request
      UploadSSHPublicKey
    -- ** Request constructor
    , uploadSSHPublicKey
    -- ** Request lenses
    , usshpkUserName
    , usshpkSSHPublicKeyBody

    -- * Response
    , UploadSSHPublicKeyResponse
    -- ** Response constructor
    , uploadSSHPublicKeyResponse
    -- ** Response lenses
    , uspkrsSSHPublicKey
    , uspkrsStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'uploadSSHPublicKey' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usshpkUserName'
--
-- * 'usshpkSSHPublicKeyBody'
data UploadSSHPublicKey = UploadSSHPublicKey'
    { _usshpkUserName         :: !Text
    , _usshpkSSHPublicKeyBody :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UploadSSHPublicKey' smart constructor.
uploadSSHPublicKey :: Text -> Text -> UploadSSHPublicKey
uploadSSHPublicKey pUserName_ pSSHPublicKeyBody_ =
    UploadSSHPublicKey'
    { _usshpkUserName = pUserName_
    , _usshpkSSHPublicKeyBody = pSSHPublicKeyBody_
    }

-- | The name of the IAM user to associate the SSH public key with.
usshpkUserName :: Lens' UploadSSHPublicKey Text
usshpkUserName = lens _usshpkUserName (\ s a -> s{_usshpkUserName = a});

-- | The SSH public key. The public key must be encoded in ssh-rsa format or
-- PEM format.
usshpkSSHPublicKeyBody :: Lens' UploadSSHPublicKey Text
usshpkSSHPublicKeyBody = lens _usshpkSSHPublicKeyBody (\ s a -> s{_usshpkSSHPublicKeyBody = a});

instance AWSRequest UploadSSHPublicKey where
        type Sv UploadSSHPublicKey = IAM
        type Rs UploadSSHPublicKey =
             UploadSSHPublicKeyResponse
        request = postQuery
        response
          = receiveXMLWrapper "UploadSSHPublicKeyResult"
              (\ s h x ->
                 UploadSSHPublicKeyResponse' <$>
                   (x .@? "SSHPublicKey") <*> (pure (fromEnum s)))

instance ToHeaders UploadSSHPublicKey where
        toHeaders = const mempty

instance ToPath UploadSSHPublicKey where
        toPath = const mempty

instance ToQuery UploadSSHPublicKey where
        toQuery UploadSSHPublicKey'{..}
          = mconcat
              ["Action" =: ("UploadSSHPublicKey" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _usshpkUserName,
               "SSHPublicKeyBody" =: _usshpkSSHPublicKeyBody]

-- | Contains the response to a successful UploadSSHPublicKey request.
--
-- /See:/ 'uploadSSHPublicKeyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uspkrsSSHPublicKey'
--
-- * 'uspkrsStatus'
data UploadSSHPublicKeyResponse = UploadSSHPublicKeyResponse'
    { _uspkrsSSHPublicKey :: !(Maybe SSHPublicKey)
    , _uspkrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UploadSSHPublicKeyResponse' smart constructor.
uploadSSHPublicKeyResponse :: Int -> UploadSSHPublicKeyResponse
uploadSSHPublicKeyResponse pStatus_ =
    UploadSSHPublicKeyResponse'
    { _uspkrsSSHPublicKey = Nothing
    , _uspkrsStatus = pStatus_
    }

-- | Contains information about the SSH public key.
uspkrsSSHPublicKey :: Lens' UploadSSHPublicKeyResponse (Maybe SSHPublicKey)
uspkrsSSHPublicKey = lens _uspkrsSSHPublicKey (\ s a -> s{_uspkrsSSHPublicKey = a});

-- | FIXME: Undocumented member.
uspkrsStatus :: Lens' UploadSSHPublicKeyResponse Int
uspkrsStatus = lens _uspkrsStatus (\ s a -> s{_uspkrsStatus = a});
