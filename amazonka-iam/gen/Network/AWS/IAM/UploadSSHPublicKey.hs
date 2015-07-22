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
    , usshpkrqUserName
    , usshpkrqSSHPublicKeyBody

    -- * Response
    , UploadSSHPublicKeyResponse
    -- ** Response constructor
    , uploadSSHPublicKeyResponse
    -- ** Response lenses
    , usshpkrsSSHPublicKey
    , usshpkrsStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'uploadSSHPublicKey' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usshpkrqUserName'
--
-- * 'usshpkrqSSHPublicKeyBody'
data UploadSSHPublicKey = UploadSSHPublicKey'
    { _usshpkrqUserName         :: !Text
    , _usshpkrqSSHPublicKeyBody :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UploadSSHPublicKey' smart constructor.
uploadSSHPublicKey :: Text -> Text -> UploadSSHPublicKey
uploadSSHPublicKey pUserName pSSHPublicKeyBody =
    UploadSSHPublicKey'
    { _usshpkrqUserName = pUserName
    , _usshpkrqSSHPublicKeyBody = pSSHPublicKeyBody
    }

-- | The name of the IAM user to associate the SSH public key with.
usshpkrqUserName :: Lens' UploadSSHPublicKey Text
usshpkrqUserName = lens _usshpkrqUserName (\ s a -> s{_usshpkrqUserName = a});

-- | The SSH public key. The public key must be encoded in ssh-rsa format or
-- PEM format.
usshpkrqSSHPublicKeyBody :: Lens' UploadSSHPublicKey Text
usshpkrqSSHPublicKeyBody = lens _usshpkrqSSHPublicKeyBody (\ s a -> s{_usshpkrqSSHPublicKeyBody = a});

instance AWSRequest UploadSSHPublicKey where
        type Sv UploadSSHPublicKey = IAM
        type Rs UploadSSHPublicKey =
             UploadSSHPublicKeyResponse
        request = post
        response
          = receiveXMLWrapper "UploadSSHPublicKeyResult"
              (\ s h x ->
                 UploadSSHPublicKeyResponse' <$>
                   (x .@? "SSHPublicKey") <*> (pure (fromEnum s)))

instance ToHeaders UploadSSHPublicKey where
        toHeaders = const mempty

instance ToPath UploadSSHPublicKey where
        toPath = const "/"

instance ToQuery UploadSSHPublicKey where
        toQuery UploadSSHPublicKey'{..}
          = mconcat
              ["Action" =: ("UploadSSHPublicKey" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _usshpkrqUserName,
               "SSHPublicKeyBody" =: _usshpkrqSSHPublicKeyBody]

-- | Contains the response to a successful UploadSSHPublicKey request.
--
-- /See:/ 'uploadSSHPublicKeyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usshpkrsSSHPublicKey'
--
-- * 'usshpkrsStatus'
data UploadSSHPublicKeyResponse = UploadSSHPublicKeyResponse'
    { _usshpkrsSSHPublicKey :: !(Maybe SSHPublicKey)
    , _usshpkrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UploadSSHPublicKeyResponse' smart constructor.
uploadSSHPublicKeyResponse :: Int -> UploadSSHPublicKeyResponse
uploadSSHPublicKeyResponse pStatus =
    UploadSSHPublicKeyResponse'
    { _usshpkrsSSHPublicKey = Nothing
    , _usshpkrsStatus = pStatus
    }

-- | Contains information about the SSH public key.
usshpkrsSSHPublicKey :: Lens' UploadSSHPublicKeyResponse (Maybe SSHPublicKey)
usshpkrsSSHPublicKey = lens _usshpkrsSSHPublicKey (\ s a -> s{_usshpkrsSSHPublicKey = a});

-- | FIXME: Undocumented member.
usshpkrsStatus :: Lens' UploadSSHPublicKeyResponse Int
usshpkrsStatus = lens _usshpkrsStatus (\ s a -> s{_usshpkrsStatus = a});
