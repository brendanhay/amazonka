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
-- Module      : Network.AWS.IAM.GetSSHPublicKey
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified SSH public key, including metadata about the
-- key.
--
-- The SSH public key retrieved by this action is used only for
-- authenticating the associated IAM user to an AWS CodeCommit repository.
-- For more information about using SSH keys to authenticate to an AWS
-- CodeCommit repository, see
-- <http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up AWS CodeCommit for SSH Connections>
-- in the /AWS CodeCommit User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetSSHPublicKey.html AWS API Reference> for GetSSHPublicKey.
module Network.AWS.IAM.GetSSHPublicKey
    (
    -- * Creating a Request
      getSSHPublicKey
    , GetSSHPublicKey
    -- * Request Lenses
    , gspkUserName
    , gspkSSHPublicKeyId
    , gspkEncoding

    -- * Destructuring the Response
    , getSSHPublicKeyResponse
    , GetSSHPublicKeyResponse
    -- * Response Lenses
    , gspkrsSSHPublicKey
    , gspkrsStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getSSHPublicKey' smart constructor.
data GetSSHPublicKey = GetSSHPublicKey'
    { _gspkUserName       :: !Text
    , _gspkSSHPublicKeyId :: !Text
    , _gspkEncoding       :: !EncodingType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetSSHPublicKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gspkUserName'
--
-- * 'gspkSSHPublicKeyId'
--
-- * 'gspkEncoding'
getSSHPublicKey
    :: Text -- ^ 'gspkUserName'
    -> Text -- ^ 'gspkSSHPublicKeyId'
    -> EncodingType -- ^ 'gspkEncoding'
    -> GetSSHPublicKey
getSSHPublicKey pUserName_ pSSHPublicKeyId_ pEncoding_ =
    GetSSHPublicKey'
    { _gspkUserName = pUserName_
    , _gspkSSHPublicKeyId = pSSHPublicKeyId_
    , _gspkEncoding = pEncoding_
    }

-- | The name of the IAM user associated with the SSH public key.
gspkUserName :: Lens' GetSSHPublicKey Text
gspkUserName = lens _gspkUserName (\ s a -> s{_gspkUserName = a});

-- | The unique identifier for the SSH public key.
gspkSSHPublicKeyId :: Lens' GetSSHPublicKey Text
gspkSSHPublicKeyId = lens _gspkSSHPublicKeyId (\ s a -> s{_gspkSSHPublicKeyId = a});

-- | Specifies the public key encoding format to use in the response. To
-- retrieve the public key in ssh-rsa format, use 'SSH'. To retrieve the
-- public key in PEM format, use 'PEM'.
gspkEncoding :: Lens' GetSSHPublicKey EncodingType
gspkEncoding = lens _gspkEncoding (\ s a -> s{_gspkEncoding = a});

instance AWSRequest GetSSHPublicKey where
        type Rs GetSSHPublicKey = GetSSHPublicKeyResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper "GetSSHPublicKeyResult"
              (\ s h x ->
                 GetSSHPublicKeyResponse' <$>
                   (x .@? "SSHPublicKey") <*> (pure (fromEnum s)))

instance ToHeaders GetSSHPublicKey where
        toHeaders = const mempty

instance ToPath GetSSHPublicKey where
        toPath = const "/"

instance ToQuery GetSSHPublicKey where
        toQuery GetSSHPublicKey'{..}
          = mconcat
              ["Action" =: ("GetSSHPublicKey" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _gspkUserName,
               "SSHPublicKeyId" =: _gspkSSHPublicKeyId,
               "Encoding" =: _gspkEncoding]

-- | Contains the response to a successful GetSSHPublicKey request.
--
-- /See:/ 'getSSHPublicKeyResponse' smart constructor.
data GetSSHPublicKeyResponse = GetSSHPublicKeyResponse'
    { _gspkrsSSHPublicKey :: !(Maybe SSHPublicKey)
    , _gspkrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetSSHPublicKeyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gspkrsSSHPublicKey'
--
-- * 'gspkrsStatus'
getSSHPublicKeyResponse
    :: Int -- ^ 'gspkrsStatus'
    -> GetSSHPublicKeyResponse
getSSHPublicKeyResponse pStatus_ =
    GetSSHPublicKeyResponse'
    { _gspkrsSSHPublicKey = Nothing
    , _gspkrsStatus = pStatus_
    }

-- | Information about the SSH public key.
gspkrsSSHPublicKey :: Lens' GetSSHPublicKeyResponse (Maybe SSHPublicKey)
gspkrsSSHPublicKey = lens _gspkrsSSHPublicKey (\ s a -> s{_gspkrsSSHPublicKey = a});

-- | The response status code.
gspkrsStatus :: Lens' GetSSHPublicKeyResponse Int
gspkrsStatus = lens _gspkrsStatus (\ s a -> s{_gspkrsStatus = a});
