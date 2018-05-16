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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified SSH public key, including metadata about the key.
--
--
-- The SSH public key retrieved by this operation is used only for authenticating the associated IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up AWS CodeCommit for SSH Connections> in the /AWS CodeCommit User Guide/ .
--
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
    , gspkrsResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSSHPublicKey' smart constructor.
data GetSSHPublicKey = GetSSHPublicKey'
  { _gspkUserName       :: !Text
  , _gspkSSHPublicKeyId :: !Text
  , _gspkEncoding       :: !EncodingType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSSHPublicKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gspkUserName' - The name of the IAM user associated with the SSH public key. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'gspkSSHPublicKeyId' - The unique identifier for the SSH public key. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- * 'gspkEncoding' - Specifies the public key encoding format to use in the response. To retrieve the public key in ssh-rsa format, use @SSH@ . To retrieve the public key in PEM format, use @PEM@ .
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


-- | The name of the IAM user associated with the SSH public key. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
gspkUserName :: Lens' GetSSHPublicKey Text
gspkUserName = lens _gspkUserName (\ s a -> s{_gspkUserName = a})

-- | The unique identifier for the SSH public key. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
gspkSSHPublicKeyId :: Lens' GetSSHPublicKey Text
gspkSSHPublicKeyId = lens _gspkSSHPublicKeyId (\ s a -> s{_gspkSSHPublicKeyId = a})

-- | Specifies the public key encoding format to use in the response. To retrieve the public key in ssh-rsa format, use @SSH@ . To retrieve the public key in PEM format, use @PEM@ .
gspkEncoding :: Lens' GetSSHPublicKey EncodingType
gspkEncoding = lens _gspkEncoding (\ s a -> s{_gspkEncoding = a})

instance AWSRequest GetSSHPublicKey where
        type Rs GetSSHPublicKey = GetSSHPublicKeyResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "GetSSHPublicKeyResult"
              (\ s h x ->
                 GetSSHPublicKeyResponse' <$>
                   (x .@? "SSHPublicKey") <*> (pure (fromEnum s)))

instance Hashable GetSSHPublicKey where

instance NFData GetSSHPublicKey where

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

-- | Contains the response to a successful 'GetSSHPublicKey' request.
--
--
--
-- /See:/ 'getSSHPublicKeyResponse' smart constructor.
data GetSSHPublicKeyResponse = GetSSHPublicKeyResponse'
  { _gspkrsSSHPublicKey   :: !(Maybe SSHPublicKey)
  , _gspkrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSSHPublicKeyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gspkrsSSHPublicKey' - A structure containing details about the SSH public key.
--
-- * 'gspkrsResponseStatus' - -- | The response status code.
getSSHPublicKeyResponse
    :: Int -- ^ 'gspkrsResponseStatus'
    -> GetSSHPublicKeyResponse
getSSHPublicKeyResponse pResponseStatus_ =
  GetSSHPublicKeyResponse'
    {_gspkrsSSHPublicKey = Nothing, _gspkrsResponseStatus = pResponseStatus_}


-- | A structure containing details about the SSH public key.
gspkrsSSHPublicKey :: Lens' GetSSHPublicKeyResponse (Maybe SSHPublicKey)
gspkrsSSHPublicKey = lens _gspkrsSSHPublicKey (\ s a -> s{_gspkrsSSHPublicKey = a})

-- | -- | The response status code.
gspkrsResponseStatus :: Lens' GetSSHPublicKeyResponse Int
gspkrsResponseStatus = lens _gspkrsResponseStatus (\ s a -> s{_gspkrsResponseStatus = a})

instance NFData GetSSHPublicKeyResponse where
