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
-- Module      : Network.AWS.Lightsail.DownloadDefaultKeyPair
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Downloads the default SSH key pair from the user's account.
--
--
module Network.AWS.Lightsail.DownloadDefaultKeyPair
    (
    -- * Creating a Request
      downloadDefaultKeyPair
    , DownloadDefaultKeyPair

    -- * Destructuring the Response
    , downloadDefaultKeyPairResponse
    , DownloadDefaultKeyPairResponse
    -- * Response Lenses
    , ddkprsPublicKeyBase64
    , ddkprsPrivateKeyBase64
    , ddkprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'downloadDefaultKeyPair' smart constructor.
data DownloadDefaultKeyPair =
  DownloadDefaultKeyPair'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DownloadDefaultKeyPair' with the minimum fields required to make a request.
--
downloadDefaultKeyPair
    :: DownloadDefaultKeyPair
downloadDefaultKeyPair = DownloadDefaultKeyPair'


instance AWSRequest DownloadDefaultKeyPair where
        type Rs DownloadDefaultKeyPair =
             DownloadDefaultKeyPairResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 DownloadDefaultKeyPairResponse' <$>
                   (x .?> "publicKeyBase64") <*>
                     (x .?> "privateKeyBase64")
                     <*> (pure (fromEnum s)))

instance Hashable DownloadDefaultKeyPair where

instance NFData DownloadDefaultKeyPair where

instance ToHeaders DownloadDefaultKeyPair where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.DownloadDefaultKeyPair" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DownloadDefaultKeyPair where
        toJSON = const (Object mempty)

instance ToPath DownloadDefaultKeyPair where
        toPath = const "/"

instance ToQuery DownloadDefaultKeyPair where
        toQuery = const mempty

-- | /See:/ 'downloadDefaultKeyPairResponse' smart constructor.
data DownloadDefaultKeyPairResponse = DownloadDefaultKeyPairResponse'
  { _ddkprsPublicKeyBase64  :: !(Maybe Text)
  , _ddkprsPrivateKeyBase64 :: !(Maybe Text)
  , _ddkprsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DownloadDefaultKeyPairResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddkprsPublicKeyBase64' - A base64-encoded public key of the @ssh-rsa@ type.
--
-- * 'ddkprsPrivateKeyBase64' - A base64-encoded RSA private key.
--
-- * 'ddkprsResponseStatus' - -- | The response status code.
downloadDefaultKeyPairResponse
    :: Int -- ^ 'ddkprsResponseStatus'
    -> DownloadDefaultKeyPairResponse
downloadDefaultKeyPairResponse pResponseStatus_ =
  DownloadDefaultKeyPairResponse'
    { _ddkprsPublicKeyBase64 = Nothing
    , _ddkprsPrivateKeyBase64 = Nothing
    , _ddkprsResponseStatus = pResponseStatus_
    }


-- | A base64-encoded public key of the @ssh-rsa@ type.
ddkprsPublicKeyBase64 :: Lens' DownloadDefaultKeyPairResponse (Maybe Text)
ddkprsPublicKeyBase64 = lens _ddkprsPublicKeyBase64 (\ s a -> s{_ddkprsPublicKeyBase64 = a})

-- | A base64-encoded RSA private key.
ddkprsPrivateKeyBase64 :: Lens' DownloadDefaultKeyPairResponse (Maybe Text)
ddkprsPrivateKeyBase64 = lens _ddkprsPrivateKeyBase64 (\ s a -> s{_ddkprsPrivateKeyBase64 = a})

-- | -- | The response status code.
ddkprsResponseStatus :: Lens' DownloadDefaultKeyPairResponse Int
ddkprsResponseStatus = lens _ddkprsResponseStatus (\ s a -> s{_ddkprsResponseStatus = a})

instance NFData DownloadDefaultKeyPairResponse where
