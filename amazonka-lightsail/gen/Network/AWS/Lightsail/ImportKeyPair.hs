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
-- Module      : Network.AWS.Lightsail.ImportKeyPair
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports a public SSH key from a specific key pair.
--
--
module Network.AWS.Lightsail.ImportKeyPair
    (
    -- * Creating a Request
      importKeyPair
    , ImportKeyPair
    -- * Request Lenses
    , ikpKeyPairName
    , ikpPublicKeyBase64

    -- * Destructuring the Response
    , importKeyPairResponse
    , ImportKeyPairResponse
    -- * Response Lenses
    , ikprsOperation
    , ikprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'importKeyPair' smart constructor.
data ImportKeyPair = ImportKeyPair'
  { _ikpKeyPairName     :: !Text
  , _ikpPublicKeyBase64 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportKeyPair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ikpKeyPairName' - The name of the key pair for which you want to import the public key.
--
-- * 'ikpPublicKeyBase64' - A base64-encoded public key of the @ssh-rsa@ type.
importKeyPair
    :: Text -- ^ 'ikpKeyPairName'
    -> Text -- ^ 'ikpPublicKeyBase64'
    -> ImportKeyPair
importKeyPair pKeyPairName_ pPublicKeyBase64_ =
  ImportKeyPair'
    {_ikpKeyPairName = pKeyPairName_, _ikpPublicKeyBase64 = pPublicKeyBase64_}


-- | The name of the key pair for which you want to import the public key.
ikpKeyPairName :: Lens' ImportKeyPair Text
ikpKeyPairName = lens _ikpKeyPairName (\ s a -> s{_ikpKeyPairName = a})

-- | A base64-encoded public key of the @ssh-rsa@ type.
ikpPublicKeyBase64 :: Lens' ImportKeyPair Text
ikpPublicKeyBase64 = lens _ikpPublicKeyBase64 (\ s a -> s{_ikpPublicKeyBase64 = a})

instance AWSRequest ImportKeyPair where
        type Rs ImportKeyPair = ImportKeyPairResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 ImportKeyPairResponse' <$>
                   (x .?> "operation") <*> (pure (fromEnum s)))

instance Hashable ImportKeyPair where

instance NFData ImportKeyPair where

instance ToHeaders ImportKeyPair where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.ImportKeyPair" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ImportKeyPair where
        toJSON ImportKeyPair'{..}
          = object
              (catMaybes
                 [Just ("keyPairName" .= _ikpKeyPairName),
                  Just ("publicKeyBase64" .= _ikpPublicKeyBase64)])

instance ToPath ImportKeyPair where
        toPath = const "/"

instance ToQuery ImportKeyPair where
        toQuery = const mempty

-- | /See:/ 'importKeyPairResponse' smart constructor.
data ImportKeyPairResponse = ImportKeyPairResponse'
  { _ikprsOperation      :: !(Maybe Operation)
  , _ikprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportKeyPairResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ikprsOperation' - An array of key-value pairs containing information about the request operation.
--
-- * 'ikprsResponseStatus' - -- | The response status code.
importKeyPairResponse
    :: Int -- ^ 'ikprsResponseStatus'
    -> ImportKeyPairResponse
importKeyPairResponse pResponseStatus_ =
  ImportKeyPairResponse'
    {_ikprsOperation = Nothing, _ikprsResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about the request operation.
ikprsOperation :: Lens' ImportKeyPairResponse (Maybe Operation)
ikprsOperation = lens _ikprsOperation (\ s a -> s{_ikprsOperation = a})

-- | -- | The response status code.
ikprsResponseStatus :: Lens' ImportKeyPairResponse Int
ikprsResponseStatus = lens _ikprsResponseStatus (\ s a -> s{_ikprsResponseStatus = a})

instance NFData ImportKeyPairResponse where
