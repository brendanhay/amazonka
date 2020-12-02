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
-- Module      : Network.AWS.Lightsail.CreateKeyPair
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates sn SSH key pair.
--
--
module Network.AWS.Lightsail.CreateKeyPair
    (
    -- * Creating a Request
      createKeyPair
    , CreateKeyPair
    -- * Request Lenses
    , ckpKeyPairName

    -- * Destructuring the Response
    , createKeyPairResponse
    , CreateKeyPairResponse
    -- * Response Lenses
    , ckprsKeyPair
    , ckprsOperation
    , ckprsPublicKeyBase64
    , ckprsPrivateKeyBase64
    , ckprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createKeyPair' smart constructor.
newtype CreateKeyPair = CreateKeyPair'
  { _ckpKeyPairName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateKeyPair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ckpKeyPairName' - The name for your new key pair.
createKeyPair
    :: Text -- ^ 'ckpKeyPairName'
    -> CreateKeyPair
createKeyPair pKeyPairName_ = CreateKeyPair' {_ckpKeyPairName = pKeyPairName_}


-- | The name for your new key pair.
ckpKeyPairName :: Lens' CreateKeyPair Text
ckpKeyPairName = lens _ckpKeyPairName (\ s a -> s{_ckpKeyPairName = a})

instance AWSRequest CreateKeyPair where
        type Rs CreateKeyPair = CreateKeyPairResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 CreateKeyPairResponse' <$>
                   (x .?> "keyPair") <*> (x .?> "operation") <*>
                     (x .?> "publicKeyBase64")
                     <*> (x .?> "privateKeyBase64")
                     <*> (pure (fromEnum s)))

instance Hashable CreateKeyPair where

instance NFData CreateKeyPair where

instance ToHeaders CreateKeyPair where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.CreateKeyPair" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateKeyPair where
        toJSON CreateKeyPair'{..}
          = object
              (catMaybes [Just ("keyPairName" .= _ckpKeyPairName)])

instance ToPath CreateKeyPair where
        toPath = const "/"

instance ToQuery CreateKeyPair where
        toQuery = const mempty

-- | /See:/ 'createKeyPairResponse' smart constructor.
data CreateKeyPairResponse = CreateKeyPairResponse'
  { _ckprsKeyPair          :: !(Maybe KeyPair)
  , _ckprsOperation        :: !(Maybe Operation)
  , _ckprsPublicKeyBase64  :: !(Maybe Text)
  , _ckprsPrivateKeyBase64 :: !(Maybe Text)
  , _ckprsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateKeyPairResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ckprsKeyPair' - An array of key-value pairs containing information about the new key pair you just created.
--
-- * 'ckprsOperation' - An array of key-value pairs containing information about the results of your create key pair request.
--
-- * 'ckprsPublicKeyBase64' - A base64-encoded public key of the @ssh-rsa@ type.
--
-- * 'ckprsPrivateKeyBase64' - A base64-encoded RSA private key.
--
-- * 'ckprsResponseStatus' - -- | The response status code.
createKeyPairResponse
    :: Int -- ^ 'ckprsResponseStatus'
    -> CreateKeyPairResponse
createKeyPairResponse pResponseStatus_ =
  CreateKeyPairResponse'
    { _ckprsKeyPair = Nothing
    , _ckprsOperation = Nothing
    , _ckprsPublicKeyBase64 = Nothing
    , _ckprsPrivateKeyBase64 = Nothing
    , _ckprsResponseStatus = pResponseStatus_
    }


-- | An array of key-value pairs containing information about the new key pair you just created.
ckprsKeyPair :: Lens' CreateKeyPairResponse (Maybe KeyPair)
ckprsKeyPair = lens _ckprsKeyPair (\ s a -> s{_ckprsKeyPair = a})

-- | An array of key-value pairs containing information about the results of your create key pair request.
ckprsOperation :: Lens' CreateKeyPairResponse (Maybe Operation)
ckprsOperation = lens _ckprsOperation (\ s a -> s{_ckprsOperation = a})

-- | A base64-encoded public key of the @ssh-rsa@ type.
ckprsPublicKeyBase64 :: Lens' CreateKeyPairResponse (Maybe Text)
ckprsPublicKeyBase64 = lens _ckprsPublicKeyBase64 (\ s a -> s{_ckprsPublicKeyBase64 = a})

-- | A base64-encoded RSA private key.
ckprsPrivateKeyBase64 :: Lens' CreateKeyPairResponse (Maybe Text)
ckprsPrivateKeyBase64 = lens _ckprsPrivateKeyBase64 (\ s a -> s{_ckprsPrivateKeyBase64 = a})

-- | -- | The response status code.
ckprsResponseStatus :: Lens' CreateKeyPairResponse Int
ckprsResponseStatus = lens _ckprsResponseStatus (\ s a -> s{_ckprsResponseStatus = a})

instance NFData CreateKeyPairResponse where
