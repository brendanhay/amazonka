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
-- Module      : Network.AWS.IoT.CreateKeysAndCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 2048-bit RSA key pair and issues an X.509 certificate using the issued public key.
--
--
-- __Note__ This is the only time AWS IoT issues the private key for this certificate, so it is important to keep it in a secure location.
--
module Network.AWS.IoT.CreateKeysAndCertificate
    (
    -- * Creating a Request
      createKeysAndCertificate
    , CreateKeysAndCertificate
    -- * Request Lenses
    , ckacSetAsActive

    -- * Destructuring the Response
    , createKeysAndCertificateResponse
    , CreateKeysAndCertificateResponse
    -- * Response Lenses
    , ckacrsKeyPair
    , ckacrsCertificatePem
    , ckacrsCertificateARN
    , ckacrsCertificateId
    , ckacrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the CreateKeysAndCertificate operation.
--
--
--
-- /See:/ 'createKeysAndCertificate' smart constructor.
newtype CreateKeysAndCertificate = CreateKeysAndCertificate'
  { _ckacSetAsActive :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateKeysAndCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ckacSetAsActive' - Specifies whether the certificate is active.
createKeysAndCertificate
    :: CreateKeysAndCertificate
createKeysAndCertificate =
  CreateKeysAndCertificate' {_ckacSetAsActive = Nothing}


-- | Specifies whether the certificate is active.
ckacSetAsActive :: Lens' CreateKeysAndCertificate (Maybe Bool)
ckacSetAsActive = lens _ckacSetAsActive (\ s a -> s{_ckacSetAsActive = a})

instance AWSRequest CreateKeysAndCertificate where
        type Rs CreateKeysAndCertificate =
             CreateKeysAndCertificateResponse
        request = postJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 CreateKeysAndCertificateResponse' <$>
                   (x .?> "keyPair") <*> (x .?> "certificatePem") <*>
                     (x .?> "certificateArn")
                     <*> (x .?> "certificateId")
                     <*> (pure (fromEnum s)))

instance Hashable CreateKeysAndCertificate where

instance NFData CreateKeysAndCertificate where

instance ToHeaders CreateKeysAndCertificate where
        toHeaders = const mempty

instance ToJSON CreateKeysAndCertificate where
        toJSON = const (Object mempty)

instance ToPath CreateKeysAndCertificate where
        toPath = const "/keys-and-certificate"

instance ToQuery CreateKeysAndCertificate where
        toQuery CreateKeysAndCertificate'{..}
          = mconcat ["setAsActive" =: _ckacSetAsActive]

-- | The output of the CreateKeysAndCertificate operation.
--
--
--
-- /See:/ 'createKeysAndCertificateResponse' smart constructor.
data CreateKeysAndCertificateResponse = CreateKeysAndCertificateResponse'
  { _ckacrsKeyPair        :: !(Maybe KeyPair)
  , _ckacrsCertificatePem :: !(Maybe Text)
  , _ckacrsCertificateARN :: !(Maybe Text)
  , _ckacrsCertificateId  :: !(Maybe Text)
  , _ckacrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateKeysAndCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ckacrsKeyPair' - The generated key pair.
--
-- * 'ckacrsCertificatePem' - The certificate data, in PEM format.
--
-- * 'ckacrsCertificateARN' - The ARN of the certificate.
--
-- * 'ckacrsCertificateId' - The ID of the certificate. AWS IoT issues a default subject name for the certificate (for example, AWS IoT Certificate).
--
-- * 'ckacrsResponseStatus' - -- | The response status code.
createKeysAndCertificateResponse
    :: Int -- ^ 'ckacrsResponseStatus'
    -> CreateKeysAndCertificateResponse
createKeysAndCertificateResponse pResponseStatus_ =
  CreateKeysAndCertificateResponse'
    { _ckacrsKeyPair = Nothing
    , _ckacrsCertificatePem = Nothing
    , _ckacrsCertificateARN = Nothing
    , _ckacrsCertificateId = Nothing
    , _ckacrsResponseStatus = pResponseStatus_
    }


-- | The generated key pair.
ckacrsKeyPair :: Lens' CreateKeysAndCertificateResponse (Maybe KeyPair)
ckacrsKeyPair = lens _ckacrsKeyPair (\ s a -> s{_ckacrsKeyPair = a})

-- | The certificate data, in PEM format.
ckacrsCertificatePem :: Lens' CreateKeysAndCertificateResponse (Maybe Text)
ckacrsCertificatePem = lens _ckacrsCertificatePem (\ s a -> s{_ckacrsCertificatePem = a})

-- | The ARN of the certificate.
ckacrsCertificateARN :: Lens' CreateKeysAndCertificateResponse (Maybe Text)
ckacrsCertificateARN = lens _ckacrsCertificateARN (\ s a -> s{_ckacrsCertificateARN = a})

-- | The ID of the certificate. AWS IoT issues a default subject name for the certificate (for example, AWS IoT Certificate).
ckacrsCertificateId :: Lens' CreateKeysAndCertificateResponse (Maybe Text)
ckacrsCertificateId = lens _ckacrsCertificateId (\ s a -> s{_ckacrsCertificateId = a})

-- | -- | The response status code.
ckacrsResponseStatus :: Lens' CreateKeysAndCertificateResponse Int
ckacrsResponseStatus = lens _ckacrsResponseStatus (\ s a -> s{_ckacrsResponseStatus = a})

instance NFData CreateKeysAndCertificateResponse
         where
