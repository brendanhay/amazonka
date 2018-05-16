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
-- Module      : Network.AWS.CertificateManagerPCA.IssueCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uses your private certificate authority (CA) to issue a client certificate. This function returns the Amazon Resource Name (ARN) of the certificate. You can retrieve the certificate by calling the 'GetCertificate' function and specifying the ARN.
--
--
module Network.AWS.CertificateManagerPCA.IssueCertificate
    (
    -- * Creating a Request
      issueCertificate
    , IssueCertificate
    -- * Request Lenses
    , icIdempotencyToken
    , icCertificateAuthorityARN
    , icCSR
    , icSigningAlgorithm
    , icValidity

    -- * Destructuring the Response
    , issueCertificateResponse
    , IssueCertificateResponse
    -- * Response Lenses
    , icrsCertificateARN
    , icrsResponseStatus
    ) where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.CertificateManagerPCA.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'issueCertificate' smart constructor.
data IssueCertificate = IssueCertificate'
  { _icIdempotencyToken        :: !(Maybe Text)
  , _icCertificateAuthorityARN :: !Text
  , _icCSR                     :: !Base64
  , _icSigningAlgorithm        :: !SigningAlgorithm
  , _icValidity                :: !Validity
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IssueCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icIdempotencyToken' - Custom string that can be used to distinguish between calls to the __IssueCertificate__ function. Idempotency tokens time out after one hour. Therefore, if you call __IssueCertificate__ multiple times with the same idempotency token within 5 minutes, ACM PCA recognizes that you are requesting only one certificate and will issue only one. If you change the idempotency token for each call, PCA recognizes that you are requesting multiple certificates.
--
-- * 'icCertificateAuthorityARN' - The Amazon Resource Name (ARN) that was returned when you called 'CreateCertificateAuthority' . This must be of the form: @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
--
-- * 'icCSR' - The certificate signing request (CSR) for the certificate you want to issue. You can use the following OpenSSL command to create the CSR and a 2048 bit RSA private key.  @openssl req -new -newkey rsa:2048 -days 365 -keyout private/test_cert_priv_key.pem -out csr/test_cert_.csr@  If you have a configuration file, you can use the following OpenSSL command. The @usr_cert@ block in the configuration file contains your X509 version 3 extensions.  @openssl req -new -config openssl_rsa.cnf -extensions usr_cert -newkey rsa:2048 -days -365 -keyout private/test_cert_priv_key.pem -out csr/test_cert_.csr@ -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'icSigningAlgorithm' - The name of the algorithm that will be used to sign the certificate to be issued.
--
-- * 'icValidity' - The type of the validity period.
issueCertificate
    :: Text -- ^ 'icCertificateAuthorityARN'
    -> ByteString -- ^ 'icCSR'
    -> SigningAlgorithm -- ^ 'icSigningAlgorithm'
    -> Validity -- ^ 'icValidity'
    -> IssueCertificate
issueCertificate pCertificateAuthorityARN_ pCSR_ pSigningAlgorithm_ pValidity_ =
  IssueCertificate'
    { _icIdempotencyToken = Nothing
    , _icCertificateAuthorityARN = pCertificateAuthorityARN_
    , _icCSR = _Base64 # pCSR_
    , _icSigningAlgorithm = pSigningAlgorithm_
    , _icValidity = pValidity_
    }


-- | Custom string that can be used to distinguish between calls to the __IssueCertificate__ function. Idempotency tokens time out after one hour. Therefore, if you call __IssueCertificate__ multiple times with the same idempotency token within 5 minutes, ACM PCA recognizes that you are requesting only one certificate and will issue only one. If you change the idempotency token for each call, PCA recognizes that you are requesting multiple certificates.
icIdempotencyToken :: Lens' IssueCertificate (Maybe Text)
icIdempotencyToken = lens _icIdempotencyToken (\ s a -> s{_icIdempotencyToken = a})

-- | The Amazon Resource Name (ARN) that was returned when you called 'CreateCertificateAuthority' . This must be of the form: @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
icCertificateAuthorityARN :: Lens' IssueCertificate Text
icCertificateAuthorityARN = lens _icCertificateAuthorityARN (\ s a -> s{_icCertificateAuthorityARN = a})

-- | The certificate signing request (CSR) for the certificate you want to issue. You can use the following OpenSSL command to create the CSR and a 2048 bit RSA private key.  @openssl req -new -newkey rsa:2048 -days 365 -keyout private/test_cert_priv_key.pem -out csr/test_cert_.csr@  If you have a configuration file, you can use the following OpenSSL command. The @usr_cert@ block in the configuration file contains your X509 version 3 extensions.  @openssl req -new -config openssl_rsa.cnf -extensions usr_cert -newkey rsa:2048 -days -365 -keyout private/test_cert_priv_key.pem -out csr/test_cert_.csr@ -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
icCSR :: Lens' IssueCertificate ByteString
icCSR = lens _icCSR (\ s a -> s{_icCSR = a}) . _Base64

-- | The name of the algorithm that will be used to sign the certificate to be issued.
icSigningAlgorithm :: Lens' IssueCertificate SigningAlgorithm
icSigningAlgorithm = lens _icSigningAlgorithm (\ s a -> s{_icSigningAlgorithm = a})

-- | The type of the validity period.
icValidity :: Lens' IssueCertificate Validity
icValidity = lens _icValidity (\ s a -> s{_icValidity = a})

instance AWSRequest IssueCertificate where
        type Rs IssueCertificate = IssueCertificateResponse
        request = postJSON certificateManagerPCA
        response
          = receiveJSON
              (\ s h x ->
                 IssueCertificateResponse' <$>
                   (x .?> "CertificateArn") <*> (pure (fromEnum s)))

instance Hashable IssueCertificate where

instance NFData IssueCertificate where

instance ToHeaders IssueCertificate where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ACMPrivateCA.IssueCertificate" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON IssueCertificate where
        toJSON IssueCertificate'{..}
          = object
              (catMaybes
                 [("IdempotencyToken" .=) <$> _icIdempotencyToken,
                  Just
                    ("CertificateAuthorityArn" .=
                       _icCertificateAuthorityARN),
                  Just ("Csr" .= _icCSR),
                  Just ("SigningAlgorithm" .= _icSigningAlgorithm),
                  Just ("Validity" .= _icValidity)])

instance ToPath IssueCertificate where
        toPath = const "/"

instance ToQuery IssueCertificate where
        toQuery = const mempty

-- | /See:/ 'issueCertificateResponse' smart constructor.
data IssueCertificateResponse = IssueCertificateResponse'
  { _icrsCertificateARN :: !(Maybe Text)
  , _icrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IssueCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icrsCertificateARN' - The Amazon Resource Name (ARN) of the issued certificate and the certificate serial number. This is of the form: @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ /certificate//286535153982981100925020015808220737245/ @
--
-- * 'icrsResponseStatus' - -- | The response status code.
issueCertificateResponse
    :: Int -- ^ 'icrsResponseStatus'
    -> IssueCertificateResponse
issueCertificateResponse pResponseStatus_ =
  IssueCertificateResponse'
    {_icrsCertificateARN = Nothing, _icrsResponseStatus = pResponseStatus_}


-- | The Amazon Resource Name (ARN) of the issued certificate and the certificate serial number. This is of the form: @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ /certificate//286535153982981100925020015808220737245/ @
icrsCertificateARN :: Lens' IssueCertificateResponse (Maybe Text)
icrsCertificateARN = lens _icrsCertificateARN (\ s a -> s{_icrsCertificateARN = a})

-- | -- | The response status code.
icrsResponseStatus :: Lens' IssueCertificateResponse Int
icrsResponseStatus = lens _icrsResponseStatus (\ s a -> s{_icrsResponseStatus = a})

instance NFData IssueCertificateResponse where
