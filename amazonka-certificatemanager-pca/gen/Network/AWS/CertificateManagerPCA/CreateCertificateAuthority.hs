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
-- Module      : Network.AWS.CertificateManagerPCA.CreateCertificateAuthority
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a private subordinate certificate authority (CA). You must specify the CA configuration, the revocation configuration, the CA type, and an optional idempotency token. The CA configuration specifies the name of the algorithm and key size to be used to create the CA private key, the type of signing algorithm that the CA uses to sign, and X.500 subject information. The CRL (certificate revocation list) configuration specifies the CRL expiration period in days (the validity period of the CRL), the Amazon S3 bucket that will contain the CRL, and a CNAME alias for the S3 bucket that is included in certificates issued by the CA. If successful, this function returns the Amazon Resource Name (ARN) of the CA.
--
--
module Network.AWS.CertificateManagerPCA.CreateCertificateAuthority
    (
    -- * Creating a Request
      createCertificateAuthority
    , CreateCertificateAuthority
    -- * Request Lenses
    , ccaIdempotencyToken
    , ccaRevocationConfiguration
    , ccaCertificateAuthorityConfiguration
    , ccaCertificateAuthorityType

    -- * Destructuring the Response
    , createCertificateAuthorityResponse
    , CreateCertificateAuthorityResponse
    -- * Response Lenses
    , ccarsCertificateAuthorityARN
    , ccarsResponseStatus
    ) where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.CertificateManagerPCA.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createCertificateAuthority' smart constructor.
data CreateCertificateAuthority = CreateCertificateAuthority'
  { _ccaIdempotencyToken                  :: !(Maybe Text)
  , _ccaRevocationConfiguration           :: !(Maybe RevocationConfiguration)
  , _ccaCertificateAuthorityConfiguration :: !CertificateAuthorityConfiguration
  , _ccaCertificateAuthorityType          :: !CertificateAuthorityType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCertificateAuthority' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccaIdempotencyToken' - Alphanumeric string that can be used to distinguish between calls to __CreateCertificateAuthority__ . Idempotency tokens time out after five minutes. Therefore, if you call __CreateCertificateAuthority__ multiple times with the same idempotency token within a five minute period, ACM PCA recognizes that you are requesting only one certificate and will issue only one. If you change the idempotency token for each call, however, ACM PCA recognizes that you are requesting multiple certificates.
--
-- * 'ccaRevocationConfiguration' - Contains a Boolean value that you can use to enable a certification revocation list (CRL) for the CA, the name of the S3 bucket to which ACM PCA will write the CRL, and an optional CNAME alias that you can use to hide the name of your bucket in the __CRL Distribution Points__ extension of your CA certificate. For more information, see the 'CrlConfiguration' structure.
--
-- * 'ccaCertificateAuthorityConfiguration' - Name and bit size of the private key algorithm, the name of the signing algorithm, and X.500 certificate subject information.
--
-- * 'ccaCertificateAuthorityType' - The type of the certificate authority. Currently, this must be __SUBORDINATE__ .
createCertificateAuthority
    :: CertificateAuthorityConfiguration -- ^ 'ccaCertificateAuthorityConfiguration'
    -> CertificateAuthorityType -- ^ 'ccaCertificateAuthorityType'
    -> CreateCertificateAuthority
createCertificateAuthority pCertificateAuthorityConfiguration_ pCertificateAuthorityType_ =
  CreateCertificateAuthority'
    { _ccaIdempotencyToken = Nothing
    , _ccaRevocationConfiguration = Nothing
    , _ccaCertificateAuthorityConfiguration =
        pCertificateAuthorityConfiguration_
    , _ccaCertificateAuthorityType = pCertificateAuthorityType_
    }


-- | Alphanumeric string that can be used to distinguish between calls to __CreateCertificateAuthority__ . Idempotency tokens time out after five minutes. Therefore, if you call __CreateCertificateAuthority__ multiple times with the same idempotency token within a five minute period, ACM PCA recognizes that you are requesting only one certificate and will issue only one. If you change the idempotency token for each call, however, ACM PCA recognizes that you are requesting multiple certificates.
ccaIdempotencyToken :: Lens' CreateCertificateAuthority (Maybe Text)
ccaIdempotencyToken = lens _ccaIdempotencyToken (\ s a -> s{_ccaIdempotencyToken = a})

-- | Contains a Boolean value that you can use to enable a certification revocation list (CRL) for the CA, the name of the S3 bucket to which ACM PCA will write the CRL, and an optional CNAME alias that you can use to hide the name of your bucket in the __CRL Distribution Points__ extension of your CA certificate. For more information, see the 'CrlConfiguration' structure.
ccaRevocationConfiguration :: Lens' CreateCertificateAuthority (Maybe RevocationConfiguration)
ccaRevocationConfiguration = lens _ccaRevocationConfiguration (\ s a -> s{_ccaRevocationConfiguration = a})

-- | Name and bit size of the private key algorithm, the name of the signing algorithm, and X.500 certificate subject information.
ccaCertificateAuthorityConfiguration :: Lens' CreateCertificateAuthority CertificateAuthorityConfiguration
ccaCertificateAuthorityConfiguration = lens _ccaCertificateAuthorityConfiguration (\ s a -> s{_ccaCertificateAuthorityConfiguration = a})

-- | The type of the certificate authority. Currently, this must be __SUBORDINATE__ .
ccaCertificateAuthorityType :: Lens' CreateCertificateAuthority CertificateAuthorityType
ccaCertificateAuthorityType = lens _ccaCertificateAuthorityType (\ s a -> s{_ccaCertificateAuthorityType = a})

instance AWSRequest CreateCertificateAuthority where
        type Rs CreateCertificateAuthority =
             CreateCertificateAuthorityResponse
        request = postJSON certificateManagerPCA
        response
          = receiveJSON
              (\ s h x ->
                 CreateCertificateAuthorityResponse' <$>
                   (x .?> "CertificateAuthorityArn") <*>
                     (pure (fromEnum s)))

instance Hashable CreateCertificateAuthority where

instance NFData CreateCertificateAuthority where

instance ToHeaders CreateCertificateAuthority where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ACMPrivateCA.CreateCertificateAuthority" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateCertificateAuthority where
        toJSON CreateCertificateAuthority'{..}
          = object
              (catMaybes
                 [("IdempotencyToken" .=) <$> _ccaIdempotencyToken,
                  ("RevocationConfiguration" .=) <$>
                    _ccaRevocationConfiguration,
                  Just
                    ("CertificateAuthorityConfiguration" .=
                       _ccaCertificateAuthorityConfiguration),
                  Just
                    ("CertificateAuthorityType" .=
                       _ccaCertificateAuthorityType)])

instance ToPath CreateCertificateAuthority where
        toPath = const "/"

instance ToQuery CreateCertificateAuthority where
        toQuery = const mempty

-- | /See:/ 'createCertificateAuthorityResponse' smart constructor.
data CreateCertificateAuthorityResponse = CreateCertificateAuthorityResponse'
  { _ccarsCertificateAuthorityARN :: !(Maybe Text)
  , _ccarsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCertificateAuthorityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccarsCertificateAuthorityARN' - If successful, the Amazon Resource Name (ARN) of the certificate authority (CA). This is of the form:  @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- * 'ccarsResponseStatus' - -- | The response status code.
createCertificateAuthorityResponse
    :: Int -- ^ 'ccarsResponseStatus'
    -> CreateCertificateAuthorityResponse
createCertificateAuthorityResponse pResponseStatus_ =
  CreateCertificateAuthorityResponse'
    { _ccarsCertificateAuthorityARN = Nothing
    , _ccarsResponseStatus = pResponseStatus_
    }


-- | If successful, the Amazon Resource Name (ARN) of the certificate authority (CA). This is of the form:  @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
ccarsCertificateAuthorityARN :: Lens' CreateCertificateAuthorityResponse (Maybe Text)
ccarsCertificateAuthorityARN = lens _ccarsCertificateAuthorityARN (\ s a -> s{_ccarsCertificateAuthorityARN = a})

-- | -- | The response status code.
ccarsResponseStatus :: Lens' CreateCertificateAuthorityResponse Int
ccarsResponseStatus = lens _ccarsResponseStatus (\ s a -> s{_ccarsResponseStatus = a})

instance NFData CreateCertificateAuthorityResponse
         where
