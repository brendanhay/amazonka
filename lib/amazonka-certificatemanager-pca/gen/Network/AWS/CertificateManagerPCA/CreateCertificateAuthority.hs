{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.CreateCertificateAuthority
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a root or subordinate private certificate authority (CA). You must specify the CA configuration, the certificate revocation list (CRL) configuration, the CA type, and an optional idempotency token to avoid accidental creation of multiple CAs. The CA configuration specifies the name of the algorithm and key size to be used to create the CA private key, the type of signing algorithm that the CA uses, and X.500 subject information. The CRL configuration specifies the CRL expiration period in days (the validity period of the CRL), the Amazon S3 bucket that will contain the CRL, and a CNAME alias for the S3 bucket that is included in certificates issued by the CA. If successful, this action returns the Amazon Resource Name (ARN) of the CA.
--
--
-- ACM Private CAA assets that are stored in Amazon S3 can be protected with encryption. For more information, see <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaCreateCa.html#crl-encryption Encrypting Your CRLs> .
module Network.AWS.CertificateManagerPCA.CreateCertificateAuthority
  ( -- * Creating a Request
    createCertificateAuthority,
    CreateCertificateAuthority,

    -- * Request Lenses
    ccaIdempotencyToken,
    ccaRevocationConfiguration,
    ccaTags,
    ccaCertificateAuthorityConfiguration,
    ccaCertificateAuthorityType,

    -- * Destructuring the Response
    createCertificateAuthorityResponse,
    CreateCertificateAuthorityResponse,

    -- * Response Lenses
    ccarsCertificateAuthorityARN,
    ccarsResponseStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createCertificateAuthority' smart constructor.
data CreateCertificateAuthority = CreateCertificateAuthority'
  { _ccaIdempotencyToken ::
      !(Maybe Text),
    _ccaRevocationConfiguration ::
      !(Maybe RevocationConfiguration),
    _ccaTags :: !(Maybe (List1 Tag)),
    _ccaCertificateAuthorityConfiguration ::
      !CertificateAuthorityConfiguration,
    _ccaCertificateAuthorityType ::
      !CertificateAuthorityType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCertificateAuthority' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccaIdempotencyToken' - Alphanumeric string that can be used to distinguish between calls to __CreateCertificateAuthority__ . For a given token, ACM Private CA creates exactly one CA. If you issue a subsequent call using the same token, ACM Private CA returns the ARN of the existing CA and takes no further action. If you change the idempotency token across multiple calls, ACM Private CA creates a unique CA for each unique token.
--
-- * 'ccaRevocationConfiguration' - Contains a Boolean value that you can use to enable a certification revocation list (CRL) for the CA, the name of the S3 bucket to which ACM Private CA will write the CRL, and an optional CNAME alias that you can use to hide the name of your bucket in the __CRL Distribution Points__ extension of your CA certificate. For more information, see the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CrlConfiguration.html CrlConfiguration> structure.
--
-- * 'ccaTags' - Key-value pairs that will be attached to the new private CA. You can associate up to 50 tags with a private CA. For information using tags with IAM to manage permissions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags> .
--
-- * 'ccaCertificateAuthorityConfiguration' - Name and bit size of the private key algorithm, the name of the signing algorithm, and X.500 certificate subject information.
--
-- * 'ccaCertificateAuthorityType' - The type of the certificate authority.
createCertificateAuthority ::
  -- | 'ccaCertificateAuthorityConfiguration'
  CertificateAuthorityConfiguration ->
  -- | 'ccaCertificateAuthorityType'
  CertificateAuthorityType ->
  CreateCertificateAuthority
createCertificateAuthority
  pCertificateAuthorityConfiguration_
  pCertificateAuthorityType_ =
    CreateCertificateAuthority'
      { _ccaIdempotencyToken = Nothing,
        _ccaRevocationConfiguration = Nothing,
        _ccaTags = Nothing,
        _ccaCertificateAuthorityConfiguration =
          pCertificateAuthorityConfiguration_,
        _ccaCertificateAuthorityType = pCertificateAuthorityType_
      }

-- | Alphanumeric string that can be used to distinguish between calls to __CreateCertificateAuthority__ . For a given token, ACM Private CA creates exactly one CA. If you issue a subsequent call using the same token, ACM Private CA returns the ARN of the existing CA and takes no further action. If you change the idempotency token across multiple calls, ACM Private CA creates a unique CA for each unique token.
ccaIdempotencyToken :: Lens' CreateCertificateAuthority (Maybe Text)
ccaIdempotencyToken = lens _ccaIdempotencyToken (\s a -> s {_ccaIdempotencyToken = a})

-- | Contains a Boolean value that you can use to enable a certification revocation list (CRL) for the CA, the name of the S3 bucket to which ACM Private CA will write the CRL, and an optional CNAME alias that you can use to hide the name of your bucket in the __CRL Distribution Points__ extension of your CA certificate. For more information, see the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CrlConfiguration.html CrlConfiguration> structure.
ccaRevocationConfiguration :: Lens' CreateCertificateAuthority (Maybe RevocationConfiguration)
ccaRevocationConfiguration = lens _ccaRevocationConfiguration (\s a -> s {_ccaRevocationConfiguration = a})

-- | Key-value pairs that will be attached to the new private CA. You can associate up to 50 tags with a private CA. For information using tags with IAM to manage permissions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_iam-tags.html Controlling Access Using IAM Tags> .
ccaTags :: Lens' CreateCertificateAuthority (Maybe (NonEmpty Tag))
ccaTags = lens _ccaTags (\s a -> s {_ccaTags = a}) . mapping _List1

-- | Name and bit size of the private key algorithm, the name of the signing algorithm, and X.500 certificate subject information.
ccaCertificateAuthorityConfiguration :: Lens' CreateCertificateAuthority CertificateAuthorityConfiguration
ccaCertificateAuthorityConfiguration = lens _ccaCertificateAuthorityConfiguration (\s a -> s {_ccaCertificateAuthorityConfiguration = a})

-- | The type of the certificate authority.
ccaCertificateAuthorityType :: Lens' CreateCertificateAuthority CertificateAuthorityType
ccaCertificateAuthorityType = lens _ccaCertificateAuthorityType (\s a -> s {_ccaCertificateAuthorityType = a})

instance AWSRequest CreateCertificateAuthority where
  type
    Rs CreateCertificateAuthority =
      CreateCertificateAuthorityResponse
  request = postJSON certificateManagerPCA
  response =
    receiveJSON
      ( \s h x ->
          CreateCertificateAuthorityResponse'
            <$> (x .?> "CertificateAuthorityArn") <*> (pure (fromEnum s))
      )

instance Hashable CreateCertificateAuthority

instance NFData CreateCertificateAuthority

instance ToHeaders CreateCertificateAuthority where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("ACMPrivateCA.CreateCertificateAuthority" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateCertificateAuthority where
  toJSON CreateCertificateAuthority' {..} =
    object
      ( catMaybes
          [ ("IdempotencyToken" .=) <$> _ccaIdempotencyToken,
            ("RevocationConfiguration" .=) <$> _ccaRevocationConfiguration,
            ("Tags" .=) <$> _ccaTags,
            Just
              ( "CertificateAuthorityConfiguration"
                  .= _ccaCertificateAuthorityConfiguration
              ),
            Just ("CertificateAuthorityType" .= _ccaCertificateAuthorityType)
          ]
      )

instance ToPath CreateCertificateAuthority where
  toPath = const "/"

instance ToQuery CreateCertificateAuthority where
  toQuery = const mempty

-- | /See:/ 'createCertificateAuthorityResponse' smart constructor.
data CreateCertificateAuthorityResponse = CreateCertificateAuthorityResponse'
  { _ccarsCertificateAuthorityARN ::
      !(Maybe Text),
    _ccarsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCertificateAuthorityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccarsCertificateAuthorityARN' - If successful, the Amazon Resource Name (ARN) of the certificate authority (CA). This is of the form:  @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- * 'ccarsResponseStatus' - -- | The response status code.
createCertificateAuthorityResponse ::
  -- | 'ccarsResponseStatus'
  Int ->
  CreateCertificateAuthorityResponse
createCertificateAuthorityResponse pResponseStatus_ =
  CreateCertificateAuthorityResponse'
    { _ccarsCertificateAuthorityARN =
        Nothing,
      _ccarsResponseStatus = pResponseStatus_
    }

-- | If successful, the Amazon Resource Name (ARN) of the certificate authority (CA). This is of the form:  @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
ccarsCertificateAuthorityARN :: Lens' CreateCertificateAuthorityResponse (Maybe Text)
ccarsCertificateAuthorityARN = lens _ccarsCertificateAuthorityARN (\s a -> s {_ccarsCertificateAuthorityARN = a})

-- | -- | The response status code.
ccarsResponseStatus :: Lens' CreateCertificateAuthorityResponse Int
ccarsResponseStatus = lens _ccarsResponseStatus (\s a -> s {_ccarsResponseStatus = a})

instance NFData CreateCertificateAuthorityResponse
