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
-- Module      : Network.AWS.CertificateManagerPCA.ImportCertificateAuthorityCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports a signed private CA certificate into ACM Private CA. This action is used when you are using a chain of trust whose root is located outside ACM Private CA. Before you can call this action, the following preparations must in place:
--
--
--     * In ACM Private CA, call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action to create the private CA that that you plan to back with the imported certificate.
--
--     * Call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetCertificateAuthorityCsr.html GetCertificateAuthorityCsr> action to generate a certificate signing request (CSR).
--
--     * Sign the CSR using a root or intermediate CA hosted by either an on-premises PKI hierarchy or by a commercial CA.
--
--     * Create a certificate chain and copy the signed certificate and the certificate chain to your working directory.
--
--
--
-- The following requirements apply when you import a CA certificate.
--
--     * You cannot import a non-self-signed certificate for use as a root CA.
--
--     * You cannot import a self-signed certificate for use as a subordinate CA.
--
--     * Your certificate chain must not include the private CA certificate that you are importing.
--
--     * Your ACM Private CA-hosted or on-premises CA certificate must be the last certificate in your chain. The subordinate certificate, if any, that your root CA signed must be next to last. The subordinate certificate signed by the preceding subordinate CA must come next, and so on until your chain is built.
--
--     * The chain must be PEM-encoded.
--
--     * The maximum allowed size of a certificate is 32 KB.
--
--     * The maximum allowed size of a certificate chain is 2 MB.
--
--
--
-- /Enforcement of Critical Constraints/
--
-- ACM Private CA allows the following extensions to be marked critical in the imported CA certificate or chain.
--
--     * Basic constraints (/must/ be marked critical)
--
--     * Subject alternative names
--
--     * Key usage
--
--     * Extended key usage
--
--     * Authority key identifier
--
--     * Subject key identifier
--
--     * Issuer alternative name
--
--     * Subject directory attributes
--
--     * Subject information access
--
--     * Certificate policies
--
--     * Policy mappings
--
--     * Inhibit anyPolicy
--
--
--
-- ACM Private CA rejects the following extensions when they are marked critical in an imported CA certificate or chain.
--
--     * Name constraints
--
--     * Policy constraints
--
--     * CRL distribution points
--
--     * Authority information access
--
--     * Freshest CRL
--
--     * Any other extension
module Network.AWS.CertificateManagerPCA.ImportCertificateAuthorityCertificate
  ( -- * Creating a Request
    importCertificateAuthorityCertificate,
    ImportCertificateAuthorityCertificate,

    -- * Request Lenses
    icacCertificateChain,
    icacCertificateAuthorityARN,
    icacCertificate,

    -- * Destructuring the Response
    importCertificateAuthorityCertificateResponse,
    ImportCertificateAuthorityCertificateResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'importCertificateAuthorityCertificate' smart constructor.
data ImportCertificateAuthorityCertificate = ImportCertificateAuthorityCertificate'
  { _icacCertificateChain ::
      !(Maybe Base64),
    _icacCertificateAuthorityARN ::
      !Text,
    _icacCertificate ::
      !Base64
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImportCertificateAuthorityCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icacCertificateChain' - A PEM-encoded file that contains all of your certificates, other than the certificate you're importing, chaining up to your root CA. Your ACM Private CA-hosted or on-premises root certificate is the last in the chain, and each certificate in the chain signs the one preceding.  This parameter must be supplied when you import a subordinate CA. When you import a root CA, there is no chain.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'icacCertificateAuthorityARN' - The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form:  @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
--
-- * 'icacCertificate' - The PEM-encoded certificate for a private CA. This may be a self-signed certificate in the case of a root CA, or it may be signed by another CA that you control.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
importCertificateAuthorityCertificate ::
  -- | 'icacCertificateAuthorityARN'
  Text ->
  -- | 'icacCertificate'
  ByteString ->
  ImportCertificateAuthorityCertificate
importCertificateAuthorityCertificate
  pCertificateAuthorityARN_
  pCertificate_ =
    ImportCertificateAuthorityCertificate'
      { _icacCertificateChain =
          Nothing,
        _icacCertificateAuthorityARN = pCertificateAuthorityARN_,
        _icacCertificate = _Base64 # pCertificate_
      }

-- | A PEM-encoded file that contains all of your certificates, other than the certificate you're importing, chaining up to your root CA. Your ACM Private CA-hosted or on-premises root certificate is the last in the chain, and each certificate in the chain signs the one preceding.  This parameter must be supplied when you import a subordinate CA. When you import a root CA, there is no chain.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
icacCertificateChain :: Lens' ImportCertificateAuthorityCertificate (Maybe ByteString)
icacCertificateChain = lens _icacCertificateChain (\s a -> s {_icacCertificateChain = a}) . mapping _Base64

-- | The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form:  @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
icacCertificateAuthorityARN :: Lens' ImportCertificateAuthorityCertificate Text
icacCertificateAuthorityARN = lens _icacCertificateAuthorityARN (\s a -> s {_icacCertificateAuthorityARN = a})

-- | The PEM-encoded certificate for a private CA. This may be a self-signed certificate in the case of a root CA, or it may be signed by another CA that you control.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
icacCertificate :: Lens' ImportCertificateAuthorityCertificate ByteString
icacCertificate = lens _icacCertificate (\s a -> s {_icacCertificate = a}) . _Base64

instance AWSRequest ImportCertificateAuthorityCertificate where
  type
    Rs ImportCertificateAuthorityCertificate =
      ImportCertificateAuthorityCertificateResponse
  request = postJSON certificateManagerPCA
  response =
    receiveNull ImportCertificateAuthorityCertificateResponse'

instance Hashable ImportCertificateAuthorityCertificate

instance NFData ImportCertificateAuthorityCertificate

instance ToHeaders ImportCertificateAuthorityCertificate where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "ACMPrivateCA.ImportCertificateAuthorityCertificate" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ImportCertificateAuthorityCertificate where
  toJSON ImportCertificateAuthorityCertificate' {..} =
    object
      ( catMaybes
          [ ("CertificateChain" .=) <$> _icacCertificateChain,
            Just ("CertificateAuthorityArn" .= _icacCertificateAuthorityARN),
            Just ("Certificate" .= _icacCertificate)
          ]
      )

instance ToPath ImportCertificateAuthorityCertificate where
  toPath = const "/"

instance ToQuery ImportCertificateAuthorityCertificate where
  toQuery = const mempty

-- | /See:/ 'importCertificateAuthorityCertificateResponse' smart constructor.
data ImportCertificateAuthorityCertificateResponse = ImportCertificateAuthorityCertificateResponse'
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ImportCertificateAuthorityCertificateResponse' with the minimum fields required to make a request.
importCertificateAuthorityCertificateResponse ::
  ImportCertificateAuthorityCertificateResponse
importCertificateAuthorityCertificateResponse =
  ImportCertificateAuthorityCertificateResponse'

instance NFData ImportCertificateAuthorityCertificateResponse
