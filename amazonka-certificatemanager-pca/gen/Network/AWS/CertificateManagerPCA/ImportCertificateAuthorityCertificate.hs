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
-- Module      : Network.AWS.CertificateManagerPCA.ImportCertificateAuthorityCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports your signed private CA certificate into ACM PCA. Before you can call this function, you must create the private certificate authority by calling the 'CreateCertificateAuthority' function. You must then generate a certificate signing request (CSR) by calling the 'GetCertificateAuthorityCsr' function. Take the CSR to your on-premises CA and use the root certificate or a subordinate certificate to sign it. Create a certificate chain and copy the signed certificate and the certificate chain to your working directory.
--
--
module Network.AWS.CertificateManagerPCA.ImportCertificateAuthorityCertificate
    (
    -- * Creating a Request
      importCertificateAuthorityCertificate
    , ImportCertificateAuthorityCertificate
    -- * Request Lenses
    , icacCertificateAuthorityARN
    , icacCertificate
    , icacCertificateChain

    -- * Destructuring the Response
    , importCertificateAuthorityCertificateResponse
    , ImportCertificateAuthorityCertificateResponse
    ) where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.CertificateManagerPCA.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'importCertificateAuthorityCertificate' smart constructor.
data ImportCertificateAuthorityCertificate = ImportCertificateAuthorityCertificate'
  { _icacCertificateAuthorityARN :: !Text
  , _icacCertificate             :: !Base64
  , _icacCertificateChain        :: !Base64
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportCertificateAuthorityCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icacCertificateAuthorityARN' - The Amazon Resource Name (ARN) that was returned when you called 'CreateCertificateAuthority' . This must be of the form:  @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
--
-- * 'icacCertificate' - The PEM-encoded certificate for your private CA. This must be signed by using your on-premises CA.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'icacCertificateChain' - A PEM-encoded file that contains all of your certificates, other than the certificate you're importing, chaining up to your root CA. Your on-premises root certificate is the last in the chain, and each certificate in the chain signs the one preceding. -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
importCertificateAuthorityCertificate
    :: Text -- ^ 'icacCertificateAuthorityARN'
    -> ByteString -- ^ 'icacCertificate'
    -> ByteString -- ^ 'icacCertificateChain'
    -> ImportCertificateAuthorityCertificate
importCertificateAuthorityCertificate pCertificateAuthorityARN_ pCertificate_ pCertificateChain_ =
  ImportCertificateAuthorityCertificate'
    { _icacCertificateAuthorityARN = pCertificateAuthorityARN_
    , _icacCertificate = _Base64 # pCertificate_
    , _icacCertificateChain = _Base64 # pCertificateChain_
    }


-- | The Amazon Resource Name (ARN) that was returned when you called 'CreateCertificateAuthority' . This must be of the form:  @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
icacCertificateAuthorityARN :: Lens' ImportCertificateAuthorityCertificate Text
icacCertificateAuthorityARN = lens _icacCertificateAuthorityARN (\ s a -> s{_icacCertificateAuthorityARN = a})

-- | The PEM-encoded certificate for your private CA. This must be signed by using your on-premises CA.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
icacCertificate :: Lens' ImportCertificateAuthorityCertificate ByteString
icacCertificate = lens _icacCertificate (\ s a -> s{_icacCertificate = a}) . _Base64

-- | A PEM-encoded file that contains all of your certificates, other than the certificate you're importing, chaining up to your root CA. Your on-premises root certificate is the last in the chain, and each certificate in the chain signs the one preceding. -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
icacCertificateChain :: Lens' ImportCertificateAuthorityCertificate ByteString
icacCertificateChain = lens _icacCertificateChain (\ s a -> s{_icacCertificateChain = a}) . _Base64

instance AWSRequest
           ImportCertificateAuthorityCertificate
         where
        type Rs ImportCertificateAuthorityCertificate =
             ImportCertificateAuthorityCertificateResponse
        request = postJSON certificateManagerPCA
        response
          = receiveNull
              ImportCertificateAuthorityCertificateResponse'

instance Hashable
           ImportCertificateAuthorityCertificate
         where

instance NFData ImportCertificateAuthorityCertificate
         where

instance ToHeaders
           ImportCertificateAuthorityCertificate
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ACMPrivateCA.ImportCertificateAuthorityCertificate"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ImportCertificateAuthorityCertificate
         where
        toJSON ImportCertificateAuthorityCertificate'{..}
          = object
              (catMaybes
                 [Just
                    ("CertificateAuthorityArn" .=
                       _icacCertificateAuthorityARN),
                  Just ("Certificate" .= _icacCertificate),
                  Just ("CertificateChain" .= _icacCertificateChain)])

instance ToPath ImportCertificateAuthorityCertificate
         where
        toPath = const "/"

instance ToQuery
           ImportCertificateAuthorityCertificate
         where
        toQuery = const mempty

-- | /See:/ 'importCertificateAuthorityCertificateResponse' smart constructor.
data ImportCertificateAuthorityCertificateResponse =
  ImportCertificateAuthorityCertificateResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportCertificateAuthorityCertificateResponse' with the minimum fields required to make a request.
--
importCertificateAuthorityCertificateResponse
    :: ImportCertificateAuthorityCertificateResponse
importCertificateAuthorityCertificateResponse =
  ImportCertificateAuthorityCertificateResponse'


instance NFData
           ImportCertificateAuthorityCertificateResponse
         where
