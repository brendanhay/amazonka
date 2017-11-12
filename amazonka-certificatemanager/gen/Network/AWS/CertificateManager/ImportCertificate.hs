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
-- Module      : Network.AWS.CertificateManager.ImportCertificate
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports an SSL/TLS certificate into AWS Certificate Manager (ACM) to use with <http://docs.aws.amazon.com/acm/latest/userguide/acm-services.html ACM's integrated AWS services> .
--
--
-- For more information about importing certificates into ACM, including the differences between certificates that you import and those that ACM provides, see <http://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing Certificates> in the /AWS Certificate Manager User Guide/ .
--
-- To import a certificate, you must provide the certificate and the matching private key. When the certificate is not self-signed, you must also provide a certificate chain. You can omit the certificate chain when importing a self-signed certificate.
--
-- The certificate, private key, and certificate chain must be PEM-encoded. For more information about converting these items to PEM format, see <http://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html#import-certificate-troubleshooting Importing Certificates Troubleshooting> in the /AWS Certificate Manager User Guide/ .
--
-- To import a new certificate, omit the @CertificateArn@ field. Include this field only when you want to replace a previously imported certificate.
--
-- When you import a certificate by using the CLI or one of the SDKs, you must specify the certificate, chain, and private key parameters as file names preceded by @file://@ . For example, you can specify a certificate saved in the @C:\temp@ folder as @C:\temp\certificate_to_import.pem@ . If you are making an HTTP or HTTPS Query request, include these parameters as BLOBs.
--
-- This operation returns the <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of the imported certificate.
--
module Network.AWS.CertificateManager.ImportCertificate
    (
    -- * Creating a Request
      importCertificate
    , ImportCertificate
    -- * Request Lenses
    , icCertificateARN
    , icCertificateChain
    , icCertificate
    , icPrivateKey

    -- * Destructuring the Response
    , importCertificateResponse
    , ImportCertificateResponse
    -- * Response Lenses
    , icrsCertificateARN
    , icrsResponseStatus
    ) where

import Network.AWS.CertificateManager.Types
import Network.AWS.CertificateManager.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'importCertificate' smart constructor.
data ImportCertificate = ImportCertificate'
  { _icCertificateARN   :: !(Maybe Text)
  , _icCertificateChain :: !(Maybe Base64)
  , _icCertificate      :: !Base64
  , _icPrivateKey       :: !(Sensitive Base64)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icCertificateARN' - The <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an imported certificate to replace. To import a new certificate, omit this field.
--
-- * 'icCertificateChain' - The certificate chain. It must be PEM-encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'icCertificate' - The certificate to import. It must meet the following requirements:     * Must be PEM-encoded.     * Must contain a 1024-bit or 2048-bit RSA public key.     * Must be valid at the time of import. You cannot import a certificate before its validity period begins (the certificate's @NotBefore@ date) or after it expires (the certificate's @NotAfter@ date).-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'icPrivateKey' - The private key that matches the public key in the certificate. It must meet the following requirements:     * Must be PEM-encoded.     * Must be unencrypted. You cannot import a private key that is protected by a password or passphrase.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
importCertificate
    :: ByteString -- ^ 'icCertificate'
    -> ByteString -- ^ 'icPrivateKey'
    -> ImportCertificate
importCertificate pCertificate_ pPrivateKey_ =
  ImportCertificate'
  { _icCertificateARN = Nothing
  , _icCertificateChain = Nothing
  , _icCertificate = _Base64 # pCertificate_
  , _icPrivateKey = _Sensitive . _Base64 # pPrivateKey_
  }


-- | The <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of an imported certificate to replace. To import a new certificate, omit this field.
icCertificateARN :: Lens' ImportCertificate (Maybe Text)
icCertificateARN = lens _icCertificateARN (\ s a -> s{_icCertificateARN = a});

-- | The certificate chain. It must be PEM-encoded.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
icCertificateChain :: Lens' ImportCertificate (Maybe ByteString)
icCertificateChain = lens _icCertificateChain (\ s a -> s{_icCertificateChain = a}) . mapping _Base64;

-- | The certificate to import. It must meet the following requirements:     * Must be PEM-encoded.     * Must contain a 1024-bit or 2048-bit RSA public key.     * Must be valid at the time of import. You cannot import a certificate before its validity period begins (the certificate's @NotBefore@ date) or after it expires (the certificate's @NotAfter@ date).-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
icCertificate :: Lens' ImportCertificate ByteString
icCertificate = lens _icCertificate (\ s a -> s{_icCertificate = a}) . _Base64;

-- | The private key that matches the public key in the certificate. It must meet the following requirements:     * Must be PEM-encoded.     * Must be unencrypted. You cannot import a private key that is protected by a password or passphrase.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
icPrivateKey :: Lens' ImportCertificate ByteString
icPrivateKey = lens _icPrivateKey (\ s a -> s{_icPrivateKey = a}) . _Sensitive . _Base64;

instance AWSRequest ImportCertificate where
        type Rs ImportCertificate = ImportCertificateResponse
        request = postJSON certificateManager
        response
          = receiveJSON
              (\ s h x ->
                 ImportCertificateResponse' <$>
                   (x .?> "CertificateArn") <*> (pure (fromEnum s)))

instance Hashable ImportCertificate where

instance NFData ImportCertificate where

instance ToHeaders ImportCertificate where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CertificateManager.ImportCertificate" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ImportCertificate where
        toJSON ImportCertificate'{..}
          = object
              (catMaybes
                 [("CertificateArn" .=) <$> _icCertificateARN,
                  ("CertificateChain" .=) <$> _icCertificateChain,
                  Just ("Certificate" .= _icCertificate),
                  Just ("PrivateKey" .= _icPrivateKey)])

instance ToPath ImportCertificate where
        toPath = const "/"

instance ToQuery ImportCertificate where
        toQuery = const mempty

-- | /See:/ 'importCertificateResponse' smart constructor.
data ImportCertificateResponse = ImportCertificateResponse'
  { _icrsCertificateARN :: !(Maybe Text)
  , _icrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icrsCertificateARN' - The <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of the imported certificate.
--
-- * 'icrsResponseStatus' - -- | The response status code.
importCertificateResponse
    :: Int -- ^ 'icrsResponseStatus'
    -> ImportCertificateResponse
importCertificateResponse pResponseStatus_ =
  ImportCertificateResponse'
  {_icrsCertificateARN = Nothing, _icrsResponseStatus = pResponseStatus_}


-- | The <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)> of the imported certificate.
icrsCertificateARN :: Lens' ImportCertificateResponse (Maybe Text)
icrsCertificateARN = lens _icrsCertificateARN (\ s a -> s{_icrsCertificateARN = a});

-- | -- | The response status code.
icrsResponseStatus :: Lens' ImportCertificateResponse Int
icrsResponseStatus = lens _icrsResponseStatus (\ s a -> s{_icrsResponseStatus = a});

instance NFData ImportCertificateResponse where
