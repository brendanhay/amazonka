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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports a certificate into AWS Certificate Manager (ACM) to use with services that are integrated with ACM. Note that <http://docs.aws.amazon.com/acm/latest/userguide/acm-services.html integrated services> allow only certificate types and keys they support to be associated with their resources. Further, their support differs depending on whether the certificate is imported into IAM or into ACM. For more information, see the documentation for each service. For more information about importing certificates into ACM, see <http://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing Certificates> in the /AWS Certificate Manager User Guide/ .
--
--
-- Note the following guidelines when importing third party certificates:
--
--     * You must enter the private key that matches the certificate you are importing.
--
--     * The private key must be unencrypted. You cannot import a private key that is protected by a password or a passphrase.
--
--     * If the certificate you are importing is not self-signed, you must enter its certificate chain.
--
--     * If a certificate chain is included, the issuer must be the subject of one of the certificates in the chain.
--
--     * The certificate, private key, and certificate chain must be PEM-encoded.
--
--     * The current time must be between the @Not Before@ and @Not After@ certificate fields.
--
--     * The @Issuer@ field must not be empty.
--
--     * The OCSP authority URL, if present, must not exceed 1000 characters.
--
--     * To import a new certificate, omit the @CertificateArn@ argument. Include this argument only when you want to replace a previously imported certificate.
--
--     * When you import a certificate by using the CLI, you must specify the certificate, the certificate chain, and the private key by their file names preceded by @file://@ . For example, you can specify a certificate saved in the @C:\temp@ folder as @file://C:\temp\certificate_to_import.pem@ . If you are making an HTTP or HTTPS Query request, include these arguments as BLOBs.
--
--     * When you import a certificate by using an SDK, you must specify the certificate, the certificate chain, and the private key files in the manner required by the programming language you're using.
--
--
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
-- * 'icCertificateChain' - The PEM encoded certificate chain.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'icCertificate' - The certificate to import.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'icPrivateKey' - The private key that matches the public key in the certificate.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
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
icCertificateARN = lens _icCertificateARN (\ s a -> s{_icCertificateARN = a})

-- | The PEM encoded certificate chain.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
icCertificateChain :: Lens' ImportCertificate (Maybe ByteString)
icCertificateChain = lens _icCertificateChain (\ s a -> s{_icCertificateChain = a}) . mapping _Base64

-- | The certificate to import.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
icCertificate :: Lens' ImportCertificate ByteString
icCertificate = lens _icCertificate (\ s a -> s{_icCertificate = a}) . _Base64

-- | The private key that matches the public key in the certificate.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
icPrivateKey :: Lens' ImportCertificate ByteString
icPrivateKey = lens _icPrivateKey (\ s a -> s{_icPrivateKey = a}) . _Sensitive . _Base64

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
icrsCertificateARN = lens _icrsCertificateARN (\ s a -> s{_icrsCertificateARN = a})

-- | -- | The response status code.
icrsResponseStatus :: Lens' ImportCertificateResponse Int
icrsResponseStatus = lens _icrsResponseStatus (\ s a -> s{_icrsResponseStatus = a})

instance NFData ImportCertificateResponse where
