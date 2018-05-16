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
-- Module      : Network.AWS.CertificateManagerPCA.RevokeCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes a certificate that you issued by calling the 'IssueCertificate' function. If you enable a certificate revocation list (CRL) when you create or update your private CA, information about the revoked certificates will be included in the CRL. ACM PCA writes the CRL to an S3 bucket that you specify. For more information about revocation, see the 'CrlConfiguration' structure. ACM PCA also writes revocation information to the audit report. For more information, see 'CreateCertificateAuthorityAuditReport' .
--
--
module Network.AWS.CertificateManagerPCA.RevokeCertificate
    (
    -- * Creating a Request
      revokeCertificate
    , RevokeCertificate
    -- * Request Lenses
    , rcCertificateAuthorityARN
    , rcCertificateSerial
    , rcRevocationReason

    -- * Destructuring the Response
    , revokeCertificateResponse
    , RevokeCertificateResponse
    ) where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.CertificateManagerPCA.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'revokeCertificate' smart constructor.
data RevokeCertificate = RevokeCertificate'
  { _rcCertificateAuthorityARN :: !Text
  , _rcCertificateSerial       :: !Text
  , _rcRevocationReason        :: !RevocationReason
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RevokeCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcCertificateAuthorityARN' - Amazon Resource Name (ARN) of the private CA that issued the certificate to be revoked. This must be of the form: @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
--
-- * 'rcCertificateSerial' - Serial number of the certificate to be revoked. This must be in hexadecimal format. You can retrieve the serial number by calling 'GetCertificate' with the Amazon Resource Name (ARN) of the certificate you want and the ARN of your private CA. The __GetCertificate__ function retrieves the certificate in the PEM format. You can use the following OpenSSL command to list the certificate in text format and copy the hexadecimal serial number.  @openssl x509 -in /file_path/ -text -noout@  You can also copy the serial number from the console or use the <http://docs.aws.amazon.comacm/latest/APIReferenceAPI_DescribeCertificate.html DescribeCertificate> function in the /AWS Certificate Manager API Reference/ .
--
-- * 'rcRevocationReason' - Specifies why you revoked the certificate.
revokeCertificate
    :: Text -- ^ 'rcCertificateAuthorityARN'
    -> Text -- ^ 'rcCertificateSerial'
    -> RevocationReason -- ^ 'rcRevocationReason'
    -> RevokeCertificate
revokeCertificate pCertificateAuthorityARN_ pCertificateSerial_ pRevocationReason_ =
  RevokeCertificate'
    { _rcCertificateAuthorityARN = pCertificateAuthorityARN_
    , _rcCertificateSerial = pCertificateSerial_
    , _rcRevocationReason = pRevocationReason_
    }


-- | Amazon Resource Name (ARN) of the private CA that issued the certificate to be revoked. This must be of the form: @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
rcCertificateAuthorityARN :: Lens' RevokeCertificate Text
rcCertificateAuthorityARN = lens _rcCertificateAuthorityARN (\ s a -> s{_rcCertificateAuthorityARN = a})

-- | Serial number of the certificate to be revoked. This must be in hexadecimal format. You can retrieve the serial number by calling 'GetCertificate' with the Amazon Resource Name (ARN) of the certificate you want and the ARN of your private CA. The __GetCertificate__ function retrieves the certificate in the PEM format. You can use the following OpenSSL command to list the certificate in text format and copy the hexadecimal serial number.  @openssl x509 -in /file_path/ -text -noout@  You can also copy the serial number from the console or use the <http://docs.aws.amazon.comacm/latest/APIReferenceAPI_DescribeCertificate.html DescribeCertificate> function in the /AWS Certificate Manager API Reference/ .
rcCertificateSerial :: Lens' RevokeCertificate Text
rcCertificateSerial = lens _rcCertificateSerial (\ s a -> s{_rcCertificateSerial = a})

-- | Specifies why you revoked the certificate.
rcRevocationReason :: Lens' RevokeCertificate RevocationReason
rcRevocationReason = lens _rcRevocationReason (\ s a -> s{_rcRevocationReason = a})

instance AWSRequest RevokeCertificate where
        type Rs RevokeCertificate = RevokeCertificateResponse
        request = postJSON certificateManagerPCA
        response = receiveNull RevokeCertificateResponse'

instance Hashable RevokeCertificate where

instance NFData RevokeCertificate where

instance ToHeaders RevokeCertificate where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ACMPrivateCA.RevokeCertificate" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RevokeCertificate where
        toJSON RevokeCertificate'{..}
          = object
              (catMaybes
                 [Just
                    ("CertificateAuthorityArn" .=
                       _rcCertificateAuthorityARN),
                  Just ("CertificateSerial" .= _rcCertificateSerial),
                  Just ("RevocationReason" .= _rcRevocationReason)])

instance ToPath RevokeCertificate where
        toPath = const "/"

instance ToQuery RevokeCertificate where
        toQuery = const mempty

-- | /See:/ 'revokeCertificateResponse' smart constructor.
data RevokeCertificateResponse =
  RevokeCertificateResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RevokeCertificateResponse' with the minimum fields required to make a request.
--
revokeCertificateResponse
    :: RevokeCertificateResponse
revokeCertificateResponse = RevokeCertificateResponse'


instance NFData RevokeCertificateResponse where
