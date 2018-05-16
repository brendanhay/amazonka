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
-- Module      : Network.AWS.CertificateManagerPCA.GetCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a certificate from your private CA. The ARN of the certificate is returned when you call the 'IssueCertificate' function. You must specify both the ARN of your private CA and the ARN of the issued certificate when calling the __GetCertificate__ function. You can retrieve the certificate if it is in the __ISSUED__ state. You can call the 'CreateCertificateAuthorityAuditReport' function to create a report that contains information about all of the certificates issued and revoked by your private CA.
--
--
module Network.AWS.CertificateManagerPCA.GetCertificate
    (
    -- * Creating a Request
      getCertificate
    , GetCertificate
    -- * Request Lenses
    , gcCertificateAuthorityARN
    , gcCertificateARN

    -- * Destructuring the Response
    , getCertificateResponse
    , GetCertificateResponse
    -- * Response Lenses
    , gcrsCertificate
    , gcrsCertificateChain
    , gcrsResponseStatus
    ) where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.CertificateManagerPCA.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCertificate' smart constructor.
data GetCertificate = GetCertificate'
  { _gcCertificateAuthorityARN :: !Text
  , _gcCertificateARN          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcCertificateAuthorityARN' - The Amazon Resource Name (ARN) that was returned when you called 'CreateCertificateAuthority' . This must be of the form:  @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- * 'gcCertificateARN' - The ARN of the issued certificate. The ARN contains the certificate serial number and must be in the following form:  @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ /certificate//286535153982981100925020015808220737245/ @
getCertificate
    :: Text -- ^ 'gcCertificateAuthorityARN'
    -> Text -- ^ 'gcCertificateARN'
    -> GetCertificate
getCertificate pCertificateAuthorityARN_ pCertificateARN_ =
  GetCertificate'
    { _gcCertificateAuthorityARN = pCertificateAuthorityARN_
    , _gcCertificateARN = pCertificateARN_
    }


-- | The Amazon Resource Name (ARN) that was returned when you called 'CreateCertificateAuthority' . This must be of the form:  @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
gcCertificateAuthorityARN :: Lens' GetCertificate Text
gcCertificateAuthorityARN = lens _gcCertificateAuthorityARN (\ s a -> s{_gcCertificateAuthorityARN = a})

-- | The ARN of the issued certificate. The ARN contains the certificate serial number and must be in the following form:  @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ /certificate//286535153982981100925020015808220737245/ @
gcCertificateARN :: Lens' GetCertificate Text
gcCertificateARN = lens _gcCertificateARN (\ s a -> s{_gcCertificateARN = a})

instance AWSRequest GetCertificate where
        type Rs GetCertificate = GetCertificateResponse
        request = postJSON certificateManagerPCA
        response
          = receiveJSON
              (\ s h x ->
                 GetCertificateResponse' <$>
                   (x .?> "Certificate") <*> (x .?> "CertificateChain")
                     <*> (pure (fromEnum s)))

instance Hashable GetCertificate where

instance NFData GetCertificate where

instance ToHeaders GetCertificate where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ACMPrivateCA.GetCertificate" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetCertificate where
        toJSON GetCertificate'{..}
          = object
              (catMaybes
                 [Just
                    ("CertificateAuthorityArn" .=
                       _gcCertificateAuthorityARN),
                  Just ("CertificateArn" .= _gcCertificateARN)])

instance ToPath GetCertificate where
        toPath = const "/"

instance ToQuery GetCertificate where
        toQuery = const mempty

-- | /See:/ 'getCertificateResponse' smart constructor.
data GetCertificateResponse = GetCertificateResponse'
  { _gcrsCertificate      :: !(Maybe Text)
  , _gcrsCertificateChain :: !(Maybe Text)
  , _gcrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrsCertificate' - The base64 PEM-encoded certificate specified by the @CertificateArn@ parameter.
--
-- * 'gcrsCertificateChain' - The base64 PEM-encoded certificate chain that chains up to the on-premises root CA certificate that you used to sign your private CA certificate.
--
-- * 'gcrsResponseStatus' - -- | The response status code.
getCertificateResponse
    :: Int -- ^ 'gcrsResponseStatus'
    -> GetCertificateResponse
getCertificateResponse pResponseStatus_ =
  GetCertificateResponse'
    { _gcrsCertificate = Nothing
    , _gcrsCertificateChain = Nothing
    , _gcrsResponseStatus = pResponseStatus_
    }


-- | The base64 PEM-encoded certificate specified by the @CertificateArn@ parameter.
gcrsCertificate :: Lens' GetCertificateResponse (Maybe Text)
gcrsCertificate = lens _gcrsCertificate (\ s a -> s{_gcrsCertificate = a})

-- | The base64 PEM-encoded certificate chain that chains up to the on-premises root CA certificate that you used to sign your private CA certificate.
gcrsCertificateChain :: Lens' GetCertificateResponse (Maybe Text)
gcrsCertificateChain = lens _gcrsCertificateChain (\ s a -> s{_gcrsCertificateChain = a})

-- | -- | The response status code.
gcrsResponseStatus :: Lens' GetCertificateResponse Int
gcrsResponseStatus = lens _gcrsResponseStatus (\ s a -> s{_gcrsResponseStatus = a})

instance NFData GetCertificateResponse where
