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
-- Module      : Network.AWS.CertificateManager.GetCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a certificate specified by an ARN and its certificate chain . The chain is an ordered list of certificates that contains the end entity certificate, intermediate certificates of subordinate CAs, and the root certificate in that order. The certificate and certificate chain are base64 encoded. If you want to decode the certificate to see the individual fields, you can use OpenSSL.
--
--
module Network.AWS.CertificateManager.GetCertificate
    (
    -- * Creating a Request
      getCertificate
    , GetCertificate
    -- * Request Lenses
    , gcCertificateARN

    -- * Destructuring the Response
    , getCertificateResponse
    , GetCertificateResponse
    -- * Response Lenses
    , gcrsCertificate
    , gcrsCertificateChain
    , gcrsResponseStatus
    ) where

import Network.AWS.CertificateManager.Types
import Network.AWS.CertificateManager.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCertificate' smart constructor.
newtype GetCertificate = GetCertificate'
  { _gcCertificateARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcCertificateARN' - String that contains a certificate ARN in the following format: @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@  For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
getCertificate
    :: Text -- ^ 'gcCertificateARN'
    -> GetCertificate
getCertificate pCertificateARN_ =
  GetCertificate' {_gcCertificateARN = pCertificateARN_}


-- | String that contains a certificate ARN in the following format: @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@  For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
gcCertificateARN :: Lens' GetCertificate Text
gcCertificateARN = lens _gcCertificateARN (\ s a -> s{_gcCertificateARN = a})

instance AWSRequest GetCertificate where
        type Rs GetCertificate = GetCertificateResponse
        request = postJSON certificateManager
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
                    ("CertificateManager.GetCertificate" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetCertificate where
        toJSON GetCertificate'{..}
          = object
              (catMaybes
                 [Just ("CertificateArn" .= _gcCertificateARN)])

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
-- * 'gcrsCertificate' - String that contains the ACM certificate represented by the ARN specified at input.
--
-- * 'gcrsCertificateChain' - The certificate chain that contains the root certificate issued by the certificate authority (CA).
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


-- | String that contains the ACM certificate represented by the ARN specified at input.
gcrsCertificate :: Lens' GetCertificateResponse (Maybe Text)
gcrsCertificate = lens _gcrsCertificate (\ s a -> s{_gcrsCertificate = a})

-- | The certificate chain that contains the root certificate issued by the certificate authority (CA).
gcrsCertificateChain :: Lens' GetCertificateResponse (Maybe Text)
gcrsCertificateChain = lens _gcrsCertificateChain (\ s a -> s{_gcrsCertificateChain = a})

-- | -- | The response status code.
gcrsResponseStatus :: Lens' GetCertificateResponse Int
gcrsResponseStatus = lens _gcrsResponseStatus (\ s a -> s{_gcrsResponseStatus = a})

instance NFData GetCertificateResponse where
