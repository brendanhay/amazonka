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
-- Module      : Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCSR
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the certificate signing request (CSR) for your private certificate authority (CA). The CSR is created when you call the 'CreateCertificateAuthority' function. Take the CSR to your on-premises X.509 infrastructure and sign it by using your root or a subordinate CA. Then import the signed certificate back into ACM PCA by calling the 'ImportCertificateAuthorityCertificate' function. The CSR is returned as a base64 PEM-encoded string.
--
--
module Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCSR
    (
    -- * Creating a Request
      getCertificateAuthorityCSR
    , GetCertificateAuthorityCSR
    -- * Request Lenses
    , gcacsrCertificateAuthorityARN

    -- * Destructuring the Response
    , getCertificateAuthorityCSRResponse
    , GetCertificateAuthorityCSRResponse
    -- * Response Lenses
    , gcacsrrsCSR
    , gcacsrrsResponseStatus
    ) where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.CertificateManagerPCA.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCertificateAuthorityCSR' smart constructor.
newtype GetCertificateAuthorityCSR = GetCertificateAuthorityCSR'
  { _gcacsrCertificateAuthorityARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCertificateAuthorityCSR' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcacsrCertificateAuthorityARN' - The Amazon Resource Name (ARN) that was returned when you called the 'CreateCertificateAuthority' function. This must be of the form:  @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
getCertificateAuthorityCSR
    :: Text -- ^ 'gcacsrCertificateAuthorityARN'
    -> GetCertificateAuthorityCSR
getCertificateAuthorityCSR pCertificateAuthorityARN_ =
  GetCertificateAuthorityCSR'
    {_gcacsrCertificateAuthorityARN = pCertificateAuthorityARN_}


-- | The Amazon Resource Name (ARN) that was returned when you called the 'CreateCertificateAuthority' function. This must be of the form:  @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
gcacsrCertificateAuthorityARN :: Lens' GetCertificateAuthorityCSR Text
gcacsrCertificateAuthorityARN = lens _gcacsrCertificateAuthorityARN (\ s a -> s{_gcacsrCertificateAuthorityARN = a})

instance AWSRequest GetCertificateAuthorityCSR where
        type Rs GetCertificateAuthorityCSR =
             GetCertificateAuthorityCSRResponse
        request = postJSON certificateManagerPCA
        response
          = receiveJSON
              (\ s h x ->
                 GetCertificateAuthorityCSRResponse' <$>
                   (x .?> "Csr") <*> (pure (fromEnum s)))

instance Hashable GetCertificateAuthorityCSR where

instance NFData GetCertificateAuthorityCSR where

instance ToHeaders GetCertificateAuthorityCSR where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ACMPrivateCA.GetCertificateAuthorityCsr" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetCertificateAuthorityCSR where
        toJSON GetCertificateAuthorityCSR'{..}
          = object
              (catMaybes
                 [Just
                    ("CertificateAuthorityArn" .=
                       _gcacsrCertificateAuthorityARN)])

instance ToPath GetCertificateAuthorityCSR where
        toPath = const "/"

instance ToQuery GetCertificateAuthorityCSR where
        toQuery = const mempty

-- | /See:/ 'getCertificateAuthorityCSRResponse' smart constructor.
data GetCertificateAuthorityCSRResponse = GetCertificateAuthorityCSRResponse'
  { _gcacsrrsCSR            :: !(Maybe Text)
  , _gcacsrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCertificateAuthorityCSRResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcacsrrsCSR' - The base64 PEM-encoded certificate signing request (CSR) for your private CA certificate.
--
-- * 'gcacsrrsResponseStatus' - -- | The response status code.
getCertificateAuthorityCSRResponse
    :: Int -- ^ 'gcacsrrsResponseStatus'
    -> GetCertificateAuthorityCSRResponse
getCertificateAuthorityCSRResponse pResponseStatus_ =
  GetCertificateAuthorityCSRResponse'
    {_gcacsrrsCSR = Nothing, _gcacsrrsResponseStatus = pResponseStatus_}


-- | The base64 PEM-encoded certificate signing request (CSR) for your private CA certificate.
gcacsrrsCSR :: Lens' GetCertificateAuthorityCSRResponse (Maybe Text)
gcacsrrsCSR = lens _gcacsrrsCSR (\ s a -> s{_gcacsrrsCSR = a})

-- | -- | The response status code.
gcacsrrsResponseStatus :: Lens' GetCertificateAuthorityCSRResponse Int
gcacsrrsResponseStatus = lens _gcacsrrsResponseStatus (\ s a -> s{_gcacsrrsResponseStatus = a})

instance NFData GetCertificateAuthorityCSRResponse
         where
