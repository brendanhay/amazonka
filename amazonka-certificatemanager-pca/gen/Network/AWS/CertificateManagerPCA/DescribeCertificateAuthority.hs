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
-- Module      : Network.AWS.CertificateManagerPCA.DescribeCertificateAuthority
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about your private certificate authority (CA). You specify the private CA on input by its ARN (Amazon Resource Name). The output contains the status of your CA. This can be any of the following:
--
--
--     * __CREATING:__ ACM PCA is creating your private certificate authority.
--
--     * __PENDING_CERTIFICATE:__ The certificate is pending. You must use your on-premises root or subordinate CA to sign your private CA CSR and then import it into PCA.
--
--     * __ACTIVE:__ Your private CA is active.
--
--     * __DISABLED:__ Your private CA has been disabled.
--
--     * __EXPIRED:__ Your private CA certificate has expired.
--
--     * __FAILED:__ Your private CA has failed. Your CA can fail for problems such a network outage or backend AWS failure or other errors. A failed CA can never return to the pending state. You must create a new CA.
--
--
--
module Network.AWS.CertificateManagerPCA.DescribeCertificateAuthority
    (
    -- * Creating a Request
      describeCertificateAuthority
    , DescribeCertificateAuthority
    -- * Request Lenses
    , dCertificateAuthorityARN

    -- * Destructuring the Response
    , describeCertificateAuthorityResponse
    , DescribeCertificateAuthorityResponse
    -- * Response Lenses
    , dcarsCertificateAuthority
    , dcarsResponseStatus
    ) where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.CertificateManagerPCA.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeCertificateAuthority' smart constructor.
newtype DescribeCertificateAuthority = DescribeCertificateAuthority'
  { _dCertificateAuthorityARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCertificateAuthority' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCertificateAuthorityARN' - The Amazon Resource Name (ARN) that was returned when you called 'CreateCertificateAuthority' . This must be of the form:  @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
describeCertificateAuthority
    :: Text -- ^ 'dCertificateAuthorityARN'
    -> DescribeCertificateAuthority
describeCertificateAuthority pCertificateAuthorityARN_ =
  DescribeCertificateAuthority'
    {_dCertificateAuthorityARN = pCertificateAuthorityARN_}


-- | The Amazon Resource Name (ARN) that was returned when you called 'CreateCertificateAuthority' . This must be of the form:  @arn:aws:acm:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
dCertificateAuthorityARN :: Lens' DescribeCertificateAuthority Text
dCertificateAuthorityARN = lens _dCertificateAuthorityARN (\ s a -> s{_dCertificateAuthorityARN = a})

instance AWSRequest DescribeCertificateAuthority
         where
        type Rs DescribeCertificateAuthority =
             DescribeCertificateAuthorityResponse
        request = postJSON certificateManagerPCA
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCertificateAuthorityResponse' <$>
                   (x .?> "CertificateAuthority") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeCertificateAuthority where

instance NFData DescribeCertificateAuthority where

instance ToHeaders DescribeCertificateAuthority where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ACMPrivateCA.DescribeCertificateAuthority" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeCertificateAuthority where
        toJSON DescribeCertificateAuthority'{..}
          = object
              (catMaybes
                 [Just
                    ("CertificateAuthorityArn" .=
                       _dCertificateAuthorityARN)])

instance ToPath DescribeCertificateAuthority where
        toPath = const "/"

instance ToQuery DescribeCertificateAuthority where
        toQuery = const mempty

-- | /See:/ 'describeCertificateAuthorityResponse' smart constructor.
data DescribeCertificateAuthorityResponse = DescribeCertificateAuthorityResponse'
  { _dcarsCertificateAuthority :: !(Maybe CertificateAuthority)
  , _dcarsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCertificateAuthorityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcarsCertificateAuthority' - A 'CertificateAuthority' structure that contains information about your private CA.
--
-- * 'dcarsResponseStatus' - -- | The response status code.
describeCertificateAuthorityResponse
    :: Int -- ^ 'dcarsResponseStatus'
    -> DescribeCertificateAuthorityResponse
describeCertificateAuthorityResponse pResponseStatus_ =
  DescribeCertificateAuthorityResponse'
    { _dcarsCertificateAuthority = Nothing
    , _dcarsResponseStatus = pResponseStatus_
    }


-- | A 'CertificateAuthority' structure that contains information about your private CA.
dcarsCertificateAuthority :: Lens' DescribeCertificateAuthorityResponse (Maybe CertificateAuthority)
dcarsCertificateAuthority = lens _dcarsCertificateAuthority (\ s a -> s{_dcarsCertificateAuthority = a})

-- | -- | The response status code.
dcarsResponseStatus :: Lens' DescribeCertificateAuthorityResponse Int
dcarsResponseStatus = lens _dcarsResponseStatus (\ s a -> s{_dcarsResponseStatus = a})

instance NFData DescribeCertificateAuthorityResponse
         where
