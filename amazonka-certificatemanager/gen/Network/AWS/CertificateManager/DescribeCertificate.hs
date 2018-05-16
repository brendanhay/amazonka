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
-- Module      : Network.AWS.CertificateManager.DescribeCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed metadata about the specified ACM certificate.
--
--
module Network.AWS.CertificateManager.DescribeCertificate
    (
    -- * Creating a Request
      describeCertificate
    , DescribeCertificate
    -- * Request Lenses
    , dCertificateARN

    -- * Destructuring the Response
    , describeCertificateResponse
    , DescribeCertificateResponse
    -- * Response Lenses
    , dcrsCertificate
    , dcrsResponseStatus
    ) where

import Network.AWS.CertificateManager.Types
import Network.AWS.CertificateManager.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeCertificate' smart constructor.
newtype DescribeCertificate = DescribeCertificate'
  { _dCertificateARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCertificateARN' - The Amazon Resource Name (ARN) of the ACM certificate. The ARN must have the following form: @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@  For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
describeCertificate
    :: Text -- ^ 'dCertificateARN'
    -> DescribeCertificate
describeCertificate pCertificateARN_ =
  DescribeCertificate' {_dCertificateARN = pCertificateARN_}


-- | The Amazon Resource Name (ARN) of the ACM certificate. The ARN must have the following form: @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@  For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
dCertificateARN :: Lens' DescribeCertificate Text
dCertificateARN = lens _dCertificateARN (\ s a -> s{_dCertificateARN = a})

instance AWSRequest DescribeCertificate where
        type Rs DescribeCertificate =
             DescribeCertificateResponse
        request = postJSON certificateManager
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCertificateResponse' <$>
                   (x .?> "Certificate") <*> (pure (fromEnum s)))

instance Hashable DescribeCertificate where

instance NFData DescribeCertificate where

instance ToHeaders DescribeCertificate where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CertificateManager.DescribeCertificate" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeCertificate where
        toJSON DescribeCertificate'{..}
          = object
              (catMaybes
                 [Just ("CertificateArn" .= _dCertificateARN)])

instance ToPath DescribeCertificate where
        toPath = const "/"

instance ToQuery DescribeCertificate where
        toQuery = const mempty

-- | /See:/ 'describeCertificateResponse' smart constructor.
data DescribeCertificateResponse = DescribeCertificateResponse'
  { _dcrsCertificate    :: !(Maybe CertificateDetail)
  , _dcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsCertificate' - Metadata about an ACM certificate.
--
-- * 'dcrsResponseStatus' - -- | The response status code.
describeCertificateResponse
    :: Int -- ^ 'dcrsResponseStatus'
    -> DescribeCertificateResponse
describeCertificateResponse pResponseStatus_ =
  DescribeCertificateResponse'
    {_dcrsCertificate = Nothing, _dcrsResponseStatus = pResponseStatus_}


-- | Metadata about an ACM certificate.
dcrsCertificate :: Lens' DescribeCertificateResponse (Maybe CertificateDetail)
dcrsCertificate = lens _dcrsCertificate (\ s a -> s{_dcrsCertificate = a})

-- | -- | The response status code.
dcrsResponseStatus :: Lens' DescribeCertificateResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\ s a -> s{_dcrsResponseStatus = a})

instance NFData DescribeCertificateResponse where
