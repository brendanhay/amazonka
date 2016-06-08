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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the fields contained in the specified ACM Certificate. For example, this action returns the certificate status, a flag that indicates whether the certificate is associated with any other AWS service, and the date at which the certificate request was created. You specify the ACM Certificate on input by its Amazon Resource Name (ARN).
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

import           Network.AWS.CertificateManager.Types
import           Network.AWS.CertificateManager.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'describeCertificate' smart constructor.
newtype DescribeCertificate = DescribeCertificate'
    { _dCertificateARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCertificateARN'
describeCertificate
    :: Text -- ^ 'dCertificateARN'
    -> DescribeCertificate
describeCertificate pCertificateARN_ =
    DescribeCertificate'
    { _dCertificateARN = pCertificateARN_
    }

-- | String that contains an ACM Certificate ARN. The ARN must be of the form:
--
-- 'arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012'
--
-- For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces>.
dCertificateARN :: Lens' DescribeCertificate Text
dCertificateARN = lens _dCertificateARN (\ s a -> s{_dCertificateARN = a});

instance AWSRequest DescribeCertificate where
        type Rs DescribeCertificate =
             DescribeCertificateResponse
        request = postJSON certificateManager
        response
          = receiveJSON
              (\ s h x ->
                 DescribeCertificateResponse' <$>
                   (x .?> "Certificate") <*> (pure (fromEnum s)))

instance Hashable DescribeCertificate

instance NFData DescribeCertificate

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

-- |
--
-- /See:/ 'describeCertificateResponse' smart constructor.
data DescribeCertificateResponse = DescribeCertificateResponse'
    { _dcrsCertificate    :: !(Maybe CertificateDetail)
    , _dcrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsCertificate'
--
-- * 'dcrsResponseStatus'
describeCertificateResponse
    :: Int -- ^ 'dcrsResponseStatus'
    -> DescribeCertificateResponse
describeCertificateResponse pResponseStatus_ =
    DescribeCertificateResponse'
    { _dcrsCertificate = Nothing
    , _dcrsResponseStatus = pResponseStatus_
    }

-- | Contains a < CertificateDetail> structure that lists the fields of an ACM Certificate.
dcrsCertificate :: Lens' DescribeCertificateResponse (Maybe CertificateDetail)
dcrsCertificate = lens _dcrsCertificate (\ s a -> s{_dcrsCertificate = a});

-- | The response status code.
dcrsResponseStatus :: Lens' DescribeCertificateResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\ s a -> s{_dcrsResponseStatus = a});

instance NFData DescribeCertificateResponse
