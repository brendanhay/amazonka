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
-- Module      : Network.AWS.CertificateManager.RenewCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Renews an eligable ACM certificate. At this time, only exported private certificates can be renewed with this operation. In order to renew your ACM PCA certificates with ACM, you must first <acm-pca/latest/userguide/PcaPermissions.html grant the ACM service principal permission to do so> . For more information, see <acm/latest/userguide/manuel-renewal.html Testing Managed Renewal> in the ACM User Guide.
--
--
module Network.AWS.CertificateManager.RenewCertificate
    (
    -- * Creating a Request
      renewCertificate
    , RenewCertificate
    -- * Request Lenses
    , rcCertificateARN

    -- * Destructuring the Response
    , renewCertificateResponse
    , RenewCertificateResponse
    ) where

import Network.AWS.CertificateManager.Types
import Network.AWS.CertificateManager.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'renewCertificate' smart constructor.
newtype RenewCertificate = RenewCertificate'
  { _rcCertificateARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RenewCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcCertificateARN' - String that contains the ARN of the ACM certificate to be renewed. This must be of the form: @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@  For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
renewCertificate
    :: Text -- ^ 'rcCertificateARN'
    -> RenewCertificate
renewCertificate pCertificateARN_ =
  RenewCertificate' {_rcCertificateARN = pCertificateARN_}


-- | String that contains the ARN of the ACM certificate to be renewed. This must be of the form: @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@  For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
rcCertificateARN :: Lens' RenewCertificate Text
rcCertificateARN = lens _rcCertificateARN (\ s a -> s{_rcCertificateARN = a})

instance AWSRequest RenewCertificate where
        type Rs RenewCertificate = RenewCertificateResponse
        request = postJSON certificateManager
        response = receiveNull RenewCertificateResponse'

instance Hashable RenewCertificate where

instance NFData RenewCertificate where

instance ToHeaders RenewCertificate where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CertificateManager.RenewCertificate" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RenewCertificate where
        toJSON RenewCertificate'{..}
          = object
              (catMaybes
                 [Just ("CertificateArn" .= _rcCertificateARN)])

instance ToPath RenewCertificate where
        toPath = const "/"

instance ToQuery RenewCertificate where
        toQuery = const mempty

-- | /See:/ 'renewCertificateResponse' smart constructor.
data RenewCertificateResponse =
  RenewCertificateResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RenewCertificateResponse' with the minimum fields required to make a request.
--
renewCertificateResponse
    :: RenewCertificateResponse
renewCertificateResponse = RenewCertificateResponse'


instance NFData RenewCertificateResponse where
