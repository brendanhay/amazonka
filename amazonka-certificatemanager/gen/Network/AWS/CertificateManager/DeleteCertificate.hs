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
-- Module      : Network.AWS.CertificateManager.DeleteCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a certificate and its associated private key. If this action succeeds, the certificate no longer appears in the list that can be displayed by calling the 'ListCertificates' action or be retrieved by calling the 'GetCertificate' action. The certificate will not be available for use by AWS services integrated with ACM.
--
--
module Network.AWS.CertificateManager.DeleteCertificate
    (
    -- * Creating a Request
      deleteCertificate
    , DeleteCertificate
    -- * Request Lenses
    , dcCertificateARN

    -- * Destructuring the Response
    , deleteCertificateResponse
    , DeleteCertificateResponse
    ) where

import Network.AWS.CertificateManager.Types
import Network.AWS.CertificateManager.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteCertificate' smart constructor.
newtype DeleteCertificate = DeleteCertificate'
  { _dcCertificateARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcCertificateARN' - String that contains the ARN of the ACM certificate to be deleted. This must be of the form: @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@  For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
deleteCertificate
    :: Text -- ^ 'dcCertificateARN'
    -> DeleteCertificate
deleteCertificate pCertificateARN_ =
  DeleteCertificate' {_dcCertificateARN = pCertificateARN_}


-- | String that contains the ARN of the ACM certificate to be deleted. This must be of the form: @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@  For more information about ARNs, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
dcCertificateARN :: Lens' DeleteCertificate Text
dcCertificateARN = lens _dcCertificateARN (\ s a -> s{_dcCertificateARN = a})

instance AWSRequest DeleteCertificate where
        type Rs DeleteCertificate = DeleteCertificateResponse
        request = postJSON certificateManager
        response = receiveNull DeleteCertificateResponse'

instance Hashable DeleteCertificate where

instance NFData DeleteCertificate where

instance ToHeaders DeleteCertificate where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CertificateManager.DeleteCertificate" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteCertificate where
        toJSON DeleteCertificate'{..}
          = object
              (catMaybes
                 [Just ("CertificateArn" .= _dcCertificateARN)])

instance ToPath DeleteCertificate where
        toPath = const "/"

instance ToQuery DeleteCertificate where
        toQuery = const mempty

-- | /See:/ 'deleteCertificateResponse' smart constructor.
data DeleteCertificateResponse =
  DeleteCertificateResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCertificateResponse' with the minimum fields required to make a request.
--
deleteCertificateResponse
    :: DeleteCertificateResponse
deleteCertificateResponse = DeleteCertificateResponse'


instance NFData DeleteCertificateResponse where
