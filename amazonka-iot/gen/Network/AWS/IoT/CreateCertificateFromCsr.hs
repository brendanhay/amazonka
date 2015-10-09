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
-- Module      : Network.AWS.IoT.CreateCertificateFromCsr
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an X.509 certificate using the specified certificate signing
-- request.
--
-- __Note__ Reusing the same certificate signing request (CSR) results in a
-- distinct certificate.
--
-- /See:/ <https://aws.amazon.com/iot#CreateCertificateFromCsr.html AWS API Reference> for CreateCertificateFromCsr.
module Network.AWS.IoT.CreateCertificateFromCsr
    (
    -- * Creating a Request
      createCertificateFromCsr
    , CreateCertificateFromCsr
    -- * Request Lenses
    , ccfcSetAsActive
    , ccfcCertificateSigningRequest

    -- * Destructuring the Response
    , createCertificateFromCsrResponse
    , CreateCertificateFromCsrResponse
    -- * Response Lenses
    , ccfcrsCertificatePem
    , ccfcrsCertificateARN
    , ccfcrsCertificateId
    , ccfcrsResponseStatus
    ) where

import           Network.AWS.IoT.Types
import           Network.AWS.IoT.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the CreateCertificateFromCsr operation.
--
-- /See:/ 'createCertificateFromCsr' smart constructor.
data CreateCertificateFromCsr = CreateCertificateFromCsr'
    { _ccfcSetAsActive               :: !(Maybe Bool)
    , _ccfcCertificateSigningRequest :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateCertificateFromCsr' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccfcSetAsActive'
--
-- * 'ccfcCertificateSigningRequest'
createCertificateFromCsr
    :: Text -- ^ 'ccfcCertificateSigningRequest'
    -> CreateCertificateFromCsr
createCertificateFromCsr pCertificateSigningRequest_ =
    CreateCertificateFromCsr'
    { _ccfcSetAsActive = Nothing
    , _ccfcCertificateSigningRequest = pCertificateSigningRequest_
    }

-- | Specifies whether the certificate is active.
ccfcSetAsActive :: Lens' CreateCertificateFromCsr (Maybe Bool)
ccfcSetAsActive = lens _ccfcSetAsActive (\ s a -> s{_ccfcSetAsActive = a});

-- | The certificate signing request (CSR).
ccfcCertificateSigningRequest :: Lens' CreateCertificateFromCsr Text
ccfcCertificateSigningRequest = lens _ccfcCertificateSigningRequest (\ s a -> s{_ccfcCertificateSigningRequest = a});

instance AWSRequest CreateCertificateFromCsr where
        type Rs CreateCertificateFromCsr =
             CreateCertificateFromCsrResponse
        request = postJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 CreateCertificateFromCsrResponse' <$>
                   (x .?> "certificatePem") <*> (x .?> "certificateArn")
                     <*> (x .?> "certificateId")
                     <*> (pure (fromEnum s)))

instance ToHeaders CreateCertificateFromCsr where
        toHeaders = const mempty

instance ToJSON CreateCertificateFromCsr where
        toJSON CreateCertificateFromCsr'{..}
          = object
              (catMaybes
                 [Just
                    ("certificateSigningRequest" .=
                       _ccfcCertificateSigningRequest)])

instance ToPath CreateCertificateFromCsr where
        toPath = const "/certificates"

instance ToQuery CreateCertificateFromCsr where
        toQuery CreateCertificateFromCsr'{..}
          = mconcat ["setAsActive" =: _ccfcSetAsActive]

-- | The output from the CreateCertificateFromCsr operation.
--
-- /See:/ 'createCertificateFromCsrResponse' smart constructor.
data CreateCertificateFromCsrResponse = CreateCertificateFromCsrResponse'
    { _ccfcrsCertificatePem :: !(Maybe Text)
    , _ccfcrsCertificateARN :: !(Maybe Text)
    , _ccfcrsCertificateId  :: !(Maybe Text)
    , _ccfcrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateCertificateFromCsrResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccfcrsCertificatePem'
--
-- * 'ccfcrsCertificateARN'
--
-- * 'ccfcrsCertificateId'
--
-- * 'ccfcrsResponseStatus'
createCertificateFromCsrResponse
    :: Int -- ^ 'ccfcrsResponseStatus'
    -> CreateCertificateFromCsrResponse
createCertificateFromCsrResponse pResponseStatus_ =
    CreateCertificateFromCsrResponse'
    { _ccfcrsCertificatePem = Nothing
    , _ccfcrsCertificateARN = Nothing
    , _ccfcrsCertificateId = Nothing
    , _ccfcrsResponseStatus = pResponseStatus_
    }

-- | The certificate data, in PEM format.
ccfcrsCertificatePem :: Lens' CreateCertificateFromCsrResponse (Maybe Text)
ccfcrsCertificatePem = lens _ccfcrsCertificatePem (\ s a -> s{_ccfcrsCertificatePem = a});

-- | The Amazon Resource Name (ARN) of the certificate. You can use the ARN
-- as a principal for policy operations.
ccfcrsCertificateARN :: Lens' CreateCertificateFromCsrResponse (Maybe Text)
ccfcrsCertificateARN = lens _ccfcrsCertificateARN (\ s a -> s{_ccfcrsCertificateARN = a});

-- | The ID of the certificate. Certificate management operations only take a
-- certificateId.
ccfcrsCertificateId :: Lens' CreateCertificateFromCsrResponse (Maybe Text)
ccfcrsCertificateId = lens _ccfcrsCertificateId (\ s a -> s{_ccfcrsCertificateId = a});

-- | The response status code.
ccfcrsResponseStatus :: Lens' CreateCertificateFromCsrResponse Int
ccfcrsResponseStatus = lens _ccfcrsResponseStatus (\ s a -> s{_ccfcrsResponseStatus = a});
