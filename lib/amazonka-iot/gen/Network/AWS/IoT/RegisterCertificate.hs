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
-- Module      : Network.AWS.IoT.RegisterCertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a device certificate with AWS IoT. If you have more than one CA certificate that has the same subject field, you must specify the CA certificate that was used to sign the device certificate being registered.
--
--
module Network.AWS.IoT.RegisterCertificate
    (
    -- * Creating a Request
      registerCertificate
    , RegisterCertificate
    -- * Request Lenses
    , rcStatus
    , rcCaCertificatePem
    , rcSetAsActive
    , rcCertificatePem

    -- * Destructuring the Response
    , registerCertificateResponse
    , RegisterCertificateResponse
    -- * Response Lenses
    , rcrsCertificateARN
    , rcrsCertificateId
    , rcrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input to the RegisterCertificate operation.
--
--
--
-- /See:/ 'registerCertificate' smart constructor.
data RegisterCertificate = RegisterCertificate'
  { _rcStatus           :: !(Maybe CertificateStatus)
  , _rcCaCertificatePem :: !(Maybe Text)
  , _rcSetAsActive      :: !(Maybe Bool)
  , _rcCertificatePem   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcStatus' - The status of the register certificate request.
--
-- * 'rcCaCertificatePem' - The CA certificate used to sign the device certificate being registered.
--
-- * 'rcSetAsActive' - A boolean value that specifies if the CA certificate is set to active.
--
-- * 'rcCertificatePem' - The certificate data, in PEM format.
registerCertificate
    :: Text -- ^ 'rcCertificatePem'
    -> RegisterCertificate
registerCertificate pCertificatePem_ =
  RegisterCertificate'
    { _rcStatus = Nothing
    , _rcCaCertificatePem = Nothing
    , _rcSetAsActive = Nothing
    , _rcCertificatePem = pCertificatePem_
    }


-- | The status of the register certificate request.
rcStatus :: Lens' RegisterCertificate (Maybe CertificateStatus)
rcStatus = lens _rcStatus (\ s a -> s{_rcStatus = a})

-- | The CA certificate used to sign the device certificate being registered.
rcCaCertificatePem :: Lens' RegisterCertificate (Maybe Text)
rcCaCertificatePem = lens _rcCaCertificatePem (\ s a -> s{_rcCaCertificatePem = a})

-- | A boolean value that specifies if the CA certificate is set to active.
rcSetAsActive :: Lens' RegisterCertificate (Maybe Bool)
rcSetAsActive = lens _rcSetAsActive (\ s a -> s{_rcSetAsActive = a})

-- | The certificate data, in PEM format.
rcCertificatePem :: Lens' RegisterCertificate Text
rcCertificatePem = lens _rcCertificatePem (\ s a -> s{_rcCertificatePem = a})

instance AWSRequest RegisterCertificate where
        type Rs RegisterCertificate =
             RegisterCertificateResponse
        request = postJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 RegisterCertificateResponse' <$>
                   (x .?> "certificateArn") <*> (x .?> "certificateId")
                     <*> (pure (fromEnum s)))

instance Hashable RegisterCertificate where

instance NFData RegisterCertificate where

instance ToHeaders RegisterCertificate where
        toHeaders = const mempty

instance ToJSON RegisterCertificate where
        toJSON RegisterCertificate'{..}
          = object
              (catMaybes
                 [("status" .=) <$> _rcStatus,
                  ("caCertificatePem" .=) <$> _rcCaCertificatePem,
                  Just ("certificatePem" .= _rcCertificatePem)])

instance ToPath RegisterCertificate where
        toPath = const "/certificate/register"

instance ToQuery RegisterCertificate where
        toQuery RegisterCertificate'{..}
          = mconcat ["setAsActive" =: _rcSetAsActive]

-- | The output from the RegisterCertificate operation.
--
--
--
-- /See:/ 'registerCertificateResponse' smart constructor.
data RegisterCertificateResponse = RegisterCertificateResponse'
  { _rcrsCertificateARN :: !(Maybe Text)
  , _rcrsCertificateId  :: !(Maybe Text)
  , _rcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcrsCertificateARN' - The certificate ARN.
--
-- * 'rcrsCertificateId' - The certificate identifier.
--
-- * 'rcrsResponseStatus' - -- | The response status code.
registerCertificateResponse
    :: Int -- ^ 'rcrsResponseStatus'
    -> RegisterCertificateResponse
registerCertificateResponse pResponseStatus_ =
  RegisterCertificateResponse'
    { _rcrsCertificateARN = Nothing
    , _rcrsCertificateId = Nothing
    , _rcrsResponseStatus = pResponseStatus_
    }


-- | The certificate ARN.
rcrsCertificateARN :: Lens' RegisterCertificateResponse (Maybe Text)
rcrsCertificateARN = lens _rcrsCertificateARN (\ s a -> s{_rcrsCertificateARN = a})

-- | The certificate identifier.
rcrsCertificateId :: Lens' RegisterCertificateResponse (Maybe Text)
rcrsCertificateId = lens _rcrsCertificateId (\ s a -> s{_rcrsCertificateId = a})

-- | -- | The response status code.
rcrsResponseStatus :: Lens' RegisterCertificateResponse Int
rcrsResponseStatus = lens _rcrsResponseStatus (\ s a -> s{_rcrsResponseStatus = a})

instance NFData RegisterCertificateResponse where
