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
-- Module      : Network.AWS.IoT.RegisterCACertificate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a CA certificate with AWS IoT. This CA certificate can then be used to sign device certificates, which can be then registered with AWS IoT. You can register up to 10 CA certificates per AWS account that have the same subject field. This enables you to have up to 10 certificate authorities sign your device certificates. If you have more than one CA certificate registered, make sure you pass the CA certificate when you register your device certificates with the RegisterCertificate API.
--
--
module Network.AWS.IoT.RegisterCACertificate
    (
    -- * Creating a Request
      registerCACertificate
    , RegisterCACertificate
    -- * Request Lenses
    , rcacSetAsActive
    , rcacAllowAutoRegistration
    , rcacRegistrationConfig
    , rcacCaCertificate
    , rcacVerificationCertificate

    -- * Destructuring the Response
    , registerCACertificateResponse
    , RegisterCACertificateResponse
    -- * Response Lenses
    , rcacrsCertificateARN
    , rcacrsCertificateId
    , rcacrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input to the RegisterCACertificate operation.
--
--
--
-- /See:/ 'registerCACertificate' smart constructor.
data RegisterCACertificate = RegisterCACertificate'
  { _rcacSetAsActive             :: !(Maybe Bool)
  , _rcacAllowAutoRegistration   :: !(Maybe Bool)
  , _rcacRegistrationConfig      :: !(Maybe RegistrationConfig)
  , _rcacCaCertificate           :: !Text
  , _rcacVerificationCertificate :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterCACertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcacSetAsActive' - A boolean value that specifies if the CA certificate is set to active.
--
-- * 'rcacAllowAutoRegistration' - Allows this CA certificate to be used for auto registration of device certificates.
--
-- * 'rcacRegistrationConfig' - Information about the registration configuration.
--
-- * 'rcacCaCertificate' - The CA certificate.
--
-- * 'rcacVerificationCertificate' - The private key verification certificate.
registerCACertificate
    :: Text -- ^ 'rcacCaCertificate'
    -> Text -- ^ 'rcacVerificationCertificate'
    -> RegisterCACertificate
registerCACertificate pCaCertificate_ pVerificationCertificate_ =
  RegisterCACertificate'
    { _rcacSetAsActive = Nothing
    , _rcacAllowAutoRegistration = Nothing
    , _rcacRegistrationConfig = Nothing
    , _rcacCaCertificate = pCaCertificate_
    , _rcacVerificationCertificate = pVerificationCertificate_
    }


-- | A boolean value that specifies if the CA certificate is set to active.
rcacSetAsActive :: Lens' RegisterCACertificate (Maybe Bool)
rcacSetAsActive = lens _rcacSetAsActive (\ s a -> s{_rcacSetAsActive = a})

-- | Allows this CA certificate to be used for auto registration of device certificates.
rcacAllowAutoRegistration :: Lens' RegisterCACertificate (Maybe Bool)
rcacAllowAutoRegistration = lens _rcacAllowAutoRegistration (\ s a -> s{_rcacAllowAutoRegistration = a})

-- | Information about the registration configuration.
rcacRegistrationConfig :: Lens' RegisterCACertificate (Maybe RegistrationConfig)
rcacRegistrationConfig = lens _rcacRegistrationConfig (\ s a -> s{_rcacRegistrationConfig = a})

-- | The CA certificate.
rcacCaCertificate :: Lens' RegisterCACertificate Text
rcacCaCertificate = lens _rcacCaCertificate (\ s a -> s{_rcacCaCertificate = a})

-- | The private key verification certificate.
rcacVerificationCertificate :: Lens' RegisterCACertificate Text
rcacVerificationCertificate = lens _rcacVerificationCertificate (\ s a -> s{_rcacVerificationCertificate = a})

instance AWSRequest RegisterCACertificate where
        type Rs RegisterCACertificate =
             RegisterCACertificateResponse
        request = postJSON ioT
        response
          = receiveJSON
              (\ s h x ->
                 RegisterCACertificateResponse' <$>
                   (x .?> "certificateArn") <*> (x .?> "certificateId")
                     <*> (pure (fromEnum s)))

instance Hashable RegisterCACertificate where

instance NFData RegisterCACertificate where

instance ToHeaders RegisterCACertificate where
        toHeaders = const mempty

instance ToJSON RegisterCACertificate where
        toJSON RegisterCACertificate'{..}
          = object
              (catMaybes
                 [("registrationConfig" .=) <$>
                    _rcacRegistrationConfig,
                  Just ("caCertificate" .= _rcacCaCertificate),
                  Just
                    ("verificationCertificate" .=
                       _rcacVerificationCertificate)])

instance ToPath RegisterCACertificate where
        toPath = const "/cacertificate"

instance ToQuery RegisterCACertificate where
        toQuery RegisterCACertificate'{..}
          = mconcat
              ["setAsActive" =: _rcacSetAsActive,
               "allowAutoRegistration" =:
                 _rcacAllowAutoRegistration]

-- | The output from the RegisterCACertificateResponse operation.
--
--
--
-- /See:/ 'registerCACertificateResponse' smart constructor.
data RegisterCACertificateResponse = RegisterCACertificateResponse'
  { _rcacrsCertificateARN :: !(Maybe Text)
  , _rcacrsCertificateId  :: !(Maybe Text)
  , _rcacrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterCACertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcacrsCertificateARN' - The CA certificate ARN.
--
-- * 'rcacrsCertificateId' - The CA certificate identifier.
--
-- * 'rcacrsResponseStatus' - -- | The response status code.
registerCACertificateResponse
    :: Int -- ^ 'rcacrsResponseStatus'
    -> RegisterCACertificateResponse
registerCACertificateResponse pResponseStatus_ =
  RegisterCACertificateResponse'
    { _rcacrsCertificateARN = Nothing
    , _rcacrsCertificateId = Nothing
    , _rcacrsResponseStatus = pResponseStatus_
    }


-- | The CA certificate ARN.
rcacrsCertificateARN :: Lens' RegisterCACertificateResponse (Maybe Text)
rcacrsCertificateARN = lens _rcacrsCertificateARN (\ s a -> s{_rcacrsCertificateARN = a})

-- | The CA certificate identifier.
rcacrsCertificateId :: Lens' RegisterCACertificateResponse (Maybe Text)
rcacrsCertificateId = lens _rcacrsCertificateId (\ s a -> s{_rcacrsCertificateId = a})

-- | -- | The response status code.
rcacrsResponseStatus :: Lens' RegisterCACertificateResponse Int
rcacrsResponseStatus = lens _rcacrsResponseStatus (\ s a -> s{_rcacrsResponseStatus = a})

instance NFData RegisterCACertificateResponse where
