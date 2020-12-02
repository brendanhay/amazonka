{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.RegisterCertificateWithoutCA
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Register a certificate that does not have a certificate authority (CA).
module Network.AWS.IoT.RegisterCertificateWithoutCA
  ( -- * Creating a Request
    registerCertificateWithoutCA,
    RegisterCertificateWithoutCA,

    -- * Request Lenses
    rcwcaStatus,
    rcwcaCertificatePem,

    -- * Destructuring the Response
    registerCertificateWithoutCAResponse,
    RegisterCertificateWithoutCAResponse,

    -- * Response Lenses
    rcwcarsCertificateARN,
    rcwcarsCertificateId,
    rcwcarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerCertificateWithoutCA' smart constructor.
data RegisterCertificateWithoutCA = RegisterCertificateWithoutCA'
  { _rcwcaStatus ::
      !(Maybe CertificateStatus),
    _rcwcaCertificatePem :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterCertificateWithoutCA' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcwcaStatus' - The status of the register certificate request.
--
-- * 'rcwcaCertificatePem' - The certificate data, in PEM format.
registerCertificateWithoutCA ::
  -- | 'rcwcaCertificatePem'
  Text ->
  RegisterCertificateWithoutCA
registerCertificateWithoutCA pCertificatePem_ =
  RegisterCertificateWithoutCA'
    { _rcwcaStatus = Nothing,
      _rcwcaCertificatePem = pCertificatePem_
    }

-- | The status of the register certificate request.
rcwcaStatus :: Lens' RegisterCertificateWithoutCA (Maybe CertificateStatus)
rcwcaStatus = lens _rcwcaStatus (\s a -> s {_rcwcaStatus = a})

-- | The certificate data, in PEM format.
rcwcaCertificatePem :: Lens' RegisterCertificateWithoutCA Text
rcwcaCertificatePem = lens _rcwcaCertificatePem (\s a -> s {_rcwcaCertificatePem = a})

instance AWSRequest RegisterCertificateWithoutCA where
  type
    Rs RegisterCertificateWithoutCA =
      RegisterCertificateWithoutCAResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          RegisterCertificateWithoutCAResponse'
            <$> (x .?> "certificateArn")
            <*> (x .?> "certificateId")
            <*> (pure (fromEnum s))
      )

instance Hashable RegisterCertificateWithoutCA

instance NFData RegisterCertificateWithoutCA

instance ToHeaders RegisterCertificateWithoutCA where
  toHeaders = const mempty

instance ToJSON RegisterCertificateWithoutCA where
  toJSON RegisterCertificateWithoutCA' {..} =
    object
      ( catMaybes
          [ ("status" .=) <$> _rcwcaStatus,
            Just ("certificatePem" .= _rcwcaCertificatePem)
          ]
      )

instance ToPath RegisterCertificateWithoutCA where
  toPath = const "/certificate/register-no-ca"

instance ToQuery RegisterCertificateWithoutCA where
  toQuery = const mempty

-- | /See:/ 'registerCertificateWithoutCAResponse' smart constructor.
data RegisterCertificateWithoutCAResponse = RegisterCertificateWithoutCAResponse'
  { _rcwcarsCertificateARN ::
      !(Maybe Text),
    _rcwcarsCertificateId ::
      !(Maybe Text),
    _rcwcarsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterCertificateWithoutCAResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcwcarsCertificateARN' - The Amazon Resource Name (ARN) of the registered certificate.
--
-- * 'rcwcarsCertificateId' - The ID of the registered certificate. (The last part of the certificate ARN contains the certificate ID.
--
-- * 'rcwcarsResponseStatus' - -- | The response status code.
registerCertificateWithoutCAResponse ::
  -- | 'rcwcarsResponseStatus'
  Int ->
  RegisterCertificateWithoutCAResponse
registerCertificateWithoutCAResponse pResponseStatus_ =
  RegisterCertificateWithoutCAResponse'
    { _rcwcarsCertificateARN =
        Nothing,
      _rcwcarsCertificateId = Nothing,
      _rcwcarsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the registered certificate.
rcwcarsCertificateARN :: Lens' RegisterCertificateWithoutCAResponse (Maybe Text)
rcwcarsCertificateARN = lens _rcwcarsCertificateARN (\s a -> s {_rcwcarsCertificateARN = a})

-- | The ID of the registered certificate. (The last part of the certificate ARN contains the certificate ID.
rcwcarsCertificateId :: Lens' RegisterCertificateWithoutCAResponse (Maybe Text)
rcwcarsCertificateId = lens _rcwcarsCertificateId (\s a -> s {_rcwcarsCertificateId = a})

-- | -- | The response status code.
rcwcarsResponseStatus :: Lens' RegisterCertificateWithoutCAResponse Int
rcwcarsResponseStatus = lens _rcwcarsResponseStatus (\s a -> s {_rcwcarsResponseStatus = a})

instance NFData RegisterCertificateWithoutCAResponse
