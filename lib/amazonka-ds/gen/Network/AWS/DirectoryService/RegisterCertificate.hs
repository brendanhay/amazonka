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
-- Module      : Network.AWS.DirectoryService.RegisterCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a certificate for secured LDAP connection.
module Network.AWS.DirectoryService.RegisterCertificate
  ( -- * Creating a Request
    registerCertificate,
    RegisterCertificate,

    -- * Request Lenses
    rcDirectoryId,
    rcCertificateData,

    -- * Destructuring the Response
    registerCertificateResponse,
    RegisterCertificateResponse,

    -- * Response Lenses
    rcrsCertificateId,
    rcrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerCertificate' smart constructor.
data RegisterCertificate = RegisterCertificate'
  { _rcDirectoryId ::
      !Text,
    _rcCertificateData :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcDirectoryId' - The identifier of the directory.
--
-- * 'rcCertificateData' - The certificate PEM string that needs to be registered.
registerCertificate ::
  -- | 'rcDirectoryId'
  Text ->
  -- | 'rcCertificateData'
  Text ->
  RegisterCertificate
registerCertificate pDirectoryId_ pCertificateData_ =
  RegisterCertificate'
    { _rcDirectoryId = pDirectoryId_,
      _rcCertificateData = pCertificateData_
    }

-- | The identifier of the directory.
rcDirectoryId :: Lens' RegisterCertificate Text
rcDirectoryId = lens _rcDirectoryId (\s a -> s {_rcDirectoryId = a})

-- | The certificate PEM string that needs to be registered.
rcCertificateData :: Lens' RegisterCertificate Text
rcCertificateData = lens _rcCertificateData (\s a -> s {_rcCertificateData = a})

instance AWSRequest RegisterCertificate where
  type Rs RegisterCertificate = RegisterCertificateResponse
  request = postJSON directoryService
  response =
    receiveJSON
      ( \s h x ->
          RegisterCertificateResponse'
            <$> (x .?> "CertificateId") <*> (pure (fromEnum s))
      )

instance Hashable RegisterCertificate

instance NFData RegisterCertificate

instance ToHeaders RegisterCertificate where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DirectoryService_20150416.RegisterCertificate" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON RegisterCertificate where
  toJSON RegisterCertificate' {..} =
    object
      ( catMaybes
          [ Just ("DirectoryId" .= _rcDirectoryId),
            Just ("CertificateData" .= _rcCertificateData)
          ]
      )

instance ToPath RegisterCertificate where
  toPath = const "/"

instance ToQuery RegisterCertificate where
  toQuery = const mempty

-- | /See:/ 'registerCertificateResponse' smart constructor.
data RegisterCertificateResponse = RegisterCertificateResponse'
  { _rcrsCertificateId ::
      !(Maybe Text),
    _rcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcrsCertificateId' - The identifier of the certificate.
--
-- * 'rcrsResponseStatus' - -- | The response status code.
registerCertificateResponse ::
  -- | 'rcrsResponseStatus'
  Int ->
  RegisterCertificateResponse
registerCertificateResponse pResponseStatus_ =
  RegisterCertificateResponse'
    { _rcrsCertificateId = Nothing,
      _rcrsResponseStatus = pResponseStatus_
    }

-- | The identifier of the certificate.
rcrsCertificateId :: Lens' RegisterCertificateResponse (Maybe Text)
rcrsCertificateId = lens _rcrsCertificateId (\s a -> s {_rcrsCertificateId = a})

-- | -- | The response status code.
rcrsResponseStatus :: Lens' RegisterCertificateResponse Int
rcrsResponseStatus = lens _rcrsResponseStatus (\s a -> s {_rcrsResponseStatus = a})

instance NFData RegisterCertificateResponse
