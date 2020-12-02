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
-- Module      : Network.AWS.DirectoryService.DeregisterCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes from the system the certificate that was registered for a secured LDAP connection.
module Network.AWS.DirectoryService.DeregisterCertificate
  ( -- * Creating a Request
    deregisterCertificate,
    DeregisterCertificate,

    -- * Request Lenses
    derDirectoryId,
    derCertificateId,

    -- * Destructuring the Response
    deregisterCertificateResponse,
    DeregisterCertificateResponse,

    -- * Response Lenses
    dcrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deregisterCertificate' smart constructor.
data DeregisterCertificate = DeregisterCertificate'
  { _derDirectoryId ::
      !Text,
    _derCertificateId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeregisterCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'derDirectoryId' - The identifier of the directory.
--
-- * 'derCertificateId' - The identifier of the certificate.
deregisterCertificate ::
  -- | 'derDirectoryId'
  Text ->
  -- | 'derCertificateId'
  Text ->
  DeregisterCertificate
deregisterCertificate pDirectoryId_ pCertificateId_ =
  DeregisterCertificate'
    { _derDirectoryId = pDirectoryId_,
      _derCertificateId = pCertificateId_
    }

-- | The identifier of the directory.
derDirectoryId :: Lens' DeregisterCertificate Text
derDirectoryId = lens _derDirectoryId (\s a -> s {_derDirectoryId = a})

-- | The identifier of the certificate.
derCertificateId :: Lens' DeregisterCertificate Text
derCertificateId = lens _derCertificateId (\s a -> s {_derCertificateId = a})

instance AWSRequest DeregisterCertificate where
  type Rs DeregisterCertificate = DeregisterCertificateResponse
  request = postJSON directoryService
  response =
    receiveEmpty
      (\s h x -> DeregisterCertificateResponse' <$> (pure (fromEnum s)))

instance Hashable DeregisterCertificate

instance NFData DeregisterCertificate

instance ToHeaders DeregisterCertificate where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DirectoryService_20150416.DeregisterCertificate" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeregisterCertificate where
  toJSON DeregisterCertificate' {..} =
    object
      ( catMaybes
          [ Just ("DirectoryId" .= _derDirectoryId),
            Just ("CertificateId" .= _derCertificateId)
          ]
      )

instance ToPath DeregisterCertificate where
  toPath = const "/"

instance ToQuery DeregisterCertificate where
  toQuery = const mempty

-- | /See:/ 'deregisterCertificateResponse' smart constructor.
newtype DeregisterCertificateResponse = DeregisterCertificateResponse'
  { _dcrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeregisterCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsResponseStatus' - -- | The response status code.
deregisterCertificateResponse ::
  -- | 'dcrsResponseStatus'
  Int ->
  DeregisterCertificateResponse
deregisterCertificateResponse pResponseStatus_ =
  DeregisterCertificateResponse'
    { _dcrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dcrsResponseStatus :: Lens' DeregisterCertificateResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\s a -> s {_dcrsResponseStatus = a})

instance NFData DeregisterCertificateResponse
