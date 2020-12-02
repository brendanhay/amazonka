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
-- Module      : Network.AWS.DirectoryService.DescribeCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays information about the certificate registered for a secured LDAP connection.
module Network.AWS.DirectoryService.DescribeCertificate
  ( -- * Creating a Request
    describeCertificate,
    DescribeCertificate,

    -- * Request Lenses
    desDirectoryId,
    desCertificateId,

    -- * Destructuring the Response
    describeCertificateResponse,
    DescribeCertificateResponse,

    -- * Response Lenses
    desrsCertificate,
    desrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeCertificate' smart constructor.
data DescribeCertificate = DescribeCertificate'
  { _desDirectoryId ::
      !Text,
    _desCertificateId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desDirectoryId' - The identifier of the directory.
--
-- * 'desCertificateId' - The identifier of the certificate.
describeCertificate ::
  -- | 'desDirectoryId'
  Text ->
  -- | 'desCertificateId'
  Text ->
  DescribeCertificate
describeCertificate pDirectoryId_ pCertificateId_ =
  DescribeCertificate'
    { _desDirectoryId = pDirectoryId_,
      _desCertificateId = pCertificateId_
    }

-- | The identifier of the directory.
desDirectoryId :: Lens' DescribeCertificate Text
desDirectoryId = lens _desDirectoryId (\s a -> s {_desDirectoryId = a})

-- | The identifier of the certificate.
desCertificateId :: Lens' DescribeCertificate Text
desCertificateId = lens _desCertificateId (\s a -> s {_desCertificateId = a})

instance AWSRequest DescribeCertificate where
  type Rs DescribeCertificate = DescribeCertificateResponse
  request = postJSON directoryService
  response =
    receiveJSON
      ( \s h x ->
          DescribeCertificateResponse'
            <$> (x .?> "Certificate") <*> (pure (fromEnum s))
      )

instance Hashable DescribeCertificate

instance NFData DescribeCertificate

instance ToHeaders DescribeCertificate where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DirectoryService_20150416.DescribeCertificate" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeCertificate where
  toJSON DescribeCertificate' {..} =
    object
      ( catMaybes
          [ Just ("DirectoryId" .= _desDirectoryId),
            Just ("CertificateId" .= _desCertificateId)
          ]
      )

instance ToPath DescribeCertificate where
  toPath = const "/"

instance ToQuery DescribeCertificate where
  toQuery = const mempty

-- | /See:/ 'describeCertificateResponse' smart constructor.
data DescribeCertificateResponse = DescribeCertificateResponse'
  { _desrsCertificate ::
      !(Maybe Certificate),
    _desrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsCertificate' - Information about the certificate, including registered date time, certificate state, the reason for the state, expiration date time, and certificate common name.
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeCertificateResponse ::
  -- | 'desrsResponseStatus'
  Int ->
  DescribeCertificateResponse
describeCertificateResponse pResponseStatus_ =
  DescribeCertificateResponse'
    { _desrsCertificate = Nothing,
      _desrsResponseStatus = pResponseStatus_
    }

-- | Information about the certificate, including registered date time, certificate state, the reason for the state, expiration date time, and certificate common name.
desrsCertificate :: Lens' DescribeCertificateResponse (Maybe Certificate)
desrsCertificate = lens _desrsCertificate (\s a -> s {_desrsCertificate = a})

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeCertificateResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\s a -> s {_desrsResponseStatus = a})

instance NFData DescribeCertificateResponse
