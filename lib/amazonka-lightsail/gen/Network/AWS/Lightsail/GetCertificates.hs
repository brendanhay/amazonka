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
-- Module      : Network.AWS.Lightsail.GetCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more Amazon Lightsail SSL/TLS certificates.
module Network.AWS.Lightsail.GetCertificates
  ( -- * Creating a Request
    getCertificates,
    GetCertificates,

    -- * Request Lenses
    gcCertificateStatuses,
    gcCertificateName,
    gcIncludeCertificateDetails,

    -- * Destructuring the Response
    getCertificatesResponse,
    GetCertificatesResponse,

    -- * Response Lenses
    gcrsCertificates,
    gcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCertificates' smart constructor.
data GetCertificates = GetCertificates'
  { _gcCertificateStatuses ::
      !(Maybe [CertificateStatus]),
    _gcCertificateName :: !(Maybe Text),
    _gcIncludeCertificateDetails :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCertificates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcCertificateStatuses' - The status of the certificates for which to return information. For example, specify @ISSUED@ to return only certificates with an @ISSUED@ status. When omitted, the response includes all of your certificates in the AWS Region where the request is made, regardless of their current status.
--
-- * 'gcCertificateName' - The name for the certificate for which to return information. When omitted, the response includes all of your certificates in the AWS Region where the request is made.
--
-- * 'gcIncludeCertificateDetails' - Indicates whether to include detailed information about the certificates in the response. When omitted, the response includes only the certificate names, Amazon Resource Names (ARNs), domain names, and tags.
getCertificates ::
  GetCertificates
getCertificates =
  GetCertificates'
    { _gcCertificateStatuses = Nothing,
      _gcCertificateName = Nothing,
      _gcIncludeCertificateDetails = Nothing
    }

-- | The status of the certificates for which to return information. For example, specify @ISSUED@ to return only certificates with an @ISSUED@ status. When omitted, the response includes all of your certificates in the AWS Region where the request is made, regardless of their current status.
gcCertificateStatuses :: Lens' GetCertificates [CertificateStatus]
gcCertificateStatuses = lens _gcCertificateStatuses (\s a -> s {_gcCertificateStatuses = a}) . _Default . _Coerce

-- | The name for the certificate for which to return information. When omitted, the response includes all of your certificates in the AWS Region where the request is made.
gcCertificateName :: Lens' GetCertificates (Maybe Text)
gcCertificateName = lens _gcCertificateName (\s a -> s {_gcCertificateName = a})

-- | Indicates whether to include detailed information about the certificates in the response. When omitted, the response includes only the certificate names, Amazon Resource Names (ARNs), domain names, and tags.
gcIncludeCertificateDetails :: Lens' GetCertificates (Maybe Bool)
gcIncludeCertificateDetails = lens _gcIncludeCertificateDetails (\s a -> s {_gcIncludeCertificateDetails = a})

instance AWSRequest GetCertificates where
  type Rs GetCertificates = GetCertificatesResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          GetCertificatesResponse'
            <$> (x .?> "certificates" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable GetCertificates

instance NFData GetCertificates

instance ToHeaders GetCertificates where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.GetCertificates" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetCertificates where
  toJSON GetCertificates' {..} =
    object
      ( catMaybes
          [ ("certificateStatuses" .=) <$> _gcCertificateStatuses,
            ("certificateName" .=) <$> _gcCertificateName,
            ("includeCertificateDetails" .=) <$> _gcIncludeCertificateDetails
          ]
      )

instance ToPath GetCertificates where
  toPath = const "/"

instance ToQuery GetCertificates where
  toQuery = const mempty

-- | /See:/ 'getCertificatesResponse' smart constructor.
data GetCertificatesResponse = GetCertificatesResponse'
  { _gcrsCertificates ::
      !(Maybe [CertificateSummary]),
    _gcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCertificatesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrsCertificates' - An object that describes certificates.
--
-- * 'gcrsResponseStatus' - -- | The response status code.
getCertificatesResponse ::
  -- | 'gcrsResponseStatus'
  Int ->
  GetCertificatesResponse
getCertificatesResponse pResponseStatus_ =
  GetCertificatesResponse'
    { _gcrsCertificates = Nothing,
      _gcrsResponseStatus = pResponseStatus_
    }

-- | An object that describes certificates.
gcrsCertificates :: Lens' GetCertificatesResponse [CertificateSummary]
gcrsCertificates = lens _gcrsCertificates (\s a -> s {_gcrsCertificates = a}) . _Default . _Coerce

-- | -- | The response status code.
gcrsResponseStatus :: Lens' GetCertificatesResponse Int
gcrsResponseStatus = lens _gcrsResponseStatus (\s a -> s {_gcrsResponseStatus = a})

instance NFData GetCertificatesResponse
