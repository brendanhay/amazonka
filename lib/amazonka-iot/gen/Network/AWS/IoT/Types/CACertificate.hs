{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CACertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CACertificate where

import Network.AWS.IoT.Types.CACertificateStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A CA certificate.
--
--
--
-- /See:/ 'cACertificate' smart constructor.
data CACertificate = CACertificate'
  { _cacStatus ::
      !(Maybe CACertificateStatus),
    _cacCertificateARN :: !(Maybe Text),
    _cacCertificateId :: !(Maybe Text),
    _cacCreationDate :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CACertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cacStatus' - The status of the CA certificate. The status value REGISTER_INACTIVE is deprecated and should not be used.
--
-- * 'cacCertificateARN' - The ARN of the CA certificate.
--
-- * 'cacCertificateId' - The ID of the CA certificate.
--
-- * 'cacCreationDate' - The date the CA certificate was created.
cACertificate ::
  CACertificate
cACertificate =
  CACertificate'
    { _cacStatus = Nothing,
      _cacCertificateARN = Nothing,
      _cacCertificateId = Nothing,
      _cacCreationDate = Nothing
    }

-- | The status of the CA certificate. The status value REGISTER_INACTIVE is deprecated and should not be used.
cacStatus :: Lens' CACertificate (Maybe CACertificateStatus)
cacStatus = lens _cacStatus (\s a -> s {_cacStatus = a})

-- | The ARN of the CA certificate.
cacCertificateARN :: Lens' CACertificate (Maybe Text)
cacCertificateARN = lens _cacCertificateARN (\s a -> s {_cacCertificateARN = a})

-- | The ID of the CA certificate.
cacCertificateId :: Lens' CACertificate (Maybe Text)
cacCertificateId = lens _cacCertificateId (\s a -> s {_cacCertificateId = a})

-- | The date the CA certificate was created.
cacCreationDate :: Lens' CACertificate (Maybe UTCTime)
cacCreationDate = lens _cacCreationDate (\s a -> s {_cacCreationDate = a}) . mapping _Time

instance FromJSON CACertificate where
  parseJSON =
    withObject
      "CACertificate"
      ( \x ->
          CACertificate'
            <$> (x .:? "status")
            <*> (x .:? "certificateArn")
            <*> (x .:? "certificateId")
            <*> (x .:? "creationDate")
      )

instance Hashable CACertificate

instance NFData CACertificate
