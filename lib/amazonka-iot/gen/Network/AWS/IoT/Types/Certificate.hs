{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Certificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Certificate where

import Network.AWS.IoT.Types.CertificateMode
import Network.AWS.IoT.Types.CertificateStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a certificate.
--
--
--
-- /See:/ 'certificate' smart constructor.
data Certificate = Certificate'
  { _cStatus ::
      !(Maybe CertificateStatus),
    _cCertificateARN :: !(Maybe Text),
    _cCertificateId :: !(Maybe Text),
    _cCertificateMode :: !(Maybe CertificateMode),
    _cCreationDate :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Certificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cStatus' - The status of the certificate. The status value REGISTER_INACTIVE is deprecated and should not be used.
--
-- * 'cCertificateARN' - The ARN of the certificate.
--
-- * 'cCertificateId' - The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
--
-- * 'cCertificateMode' - The mode of the certificate.
--
-- * 'cCreationDate' - The date and time the certificate was created.
certificate ::
  Certificate
certificate =
  Certificate'
    { _cStatus = Nothing,
      _cCertificateARN = Nothing,
      _cCertificateId = Nothing,
      _cCertificateMode = Nothing,
      _cCreationDate = Nothing
    }

-- | The status of the certificate. The status value REGISTER_INACTIVE is deprecated and should not be used.
cStatus :: Lens' Certificate (Maybe CertificateStatus)
cStatus = lens _cStatus (\s a -> s {_cStatus = a})

-- | The ARN of the certificate.
cCertificateARN :: Lens' Certificate (Maybe Text)
cCertificateARN = lens _cCertificateARN (\s a -> s {_cCertificateARN = a})

-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
cCertificateId :: Lens' Certificate (Maybe Text)
cCertificateId = lens _cCertificateId (\s a -> s {_cCertificateId = a})

-- | The mode of the certificate.
cCertificateMode :: Lens' Certificate (Maybe CertificateMode)
cCertificateMode = lens _cCertificateMode (\s a -> s {_cCertificateMode = a})

-- | The date and time the certificate was created.
cCreationDate :: Lens' Certificate (Maybe UTCTime)
cCreationDate = lens _cCreationDate (\s a -> s {_cCreationDate = a}) . mapping _Time

instance FromJSON Certificate where
  parseJSON =
    withObject
      "Certificate"
      ( \x ->
          Certificate'
            <$> (x .:? "status")
            <*> (x .:? "certificateArn")
            <*> (x .:? "certificateId")
            <*> (x .:? "certificateMode")
            <*> (x .:? "creationDate")
      )

instance Hashable Certificate

instance NFData Certificate
