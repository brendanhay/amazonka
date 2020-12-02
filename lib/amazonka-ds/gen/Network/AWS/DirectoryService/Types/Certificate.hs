{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.Certificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.Certificate where

import Network.AWS.DirectoryService.Types.CertificateState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the certificate.
--
--
--
-- /See:/ 'certificate' smart constructor.
data Certificate = Certificate'
  { _cState ::
      !(Maybe CertificateState),
    _cCommonName :: !(Maybe Text),
    _cCertificateId :: !(Maybe Text),
    _cExpiryDateTime :: !(Maybe POSIX),
    _cRegisteredDateTime :: !(Maybe POSIX),
    _cStateReason :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Certificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cState' - The state of the certificate.
--
-- * 'cCommonName' - The common name for the certificate.
--
-- * 'cCertificateId' - The identifier of the certificate.
--
-- * 'cExpiryDateTime' - The date and time when the certificate will expire.
--
-- * 'cRegisteredDateTime' - The date and time that the certificate was registered.
--
-- * 'cStateReason' - Describes a state change for the certificate.
certificate ::
  Certificate
certificate =
  Certificate'
    { _cState = Nothing,
      _cCommonName = Nothing,
      _cCertificateId = Nothing,
      _cExpiryDateTime = Nothing,
      _cRegisteredDateTime = Nothing,
      _cStateReason = Nothing
    }

-- | The state of the certificate.
cState :: Lens' Certificate (Maybe CertificateState)
cState = lens _cState (\s a -> s {_cState = a})

-- | The common name for the certificate.
cCommonName :: Lens' Certificate (Maybe Text)
cCommonName = lens _cCommonName (\s a -> s {_cCommonName = a})

-- | The identifier of the certificate.
cCertificateId :: Lens' Certificate (Maybe Text)
cCertificateId = lens _cCertificateId (\s a -> s {_cCertificateId = a})

-- | The date and time when the certificate will expire.
cExpiryDateTime :: Lens' Certificate (Maybe UTCTime)
cExpiryDateTime = lens _cExpiryDateTime (\s a -> s {_cExpiryDateTime = a}) . mapping _Time

-- | The date and time that the certificate was registered.
cRegisteredDateTime :: Lens' Certificate (Maybe UTCTime)
cRegisteredDateTime = lens _cRegisteredDateTime (\s a -> s {_cRegisteredDateTime = a}) . mapping _Time

-- | Describes a state change for the certificate.
cStateReason :: Lens' Certificate (Maybe Text)
cStateReason = lens _cStateReason (\s a -> s {_cStateReason = a})

instance FromJSON Certificate where
  parseJSON =
    withObject
      "Certificate"
      ( \x ->
          Certificate'
            <$> (x .:? "State")
            <*> (x .:? "CommonName")
            <*> (x .:? "CertificateId")
            <*> (x .:? "ExpiryDateTime")
            <*> (x .:? "RegisteredDateTime")
            <*> (x .:? "StateReason")
      )

instance Hashable Certificate

instance NFData Certificate
