{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.CertificateConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.CertificateConfiguration where

import Network.AWS.GameLift.Types.CertificateType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the use of a TLS/SSL certificate for a fleet. TLS certificate generation is enabled at the fleet level, with one certificate generated for the fleet. When this feature is enabled, the certificate can be retrieved using the <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-serversdk.html GameLift Server SDK> call @GetInstanceCertificate@ . All instances in a fleet share the same certificate.
--
--
--
-- /See:/ 'certificateConfiguration' smart constructor.
newtype CertificateConfiguration = CertificateConfiguration'
  { _ccCertificateType ::
      CertificateType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CertificateConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccCertificateType' - Indicates whether a TLS/SSL certificate was generated for a fleet.
certificateConfiguration ::
  -- | 'ccCertificateType'
  CertificateType ->
  CertificateConfiguration
certificateConfiguration pCertificateType_ =
  CertificateConfiguration' {_ccCertificateType = pCertificateType_}

-- | Indicates whether a TLS/SSL certificate was generated for a fleet.
ccCertificateType :: Lens' CertificateConfiguration CertificateType
ccCertificateType = lens _ccCertificateType (\s a -> s {_ccCertificateType = a})

instance FromJSON CertificateConfiguration where
  parseJSON =
    withObject
      "CertificateConfiguration"
      (\x -> CertificateConfiguration' <$> (x .: "CertificateType"))

instance Hashable CertificateConfiguration

instance NFData CertificateConfiguration

instance ToJSON CertificateConfiguration where
  toJSON CertificateConfiguration' {..} =
    object
      (catMaybes [Just ("CertificateType" .= _ccCertificateType)])
