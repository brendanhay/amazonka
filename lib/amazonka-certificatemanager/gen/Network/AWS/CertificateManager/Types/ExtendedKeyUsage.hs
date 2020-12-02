{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.ExtendedKeyUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.ExtendedKeyUsage where

import Network.AWS.CertificateManager.Types.ExtendedKeyUsageName
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Extended Key Usage X.509 v3 extension defines one or more purposes for which the public key can be used. This is in addition to or in place of the basic purposes specified by the Key Usage extension.
--
--
--
-- /See:/ 'extendedKeyUsage' smart constructor.
data ExtendedKeyUsage = ExtendedKeyUsage'
  { _ekuOId :: !(Maybe Text),
    _ekuName :: !(Maybe ExtendedKeyUsageName)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExtendedKeyUsage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ekuOId' - An object identifier (OID) for the extension value. OIDs are strings of numbers separated by periods. The following OIDs are defined in RFC 3280 and RFC 5280.      * @1.3.6.1.5.5.7.3.1 (TLS_WEB_SERVER_AUTHENTICATION)@      * @1.3.6.1.5.5.7.3.2 (TLS_WEB_CLIENT_AUTHENTICATION)@      * @1.3.6.1.5.5.7.3.3 (CODE_SIGNING)@      * @1.3.6.1.5.5.7.3.4 (EMAIL_PROTECTION)@      * @1.3.6.1.5.5.7.3.8 (TIME_STAMPING)@      * @1.3.6.1.5.5.7.3.9 (OCSP_SIGNING)@      * @1.3.6.1.5.5.7.3.5 (IPSEC_END_SYSTEM)@      * @1.3.6.1.5.5.7.3.6 (IPSEC_TUNNEL)@      * @1.3.6.1.5.5.7.3.7 (IPSEC_USER)@
--
-- * 'ekuName' - The name of an Extended Key Usage value.
extendedKeyUsage ::
  ExtendedKeyUsage
extendedKeyUsage =
  ExtendedKeyUsage' {_ekuOId = Nothing, _ekuName = Nothing}

-- | An object identifier (OID) for the extension value. OIDs are strings of numbers separated by periods. The following OIDs are defined in RFC 3280 and RFC 5280.      * @1.3.6.1.5.5.7.3.1 (TLS_WEB_SERVER_AUTHENTICATION)@      * @1.3.6.1.5.5.7.3.2 (TLS_WEB_CLIENT_AUTHENTICATION)@      * @1.3.6.1.5.5.7.3.3 (CODE_SIGNING)@      * @1.3.6.1.5.5.7.3.4 (EMAIL_PROTECTION)@      * @1.3.6.1.5.5.7.3.8 (TIME_STAMPING)@      * @1.3.6.1.5.5.7.3.9 (OCSP_SIGNING)@      * @1.3.6.1.5.5.7.3.5 (IPSEC_END_SYSTEM)@      * @1.3.6.1.5.5.7.3.6 (IPSEC_TUNNEL)@      * @1.3.6.1.5.5.7.3.7 (IPSEC_USER)@
ekuOId :: Lens' ExtendedKeyUsage (Maybe Text)
ekuOId = lens _ekuOId (\s a -> s {_ekuOId = a})

-- | The name of an Extended Key Usage value.
ekuName :: Lens' ExtendedKeyUsage (Maybe ExtendedKeyUsageName)
ekuName = lens _ekuName (\s a -> s {_ekuName = a})

instance FromJSON ExtendedKeyUsage where
  parseJSON =
    withObject
      "ExtendedKeyUsage"
      (\x -> ExtendedKeyUsage' <$> (x .:? "OID") <*> (x .:? "Name"))

instance Hashable ExtendedKeyUsage

instance NFData ExtendedKeyUsage
