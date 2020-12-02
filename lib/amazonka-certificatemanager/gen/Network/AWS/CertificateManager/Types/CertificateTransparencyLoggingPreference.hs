{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.CertificateTransparencyLoggingPreference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.CertificateTransparencyLoggingPreference where

import Network.AWS.Prelude

data CertificateTransparencyLoggingPreference
  = Disabled
  | Enabled
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText CertificateTransparencyLoggingPreference where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "enabled" -> pure Enabled
      e ->
        fromTextError $
          "Failure parsing CertificateTransparencyLoggingPreference from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText CertificateTransparencyLoggingPreference where
  toText = \case
    Disabled -> "DISABLED"
    Enabled -> "ENABLED"

instance Hashable CertificateTransparencyLoggingPreference

instance NFData CertificateTransparencyLoggingPreference

instance ToByteString CertificateTransparencyLoggingPreference

instance ToQuery CertificateTransparencyLoggingPreference

instance ToHeader CertificateTransparencyLoggingPreference

instance ToJSON CertificateTransparencyLoggingPreference where
  toJSON = toJSONText

instance FromJSON CertificateTransparencyLoggingPreference where
  parseJSON = parseJSONText "CertificateTransparencyLoggingPreference"
