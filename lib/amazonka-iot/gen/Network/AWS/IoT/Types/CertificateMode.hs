{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CertificateMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CertificateMode where

import Network.AWS.Prelude

data CertificateMode
  = Default
  | SNIOnly
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

instance FromText CertificateMode where
  parser =
    takeLowerText >>= \case
      "default" -> pure Default
      "sni_only" -> pure SNIOnly
      e ->
        fromTextError $
          "Failure parsing CertificateMode from value: '" <> e
            <> "'. Accepted values: default, sni_only"

instance ToText CertificateMode where
  toText = \case
    Default -> "DEFAULT"
    SNIOnly -> "SNI_ONLY"

instance Hashable CertificateMode

instance NFData CertificateMode

instance ToByteString CertificateMode

instance ToQuery CertificateMode

instance ToHeader CertificateMode

instance FromJSON CertificateMode where
  parseJSON = parseJSONText "CertificateMode"
