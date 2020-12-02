{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.CertificateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.CertificateType where

import Network.AWS.Prelude

data CertificateType
  = Disabled
  | Generated
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

instance FromText CertificateType where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "generated" -> pure Generated
      e ->
        fromTextError $
          "Failure parsing CertificateType from value: '" <> e
            <> "'. Accepted values: disabled, generated"

instance ToText CertificateType where
  toText = \case
    Disabled -> "DISABLED"
    Generated -> "GENERATED"

instance Hashable CertificateType

instance NFData CertificateType

instance ToByteString CertificateType

instance ToQuery CertificateType

instance ToHeader CertificateType

instance ToJSON CertificateType where
  toJSON = toJSONText

instance FromJSON CertificateType where
  parseJSON = parseJSONText "CertificateType"
