{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AdvancedSecurityModeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AdvancedSecurityModeType where

import Network.AWS.Prelude

data AdvancedSecurityModeType
  = Audit
  | Enforced
  | Off
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

instance FromText AdvancedSecurityModeType where
  parser =
    takeLowerText >>= \case
      "audit" -> pure Audit
      "enforced" -> pure Enforced
      "off" -> pure Off
      e ->
        fromTextError $
          "Failure parsing AdvancedSecurityModeType from value: '" <> e
            <> "'. Accepted values: audit, enforced, off"

instance ToText AdvancedSecurityModeType where
  toText = \case
    Audit -> "AUDIT"
    Enforced -> "ENFORCED"
    Off -> "OFF"

instance Hashable AdvancedSecurityModeType

instance NFData AdvancedSecurityModeType

instance ToByteString AdvancedSecurityModeType

instance ToQuery AdvancedSecurityModeType

instance ToHeader AdvancedSecurityModeType

instance ToJSON AdvancedSecurityModeType where
  toJSON = toJSONText

instance FromJSON AdvancedSecurityModeType where
  parseJSON = parseJSONText "AdvancedSecurityModeType"
