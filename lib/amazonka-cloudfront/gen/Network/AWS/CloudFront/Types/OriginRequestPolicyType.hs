{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyType where

import Network.AWS.Prelude

data OriginRequestPolicyType
  = Custom
  | Managed
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

instance FromText OriginRequestPolicyType where
  parser =
    takeLowerText >>= \case
      "custom" -> pure Custom
      "managed" -> pure Managed
      e ->
        fromTextError $
          "Failure parsing OriginRequestPolicyType from value: '" <> e
            <> "'. Accepted values: custom, managed"

instance ToText OriginRequestPolicyType where
  toText = \case
    Custom -> "custom"
    Managed -> "managed"

instance Hashable OriginRequestPolicyType

instance NFData OriginRequestPolicyType

instance ToByteString OriginRequestPolicyType

instance ToQuery OriginRequestPolicyType

instance ToHeader OriginRequestPolicyType

instance FromXML OriginRequestPolicyType where
  parseXML = parseXMLText "OriginRequestPolicyType"

instance ToXML OriginRequestPolicyType where
  toXML = toXMLText
