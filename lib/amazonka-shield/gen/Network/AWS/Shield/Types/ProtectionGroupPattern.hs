{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.ProtectionGroupPattern
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectionGroupPattern where

import Network.AWS.Prelude

data ProtectionGroupPattern
  = All
  | Arbitrary
  | ByResourceType
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

instance FromText ProtectionGroupPattern where
  parser =
    takeLowerText >>= \case
      "all" -> pure All
      "arbitrary" -> pure Arbitrary
      "by_resource_type" -> pure ByResourceType
      e ->
        fromTextError $
          "Failure parsing ProtectionGroupPattern from value: '" <> e
            <> "'. Accepted values: all, arbitrary, by_resource_type"

instance ToText ProtectionGroupPattern where
  toText = \case
    All -> "ALL"
    Arbitrary -> "ARBITRARY"
    ByResourceType -> "BY_RESOURCE_TYPE"

instance Hashable ProtectionGroupPattern

instance NFData ProtectionGroupPattern

instance ToByteString ProtectionGroupPattern

instance ToQuery ProtectionGroupPattern

instance ToHeader ProtectionGroupPattern

instance ToJSON ProtectionGroupPattern where
  toJSON = toJSONText

instance FromJSON ProtectionGroupPattern where
  parseJSON = parseJSONText "ProtectionGroupPattern"
