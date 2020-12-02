{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.EnablementType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.EnablementType where

import Network.AWS.Prelude

data EnablementType
  = ETEnabled
  | ETPending
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

instance FromText EnablementType where
  parser =
    takeLowerText >>= \case
      "enabled" -> pure ETEnabled
      "pending" -> pure ETPending
      e ->
        fromTextError $
          "Failure parsing EnablementType from value: '" <> e
            <> "'. Accepted values: enabled, pending"

instance ToText EnablementType where
  toText = \case
    ETEnabled -> "ENABLED"
    ETPending -> "PENDING"

instance Hashable EnablementType

instance NFData EnablementType

instance ToByteString EnablementType

instance ToQuery EnablementType

instance ToHeader EnablementType

instance FromJSON EnablementType where
  parseJSON = parseJSONText "EnablementType"
