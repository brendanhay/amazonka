{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TagFilterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TagFilterType where

import Network.AWS.Prelude

data TagFilterType
  = TFTKeyAndValue
  | TFTKeyOnly
  | TFTValueOnly
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

instance FromText TagFilterType where
  parser =
    takeLowerText >>= \case
      "key_and_value" -> pure TFTKeyAndValue
      "key_only" -> pure TFTKeyOnly
      "value_only" -> pure TFTValueOnly
      e ->
        fromTextError $
          "Failure parsing TagFilterType from value: '" <> e
            <> "'. Accepted values: key_and_value, key_only, value_only"

instance ToText TagFilterType where
  toText = \case
    TFTKeyAndValue -> "KEY_AND_VALUE"
    TFTKeyOnly -> "KEY_ONLY"
    TFTValueOnly -> "VALUE_ONLY"

instance Hashable TagFilterType

instance NFData TagFilterType

instance ToByteString TagFilterType

instance ToQuery TagFilterType

instance ToHeader TagFilterType

instance ToJSON TagFilterType where
  toJSON = toJSONText

instance FromJSON TagFilterType where
  parseJSON = parseJSONText "TagFilterType"
