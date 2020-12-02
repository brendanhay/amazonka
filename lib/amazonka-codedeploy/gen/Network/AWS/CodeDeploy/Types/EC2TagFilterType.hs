{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.EC2TagFilterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.EC2TagFilterType where

import Network.AWS.Prelude

data EC2TagFilterType
  = KeyAndValue
  | KeyOnly
  | ValueOnly
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

instance FromText EC2TagFilterType where
  parser =
    takeLowerText >>= \case
      "key_and_value" -> pure KeyAndValue
      "key_only" -> pure KeyOnly
      "value_only" -> pure ValueOnly
      e ->
        fromTextError $
          "Failure parsing EC2TagFilterType from value: '" <> e
            <> "'. Accepted values: key_and_value, key_only, value_only"

instance ToText EC2TagFilterType where
  toText = \case
    KeyAndValue -> "KEY_AND_VALUE"
    KeyOnly -> "KEY_ONLY"
    ValueOnly -> "VALUE_ONLY"

instance Hashable EC2TagFilterType

instance NFData EC2TagFilterType

instance ToByteString EC2TagFilterType

instance ToQuery EC2TagFilterType

instance ToHeader EC2TagFilterType

instance ToJSON EC2TagFilterType where
  toJSON = toJSONText

instance FromJSON EC2TagFilterType where
  parseJSON = parseJSONText "EC2TagFilterType"
