{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ObfuscationSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.ObfuscationSetting where

import Network.AWS.Prelude

data ObfuscationSetting
  = DefaultObfuscation
  | None
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

instance FromText ObfuscationSetting where
  parser =
    takeLowerText >>= \case
      "default_obfuscation" -> pure DefaultObfuscation
      "none" -> pure None
      e ->
        fromTextError $
          "Failure parsing ObfuscationSetting from value: '" <> e
            <> "'. Accepted values: default_obfuscation, none"

instance ToText ObfuscationSetting where
  toText = \case
    DefaultObfuscation -> "DEFAULT_OBFUSCATION"
    None -> "NONE"

instance Hashable ObfuscationSetting

instance NFData ObfuscationSetting

instance ToByteString ObfuscationSetting

instance ToQuery ObfuscationSetting

instance ToHeader ObfuscationSetting

instance ToJSON ObfuscationSetting where
  toJSON = toJSONText

instance FromJSON ObfuscationSetting where
  parseJSON = parseJSONText "ObfuscationSetting"
