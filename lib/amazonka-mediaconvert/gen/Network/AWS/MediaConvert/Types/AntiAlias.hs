{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AntiAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AntiAlias where

import Network.AWS.Prelude

-- | The anti-alias filter is automatically applied to all outputs. The service no longer accepts the value DISABLED for AntiAlias. If you specify that in your job, the service will ignore the setting.
data AntiAlias
  = AADisabled
  | AAEnabled
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

instance FromText AntiAlias where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure AADisabled
      "enabled" -> pure AAEnabled
      e ->
        fromTextError $
          "Failure parsing AntiAlias from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText AntiAlias where
  toText = \case
    AADisabled -> "DISABLED"
    AAEnabled -> "ENABLED"

instance Hashable AntiAlias

instance NFData AntiAlias

instance ToByteString AntiAlias

instance ToQuery AntiAlias

instance ToHeader AntiAlias

instance ToJSON AntiAlias where
  toJSON = toJSONText

instance FromJSON AntiAlias where
  parseJSON = parseJSONText "AntiAlias"
