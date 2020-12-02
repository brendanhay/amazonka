{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types.ContainerLevelMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStore.Types.ContainerLevelMetrics where

import Network.AWS.Prelude

data ContainerLevelMetrics
  = Disabled
  | Enabled
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

instance FromText ContainerLevelMetrics where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "enabled" -> pure Enabled
      e ->
        fromTextError $
          "Failure parsing ContainerLevelMetrics from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText ContainerLevelMetrics where
  toText = \case
    Disabled -> "DISABLED"
    Enabled -> "ENABLED"

instance Hashable ContainerLevelMetrics

instance NFData ContainerLevelMetrics

instance ToByteString ContainerLevelMetrics

instance ToQuery ContainerLevelMetrics

instance ToHeader ContainerLevelMetrics

instance ToJSON ContainerLevelMetrics where
  toJSON = toJSONText

instance FromJSON ContainerLevelMetrics where
  parseJSON = parseJSONText "ContainerLevelMetrics"
