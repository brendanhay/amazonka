{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.GlobalConfigurationLowFramerateInputs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.GlobalConfigurationLowFramerateInputs where

import Network.AWS.Prelude

-- | Global Configuration Low Framerate Inputs
data GlobalConfigurationLowFramerateInputs
  = GCLFIDisabled
  | GCLFIEnabled
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

instance FromText GlobalConfigurationLowFramerateInputs where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure GCLFIDisabled
      "enabled" -> pure GCLFIEnabled
      e ->
        fromTextError $
          "Failure parsing GlobalConfigurationLowFramerateInputs from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText GlobalConfigurationLowFramerateInputs where
  toText = \case
    GCLFIDisabled -> "DISABLED"
    GCLFIEnabled -> "ENABLED"

instance Hashable GlobalConfigurationLowFramerateInputs

instance NFData GlobalConfigurationLowFramerateInputs

instance ToByteString GlobalConfigurationLowFramerateInputs

instance ToQuery GlobalConfigurationLowFramerateInputs

instance ToHeader GlobalConfigurationLowFramerateInputs

instance ToJSON GlobalConfigurationLowFramerateInputs where
  toJSON = toJSONText

instance FromJSON GlobalConfigurationLowFramerateInputs where
  parseJSON = parseJSONText "GlobalConfigurationLowFramerateInputs"
