{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.GlobalConfigurationInputEndAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.GlobalConfigurationInputEndAction where

import Network.AWS.Prelude

-- | Global Configuration Input End Action
data GlobalConfigurationInputEndAction
  = GCIEANone
  | GCIEASwitchAndLoopInputs
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

instance FromText GlobalConfigurationInputEndAction where
  parser =
    takeLowerText >>= \case
      "none" -> pure GCIEANone
      "switch_and_loop_inputs" -> pure GCIEASwitchAndLoopInputs
      e ->
        fromTextError $
          "Failure parsing GlobalConfigurationInputEndAction from value: '" <> e
            <> "'. Accepted values: none, switch_and_loop_inputs"

instance ToText GlobalConfigurationInputEndAction where
  toText = \case
    GCIEANone -> "NONE"
    GCIEASwitchAndLoopInputs -> "SWITCH_AND_LOOP_INPUTS"

instance Hashable GlobalConfigurationInputEndAction

instance NFData GlobalConfigurationInputEndAction

instance ToByteString GlobalConfigurationInputEndAction

instance ToQuery GlobalConfigurationInputEndAction

instance ToHeader GlobalConfigurationInputEndAction

instance ToJSON GlobalConfigurationInputEndAction where
  toJSON = toJSONText

instance FromJSON GlobalConfigurationInputEndAction where
  parseJSON = parseJSONText "GlobalConfigurationInputEndAction"
