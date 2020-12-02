{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ScalingBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ScalingBehavior where

import Network.AWS.Prelude

-- | Specify how the service handles outputs that have a different aspect ratio from the input aspect ratio. Choose Stretch to output (STRETCH_TO_OUTPUT) to have the service stretch your video image to fit. Keep the setting Default (DEFAULT) to have the service letterbox your video instead. This setting overrides any value that you specify for the setting Selection placement (position) in this output.
data ScalingBehavior
  = SBDefault
  | SBStretchToOutput
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

instance FromText ScalingBehavior where
  parser =
    takeLowerText >>= \case
      "default" -> pure SBDefault
      "stretch_to_output" -> pure SBStretchToOutput
      e ->
        fromTextError $
          "Failure parsing ScalingBehavior from value: '" <> e
            <> "'. Accepted values: default, stretch_to_output"

instance ToText ScalingBehavior where
  toText = \case
    SBDefault -> "DEFAULT"
    SBStretchToOutput -> "STRETCH_TO_OUTPUT"

instance Hashable ScalingBehavior

instance NFData ScalingBehavior

instance ToByteString ScalingBehavior

instance ToQuery ScalingBehavior

instance ToHeader ScalingBehavior

instance ToJSON ScalingBehavior where
  toJSON = toJSONText

instance FromJSON ScalingBehavior where
  parseJSON = parseJSONText "ScalingBehavior"
