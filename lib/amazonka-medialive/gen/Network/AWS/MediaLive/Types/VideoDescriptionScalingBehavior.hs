{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoDescriptionScalingBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoDescriptionScalingBehavior where

import Network.AWS.Prelude

-- | Video Description Scaling Behavior
data VideoDescriptionScalingBehavior
  = Default
  | StretchToOutput
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

instance FromText VideoDescriptionScalingBehavior where
  parser =
    takeLowerText >>= \case
      "default" -> pure Default
      "stretch_to_output" -> pure StretchToOutput
      e ->
        fromTextError $
          "Failure parsing VideoDescriptionScalingBehavior from value: '" <> e
            <> "'. Accepted values: default, stretch_to_output"

instance ToText VideoDescriptionScalingBehavior where
  toText = \case
    Default -> "DEFAULT"
    StretchToOutput -> "STRETCH_TO_OUTPUT"

instance Hashable VideoDescriptionScalingBehavior

instance NFData VideoDescriptionScalingBehavior

instance ToByteString VideoDescriptionScalingBehavior

instance ToQuery VideoDescriptionScalingBehavior

instance ToHeader VideoDescriptionScalingBehavior

instance ToJSON VideoDescriptionScalingBehavior where
  toJSON = toJSONText

instance FromJSON VideoDescriptionScalingBehavior where
  parseJSON = parseJSONText "VideoDescriptionScalingBehavior"
