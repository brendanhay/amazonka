{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioType where

import Network.AWS.Prelude

-- | Audio Type
data AudioType
  = CleanEffects
  | HearingImpaired
  | Undefined
  | VisualImpairedCommentary
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

instance FromText AudioType where
  parser =
    takeLowerText >>= \case
      "clean_effects" -> pure CleanEffects
      "hearing_impaired" -> pure HearingImpaired
      "undefined" -> pure Undefined
      "visual_impaired_commentary" -> pure VisualImpairedCommentary
      e ->
        fromTextError $
          "Failure parsing AudioType from value: '" <> e
            <> "'. Accepted values: clean_effects, hearing_impaired, undefined, visual_impaired_commentary"

instance ToText AudioType where
  toText = \case
    CleanEffects -> "CLEAN_EFFECTS"
    HearingImpaired -> "HEARING_IMPAIRED"
    Undefined -> "UNDEFINED"
    VisualImpairedCommentary -> "VISUAL_IMPAIRED_COMMENTARY"

instance Hashable AudioType

instance NFData AudioType

instance ToByteString AudioType

instance ToQuery AudioType

instance ToHeader AudioType

instance ToJSON AudioType where
  toJSON = toJSONText

instance FromJSON AudioType where
  parseJSON = parseJSONText "AudioType"
