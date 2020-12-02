{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DropFrameTimecode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DropFrameTimecode where

import Network.AWS.Prelude

-- | Applies only to 29.97 fps outputs. When this feature is enabled, the service will use drop-frame timecode on outputs. If it is not possible to use drop-frame timecode, the system will fall back to non-drop-frame. This setting is enabled by default when Timecode insertion (TimecodeInsertion) is enabled.
data DropFrameTimecode
  = DFTDisabled
  | DFTEnabled
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

instance FromText DropFrameTimecode where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure DFTDisabled
      "enabled" -> pure DFTEnabled
      e ->
        fromTextError $
          "Failure parsing DropFrameTimecode from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText DropFrameTimecode where
  toText = \case
    DFTDisabled -> "DISABLED"
    DFTEnabled -> "ENABLED"

instance Hashable DropFrameTimecode

instance NFData DropFrameTimecode

instance ToByteString DropFrameTimecode

instance ToQuery DropFrameTimecode

instance ToHeader DropFrameTimecode

instance ToJSON DropFrameTimecode where
  toJSON = toJSONText

instance FromJSON DropFrameTimecode where
  parseJSON = parseJSONText "DropFrameTimecode"
