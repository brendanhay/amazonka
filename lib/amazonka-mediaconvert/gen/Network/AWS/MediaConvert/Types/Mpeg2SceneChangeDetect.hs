{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2SceneChangeDetect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2SceneChangeDetect where

import Network.AWS.Prelude

-- | Enable this setting to insert I-frames at scene changes that the service automatically detects. This improves video quality and is enabled by default.
data Mpeg2SceneChangeDetect
  = MSCDDisabled
  | MSCDEnabled
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

instance FromText Mpeg2SceneChangeDetect where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure MSCDDisabled
      "enabled" -> pure MSCDEnabled
      e ->
        fromTextError $
          "Failure parsing Mpeg2SceneChangeDetect from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText Mpeg2SceneChangeDetect where
  toText = \case
    MSCDDisabled -> "DISABLED"
    MSCDEnabled -> "ENABLED"

instance Hashable Mpeg2SceneChangeDetect

instance NFData Mpeg2SceneChangeDetect

instance ToByteString Mpeg2SceneChangeDetect

instance ToQuery Mpeg2SceneChangeDetect

instance ToHeader Mpeg2SceneChangeDetect

instance ToJSON Mpeg2SceneChangeDetect where
  toJSON = toJSONText

instance FromJSON Mpeg2SceneChangeDetect where
  parseJSON = parseJSONText "Mpeg2SceneChangeDetect"
