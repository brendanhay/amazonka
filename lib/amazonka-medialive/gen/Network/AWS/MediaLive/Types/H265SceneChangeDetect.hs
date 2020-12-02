{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265SceneChangeDetect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265SceneChangeDetect where

import Network.AWS.Prelude

-- | H265 Scene Change Detect
data H265SceneChangeDetect
  = HSCDDisabled
  | HSCDEnabled
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

instance FromText H265SceneChangeDetect where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure HSCDDisabled
      "enabled" -> pure HSCDEnabled
      e ->
        fromTextError $
          "Failure parsing H265SceneChangeDetect from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText H265SceneChangeDetect where
  toText = \case
    HSCDDisabled -> "DISABLED"
    HSCDEnabled -> "ENABLED"

instance Hashable H265SceneChangeDetect

instance NFData H265SceneChangeDetect

instance ToByteString H265SceneChangeDetect

instance ToQuery H265SceneChangeDetect

instance ToHeader H265SceneChangeDetect

instance ToJSON H265SceneChangeDetect where
  toJSON = toJSONText

instance FromJSON H265SceneChangeDetect where
  parseJSON = parseJSONText "H265SceneChangeDetect"
