{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoSelectorColorSpaceUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoSelectorColorSpaceUsage where

import Network.AWS.Prelude

-- | Video Selector Color Space Usage
data VideoSelectorColorSpaceUsage
  = Fallback
  | Force
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

instance FromText VideoSelectorColorSpaceUsage where
  parser =
    takeLowerText >>= \case
      "fallback" -> pure Fallback
      "force" -> pure Force
      e ->
        fromTextError $
          "Failure parsing VideoSelectorColorSpaceUsage from value: '" <> e
            <> "'. Accepted values: fallback, force"

instance ToText VideoSelectorColorSpaceUsage where
  toText = \case
    Fallback -> "FALLBACK"
    Force -> "FORCE"

instance Hashable VideoSelectorColorSpaceUsage

instance NFData VideoSelectorColorSpaceUsage

instance ToByteString VideoSelectorColorSpaceUsage

instance ToQuery VideoSelectorColorSpaceUsage

instance ToHeader VideoSelectorColorSpaceUsage

instance ToJSON VideoSelectorColorSpaceUsage where
  toJSON = toJSONText

instance FromJSON VideoSelectorColorSpaceUsage where
  parseJSON = parseJSONText "VideoSelectorColorSpaceUsage"
