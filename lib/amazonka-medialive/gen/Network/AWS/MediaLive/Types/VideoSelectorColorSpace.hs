{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.VideoSelectorColorSpace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.VideoSelectorColorSpace where

import Network.AWS.Prelude

-- | Video Selector Color Space
data VideoSelectorColorSpace
  = Follow
  | Rec601
  | Rec709
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

instance FromText VideoSelectorColorSpace where
  parser =
    takeLowerText >>= \case
      "follow" -> pure Follow
      "rec_601" -> pure Rec601
      "rec_709" -> pure Rec709
      e ->
        fromTextError $
          "Failure parsing VideoSelectorColorSpace from value: '" <> e
            <> "'. Accepted values: follow, rec_601, rec_709"

instance ToText VideoSelectorColorSpace where
  toText = \case
    Follow -> "FOLLOW"
    Rec601 -> "REC_601"
    Rec709 -> "REC_709"

instance Hashable VideoSelectorColorSpace

instance NFData VideoSelectorColorSpace

instance ToByteString VideoSelectorColorSpace

instance ToQuery VideoSelectorColorSpace

instance ToHeader VideoSelectorColorSpace

instance ToJSON VideoSelectorColorSpace where
  toJSON = toJSONText

instance FromJSON VideoSelectorColorSpace where
  parseJSON = parseJSONText "VideoSelectorColorSpace"
