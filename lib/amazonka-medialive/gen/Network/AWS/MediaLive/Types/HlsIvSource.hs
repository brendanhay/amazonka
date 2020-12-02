{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsIvSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsIvSource where

import Network.AWS.Prelude

-- | Hls Iv Source
data HlsIvSource
  = Explicit
  | FollowsSegmentNumber
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

instance FromText HlsIvSource where
  parser =
    takeLowerText >>= \case
      "explicit" -> pure Explicit
      "follows_segment_number" -> pure FollowsSegmentNumber
      e ->
        fromTextError $
          "Failure parsing HlsIvSource from value: '" <> e
            <> "'. Accepted values: explicit, follows_segment_number"

instance ToText HlsIvSource where
  toText = \case
    Explicit -> "EXPLICIT"
    FollowsSegmentNumber -> "FOLLOWS_SEGMENT_NUMBER"

instance Hashable HlsIvSource

instance NFData HlsIvSource

instance ToByteString HlsIvSource

instance ToQuery HlsIvSource

instance ToHeader HlsIvSource

instance ToJSON HlsIvSource where
  toJSON = toJSONText

instance FromJSON HlsIvSource where
  parseJSON = parseJSONText "HlsIvSource"
