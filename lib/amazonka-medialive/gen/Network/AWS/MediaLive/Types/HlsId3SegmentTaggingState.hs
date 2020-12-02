{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsId3SegmentTaggingState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsId3SegmentTaggingState where

import Network.AWS.Prelude

-- | State of HLS ID3 Segment Tagging
data HlsId3SegmentTaggingState
  = HISTSDisabled
  | HISTSEnabled
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

instance FromText HlsId3SegmentTaggingState where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure HISTSDisabled
      "enabled" -> pure HISTSEnabled
      e ->
        fromTextError $
          "Failure parsing HlsId3SegmentTaggingState from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText HlsId3SegmentTaggingState where
  toText = \case
    HISTSDisabled -> "DISABLED"
    HISTSEnabled -> "ENABLED"

instance Hashable HlsId3SegmentTaggingState

instance NFData HlsId3SegmentTaggingState

instance ToByteString HlsId3SegmentTaggingState

instance ToQuery HlsId3SegmentTaggingState

instance ToHeader HlsId3SegmentTaggingState

instance ToJSON HlsId3SegmentTaggingState where
  toJSON = toJSONText

instance FromJSON HlsId3SegmentTaggingState where
  parseJSON = parseJSONText "HlsId3SegmentTaggingState"
