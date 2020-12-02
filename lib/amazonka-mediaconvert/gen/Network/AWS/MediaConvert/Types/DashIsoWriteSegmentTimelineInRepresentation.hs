{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DashIsoWriteSegmentTimelineInRepresentation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DashIsoWriteSegmentTimelineInRepresentation where

import Network.AWS.Prelude

-- | When you enable Precise segment duration in manifests (writeSegmentTimelineInRepresentation), your DASH manifest shows precise segment durations. The segment duration information appears inside the SegmentTimeline element, inside SegmentTemplate at the Representation level. When this feature isn't enabled, the segment durations in your DASH manifest are approximate. The segment duration information appears in the duration attribute of the SegmentTemplate element.
data DashIsoWriteSegmentTimelineInRepresentation
  = DIWSTIRDisabled
  | DIWSTIREnabled
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

instance FromText DashIsoWriteSegmentTimelineInRepresentation where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure DIWSTIRDisabled
      "enabled" -> pure DIWSTIREnabled
      e ->
        fromTextError $
          "Failure parsing DashIsoWriteSegmentTimelineInRepresentation from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText DashIsoWriteSegmentTimelineInRepresentation where
  toText = \case
    DIWSTIRDisabled -> "DISABLED"
    DIWSTIREnabled -> "ENABLED"

instance Hashable DashIsoWriteSegmentTimelineInRepresentation

instance NFData DashIsoWriteSegmentTimelineInRepresentation

instance ToByteString DashIsoWriteSegmentTimelineInRepresentation

instance ToQuery DashIsoWriteSegmentTimelineInRepresentation

instance ToHeader DashIsoWriteSegmentTimelineInRepresentation

instance ToJSON DashIsoWriteSegmentTimelineInRepresentation where
  toJSON = toJSONText

instance FromJSON DashIsoWriteSegmentTimelineInRepresentation where
  parseJSON = parseJSONText "DashIsoWriteSegmentTimelineInRepresentation"
