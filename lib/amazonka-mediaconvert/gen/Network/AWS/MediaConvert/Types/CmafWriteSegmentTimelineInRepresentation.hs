{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafWriteSegmentTimelineInRepresentation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafWriteSegmentTimelineInRepresentation where

import Network.AWS.Prelude

-- | When you enable Precise segment duration in DASH manifests (writeSegmentTimelineInRepresentation), your DASH manifest shows precise segment durations. The segment duration information appears inside the SegmentTimeline element, inside SegmentTemplate at the Representation level. When this feature isn't enabled, the segment durations in your DASH manifest are approximate. The segment duration information appears in the duration attribute of the SegmentTemplate element.
data CmafWriteSegmentTimelineInRepresentation
  = CWSTIRDisabled
  | CWSTIREnabled
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

instance FromText CmafWriteSegmentTimelineInRepresentation where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure CWSTIRDisabled
      "enabled" -> pure CWSTIREnabled
      e ->
        fromTextError $
          "Failure parsing CmafWriteSegmentTimelineInRepresentation from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText CmafWriteSegmentTimelineInRepresentation where
  toText = \case
    CWSTIRDisabled -> "DISABLED"
    CWSTIREnabled -> "ENABLED"

instance Hashable CmafWriteSegmentTimelineInRepresentation

instance NFData CmafWriteSegmentTimelineInRepresentation

instance ToByteString CmafWriteSegmentTimelineInRepresentation

instance ToQuery CmafWriteSegmentTimelineInRepresentation

instance ToHeader CmafWriteSegmentTimelineInRepresentation

instance ToJSON CmafWriteSegmentTimelineInRepresentation where
  toJSON = toJSONText

instance FromJSON CmafWriteSegmentTimelineInRepresentation where
  parseJSON = parseJSONText "CmafWriteSegmentTimelineInRepresentation"
