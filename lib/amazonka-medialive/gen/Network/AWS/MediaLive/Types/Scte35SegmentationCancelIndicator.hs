{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35SegmentationCancelIndicator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35SegmentationCancelIndicator where

import Network.AWS.Prelude

-- | Corresponds to SCTE-35 segmentation_event_cancel_indicator. SEGMENTATION_EVENT_NOT_CANCELED corresponds to 0 in the SCTE-35 specification and indicates that this is an insertion request. SEGMENTATION_EVENT_CANCELED corresponds to 1 in the SCTE-35 specification and indicates that this is a cancelation request, in which case complete this field and the existing event ID to cancel.
data Scte35SegmentationCancelIndicator
  = SegmentationEventCanceled
  | SegmentationEventNotCanceled
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

instance FromText Scte35SegmentationCancelIndicator where
  parser =
    takeLowerText >>= \case
      "segmentation_event_canceled" -> pure SegmentationEventCanceled
      "segmentation_event_not_canceled" -> pure SegmentationEventNotCanceled
      e ->
        fromTextError $
          "Failure parsing Scte35SegmentationCancelIndicator from value: '" <> e
            <> "'. Accepted values: segmentation_event_canceled, segmentation_event_not_canceled"

instance ToText Scte35SegmentationCancelIndicator where
  toText = \case
    SegmentationEventCanceled -> "SEGMENTATION_EVENT_CANCELED"
    SegmentationEventNotCanceled -> "SEGMENTATION_EVENT_NOT_CANCELED"

instance Hashable Scte35SegmentationCancelIndicator

instance NFData Scte35SegmentationCancelIndicator

instance ToByteString Scte35SegmentationCancelIndicator

instance ToQuery Scte35SegmentationCancelIndicator

instance ToHeader Scte35SegmentationCancelIndicator

instance ToJSON Scte35SegmentationCancelIndicator where
  toJSON = toJSONText

instance FromJSON Scte35SegmentationCancelIndicator where
  parseJSON = parseJSONText "Scte35SegmentationCancelIndicator"
