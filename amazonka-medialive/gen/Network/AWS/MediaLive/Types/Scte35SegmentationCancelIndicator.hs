{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35SegmentationCancelIndicator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35SegmentationCancelIndicator
  ( Scte35SegmentationCancelIndicator
      ( ..,
        Scte35SegmentationCancelIndicator_SEGMENTATION_EVENT_CANCELED,
        Scte35SegmentationCancelIndicator_SEGMENTATION_EVENT_NOT_CANCELED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Corresponds to SCTE-35 segmentation_event_cancel_indicator.
-- SEGMENTATION_EVENT_NOT_CANCELED corresponds to 0 in the SCTE-35
-- specification and indicates that this is an insertion request.
-- SEGMENTATION_EVENT_CANCELED corresponds to 1 in the SCTE-35
-- specification and indicates that this is a cancelation request, in which
-- case complete this field and the existing event ID to cancel.
newtype Scte35SegmentationCancelIndicator = Scte35SegmentationCancelIndicator'
  { fromScte35SegmentationCancelIndicator ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern Scte35SegmentationCancelIndicator_SEGMENTATION_EVENT_CANCELED :: Scte35SegmentationCancelIndicator
pattern Scte35SegmentationCancelIndicator_SEGMENTATION_EVENT_CANCELED = Scte35SegmentationCancelIndicator' "SEGMENTATION_EVENT_CANCELED"

pattern Scte35SegmentationCancelIndicator_SEGMENTATION_EVENT_NOT_CANCELED :: Scte35SegmentationCancelIndicator
pattern Scte35SegmentationCancelIndicator_SEGMENTATION_EVENT_NOT_CANCELED = Scte35SegmentationCancelIndicator' "SEGMENTATION_EVENT_NOT_CANCELED"

{-# COMPLETE
  Scte35SegmentationCancelIndicator_SEGMENTATION_EVENT_CANCELED,
  Scte35SegmentationCancelIndicator_SEGMENTATION_EVENT_NOT_CANCELED,
  Scte35SegmentationCancelIndicator'
  #-}
