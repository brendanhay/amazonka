{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

-- | Corresponds to SCTE-35 segmentation_event_cancel_indicator.
-- SEGMENTATION_EVENT_NOT_CANCELED corresponds to 0 in the SCTE-35
-- specification and indicates that this is an insertion request.
-- SEGMENTATION_EVENT_CANCELED corresponds to 1 in the SCTE-35
-- specification and indicates that this is a cancelation request, in which
-- case complete this field and the existing event ID to cancel.
newtype Scte35SegmentationCancelIndicator = Scte35SegmentationCancelIndicator'
  { fromScte35SegmentationCancelIndicator ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
