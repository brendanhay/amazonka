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
-- Module      : Amazonka.MediaLive.Types.Scte35SegmentationCancelIndicator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Scte35SegmentationCancelIndicator
  ( Scte35SegmentationCancelIndicator
      ( ..,
        Scte35SegmentationCancelIndicator_SEGMENTATION_EVENT_CANCELED,
        Scte35SegmentationCancelIndicator_SEGMENTATION_EVENT_NOT_CANCELED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Corresponds to SCTE-35 segmentation_event_cancel_indicator.
-- SEGMENTATION_EVENT_NOT_CANCELED corresponds to 0 in the SCTE-35
-- specification and indicates that this is an insertion request.
-- SEGMENTATION_EVENT_CANCELED corresponds to 1 in the SCTE-35
-- specification and indicates that this is a cancelation request, in which
-- case complete this field and the existing event ID to cancel.
newtype Scte35SegmentationCancelIndicator = Scte35SegmentationCancelIndicator'
  { fromScte35SegmentationCancelIndicator ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
