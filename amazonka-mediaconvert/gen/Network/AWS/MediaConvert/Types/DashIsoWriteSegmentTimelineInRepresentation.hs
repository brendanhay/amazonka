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
-- Module      : Network.AWS.MediaConvert.Types.DashIsoWriteSegmentTimelineInRepresentation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DashIsoWriteSegmentTimelineInRepresentation
  ( DashIsoWriteSegmentTimelineInRepresentation
      ( ..,
        DashIsoWriteSegmentTimelineInRepresentation_DISABLED,
        DashIsoWriteSegmentTimelineInRepresentation_ENABLED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | When you enable Precise segment duration in manifests
-- (writeSegmentTimelineInRepresentation), your DASH manifest shows precise
-- segment durations. The segment duration information appears inside the
-- SegmentTimeline element, inside SegmentTemplate at the Representation
-- level. When this feature isn\'t enabled, the segment durations in your
-- DASH manifest are approximate. The segment duration information appears
-- in the duration attribute of the SegmentTemplate element.
newtype DashIsoWriteSegmentTimelineInRepresentation = DashIsoWriteSegmentTimelineInRepresentation'
  { fromDashIsoWriteSegmentTimelineInRepresentation ::
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

pattern DashIsoWriteSegmentTimelineInRepresentation_DISABLED :: DashIsoWriteSegmentTimelineInRepresentation
pattern DashIsoWriteSegmentTimelineInRepresentation_DISABLED = DashIsoWriteSegmentTimelineInRepresentation' "DISABLED"

pattern DashIsoWriteSegmentTimelineInRepresentation_ENABLED :: DashIsoWriteSegmentTimelineInRepresentation
pattern DashIsoWriteSegmentTimelineInRepresentation_ENABLED = DashIsoWriteSegmentTimelineInRepresentation' "ENABLED"

{-# COMPLETE
  DashIsoWriteSegmentTimelineInRepresentation_DISABLED,
  DashIsoWriteSegmentTimelineInRepresentation_ENABLED,
  DashIsoWriteSegmentTimelineInRepresentation'
  #-}
