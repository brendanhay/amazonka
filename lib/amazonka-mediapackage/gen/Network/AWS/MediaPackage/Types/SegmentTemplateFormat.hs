-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.SegmentTemplateFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.SegmentTemplateFormat
  ( SegmentTemplateFormat
      ( SegmentTemplateFormat',
        NumberWithDuration,
        NumberWithTimeline,
        TimeWithTimeline
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SegmentTemplateFormat = SegmentTemplateFormat' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern NumberWithDuration :: SegmentTemplateFormat
pattern NumberWithDuration = SegmentTemplateFormat' "NUMBER_WITH_DURATION"

pattern NumberWithTimeline :: SegmentTemplateFormat
pattern NumberWithTimeline = SegmentTemplateFormat' "NUMBER_WITH_TIMELINE"

pattern TimeWithTimeline :: SegmentTemplateFormat
pattern TimeWithTimeline = SegmentTemplateFormat' "TIME_WITH_TIMELINE"

{-# COMPLETE
  NumberWithDuration,
  NumberWithTimeline,
  TimeWithTimeline,
  SegmentTemplateFormat'
  #-}
