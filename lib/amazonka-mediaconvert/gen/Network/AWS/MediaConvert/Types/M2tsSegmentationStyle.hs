{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsSegmentationStyle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsSegmentationStyle
  ( M2tsSegmentationStyle
      ( M2tsSegmentationStyle',
        M2tsSegmentationStyleMaintainCadence,
        M2tsSegmentationStyleResetCadence,
        fromM2tsSegmentationStyle
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The segmentation style parameter controls how segmentation markers are inserted into the transport stream. With avails, it is possible that segments may be truncated, which can influence where future segmentation markers are inserted. When a segmentation style of "reset_cadence" is selected and a segment is truncated due to an avail, we will reset the segmentation cadence. This means the subsequent segment will have a duration of of $segmentation_time seconds. When a segmentation style of "maintain_cadence" is selected and a segment is truncated due to an avail, we will not reset the segmentation cadence. This means the subsequent segment will likely be truncated as well. However, all segments after that will have a duration of $segmentation_time seconds. Note that EBP lookahead is a slight exception to this rule.
newtype M2tsSegmentationStyle = M2tsSegmentationStyle'
  { fromM2tsSegmentationStyle ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern M2tsSegmentationStyleMaintainCadence :: M2tsSegmentationStyle
pattern M2tsSegmentationStyleMaintainCadence = M2tsSegmentationStyle' "MAINTAIN_CADENCE"

pattern M2tsSegmentationStyleResetCadence :: M2tsSegmentationStyle
pattern M2tsSegmentationStyleResetCadence = M2tsSegmentationStyle' "RESET_CADENCE"

{-# COMPLETE
  M2tsSegmentationStyleMaintainCadence,
  M2tsSegmentationStyleResetCadence,
  M2tsSegmentationStyle'
  #-}
