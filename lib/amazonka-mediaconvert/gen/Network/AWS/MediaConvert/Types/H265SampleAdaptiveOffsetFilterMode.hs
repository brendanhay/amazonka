{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265SampleAdaptiveOffsetFilterMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265SampleAdaptiveOffsetFilterMode
  ( H265SampleAdaptiveOffsetFilterMode
    ( H265SampleAdaptiveOffsetFilterMode'
    , H265SampleAdaptiveOffsetFilterModeDefault
    , H265SampleAdaptiveOffsetFilterModeAdaptive
    , H265SampleAdaptiveOffsetFilterModeOff
    , fromH265SampleAdaptiveOffsetFilterMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Specify Sample Adaptive Offset (SAO) filter strength.  Adaptive mode dynamically selects best strength based on content
newtype H265SampleAdaptiveOffsetFilterMode = H265SampleAdaptiveOffsetFilterMode'{fromH265SampleAdaptiveOffsetFilterMode
                                                                                 :: Core.Text}
                                               deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                               Core.Show, Core.Generic)
                                               deriving newtype (Core.IsString, Core.Hashable,
                                                                 Core.NFData, Core.ToJSONKey,
                                                                 Core.FromJSONKey, Core.ToJSON,
                                                                 Core.FromJSON, Core.ToXML,
                                                                 Core.FromXML, Core.ToText,
                                                                 Core.FromText, Core.ToByteString,
                                                                 Core.ToQuery, Core.ToHeader)

pattern H265SampleAdaptiveOffsetFilterModeDefault :: H265SampleAdaptiveOffsetFilterMode
pattern H265SampleAdaptiveOffsetFilterModeDefault = H265SampleAdaptiveOffsetFilterMode' "DEFAULT"

pattern H265SampleAdaptiveOffsetFilterModeAdaptive :: H265SampleAdaptiveOffsetFilterMode
pattern H265SampleAdaptiveOffsetFilterModeAdaptive = H265SampleAdaptiveOffsetFilterMode' "ADAPTIVE"

pattern H265SampleAdaptiveOffsetFilterModeOff :: H265SampleAdaptiveOffsetFilterMode
pattern H265SampleAdaptiveOffsetFilterModeOff = H265SampleAdaptiveOffsetFilterMode' "OFF"

{-# COMPLETE 
  H265SampleAdaptiveOffsetFilterModeDefault,

  H265SampleAdaptiveOffsetFilterModeAdaptive,

  H265SampleAdaptiveOffsetFilterModeOff,
  H265SampleAdaptiveOffsetFilterMode'
  #-}
