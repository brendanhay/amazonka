{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265WriteMp4PackagingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.H265WriteMp4PackagingType
  ( H265WriteMp4PackagingType
    ( H265WriteMp4PackagingType'
    , H265WriteMp4PackagingTypeHVC1
    , H265WriteMp4PackagingTypeHEV1
    , fromH265WriteMp4PackagingType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | If the location of parameter set NAL units doesn't matter in your workflow, ignore this setting. Use this setting only with CMAF or DASH outputs, or with standalone file outputs in an MPEG-4 container (MP4 outputs). Choose HVC1 to mark your output as HVC1. This makes your output compliant with the following specification: ISO IECJTC1 SC29 N13798 Text ISO/IEC FDIS 14496-15 3rd Edition. For these outputs, the service stores parameter set NAL units in the sample headers but not in the samples directly. For MP4 outputs, when you choose HVC1, your output video might not work properly with some downstream systems and video players. The service defaults to marking your output as HEV1. For these outputs, the service writes parameter set NAL units directly into the samples.
newtype H265WriteMp4PackagingType = H265WriteMp4PackagingType'{fromH265WriteMp4PackagingType
                                                               :: Core.Text}
                                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                      Core.Generic)
                                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                        Core.ToJSONKey, Core.FromJSONKey,
                                                        Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                        Core.FromXML, Core.ToText, Core.FromText,
                                                        Core.ToByteString, Core.ToQuery,
                                                        Core.ToHeader)

pattern H265WriteMp4PackagingTypeHVC1 :: H265WriteMp4PackagingType
pattern H265WriteMp4PackagingTypeHVC1 = H265WriteMp4PackagingType' "HVC1"

pattern H265WriteMp4PackagingTypeHEV1 :: H265WriteMp4PackagingType
pattern H265WriteMp4PackagingTypeHEV1 = H265WriteMp4PackagingType' "HEV1"

{-# COMPLETE 
  H265WriteMp4PackagingTypeHVC1,

  H265WriteMp4PackagingTypeHEV1,
  H265WriteMp4PackagingType'
  #-}
