{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioOnlyHlsSegmentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AudioOnlyHlsSegmentType
  ( AudioOnlyHlsSegmentType
    ( AudioOnlyHlsSegmentType'
    , AudioOnlyHlsSegmentTypeAac
    , AudioOnlyHlsSegmentTypeFMP4
    , fromAudioOnlyHlsSegmentType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Audio Only Hls Segment Type
newtype AudioOnlyHlsSegmentType = AudioOnlyHlsSegmentType'{fromAudioOnlyHlsSegmentType
                                                           :: Core.Text}
                                    deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                    Core.Generic)
                                    deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                      Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                      Core.FromJSON, Core.ToXML, Core.FromXML,
                                                      Core.ToText, Core.FromText, Core.ToByteString,
                                                      Core.ToQuery, Core.ToHeader)

pattern AudioOnlyHlsSegmentTypeAac :: AudioOnlyHlsSegmentType
pattern AudioOnlyHlsSegmentTypeAac = AudioOnlyHlsSegmentType' "AAC"

pattern AudioOnlyHlsSegmentTypeFMP4 :: AudioOnlyHlsSegmentType
pattern AudioOnlyHlsSegmentTypeFMP4 = AudioOnlyHlsSegmentType' "FMP4"

{-# COMPLETE 
  AudioOnlyHlsSegmentTypeAac,

  AudioOnlyHlsSegmentTypeFMP4,
  AudioOnlyHlsSegmentType'
  #-}
