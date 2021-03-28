{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsAudioOnlyHeader
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.HlsAudioOnlyHeader
  ( HlsAudioOnlyHeader
    ( HlsAudioOnlyHeader'
    , HlsAudioOnlyHeaderInclude
    , HlsAudioOnlyHeaderExclude
    , fromHlsAudioOnlyHeader
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Ignore this setting unless you are using FairPlay DRM with Verimatrix and you encounter playback issues. Keep the default value, Include (INCLUDE), to output audio-only headers. Choose Exclude (EXCLUDE) to remove the audio-only headers from your audio segments.
newtype HlsAudioOnlyHeader = HlsAudioOnlyHeader'{fromHlsAudioOnlyHeader
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern HlsAudioOnlyHeaderInclude :: HlsAudioOnlyHeader
pattern HlsAudioOnlyHeaderInclude = HlsAudioOnlyHeader' "INCLUDE"

pattern HlsAudioOnlyHeaderExclude :: HlsAudioOnlyHeader
pattern HlsAudioOnlyHeaderExclude = HlsAudioOnlyHeader' "EXCLUDE"

{-# COMPLETE 
  HlsAudioOnlyHeaderInclude,

  HlsAudioOnlyHeaderExclude,
  HlsAudioOnlyHeader'
  #-}
