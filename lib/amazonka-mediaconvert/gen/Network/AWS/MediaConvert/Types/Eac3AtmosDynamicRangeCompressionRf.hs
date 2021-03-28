{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionRf
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionRf
  ( Eac3AtmosDynamicRangeCompressionRf
    ( Eac3AtmosDynamicRangeCompressionRf'
    , Eac3AtmosDynamicRangeCompressionRfNone
    , Eac3AtmosDynamicRangeCompressionRfFilmStandard
    , Eac3AtmosDynamicRangeCompressionRfFilmLight
    , Eac3AtmosDynamicRangeCompressionRfMusicStandard
    , Eac3AtmosDynamicRangeCompressionRfMusicLight
    , Eac3AtmosDynamicRangeCompressionRfSpeech
    , fromEac3AtmosDynamicRangeCompressionRf
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Specify how the service limits the audio dynamic range when compressing the audio.
newtype Eac3AtmosDynamicRangeCompressionRf = Eac3AtmosDynamicRangeCompressionRf'{fromEac3AtmosDynamicRangeCompressionRf
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

pattern Eac3AtmosDynamicRangeCompressionRfNone :: Eac3AtmosDynamicRangeCompressionRf
pattern Eac3AtmosDynamicRangeCompressionRfNone = Eac3AtmosDynamicRangeCompressionRf' "NONE"

pattern Eac3AtmosDynamicRangeCompressionRfFilmStandard :: Eac3AtmosDynamicRangeCompressionRf
pattern Eac3AtmosDynamicRangeCompressionRfFilmStandard = Eac3AtmosDynamicRangeCompressionRf' "FILM_STANDARD"

pattern Eac3AtmosDynamicRangeCompressionRfFilmLight :: Eac3AtmosDynamicRangeCompressionRf
pattern Eac3AtmosDynamicRangeCompressionRfFilmLight = Eac3AtmosDynamicRangeCompressionRf' "FILM_LIGHT"

pattern Eac3AtmosDynamicRangeCompressionRfMusicStandard :: Eac3AtmosDynamicRangeCompressionRf
pattern Eac3AtmosDynamicRangeCompressionRfMusicStandard = Eac3AtmosDynamicRangeCompressionRf' "MUSIC_STANDARD"

pattern Eac3AtmosDynamicRangeCompressionRfMusicLight :: Eac3AtmosDynamicRangeCompressionRf
pattern Eac3AtmosDynamicRangeCompressionRfMusicLight = Eac3AtmosDynamicRangeCompressionRf' "MUSIC_LIGHT"

pattern Eac3AtmosDynamicRangeCompressionRfSpeech :: Eac3AtmosDynamicRangeCompressionRf
pattern Eac3AtmosDynamicRangeCompressionRfSpeech = Eac3AtmosDynamicRangeCompressionRf' "SPEECH"

{-# COMPLETE 
  Eac3AtmosDynamicRangeCompressionRfNone,

  Eac3AtmosDynamicRangeCompressionRfFilmStandard,

  Eac3AtmosDynamicRangeCompressionRfFilmLight,

  Eac3AtmosDynamicRangeCompressionRfMusicStandard,

  Eac3AtmosDynamicRangeCompressionRfMusicLight,

  Eac3AtmosDynamicRangeCompressionRfSpeech,
  Eac3AtmosDynamicRangeCompressionRf'
  #-}
