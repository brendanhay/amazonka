{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionRf
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionRf
  ( Eac3DynamicRangeCompressionRf
      ( Eac3DynamicRangeCompressionRf',
        Eac3DynamicRangeCompressionRfNone,
        Eac3DynamicRangeCompressionRfFilmStandard,
        Eac3DynamicRangeCompressionRfFilmLight,
        Eac3DynamicRangeCompressionRfMusicStandard,
        Eac3DynamicRangeCompressionRfMusicLight,
        Eac3DynamicRangeCompressionRfSpeech,
        fromEac3DynamicRangeCompressionRf
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specify how the service limits the audio dynamic range when compressing the audio.
newtype Eac3DynamicRangeCompressionRf = Eac3DynamicRangeCompressionRf'
  { fromEac3DynamicRangeCompressionRf ::
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

pattern Eac3DynamicRangeCompressionRfNone :: Eac3DynamicRangeCompressionRf
pattern Eac3DynamicRangeCompressionRfNone = Eac3DynamicRangeCompressionRf' "NONE"

pattern Eac3DynamicRangeCompressionRfFilmStandard :: Eac3DynamicRangeCompressionRf
pattern Eac3DynamicRangeCompressionRfFilmStandard = Eac3DynamicRangeCompressionRf' "FILM_STANDARD"

pattern Eac3DynamicRangeCompressionRfFilmLight :: Eac3DynamicRangeCompressionRf
pattern Eac3DynamicRangeCompressionRfFilmLight = Eac3DynamicRangeCompressionRf' "FILM_LIGHT"

pattern Eac3DynamicRangeCompressionRfMusicStandard :: Eac3DynamicRangeCompressionRf
pattern Eac3DynamicRangeCompressionRfMusicStandard = Eac3DynamicRangeCompressionRf' "MUSIC_STANDARD"

pattern Eac3DynamicRangeCompressionRfMusicLight :: Eac3DynamicRangeCompressionRf
pattern Eac3DynamicRangeCompressionRfMusicLight = Eac3DynamicRangeCompressionRf' "MUSIC_LIGHT"

pattern Eac3DynamicRangeCompressionRfSpeech :: Eac3DynamicRangeCompressionRf
pattern Eac3DynamicRangeCompressionRfSpeech = Eac3DynamicRangeCompressionRf' "SPEECH"

{-# COMPLETE
  Eac3DynamicRangeCompressionRfNone,
  Eac3DynamicRangeCompressionRfFilmStandard,
  Eac3DynamicRangeCompressionRfFilmLight,
  Eac3DynamicRangeCompressionRfMusicStandard,
  Eac3DynamicRangeCompressionRfMusicLight,
  Eac3DynamicRangeCompressionRfSpeech,
  Eac3DynamicRangeCompressionRf'
  #-}
