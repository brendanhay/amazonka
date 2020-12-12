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
        EDRCRFilmLight,
        EDRCRFilmStandard,
        EDRCRMusicLight,
        EDRCRMusicStandard,
        EDRCRNone,
        EDRCRSpeech
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify how the service limits the audio dynamic range when compressing the audio.
newtype Eac3DynamicRangeCompressionRf = Eac3DynamicRangeCompressionRf' Lude.Text
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

pattern EDRCRFilmLight :: Eac3DynamicRangeCompressionRf
pattern EDRCRFilmLight = Eac3DynamicRangeCompressionRf' "FILM_LIGHT"

pattern EDRCRFilmStandard :: Eac3DynamicRangeCompressionRf
pattern EDRCRFilmStandard = Eac3DynamicRangeCompressionRf' "FILM_STANDARD"

pattern EDRCRMusicLight :: Eac3DynamicRangeCompressionRf
pattern EDRCRMusicLight = Eac3DynamicRangeCompressionRf' "MUSIC_LIGHT"

pattern EDRCRMusicStandard :: Eac3DynamicRangeCompressionRf
pattern EDRCRMusicStandard = Eac3DynamicRangeCompressionRf' "MUSIC_STANDARD"

pattern EDRCRNone :: Eac3DynamicRangeCompressionRf
pattern EDRCRNone = Eac3DynamicRangeCompressionRf' "NONE"

pattern EDRCRSpeech :: Eac3DynamicRangeCompressionRf
pattern EDRCRSpeech = Eac3DynamicRangeCompressionRf' "SPEECH"

{-# COMPLETE
  EDRCRFilmLight,
  EDRCRFilmStandard,
  EDRCRMusicLight,
  EDRCRMusicStandard,
  EDRCRNone,
  EDRCRSpeech,
  Eac3DynamicRangeCompressionRf'
  #-}
