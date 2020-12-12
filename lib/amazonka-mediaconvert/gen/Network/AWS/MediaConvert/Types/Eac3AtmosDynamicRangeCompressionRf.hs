{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionRf
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionRf
  ( Eac3AtmosDynamicRangeCompressionRf
      ( Eac3AtmosDynamicRangeCompressionRf',
        EADRCRFilmLight,
        EADRCRFilmStandard,
        EADRCRMusicLight,
        EADRCRMusicStandard,
        EADRCRNone,
        EADRCRSpeech
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify how the service limits the audio dynamic range when compressing the audio.
newtype Eac3AtmosDynamicRangeCompressionRf = Eac3AtmosDynamicRangeCompressionRf' Lude.Text
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

pattern EADRCRFilmLight :: Eac3AtmosDynamicRangeCompressionRf
pattern EADRCRFilmLight = Eac3AtmosDynamicRangeCompressionRf' "FILM_LIGHT"

pattern EADRCRFilmStandard :: Eac3AtmosDynamicRangeCompressionRf
pattern EADRCRFilmStandard = Eac3AtmosDynamicRangeCompressionRf' "FILM_STANDARD"

pattern EADRCRMusicLight :: Eac3AtmosDynamicRangeCompressionRf
pattern EADRCRMusicLight = Eac3AtmosDynamicRangeCompressionRf' "MUSIC_LIGHT"

pattern EADRCRMusicStandard :: Eac3AtmosDynamicRangeCompressionRf
pattern EADRCRMusicStandard = Eac3AtmosDynamicRangeCompressionRf' "MUSIC_STANDARD"

pattern EADRCRNone :: Eac3AtmosDynamicRangeCompressionRf
pattern EADRCRNone = Eac3AtmosDynamicRangeCompressionRf' "NONE"

pattern EADRCRSpeech :: Eac3AtmosDynamicRangeCompressionRf
pattern EADRCRSpeech = Eac3AtmosDynamicRangeCompressionRf' "SPEECH"

{-# COMPLETE
  EADRCRFilmLight,
  EADRCRFilmStandard,
  EADRCRMusicLight,
  EADRCRMusicStandard,
  EADRCRNone,
  EADRCRSpeech,
  Eac3AtmosDynamicRangeCompressionRf'
  #-}
