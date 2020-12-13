{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionLine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionLine
  ( Eac3AtmosDynamicRangeCompressionLine
      ( Eac3AtmosDynamicRangeCompressionLine',
        EADRCLNone,
        EADRCLFilmStandard,
        EADRCLFilmLight,
        EADRCLMusicStandard,
        EADRCLMusicLight,
        EADRCLSpeech
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify the absolute peak level for a signal with dynamic range compression.
newtype Eac3AtmosDynamicRangeCompressionLine = Eac3AtmosDynamicRangeCompressionLine' Lude.Text
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

pattern EADRCLNone :: Eac3AtmosDynamicRangeCompressionLine
pattern EADRCLNone = Eac3AtmosDynamicRangeCompressionLine' "NONE"

pattern EADRCLFilmStandard :: Eac3AtmosDynamicRangeCompressionLine
pattern EADRCLFilmStandard = Eac3AtmosDynamicRangeCompressionLine' "FILM_STANDARD"

pattern EADRCLFilmLight :: Eac3AtmosDynamicRangeCompressionLine
pattern EADRCLFilmLight = Eac3AtmosDynamicRangeCompressionLine' "FILM_LIGHT"

pattern EADRCLMusicStandard :: Eac3AtmosDynamicRangeCompressionLine
pattern EADRCLMusicStandard = Eac3AtmosDynamicRangeCompressionLine' "MUSIC_STANDARD"

pattern EADRCLMusicLight :: Eac3AtmosDynamicRangeCompressionLine
pattern EADRCLMusicLight = Eac3AtmosDynamicRangeCompressionLine' "MUSIC_LIGHT"

pattern EADRCLSpeech :: Eac3AtmosDynamicRangeCompressionLine
pattern EADRCLSpeech = Eac3AtmosDynamicRangeCompressionLine' "SPEECH"

{-# COMPLETE
  EADRCLNone,
  EADRCLFilmStandard,
  EADRCLFilmLight,
  EADRCLMusicStandard,
  EADRCLMusicLight,
  EADRCLSpeech,
  Eac3AtmosDynamicRangeCompressionLine'
  #-}
