{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionLine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionLine
  ( Eac3DynamicRangeCompressionLine
      ( Eac3DynamicRangeCompressionLine',
        Eac3DynamicRangeCompressionLineNone,
        Eac3DynamicRangeCompressionLineFilmStandard,
        Eac3DynamicRangeCompressionLineFilmLight,
        Eac3DynamicRangeCompressionLineMusicStandard,
        Eac3DynamicRangeCompressionLineMusicLight,
        Eac3DynamicRangeCompressionLineSpeech,
        fromEac3DynamicRangeCompressionLine
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specify the absolute peak level for a signal with dynamic range compression.
newtype Eac3DynamicRangeCompressionLine = Eac3DynamicRangeCompressionLine'
  { fromEac3DynamicRangeCompressionLine ::
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

pattern Eac3DynamicRangeCompressionLineNone :: Eac3DynamicRangeCompressionLine
pattern Eac3DynamicRangeCompressionLineNone = Eac3DynamicRangeCompressionLine' "NONE"

pattern Eac3DynamicRangeCompressionLineFilmStandard :: Eac3DynamicRangeCompressionLine
pattern Eac3DynamicRangeCompressionLineFilmStandard = Eac3DynamicRangeCompressionLine' "FILM_STANDARD"

pattern Eac3DynamicRangeCompressionLineFilmLight :: Eac3DynamicRangeCompressionLine
pattern Eac3DynamicRangeCompressionLineFilmLight = Eac3DynamicRangeCompressionLine' "FILM_LIGHT"

pattern Eac3DynamicRangeCompressionLineMusicStandard :: Eac3DynamicRangeCompressionLine
pattern Eac3DynamicRangeCompressionLineMusicStandard = Eac3DynamicRangeCompressionLine' "MUSIC_STANDARD"

pattern Eac3DynamicRangeCompressionLineMusicLight :: Eac3DynamicRangeCompressionLine
pattern Eac3DynamicRangeCompressionLineMusicLight = Eac3DynamicRangeCompressionLine' "MUSIC_LIGHT"

pattern Eac3DynamicRangeCompressionLineSpeech :: Eac3DynamicRangeCompressionLine
pattern Eac3DynamicRangeCompressionLineSpeech = Eac3DynamicRangeCompressionLine' "SPEECH"

{-# COMPLETE
  Eac3DynamicRangeCompressionLineNone,
  Eac3DynamicRangeCompressionLineFilmStandard,
  Eac3DynamicRangeCompressionLineFilmLight,
  Eac3DynamicRangeCompressionLineMusicStandard,
  Eac3DynamicRangeCompressionLineMusicLight,
  Eac3DynamicRangeCompressionLineSpeech,
  Eac3DynamicRangeCompressionLine'
  #-}
