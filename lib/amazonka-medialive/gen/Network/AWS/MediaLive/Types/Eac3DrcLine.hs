-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3DrcLine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3DrcLine
  ( Eac3DrcLine
      ( Eac3DrcLine',
        EDLFilmLight,
        EDLFilmStandard,
        EDLMusicLight,
        EDLMusicStandard,
        EDLNone,
        EDLSpeech
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Eac3 Drc Line
newtype Eac3DrcLine = Eac3DrcLine' Lude.Text
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

pattern EDLFilmLight :: Eac3DrcLine
pattern EDLFilmLight = Eac3DrcLine' "FILM_LIGHT"

pattern EDLFilmStandard :: Eac3DrcLine
pattern EDLFilmStandard = Eac3DrcLine' "FILM_STANDARD"

pattern EDLMusicLight :: Eac3DrcLine
pattern EDLMusicLight = Eac3DrcLine' "MUSIC_LIGHT"

pattern EDLMusicStandard :: Eac3DrcLine
pattern EDLMusicStandard = Eac3DrcLine' "MUSIC_STANDARD"

pattern EDLNone :: Eac3DrcLine
pattern EDLNone = Eac3DrcLine' "NONE"

pattern EDLSpeech :: Eac3DrcLine
pattern EDLSpeech = Eac3DrcLine' "SPEECH"

{-# COMPLETE
  EDLFilmLight,
  EDLFilmStandard,
  EDLMusicLight,
  EDLMusicStandard,
  EDLNone,
  EDLSpeech,
  Eac3DrcLine'
  #-}
