-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3DrcRf
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3DrcRf
  ( Eac3DrcRf
      ( Eac3DrcRf',
        EDRFilmLight,
        EDRFilmStandard,
        EDRMusicLight,
        EDRMusicStandard,
        EDRNone,
        EDRSpeech
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Eac3 Drc Rf
newtype Eac3DrcRf = Eac3DrcRf' Lude.Text
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

pattern EDRFilmLight :: Eac3DrcRf
pattern EDRFilmLight = Eac3DrcRf' "FILM_LIGHT"

pattern EDRFilmStandard :: Eac3DrcRf
pattern EDRFilmStandard = Eac3DrcRf' "FILM_STANDARD"

pattern EDRMusicLight :: Eac3DrcRf
pattern EDRMusicLight = Eac3DrcRf' "MUSIC_LIGHT"

pattern EDRMusicStandard :: Eac3DrcRf
pattern EDRMusicStandard = Eac3DrcRf' "MUSIC_STANDARD"

pattern EDRNone :: Eac3DrcRf
pattern EDRNone = Eac3DrcRf' "NONE"

pattern EDRSpeech :: Eac3DrcRf
pattern EDRSpeech = Eac3DrcRf' "SPEECH"

{-# COMPLETE
  EDRFilmLight,
  EDRFilmStandard,
  EDRMusicLight,
  EDRMusicStandard,
  EDRNone,
  EDRSpeech,
  Eac3DrcRf'
  #-}
