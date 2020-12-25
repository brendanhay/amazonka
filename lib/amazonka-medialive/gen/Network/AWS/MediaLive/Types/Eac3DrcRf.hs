{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        Eac3DrcRfFilmLight,
        Eac3DrcRfFilmStandard,
        Eac3DrcRfMusicLight,
        Eac3DrcRfMusicStandard,
        Eac3DrcRfNone,
        Eac3DrcRfSpeech,
        fromEac3DrcRf
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Eac3 Drc Rf
newtype Eac3DrcRf = Eac3DrcRf' {fromEac3DrcRf :: Core.Text}
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

pattern Eac3DrcRfFilmLight :: Eac3DrcRf
pattern Eac3DrcRfFilmLight = Eac3DrcRf' "FILM_LIGHT"

pattern Eac3DrcRfFilmStandard :: Eac3DrcRf
pattern Eac3DrcRfFilmStandard = Eac3DrcRf' "FILM_STANDARD"

pattern Eac3DrcRfMusicLight :: Eac3DrcRf
pattern Eac3DrcRfMusicLight = Eac3DrcRf' "MUSIC_LIGHT"

pattern Eac3DrcRfMusicStandard :: Eac3DrcRf
pattern Eac3DrcRfMusicStandard = Eac3DrcRf' "MUSIC_STANDARD"

pattern Eac3DrcRfNone :: Eac3DrcRf
pattern Eac3DrcRfNone = Eac3DrcRf' "NONE"

pattern Eac3DrcRfSpeech :: Eac3DrcRf
pattern Eac3DrcRfSpeech = Eac3DrcRf' "SPEECH"

{-# COMPLETE
  Eac3DrcRfFilmLight,
  Eac3DrcRfFilmStandard,
  Eac3DrcRfMusicLight,
  Eac3DrcRfMusicStandard,
  Eac3DrcRfNone,
  Eac3DrcRfSpeech,
  Eac3DrcRf'
  #-}
