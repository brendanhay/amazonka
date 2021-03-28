{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3DrcLine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Eac3DrcLine
  ( Eac3DrcLine
    ( Eac3DrcLine'
    , Eac3DrcLineFilmLight
    , Eac3DrcLineFilmStandard
    , Eac3DrcLineMusicLight
    , Eac3DrcLineMusicStandard
    , Eac3DrcLineNone
    , Eac3DrcLineSpeech
    , fromEac3DrcLine
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Eac3 Drc Line
newtype Eac3DrcLine = Eac3DrcLine'{fromEac3DrcLine :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern Eac3DrcLineFilmLight :: Eac3DrcLine
pattern Eac3DrcLineFilmLight = Eac3DrcLine' "FILM_LIGHT"

pattern Eac3DrcLineFilmStandard :: Eac3DrcLine
pattern Eac3DrcLineFilmStandard = Eac3DrcLine' "FILM_STANDARD"

pattern Eac3DrcLineMusicLight :: Eac3DrcLine
pattern Eac3DrcLineMusicLight = Eac3DrcLine' "MUSIC_LIGHT"

pattern Eac3DrcLineMusicStandard :: Eac3DrcLine
pattern Eac3DrcLineMusicStandard = Eac3DrcLine' "MUSIC_STANDARD"

pattern Eac3DrcLineNone :: Eac3DrcLine
pattern Eac3DrcLineNone = Eac3DrcLine' "NONE"

pattern Eac3DrcLineSpeech :: Eac3DrcLine
pattern Eac3DrcLineSpeech = Eac3DrcLine' "SPEECH"

{-# COMPLETE 
  Eac3DrcLineFilmLight,

  Eac3DrcLineFilmStandard,

  Eac3DrcLineMusicLight,

  Eac3DrcLineMusicStandard,

  Eac3DrcLineNone,

  Eac3DrcLineSpeech,
  Eac3DrcLine'
  #-}
