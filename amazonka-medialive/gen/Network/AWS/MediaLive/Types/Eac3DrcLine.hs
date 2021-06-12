{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3DrcLine
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3DrcLine
  ( Eac3DrcLine
      ( ..,
        Eac3DrcLine_FILM_LIGHT,
        Eac3DrcLine_FILM_STANDARD,
        Eac3DrcLine_MUSIC_LIGHT,
        Eac3DrcLine_MUSIC_STANDARD,
        Eac3DrcLine_NONE,
        Eac3DrcLine_SPEECH
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Eac3 Drc Line
newtype Eac3DrcLine = Eac3DrcLine'
  { fromEac3DrcLine ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern Eac3DrcLine_FILM_LIGHT :: Eac3DrcLine
pattern Eac3DrcLine_FILM_LIGHT = Eac3DrcLine' "FILM_LIGHT"

pattern Eac3DrcLine_FILM_STANDARD :: Eac3DrcLine
pattern Eac3DrcLine_FILM_STANDARD = Eac3DrcLine' "FILM_STANDARD"

pattern Eac3DrcLine_MUSIC_LIGHT :: Eac3DrcLine
pattern Eac3DrcLine_MUSIC_LIGHT = Eac3DrcLine' "MUSIC_LIGHT"

pattern Eac3DrcLine_MUSIC_STANDARD :: Eac3DrcLine
pattern Eac3DrcLine_MUSIC_STANDARD = Eac3DrcLine' "MUSIC_STANDARD"

pattern Eac3DrcLine_NONE :: Eac3DrcLine
pattern Eac3DrcLine_NONE = Eac3DrcLine' "NONE"

pattern Eac3DrcLine_SPEECH :: Eac3DrcLine
pattern Eac3DrcLine_SPEECH = Eac3DrcLine' "SPEECH"

{-# COMPLETE
  Eac3DrcLine_FILM_LIGHT,
  Eac3DrcLine_FILM_STANDARD,
  Eac3DrcLine_MUSIC_LIGHT,
  Eac3DrcLine_MUSIC_STANDARD,
  Eac3DrcLine_NONE,
  Eac3DrcLine_SPEECH,
  Eac3DrcLine'
  #-}
