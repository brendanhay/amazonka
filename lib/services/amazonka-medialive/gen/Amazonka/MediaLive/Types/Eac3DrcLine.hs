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
-- Module      : Amazonka.MediaLive.Types.Eac3DrcLine
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Eac3DrcLine
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Eac3 Drc Line
newtype Eac3DrcLine = Eac3DrcLine'
  { fromEac3DrcLine ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
