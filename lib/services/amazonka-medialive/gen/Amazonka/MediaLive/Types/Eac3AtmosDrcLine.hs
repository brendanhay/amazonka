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
-- Module      : Amazonka.MediaLive.Types.Eac3AtmosDrcLine
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Eac3AtmosDrcLine
  ( Eac3AtmosDrcLine
      ( ..,
        Eac3AtmosDrcLine_FILM_LIGHT,
        Eac3AtmosDrcLine_FILM_STANDARD,
        Eac3AtmosDrcLine_MUSIC_LIGHT,
        Eac3AtmosDrcLine_MUSIC_STANDARD,
        Eac3AtmosDrcLine_NONE,
        Eac3AtmosDrcLine_SPEECH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Eac3 Atmos Drc Line
newtype Eac3AtmosDrcLine = Eac3AtmosDrcLine'
  { fromEac3AtmosDrcLine ::
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

pattern Eac3AtmosDrcLine_FILM_LIGHT :: Eac3AtmosDrcLine
pattern Eac3AtmosDrcLine_FILM_LIGHT = Eac3AtmosDrcLine' "FILM_LIGHT"

pattern Eac3AtmosDrcLine_FILM_STANDARD :: Eac3AtmosDrcLine
pattern Eac3AtmosDrcLine_FILM_STANDARD = Eac3AtmosDrcLine' "FILM_STANDARD"

pattern Eac3AtmosDrcLine_MUSIC_LIGHT :: Eac3AtmosDrcLine
pattern Eac3AtmosDrcLine_MUSIC_LIGHT = Eac3AtmosDrcLine' "MUSIC_LIGHT"

pattern Eac3AtmosDrcLine_MUSIC_STANDARD :: Eac3AtmosDrcLine
pattern Eac3AtmosDrcLine_MUSIC_STANDARD = Eac3AtmosDrcLine' "MUSIC_STANDARD"

pattern Eac3AtmosDrcLine_NONE :: Eac3AtmosDrcLine
pattern Eac3AtmosDrcLine_NONE = Eac3AtmosDrcLine' "NONE"

pattern Eac3AtmosDrcLine_SPEECH :: Eac3AtmosDrcLine
pattern Eac3AtmosDrcLine_SPEECH = Eac3AtmosDrcLine' "SPEECH"

{-# COMPLETE
  Eac3AtmosDrcLine_FILM_LIGHT,
  Eac3AtmosDrcLine_FILM_STANDARD,
  Eac3AtmosDrcLine_MUSIC_LIGHT,
  Eac3AtmosDrcLine_MUSIC_STANDARD,
  Eac3AtmosDrcLine_NONE,
  Eac3AtmosDrcLine_SPEECH,
  Eac3AtmosDrcLine'
  #-}
