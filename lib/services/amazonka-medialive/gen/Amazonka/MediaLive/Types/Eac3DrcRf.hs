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
-- Module      : Amazonka.MediaLive.Types.Eac3DrcRf
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Eac3DrcRf
  ( Eac3DrcRf
      ( ..,
        Eac3DrcRf_FILM_LIGHT,
        Eac3DrcRf_FILM_STANDARD,
        Eac3DrcRf_MUSIC_LIGHT,
        Eac3DrcRf_MUSIC_STANDARD,
        Eac3DrcRf_NONE,
        Eac3DrcRf_SPEECH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Eac3 Drc Rf
newtype Eac3DrcRf = Eac3DrcRf'
  { fromEac3DrcRf ::
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

pattern Eac3DrcRf_FILM_LIGHT :: Eac3DrcRf
pattern Eac3DrcRf_FILM_LIGHT = Eac3DrcRf' "FILM_LIGHT"

pattern Eac3DrcRf_FILM_STANDARD :: Eac3DrcRf
pattern Eac3DrcRf_FILM_STANDARD = Eac3DrcRf' "FILM_STANDARD"

pattern Eac3DrcRf_MUSIC_LIGHT :: Eac3DrcRf
pattern Eac3DrcRf_MUSIC_LIGHT = Eac3DrcRf' "MUSIC_LIGHT"

pattern Eac3DrcRf_MUSIC_STANDARD :: Eac3DrcRf
pattern Eac3DrcRf_MUSIC_STANDARD = Eac3DrcRf' "MUSIC_STANDARD"

pattern Eac3DrcRf_NONE :: Eac3DrcRf
pattern Eac3DrcRf_NONE = Eac3DrcRf' "NONE"

pattern Eac3DrcRf_SPEECH :: Eac3DrcRf
pattern Eac3DrcRf_SPEECH = Eac3DrcRf' "SPEECH"

{-# COMPLETE
  Eac3DrcRf_FILM_LIGHT,
  Eac3DrcRf_FILM_STANDARD,
  Eac3DrcRf_MUSIC_LIGHT,
  Eac3DrcRf_MUSIC_STANDARD,
  Eac3DrcRf_NONE,
  Eac3DrcRf_SPEECH,
  Eac3DrcRf'
  #-}
