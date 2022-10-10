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
-- Module      : Amazonka.MediaLive.Types.Eac3AtmosDrcRf
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Eac3AtmosDrcRf
  ( Eac3AtmosDrcRf
      ( ..,
        Eac3AtmosDrcRf_FILM_LIGHT,
        Eac3AtmosDrcRf_FILM_STANDARD,
        Eac3AtmosDrcRf_MUSIC_LIGHT,
        Eac3AtmosDrcRf_MUSIC_STANDARD,
        Eac3AtmosDrcRf_NONE,
        Eac3AtmosDrcRf_SPEECH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Eac3 Atmos Drc Rf
newtype Eac3AtmosDrcRf = Eac3AtmosDrcRf'
  { fromEac3AtmosDrcRf ::
      Core.Text
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

pattern Eac3AtmosDrcRf_FILM_LIGHT :: Eac3AtmosDrcRf
pattern Eac3AtmosDrcRf_FILM_LIGHT = Eac3AtmosDrcRf' "FILM_LIGHT"

pattern Eac3AtmosDrcRf_FILM_STANDARD :: Eac3AtmosDrcRf
pattern Eac3AtmosDrcRf_FILM_STANDARD = Eac3AtmosDrcRf' "FILM_STANDARD"

pattern Eac3AtmosDrcRf_MUSIC_LIGHT :: Eac3AtmosDrcRf
pattern Eac3AtmosDrcRf_MUSIC_LIGHT = Eac3AtmosDrcRf' "MUSIC_LIGHT"

pattern Eac3AtmosDrcRf_MUSIC_STANDARD :: Eac3AtmosDrcRf
pattern Eac3AtmosDrcRf_MUSIC_STANDARD = Eac3AtmosDrcRf' "MUSIC_STANDARD"

pattern Eac3AtmosDrcRf_NONE :: Eac3AtmosDrcRf
pattern Eac3AtmosDrcRf_NONE = Eac3AtmosDrcRf' "NONE"

pattern Eac3AtmosDrcRf_SPEECH :: Eac3AtmosDrcRf
pattern Eac3AtmosDrcRf_SPEECH = Eac3AtmosDrcRf' "SPEECH"

{-# COMPLETE
  Eac3AtmosDrcRf_FILM_LIGHT,
  Eac3AtmosDrcRf_FILM_STANDARD,
  Eac3AtmosDrcRf_MUSIC_LIGHT,
  Eac3AtmosDrcRf_MUSIC_STANDARD,
  Eac3AtmosDrcRf_NONE,
  Eac3AtmosDrcRf_SPEECH,
  Eac3AtmosDrcRf'
  #-}
