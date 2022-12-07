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
-- Module      : Amazonka.MediaConvert.Types.Eac3DynamicRangeCompressionRf
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Eac3DynamicRangeCompressionRf
  ( Eac3DynamicRangeCompressionRf
      ( ..,
        Eac3DynamicRangeCompressionRf_FILM_LIGHT,
        Eac3DynamicRangeCompressionRf_FILM_STANDARD,
        Eac3DynamicRangeCompressionRf_MUSIC_LIGHT,
        Eac3DynamicRangeCompressionRf_MUSIC_STANDARD,
        Eac3DynamicRangeCompressionRf_NONE,
        Eac3DynamicRangeCompressionRf_SPEECH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Choose the Dolby Digital dynamic range control (DRC) profile that
-- MediaConvert uses when encoding the metadata in the Dolby Digital stream
-- for the RF operating mode. Related setting: When you use this setting,
-- MediaConvert ignores any value you provide for Dynamic range compression
-- profile (DynamicRangeCompressionProfile). For information about the
-- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
-- Control chapter of the Dolby Metadata Guide at
-- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
newtype Eac3DynamicRangeCompressionRf = Eac3DynamicRangeCompressionRf'
  { fromEac3DynamicRangeCompressionRf ::
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

pattern Eac3DynamicRangeCompressionRf_FILM_LIGHT :: Eac3DynamicRangeCompressionRf
pattern Eac3DynamicRangeCompressionRf_FILM_LIGHT = Eac3DynamicRangeCompressionRf' "FILM_LIGHT"

pattern Eac3DynamicRangeCompressionRf_FILM_STANDARD :: Eac3DynamicRangeCompressionRf
pattern Eac3DynamicRangeCompressionRf_FILM_STANDARD = Eac3DynamicRangeCompressionRf' "FILM_STANDARD"

pattern Eac3DynamicRangeCompressionRf_MUSIC_LIGHT :: Eac3DynamicRangeCompressionRf
pattern Eac3DynamicRangeCompressionRf_MUSIC_LIGHT = Eac3DynamicRangeCompressionRf' "MUSIC_LIGHT"

pattern Eac3DynamicRangeCompressionRf_MUSIC_STANDARD :: Eac3DynamicRangeCompressionRf
pattern Eac3DynamicRangeCompressionRf_MUSIC_STANDARD = Eac3DynamicRangeCompressionRf' "MUSIC_STANDARD"

pattern Eac3DynamicRangeCompressionRf_NONE :: Eac3DynamicRangeCompressionRf
pattern Eac3DynamicRangeCompressionRf_NONE = Eac3DynamicRangeCompressionRf' "NONE"

pattern Eac3DynamicRangeCompressionRf_SPEECH :: Eac3DynamicRangeCompressionRf
pattern Eac3DynamicRangeCompressionRf_SPEECH = Eac3DynamicRangeCompressionRf' "SPEECH"

{-# COMPLETE
  Eac3DynamicRangeCompressionRf_FILM_LIGHT,
  Eac3DynamicRangeCompressionRf_FILM_STANDARD,
  Eac3DynamicRangeCompressionRf_MUSIC_LIGHT,
  Eac3DynamicRangeCompressionRf_MUSIC_STANDARD,
  Eac3DynamicRangeCompressionRf_NONE,
  Eac3DynamicRangeCompressionRf_SPEECH,
  Eac3DynamicRangeCompressionRf'
  #-}
