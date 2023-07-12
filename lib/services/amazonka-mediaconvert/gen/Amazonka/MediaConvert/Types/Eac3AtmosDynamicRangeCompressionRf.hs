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
-- Module      : Amazonka.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionRf
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionRf
  ( Eac3AtmosDynamicRangeCompressionRf
      ( ..,
        Eac3AtmosDynamicRangeCompressionRf_FILM_LIGHT,
        Eac3AtmosDynamicRangeCompressionRf_FILM_STANDARD,
        Eac3AtmosDynamicRangeCompressionRf_MUSIC_LIGHT,
        Eac3AtmosDynamicRangeCompressionRf_MUSIC_STANDARD,
        Eac3AtmosDynamicRangeCompressionRf_NONE,
        Eac3AtmosDynamicRangeCompressionRf_SPEECH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Choose the Dolby dynamic range control (DRC) profile that MediaConvert
-- uses when encoding the metadata in the Dolby stream for the RF operating
-- mode. Default value: Film light (ATMOS_STORAGE_DDP_COMPR_FILM_LIGHT)
-- Related setting: To have MediaConvert use the value you specify here,
-- keep the default value, Custom (SPECIFIED) for the setting Dynamic range
-- control (DynamicRangeControl). Otherwise, MediaConvert ignores Dynamic
-- range compression RF (DynamicRangeCompressionRf). For information about
-- the Dolby DRC operating modes and profiles, see the Dynamic Range
-- Control chapter of the Dolby Metadata Guide at
-- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
newtype Eac3AtmosDynamicRangeCompressionRf = Eac3AtmosDynamicRangeCompressionRf'
  { fromEac3AtmosDynamicRangeCompressionRf ::
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

pattern Eac3AtmosDynamicRangeCompressionRf_FILM_LIGHT :: Eac3AtmosDynamicRangeCompressionRf
pattern Eac3AtmosDynamicRangeCompressionRf_FILM_LIGHT = Eac3AtmosDynamicRangeCompressionRf' "FILM_LIGHT"

pattern Eac3AtmosDynamicRangeCompressionRf_FILM_STANDARD :: Eac3AtmosDynamicRangeCompressionRf
pattern Eac3AtmosDynamicRangeCompressionRf_FILM_STANDARD = Eac3AtmosDynamicRangeCompressionRf' "FILM_STANDARD"

pattern Eac3AtmosDynamicRangeCompressionRf_MUSIC_LIGHT :: Eac3AtmosDynamicRangeCompressionRf
pattern Eac3AtmosDynamicRangeCompressionRf_MUSIC_LIGHT = Eac3AtmosDynamicRangeCompressionRf' "MUSIC_LIGHT"

pattern Eac3AtmosDynamicRangeCompressionRf_MUSIC_STANDARD :: Eac3AtmosDynamicRangeCompressionRf
pattern Eac3AtmosDynamicRangeCompressionRf_MUSIC_STANDARD = Eac3AtmosDynamicRangeCompressionRf' "MUSIC_STANDARD"

pattern Eac3AtmosDynamicRangeCompressionRf_NONE :: Eac3AtmosDynamicRangeCompressionRf
pattern Eac3AtmosDynamicRangeCompressionRf_NONE = Eac3AtmosDynamicRangeCompressionRf' "NONE"

pattern Eac3AtmosDynamicRangeCompressionRf_SPEECH :: Eac3AtmosDynamicRangeCompressionRf
pattern Eac3AtmosDynamicRangeCompressionRf_SPEECH = Eac3AtmosDynamicRangeCompressionRf' "SPEECH"

{-# COMPLETE
  Eac3AtmosDynamicRangeCompressionRf_FILM_LIGHT,
  Eac3AtmosDynamicRangeCompressionRf_FILM_STANDARD,
  Eac3AtmosDynamicRangeCompressionRf_MUSIC_LIGHT,
  Eac3AtmosDynamicRangeCompressionRf_MUSIC_STANDARD,
  Eac3AtmosDynamicRangeCompressionRf_NONE,
  Eac3AtmosDynamicRangeCompressionRf_SPEECH,
  Eac3AtmosDynamicRangeCompressionRf'
  #-}
