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
-- Module      : Amazonka.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionLine
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionLine
  ( Eac3AtmosDynamicRangeCompressionLine
      ( ..,
        Eac3AtmosDynamicRangeCompressionLine_FILM_LIGHT,
        Eac3AtmosDynamicRangeCompressionLine_FILM_STANDARD,
        Eac3AtmosDynamicRangeCompressionLine_MUSIC_LIGHT,
        Eac3AtmosDynamicRangeCompressionLine_MUSIC_STANDARD,
        Eac3AtmosDynamicRangeCompressionLine_NONE,
        Eac3AtmosDynamicRangeCompressionLine_SPEECH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Choose the Dolby dynamic range control (DRC) profile that MediaConvert
-- uses when encoding the metadata in the Dolby stream for the line
-- operating mode. Default value: Film light
-- (ATMOS_STORAGE_DDP_COMPR_FILM_LIGHT) Related setting: To have
-- MediaConvert use the value you specify here, keep the default value,
-- Custom (SPECIFIED) for the setting Dynamic range control
-- (DynamicRangeControl). Otherwise, MediaConvert ignores Dynamic range
-- compression line (DynamicRangeCompressionLine). For information about
-- the Dolby DRC operating modes and profiles, see the Dynamic Range
-- Control chapter of the Dolby Metadata Guide at
-- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
newtype Eac3AtmosDynamicRangeCompressionLine = Eac3AtmosDynamicRangeCompressionLine'
  { fromEac3AtmosDynamicRangeCompressionLine ::
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

pattern Eac3AtmosDynamicRangeCompressionLine_FILM_LIGHT :: Eac3AtmosDynamicRangeCompressionLine
pattern Eac3AtmosDynamicRangeCompressionLine_FILM_LIGHT = Eac3AtmosDynamicRangeCompressionLine' "FILM_LIGHT"

pattern Eac3AtmosDynamicRangeCompressionLine_FILM_STANDARD :: Eac3AtmosDynamicRangeCompressionLine
pattern Eac3AtmosDynamicRangeCompressionLine_FILM_STANDARD = Eac3AtmosDynamicRangeCompressionLine' "FILM_STANDARD"

pattern Eac3AtmosDynamicRangeCompressionLine_MUSIC_LIGHT :: Eac3AtmosDynamicRangeCompressionLine
pattern Eac3AtmosDynamicRangeCompressionLine_MUSIC_LIGHT = Eac3AtmosDynamicRangeCompressionLine' "MUSIC_LIGHT"

pattern Eac3AtmosDynamicRangeCompressionLine_MUSIC_STANDARD :: Eac3AtmosDynamicRangeCompressionLine
pattern Eac3AtmosDynamicRangeCompressionLine_MUSIC_STANDARD = Eac3AtmosDynamicRangeCompressionLine' "MUSIC_STANDARD"

pattern Eac3AtmosDynamicRangeCompressionLine_NONE :: Eac3AtmosDynamicRangeCompressionLine
pattern Eac3AtmosDynamicRangeCompressionLine_NONE = Eac3AtmosDynamicRangeCompressionLine' "NONE"

pattern Eac3AtmosDynamicRangeCompressionLine_SPEECH :: Eac3AtmosDynamicRangeCompressionLine
pattern Eac3AtmosDynamicRangeCompressionLine_SPEECH = Eac3AtmosDynamicRangeCompressionLine' "SPEECH"

{-# COMPLETE
  Eac3AtmosDynamicRangeCompressionLine_FILM_LIGHT,
  Eac3AtmosDynamicRangeCompressionLine_FILM_STANDARD,
  Eac3AtmosDynamicRangeCompressionLine_MUSIC_LIGHT,
  Eac3AtmosDynamicRangeCompressionLine_MUSIC_STANDARD,
  Eac3AtmosDynamicRangeCompressionLine_NONE,
  Eac3AtmosDynamicRangeCompressionLine_SPEECH,
  Eac3AtmosDynamicRangeCompressionLine'
  #-}
