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
-- Module      : Amazonka.MediaConvert.Types.Eac3DynamicRangeCompressionLine
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Eac3DynamicRangeCompressionLine
  ( Eac3DynamicRangeCompressionLine
      ( ..,
        Eac3DynamicRangeCompressionLine_FILM_LIGHT,
        Eac3DynamicRangeCompressionLine_FILM_STANDARD,
        Eac3DynamicRangeCompressionLine_MUSIC_LIGHT,
        Eac3DynamicRangeCompressionLine_MUSIC_STANDARD,
        Eac3DynamicRangeCompressionLine_NONE,
        Eac3DynamicRangeCompressionLine_SPEECH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Choose the Dolby Digital dynamic range control (DRC) profile that
-- MediaConvert uses when encoding the metadata in the Dolby Digital stream
-- for the line operating mode. Related setting: When you use this setting,
-- MediaConvert ignores any value you provide for Dynamic range compression
-- profile (DynamicRangeCompressionProfile). For information about the
-- Dolby Digital DRC operating modes and profiles, see the Dynamic Range
-- Control chapter of the Dolby Metadata Guide at
-- https:\/\/developer.dolby.com\/globalassets\/professional\/documents\/dolby-metadata-guide.pdf.
newtype Eac3DynamicRangeCompressionLine = Eac3DynamicRangeCompressionLine'
  { fromEac3DynamicRangeCompressionLine ::
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

pattern Eac3DynamicRangeCompressionLine_FILM_LIGHT :: Eac3DynamicRangeCompressionLine
pattern Eac3DynamicRangeCompressionLine_FILM_LIGHT = Eac3DynamicRangeCompressionLine' "FILM_LIGHT"

pattern Eac3DynamicRangeCompressionLine_FILM_STANDARD :: Eac3DynamicRangeCompressionLine
pattern Eac3DynamicRangeCompressionLine_FILM_STANDARD = Eac3DynamicRangeCompressionLine' "FILM_STANDARD"

pattern Eac3DynamicRangeCompressionLine_MUSIC_LIGHT :: Eac3DynamicRangeCompressionLine
pattern Eac3DynamicRangeCompressionLine_MUSIC_LIGHT = Eac3DynamicRangeCompressionLine' "MUSIC_LIGHT"

pattern Eac3DynamicRangeCompressionLine_MUSIC_STANDARD :: Eac3DynamicRangeCompressionLine
pattern Eac3DynamicRangeCompressionLine_MUSIC_STANDARD = Eac3DynamicRangeCompressionLine' "MUSIC_STANDARD"

pattern Eac3DynamicRangeCompressionLine_NONE :: Eac3DynamicRangeCompressionLine
pattern Eac3DynamicRangeCompressionLine_NONE = Eac3DynamicRangeCompressionLine' "NONE"

pattern Eac3DynamicRangeCompressionLine_SPEECH :: Eac3DynamicRangeCompressionLine
pattern Eac3DynamicRangeCompressionLine_SPEECH = Eac3DynamicRangeCompressionLine' "SPEECH"

{-# COMPLETE
  Eac3DynamicRangeCompressionLine_FILM_LIGHT,
  Eac3DynamicRangeCompressionLine_FILM_STANDARD,
  Eac3DynamicRangeCompressionLine_MUSIC_LIGHT,
  Eac3DynamicRangeCompressionLine_MUSIC_STANDARD,
  Eac3DynamicRangeCompressionLine_NONE,
  Eac3DynamicRangeCompressionLine_SPEECH,
  Eac3DynamicRangeCompressionLine'
  #-}
