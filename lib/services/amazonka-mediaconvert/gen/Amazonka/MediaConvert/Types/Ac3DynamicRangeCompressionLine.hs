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
-- Module      : Amazonka.MediaConvert.Types.Ac3DynamicRangeCompressionLine
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Ac3DynamicRangeCompressionLine
  ( Ac3DynamicRangeCompressionLine
      ( ..,
        Ac3DynamicRangeCompressionLine_FILM_LIGHT,
        Ac3DynamicRangeCompressionLine_FILM_STANDARD,
        Ac3DynamicRangeCompressionLine_MUSIC_LIGHT,
        Ac3DynamicRangeCompressionLine_MUSIC_STANDARD,
        Ac3DynamicRangeCompressionLine_NONE,
        Ac3DynamicRangeCompressionLine_SPEECH
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
newtype Ac3DynamicRangeCompressionLine = Ac3DynamicRangeCompressionLine'
  { fromAc3DynamicRangeCompressionLine ::
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

pattern Ac3DynamicRangeCompressionLine_FILM_LIGHT :: Ac3DynamicRangeCompressionLine
pattern Ac3DynamicRangeCompressionLine_FILM_LIGHT = Ac3DynamicRangeCompressionLine' "FILM_LIGHT"

pattern Ac3DynamicRangeCompressionLine_FILM_STANDARD :: Ac3DynamicRangeCompressionLine
pattern Ac3DynamicRangeCompressionLine_FILM_STANDARD = Ac3DynamicRangeCompressionLine' "FILM_STANDARD"

pattern Ac3DynamicRangeCompressionLine_MUSIC_LIGHT :: Ac3DynamicRangeCompressionLine
pattern Ac3DynamicRangeCompressionLine_MUSIC_LIGHT = Ac3DynamicRangeCompressionLine' "MUSIC_LIGHT"

pattern Ac3DynamicRangeCompressionLine_MUSIC_STANDARD :: Ac3DynamicRangeCompressionLine
pattern Ac3DynamicRangeCompressionLine_MUSIC_STANDARD = Ac3DynamicRangeCompressionLine' "MUSIC_STANDARD"

pattern Ac3DynamicRangeCompressionLine_NONE :: Ac3DynamicRangeCompressionLine
pattern Ac3DynamicRangeCompressionLine_NONE = Ac3DynamicRangeCompressionLine' "NONE"

pattern Ac3DynamicRangeCompressionLine_SPEECH :: Ac3DynamicRangeCompressionLine
pattern Ac3DynamicRangeCompressionLine_SPEECH = Ac3DynamicRangeCompressionLine' "SPEECH"

{-# COMPLETE
  Ac3DynamicRangeCompressionLine_FILM_LIGHT,
  Ac3DynamicRangeCompressionLine_FILM_STANDARD,
  Ac3DynamicRangeCompressionLine_MUSIC_LIGHT,
  Ac3DynamicRangeCompressionLine_MUSIC_STANDARD,
  Ac3DynamicRangeCompressionLine_NONE,
  Ac3DynamicRangeCompressionLine_SPEECH,
  Ac3DynamicRangeCompressionLine'
  #-}
