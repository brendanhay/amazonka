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
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionLine
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionLine
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specify the absolute peak level for a signal with dynamic range
-- compression.
newtype Eac3AtmosDynamicRangeCompressionLine = Eac3AtmosDynamicRangeCompressionLine'
  { fromEac3AtmosDynamicRangeCompressionLine ::
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
