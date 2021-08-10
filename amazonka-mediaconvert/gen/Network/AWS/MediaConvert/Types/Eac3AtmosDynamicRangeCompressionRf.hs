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
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionRf
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionRf
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specify how the service limits the audio dynamic range when compressing
-- the audio.
newtype Eac3AtmosDynamicRangeCompressionRf = Eac3AtmosDynamicRangeCompressionRf'
  { fromEac3AtmosDynamicRangeCompressionRf ::
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
