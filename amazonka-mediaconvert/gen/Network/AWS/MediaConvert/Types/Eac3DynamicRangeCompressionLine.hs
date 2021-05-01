{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionLine
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionLine
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

import qualified Network.AWS.Prelude as Prelude

-- | Specify the absolute peak level for a signal with dynamic range
-- compression.
newtype Eac3DynamicRangeCompressionLine = Eac3DynamicRangeCompressionLine'
  { fromEac3DynamicRangeCompressionLine ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
