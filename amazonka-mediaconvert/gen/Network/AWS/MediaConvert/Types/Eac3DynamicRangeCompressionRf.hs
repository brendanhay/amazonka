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
-- Module      : Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionRf
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionRf
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

import qualified Network.AWS.Prelude as Prelude

-- | Specify how the service limits the audio dynamic range when compressing
-- the audio.
newtype Eac3DynamicRangeCompressionRf = Eac3DynamicRangeCompressionRf'
  { fromEac3DynamicRangeCompressionRf ::
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
