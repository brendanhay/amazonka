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
-- Module      : Amazonka.MediaConvert.Types.DvbSubSubtitleFallbackFont
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.DvbSubSubtitleFallbackFont
  ( DvbSubSubtitleFallbackFont
      ( ..,
        DvbSubSubtitleFallbackFont_BEST_MATCH,
        DvbSubSubtitleFallbackFont_MONOSPACED_SANSSERIF,
        DvbSubSubtitleFallbackFont_MONOSPACED_SERIF,
        DvbSubSubtitleFallbackFont_PROPORTIONAL_SANSSERIF,
        DvbSubSubtitleFallbackFont_PROPORTIONAL_SERIF
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the font that you want the service to use for your burn in
-- captions when your input captions specify a font that MediaConvert
-- doesn\'t support. When you set Fallback font (FallbackFont) to best
-- match (BEST_MATCH), or leave blank, MediaConvert uses a supported font
-- that most closely matches the font that your input captions specify.
-- When there are multiple unsupported fonts in your input captions,
-- MediaConvert matches each font with the supported font that matches
-- best. When you explicitly choose a replacement font, MediaConvert uses
-- that font to replace all unsupported fonts from your input.
newtype DvbSubSubtitleFallbackFont = DvbSubSubtitleFallbackFont'
  { fromDvbSubSubtitleFallbackFont ::
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

pattern DvbSubSubtitleFallbackFont_BEST_MATCH :: DvbSubSubtitleFallbackFont
pattern DvbSubSubtitleFallbackFont_BEST_MATCH = DvbSubSubtitleFallbackFont' "BEST_MATCH"

pattern DvbSubSubtitleFallbackFont_MONOSPACED_SANSSERIF :: DvbSubSubtitleFallbackFont
pattern DvbSubSubtitleFallbackFont_MONOSPACED_SANSSERIF = DvbSubSubtitleFallbackFont' "MONOSPACED_SANSSERIF"

pattern DvbSubSubtitleFallbackFont_MONOSPACED_SERIF :: DvbSubSubtitleFallbackFont
pattern DvbSubSubtitleFallbackFont_MONOSPACED_SERIF = DvbSubSubtitleFallbackFont' "MONOSPACED_SERIF"

pattern DvbSubSubtitleFallbackFont_PROPORTIONAL_SANSSERIF :: DvbSubSubtitleFallbackFont
pattern DvbSubSubtitleFallbackFont_PROPORTIONAL_SANSSERIF = DvbSubSubtitleFallbackFont' "PROPORTIONAL_SANSSERIF"

pattern DvbSubSubtitleFallbackFont_PROPORTIONAL_SERIF :: DvbSubSubtitleFallbackFont
pattern DvbSubSubtitleFallbackFont_PROPORTIONAL_SERIF = DvbSubSubtitleFallbackFont' "PROPORTIONAL_SERIF"

{-# COMPLETE
  DvbSubSubtitleFallbackFont_BEST_MATCH,
  DvbSubSubtitleFallbackFont_MONOSPACED_SANSSERIF,
  DvbSubSubtitleFallbackFont_MONOSPACED_SERIF,
  DvbSubSubtitleFallbackFont_PROPORTIONAL_SANSSERIF,
  DvbSubSubtitleFallbackFont_PROPORTIONAL_SERIF,
  DvbSubSubtitleFallbackFont'
  #-}
