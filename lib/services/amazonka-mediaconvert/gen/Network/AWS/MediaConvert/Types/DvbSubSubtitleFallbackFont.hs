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
-- Module      : Network.AWS.MediaConvert.Types.DvbSubSubtitleFallbackFont
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubSubtitleFallbackFont
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specify the font that you want the service to use for your burn in
-- captions when your input captions specify a font that MediaConvert
-- doesn\'t support. When you keep the default value, Best match
-- (BEST_MATCH), MediaConvert uses a supported font that most closely
-- matches the font that your input captions specify. When there are
-- multiple unsupported fonts in your input captions, MediaConvert matches
-- each font with the supported font that matches best. When you explicitly
-- choose a replacement font, MediaConvert uses that font to replace all
-- unsupported fonts from your input.
newtype DvbSubSubtitleFallbackFont = DvbSubSubtitleFallbackFont'
  { fromDvbSubSubtitleFallbackFont ::
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
