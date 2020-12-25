{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitleShadowColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubtitleShadowColor
  ( DvbSubtitleShadowColor
      ( DvbSubtitleShadowColor',
        DvbSubtitleShadowColorNone,
        DvbSubtitleShadowColorBlack,
        DvbSubtitleShadowColorWhite,
        fromDvbSubtitleShadowColor
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specifies the color of the shadow cast by the captions.
--
-- All burn-in and DVB-Sub font settings must match.
newtype DvbSubtitleShadowColor = DvbSubtitleShadowColor'
  { fromDvbSubtitleShadowColor ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern DvbSubtitleShadowColorNone :: DvbSubtitleShadowColor
pattern DvbSubtitleShadowColorNone = DvbSubtitleShadowColor' "NONE"

pattern DvbSubtitleShadowColorBlack :: DvbSubtitleShadowColor
pattern DvbSubtitleShadowColorBlack = DvbSubtitleShadowColor' "BLACK"

pattern DvbSubtitleShadowColorWhite :: DvbSubtitleShadowColor
pattern DvbSubtitleShadowColorWhite = DvbSubtitleShadowColor' "WHITE"

{-# COMPLETE
  DvbSubtitleShadowColorNone,
  DvbSubtitleShadowColorBlack,
  DvbSubtitleShadowColorWhite,
  DvbSubtitleShadowColor'
  #-}
