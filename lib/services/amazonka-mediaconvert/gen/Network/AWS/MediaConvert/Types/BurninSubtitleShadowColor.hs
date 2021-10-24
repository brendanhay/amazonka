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
-- Module      : Network.AWS.MediaConvert.Types.BurninSubtitleShadowColor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.BurninSubtitleShadowColor
  ( BurninSubtitleShadowColor
      ( ..,
        BurninSubtitleShadowColor_AUTO,
        BurninSubtitleShadowColor_BLACK,
        BurninSubtitleShadowColor_NONE,
        BurninSubtitleShadowColor_WHITE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specify the color of the shadow cast by the captions. Leave Shadow color
-- (ShadowColor) blank and set Style passthrough (StylePassthrough) to
-- enabled to use the shadow color data from your input captions, if
-- present.
newtype BurninSubtitleShadowColor = BurninSubtitleShadowColor'
  { fromBurninSubtitleShadowColor ::
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

pattern BurninSubtitleShadowColor_AUTO :: BurninSubtitleShadowColor
pattern BurninSubtitleShadowColor_AUTO = BurninSubtitleShadowColor' "AUTO"

pattern BurninSubtitleShadowColor_BLACK :: BurninSubtitleShadowColor
pattern BurninSubtitleShadowColor_BLACK = BurninSubtitleShadowColor' "BLACK"

pattern BurninSubtitleShadowColor_NONE :: BurninSubtitleShadowColor
pattern BurninSubtitleShadowColor_NONE = BurninSubtitleShadowColor' "NONE"

pattern BurninSubtitleShadowColor_WHITE :: BurninSubtitleShadowColor
pattern BurninSubtitleShadowColor_WHITE = BurninSubtitleShadowColor' "WHITE"

{-# COMPLETE
  BurninSubtitleShadowColor_AUTO,
  BurninSubtitleShadowColor_BLACK,
  BurninSubtitleShadowColor_NONE,
  BurninSubtitleShadowColor_WHITE,
  BurninSubtitleShadowColor'
  #-}
