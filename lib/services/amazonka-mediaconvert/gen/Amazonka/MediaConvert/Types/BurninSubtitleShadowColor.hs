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
-- Module      : Amazonka.MediaConvert.Types.BurninSubtitleShadowColor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.BurninSubtitleShadowColor
  ( BurninSubtitleShadowColor
      ( ..,
        BurninSubtitleShadowColor_AUTO,
        BurninSubtitleShadowColor_BLACK,
        BurninSubtitleShadowColor_NONE,
        BurninSubtitleShadowColor_WHITE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the color of the shadow cast by the captions. Leave Shadow color
-- (ShadowColor) blank and set Style passthrough (StylePassthrough) to
-- enabled to use the shadow color data from your input captions, if
-- present.
newtype BurninSubtitleShadowColor = BurninSubtitleShadowColor'
  { fromBurninSubtitleShadowColor ::
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
