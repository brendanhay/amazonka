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
-- Module      : Amazonka.MediaConvert.Types.BurninSubtitleOutlineColor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.BurninSubtitleOutlineColor
  ( BurninSubtitleOutlineColor
      ( ..,
        BurninSubtitleOutlineColor_AUTO,
        BurninSubtitleOutlineColor_BLACK,
        BurninSubtitleOutlineColor_BLUE,
        BurninSubtitleOutlineColor_GREEN,
        BurninSubtitleOutlineColor_RED,
        BurninSubtitleOutlineColor_WHITE,
        BurninSubtitleOutlineColor_YELLOW
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify font outline color. Leave Outline color (OutlineColor) blank and
-- set Style passthrough (StylePassthrough) to enabled to use the font
-- outline color data from your input captions, if present.
newtype BurninSubtitleOutlineColor = BurninSubtitleOutlineColor'
  { fromBurninSubtitleOutlineColor ::
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

pattern BurninSubtitleOutlineColor_AUTO :: BurninSubtitleOutlineColor
pattern BurninSubtitleOutlineColor_AUTO = BurninSubtitleOutlineColor' "AUTO"

pattern BurninSubtitleOutlineColor_BLACK :: BurninSubtitleOutlineColor
pattern BurninSubtitleOutlineColor_BLACK = BurninSubtitleOutlineColor' "BLACK"

pattern BurninSubtitleOutlineColor_BLUE :: BurninSubtitleOutlineColor
pattern BurninSubtitleOutlineColor_BLUE = BurninSubtitleOutlineColor' "BLUE"

pattern BurninSubtitleOutlineColor_GREEN :: BurninSubtitleOutlineColor
pattern BurninSubtitleOutlineColor_GREEN = BurninSubtitleOutlineColor' "GREEN"

pattern BurninSubtitleOutlineColor_RED :: BurninSubtitleOutlineColor
pattern BurninSubtitleOutlineColor_RED = BurninSubtitleOutlineColor' "RED"

pattern BurninSubtitleOutlineColor_WHITE :: BurninSubtitleOutlineColor
pattern BurninSubtitleOutlineColor_WHITE = BurninSubtitleOutlineColor' "WHITE"

pattern BurninSubtitleOutlineColor_YELLOW :: BurninSubtitleOutlineColor
pattern BurninSubtitleOutlineColor_YELLOW = BurninSubtitleOutlineColor' "YELLOW"

{-# COMPLETE
  BurninSubtitleOutlineColor_AUTO,
  BurninSubtitleOutlineColor_BLACK,
  BurninSubtitleOutlineColor_BLUE,
  BurninSubtitleOutlineColor_GREEN,
  BurninSubtitleOutlineColor_RED,
  BurninSubtitleOutlineColor_WHITE,
  BurninSubtitleOutlineColor_YELLOW,
  BurninSubtitleOutlineColor'
  #-}
