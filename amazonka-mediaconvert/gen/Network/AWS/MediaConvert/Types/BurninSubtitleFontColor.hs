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
-- Module      : Network.AWS.MediaConvert.Types.BurninSubtitleFontColor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.BurninSubtitleFontColor
  ( BurninSubtitleFontColor
      ( ..,
        BurninSubtitleFontColor_BLACK,
        BurninSubtitleFontColor_BLUE,
        BurninSubtitleFontColor_GREEN,
        BurninSubtitleFontColor_RED,
        BurninSubtitleFontColor_WHITE,
        BurninSubtitleFontColor_YELLOW
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the color of the burned-in captions. This option is not valid
-- for source captions that are STL, 608\/embedded or teletext. These
-- source settings are already pre-defined by the caption stream. All
-- burn-in and DVB-Sub font settings must match.
newtype BurninSubtitleFontColor = BurninSubtitleFontColor'
  { fromBurninSubtitleFontColor ::
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

pattern BurninSubtitleFontColor_BLACK :: BurninSubtitleFontColor
pattern BurninSubtitleFontColor_BLACK = BurninSubtitleFontColor' "BLACK"

pattern BurninSubtitleFontColor_BLUE :: BurninSubtitleFontColor
pattern BurninSubtitleFontColor_BLUE = BurninSubtitleFontColor' "BLUE"

pattern BurninSubtitleFontColor_GREEN :: BurninSubtitleFontColor
pattern BurninSubtitleFontColor_GREEN = BurninSubtitleFontColor' "GREEN"

pattern BurninSubtitleFontColor_RED :: BurninSubtitleFontColor
pattern BurninSubtitleFontColor_RED = BurninSubtitleFontColor' "RED"

pattern BurninSubtitleFontColor_WHITE :: BurninSubtitleFontColor
pattern BurninSubtitleFontColor_WHITE = BurninSubtitleFontColor' "WHITE"

pattern BurninSubtitleFontColor_YELLOW :: BurninSubtitleFontColor
pattern BurninSubtitleFontColor_YELLOW = BurninSubtitleFontColor' "YELLOW"

{-# COMPLETE
  BurninSubtitleFontColor_BLACK,
  BurninSubtitleFontColor_BLUE,
  BurninSubtitleFontColor_GREEN,
  BurninSubtitleFontColor_RED,
  BurninSubtitleFontColor_WHITE,
  BurninSubtitleFontColor_YELLOW,
  BurninSubtitleFontColor'
  #-}
