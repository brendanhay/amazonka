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
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitleFontColor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubtitleFontColor
  ( DvbSubtitleFontColor
      ( ..,
        DvbSubtitleFontColor_BLACK,
        DvbSubtitleFontColor_BLUE,
        DvbSubtitleFontColor_GREEN,
        DvbSubtitleFontColor_RED,
        DvbSubtitleFontColor_WHITE,
        DvbSubtitleFontColor_YELLOW
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Specifies the color of the burned-in captions. This option is not valid
-- for source captions that are STL, 608\/embedded or teletext. These
-- source settings are already pre-defined by the caption stream. All
-- burn-in and DVB-Sub font settings must match.
newtype DvbSubtitleFontColor = DvbSubtitleFontColor'
  { fromDvbSubtitleFontColor ::
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

pattern DvbSubtitleFontColor_BLACK :: DvbSubtitleFontColor
pattern DvbSubtitleFontColor_BLACK = DvbSubtitleFontColor' "BLACK"

pattern DvbSubtitleFontColor_BLUE :: DvbSubtitleFontColor
pattern DvbSubtitleFontColor_BLUE = DvbSubtitleFontColor' "BLUE"

pattern DvbSubtitleFontColor_GREEN :: DvbSubtitleFontColor
pattern DvbSubtitleFontColor_GREEN = DvbSubtitleFontColor' "GREEN"

pattern DvbSubtitleFontColor_RED :: DvbSubtitleFontColor
pattern DvbSubtitleFontColor_RED = DvbSubtitleFontColor' "RED"

pattern DvbSubtitleFontColor_WHITE :: DvbSubtitleFontColor
pattern DvbSubtitleFontColor_WHITE = DvbSubtitleFontColor' "WHITE"

pattern DvbSubtitleFontColor_YELLOW :: DvbSubtitleFontColor
pattern DvbSubtitleFontColor_YELLOW = DvbSubtitleFontColor' "YELLOW"

{-# COMPLETE
  DvbSubtitleFontColor_BLACK,
  DvbSubtitleFontColor_BLUE,
  DvbSubtitleFontColor_GREEN,
  DvbSubtitleFontColor_RED,
  DvbSubtitleFontColor_WHITE,
  DvbSubtitleFontColor_YELLOW,
  DvbSubtitleFontColor'
  #-}
