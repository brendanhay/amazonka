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
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationFontColor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSubDestinationFontColor
  ( DvbSubDestinationFontColor
      ( ..,
        DvbSubDestinationFontColor_BLACK,
        DvbSubDestinationFontColor_BLUE,
        DvbSubDestinationFontColor_GREEN,
        DvbSubDestinationFontColor_RED,
        DvbSubDestinationFontColor_WHITE,
        DvbSubDestinationFontColor_YELLOW
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Dvb Sub Destination Font Color
newtype DvbSubDestinationFontColor = DvbSubDestinationFontColor'
  { fromDvbSubDestinationFontColor ::
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

pattern DvbSubDestinationFontColor_BLACK :: DvbSubDestinationFontColor
pattern DvbSubDestinationFontColor_BLACK = DvbSubDestinationFontColor' "BLACK"

pattern DvbSubDestinationFontColor_BLUE :: DvbSubDestinationFontColor
pattern DvbSubDestinationFontColor_BLUE = DvbSubDestinationFontColor' "BLUE"

pattern DvbSubDestinationFontColor_GREEN :: DvbSubDestinationFontColor
pattern DvbSubDestinationFontColor_GREEN = DvbSubDestinationFontColor' "GREEN"

pattern DvbSubDestinationFontColor_RED :: DvbSubDestinationFontColor
pattern DvbSubDestinationFontColor_RED = DvbSubDestinationFontColor' "RED"

pattern DvbSubDestinationFontColor_WHITE :: DvbSubDestinationFontColor
pattern DvbSubDestinationFontColor_WHITE = DvbSubDestinationFontColor' "WHITE"

pattern DvbSubDestinationFontColor_YELLOW :: DvbSubDestinationFontColor
pattern DvbSubDestinationFontColor_YELLOW = DvbSubDestinationFontColor' "YELLOW"

{-# COMPLETE
  DvbSubDestinationFontColor_BLACK,
  DvbSubDestinationFontColor_BLUE,
  DvbSubDestinationFontColor_GREEN,
  DvbSubDestinationFontColor_RED,
  DvbSubDestinationFontColor_WHITE,
  DvbSubDestinationFontColor_YELLOW,
  DvbSubDestinationFontColor'
  #-}
