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

import qualified Network.AWS.Core as Core

-- | Dvb Sub Destination Font Color
newtype DvbSubDestinationFontColor = DvbSubDestinationFontColor'
  { fromDvbSubDestinationFontColor ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
