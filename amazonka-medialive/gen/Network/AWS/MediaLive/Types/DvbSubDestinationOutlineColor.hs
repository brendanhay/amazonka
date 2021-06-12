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
-- Module      : Network.AWS.MediaLive.Types.DvbSubDestinationOutlineColor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DvbSubDestinationOutlineColor
  ( DvbSubDestinationOutlineColor
      ( ..,
        DvbSubDestinationOutlineColor_BLACK,
        DvbSubDestinationOutlineColor_BLUE,
        DvbSubDestinationOutlineColor_GREEN,
        DvbSubDestinationOutlineColor_RED,
        DvbSubDestinationOutlineColor_WHITE,
        DvbSubDestinationOutlineColor_YELLOW
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Dvb Sub Destination Outline Color
newtype DvbSubDestinationOutlineColor = DvbSubDestinationOutlineColor'
  { fromDvbSubDestinationOutlineColor ::
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

pattern DvbSubDestinationOutlineColor_BLACK :: DvbSubDestinationOutlineColor
pattern DvbSubDestinationOutlineColor_BLACK = DvbSubDestinationOutlineColor' "BLACK"

pattern DvbSubDestinationOutlineColor_BLUE :: DvbSubDestinationOutlineColor
pattern DvbSubDestinationOutlineColor_BLUE = DvbSubDestinationOutlineColor' "BLUE"

pattern DvbSubDestinationOutlineColor_GREEN :: DvbSubDestinationOutlineColor
pattern DvbSubDestinationOutlineColor_GREEN = DvbSubDestinationOutlineColor' "GREEN"

pattern DvbSubDestinationOutlineColor_RED :: DvbSubDestinationOutlineColor
pattern DvbSubDestinationOutlineColor_RED = DvbSubDestinationOutlineColor' "RED"

pattern DvbSubDestinationOutlineColor_WHITE :: DvbSubDestinationOutlineColor
pattern DvbSubDestinationOutlineColor_WHITE = DvbSubDestinationOutlineColor' "WHITE"

pattern DvbSubDestinationOutlineColor_YELLOW :: DvbSubDestinationOutlineColor
pattern DvbSubDestinationOutlineColor_YELLOW = DvbSubDestinationOutlineColor' "YELLOW"

{-# COMPLETE
  DvbSubDestinationOutlineColor_BLACK,
  DvbSubDestinationOutlineColor_BLUE,
  DvbSubDestinationOutlineColor_GREEN,
  DvbSubDestinationOutlineColor_RED,
  DvbSubDestinationOutlineColor_WHITE,
  DvbSubDestinationOutlineColor_YELLOW,
  DvbSubDestinationOutlineColor'
  #-}
