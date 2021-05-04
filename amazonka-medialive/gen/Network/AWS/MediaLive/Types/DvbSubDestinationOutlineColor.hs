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

import qualified Network.AWS.Prelude as Prelude

-- | Dvb Sub Destination Outline Color
newtype DvbSubDestinationOutlineColor = DvbSubDestinationOutlineColor'
  { fromDvbSubDestinationOutlineColor ::
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
