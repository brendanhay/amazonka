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
-- Module      : Amazonka.MediaLive.Types.DvbSubDestinationOutlineColor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.DvbSubDestinationOutlineColor
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Dvb Sub Destination Outline Color
newtype DvbSubDestinationOutlineColor = DvbSubDestinationOutlineColor'
  { fromDvbSubDestinationOutlineColor ::
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
