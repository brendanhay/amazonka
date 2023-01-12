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
-- Module      : Amazonka.MediaLive.Types.DvbSubDestinationFontColor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.DvbSubDestinationFontColor
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Dvb Sub Destination Font Color
newtype DvbSubDestinationFontColor = DvbSubDestinationFontColor'
  { fromDvbSubDestinationFontColor ::
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
