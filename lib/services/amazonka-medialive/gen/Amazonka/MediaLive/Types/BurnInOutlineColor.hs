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
-- Module      : Amazonka.MediaLive.Types.BurnInOutlineColor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.BurnInOutlineColor
  ( BurnInOutlineColor
      ( ..,
        BurnInOutlineColor_BLACK,
        BurnInOutlineColor_BLUE,
        BurnInOutlineColor_GREEN,
        BurnInOutlineColor_RED,
        BurnInOutlineColor_WHITE,
        BurnInOutlineColor_YELLOW
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Burn In Outline Color
newtype BurnInOutlineColor = BurnInOutlineColor'
  { fromBurnInOutlineColor ::
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

pattern BurnInOutlineColor_BLACK :: BurnInOutlineColor
pattern BurnInOutlineColor_BLACK = BurnInOutlineColor' "BLACK"

pattern BurnInOutlineColor_BLUE :: BurnInOutlineColor
pattern BurnInOutlineColor_BLUE = BurnInOutlineColor' "BLUE"

pattern BurnInOutlineColor_GREEN :: BurnInOutlineColor
pattern BurnInOutlineColor_GREEN = BurnInOutlineColor' "GREEN"

pattern BurnInOutlineColor_RED :: BurnInOutlineColor
pattern BurnInOutlineColor_RED = BurnInOutlineColor' "RED"

pattern BurnInOutlineColor_WHITE :: BurnInOutlineColor
pattern BurnInOutlineColor_WHITE = BurnInOutlineColor' "WHITE"

pattern BurnInOutlineColor_YELLOW :: BurnInOutlineColor
pattern BurnInOutlineColor_YELLOW = BurnInOutlineColor' "YELLOW"

{-# COMPLETE
  BurnInOutlineColor_BLACK,
  BurnInOutlineColor_BLUE,
  BurnInOutlineColor_GREEN,
  BurnInOutlineColor_RED,
  BurnInOutlineColor_WHITE,
  BurnInOutlineColor_YELLOW,
  BurnInOutlineColor'
  #-}
