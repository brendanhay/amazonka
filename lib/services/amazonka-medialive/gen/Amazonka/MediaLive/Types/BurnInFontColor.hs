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
-- Module      : Amazonka.MediaLive.Types.BurnInFontColor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.BurnInFontColor
  ( BurnInFontColor
      ( ..,
        BurnInFontColor_BLACK,
        BurnInFontColor_BLUE,
        BurnInFontColor_GREEN,
        BurnInFontColor_RED,
        BurnInFontColor_WHITE,
        BurnInFontColor_YELLOW
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Burn In Font Color
newtype BurnInFontColor = BurnInFontColor'
  { fromBurnInFontColor ::
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

pattern BurnInFontColor_BLACK :: BurnInFontColor
pattern BurnInFontColor_BLACK = BurnInFontColor' "BLACK"

pattern BurnInFontColor_BLUE :: BurnInFontColor
pattern BurnInFontColor_BLUE = BurnInFontColor' "BLUE"

pattern BurnInFontColor_GREEN :: BurnInFontColor
pattern BurnInFontColor_GREEN = BurnInFontColor' "GREEN"

pattern BurnInFontColor_RED :: BurnInFontColor
pattern BurnInFontColor_RED = BurnInFontColor' "RED"

pattern BurnInFontColor_WHITE :: BurnInFontColor
pattern BurnInFontColor_WHITE = BurnInFontColor' "WHITE"

pattern BurnInFontColor_YELLOW :: BurnInFontColor
pattern BurnInFontColor_YELLOW = BurnInFontColor' "YELLOW"

{-# COMPLETE
  BurnInFontColor_BLACK,
  BurnInFontColor_BLUE,
  BurnInFontColor_GREEN,
  BurnInFontColor_RED,
  BurnInFontColor_WHITE,
  BurnInFontColor_YELLOW,
  BurnInFontColor'
  #-}
