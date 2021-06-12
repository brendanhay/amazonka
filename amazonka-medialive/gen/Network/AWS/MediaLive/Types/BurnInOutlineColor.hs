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
-- Module      : Network.AWS.MediaLive.Types.BurnInOutlineColor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BurnInOutlineColor
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

import qualified Network.AWS.Core as Core

-- | Burn In Outline Color
newtype BurnInOutlineColor = BurnInOutlineColor'
  { fromBurnInOutlineColor ::
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
