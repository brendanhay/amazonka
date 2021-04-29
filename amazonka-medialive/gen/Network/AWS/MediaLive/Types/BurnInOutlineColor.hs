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

import qualified Network.AWS.Prelude as Prelude

-- | Burn In Outline Color
newtype BurnInOutlineColor = BurnInOutlineColor'
  { fromBurnInOutlineColor ::
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
