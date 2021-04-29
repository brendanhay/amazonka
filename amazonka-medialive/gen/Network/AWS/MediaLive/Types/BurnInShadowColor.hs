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
-- Module      : Network.AWS.MediaLive.Types.BurnInShadowColor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BurnInShadowColor
  ( BurnInShadowColor
      ( ..,
        BurnInShadowColor_BLACK,
        BurnInShadowColor_NONE,
        BurnInShadowColor_WHITE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Burn In Shadow Color
newtype BurnInShadowColor = BurnInShadowColor'
  { fromBurnInShadowColor ::
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

pattern BurnInShadowColor_BLACK :: BurnInShadowColor
pattern BurnInShadowColor_BLACK = BurnInShadowColor' "BLACK"

pattern BurnInShadowColor_NONE :: BurnInShadowColor
pattern BurnInShadowColor_NONE = BurnInShadowColor' "NONE"

pattern BurnInShadowColor_WHITE :: BurnInShadowColor
pattern BurnInShadowColor_WHITE = BurnInShadowColor' "WHITE"

{-# COMPLETE
  BurnInShadowColor_BLACK,
  BurnInShadowColor_NONE,
  BurnInShadowColor_WHITE,
  BurnInShadowColor'
  #-}
