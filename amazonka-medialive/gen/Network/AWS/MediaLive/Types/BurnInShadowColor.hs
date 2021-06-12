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

import qualified Network.AWS.Core as Core

-- | Burn In Shadow Color
newtype BurnInShadowColor = BurnInShadowColor'
  { fromBurnInShadowColor ::
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
