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
-- Module      : Amazonka.MediaLive.Types.BurnInShadowColor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.BurnInShadowColor
  ( BurnInShadowColor
      ( ..,
        BurnInShadowColor_BLACK,
        BurnInShadowColor_NONE,
        BurnInShadowColor_WHITE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Burn In Shadow Color
newtype BurnInShadowColor = BurnInShadowColor'
  { fromBurnInShadowColor ::
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
