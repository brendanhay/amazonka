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
-- Module      : Network.AWS.MediaLive.Types.BurnInBackgroundColor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BurnInBackgroundColor
  ( BurnInBackgroundColor
      ( ..,
        BurnInBackgroundColor_BLACK,
        BurnInBackgroundColor_NONE,
        BurnInBackgroundColor_WHITE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Burn In Background Color
newtype BurnInBackgroundColor = BurnInBackgroundColor'
  { fromBurnInBackgroundColor ::
      Core.Text
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

pattern BurnInBackgroundColor_BLACK :: BurnInBackgroundColor
pattern BurnInBackgroundColor_BLACK = BurnInBackgroundColor' "BLACK"

pattern BurnInBackgroundColor_NONE :: BurnInBackgroundColor
pattern BurnInBackgroundColor_NONE = BurnInBackgroundColor' "NONE"

pattern BurnInBackgroundColor_WHITE :: BurnInBackgroundColor
pattern BurnInBackgroundColor_WHITE = BurnInBackgroundColor' "WHITE"

{-# COMPLETE
  BurnInBackgroundColor_BLACK,
  BurnInBackgroundColor_NONE,
  BurnInBackgroundColor_WHITE,
  BurnInBackgroundColor'
  #-}
