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
-- Module      : Amazonka.MediaLive.Types.BurnInAlignment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.BurnInAlignment
  ( BurnInAlignment
      ( ..,
        BurnInAlignment_CENTERED,
        BurnInAlignment_LEFT,
        BurnInAlignment_SMART
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Burn In Alignment
newtype BurnInAlignment = BurnInAlignment'
  { fromBurnInAlignment ::
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

pattern BurnInAlignment_CENTERED :: BurnInAlignment
pattern BurnInAlignment_CENTERED = BurnInAlignment' "CENTERED"

pattern BurnInAlignment_LEFT :: BurnInAlignment
pattern BurnInAlignment_LEFT = BurnInAlignment' "LEFT"

pattern BurnInAlignment_SMART :: BurnInAlignment
pattern BurnInAlignment_SMART = BurnInAlignment' "SMART"

{-# COMPLETE
  BurnInAlignment_CENTERED,
  BurnInAlignment_LEFT,
  BurnInAlignment_SMART,
  BurnInAlignment'
  #-}
