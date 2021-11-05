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
-- Module      : Amazonka.SnowDeviceManagement.Types.UnlockState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SnowDeviceManagement.Types.UnlockState
  ( UnlockState
      ( ..,
        UnlockState_LOCKED,
        UnlockState_UNLOCKED,
        UnlockState_UNLOCKING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype UnlockState = UnlockState'
  { fromUnlockState ::
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

pattern UnlockState_LOCKED :: UnlockState
pattern UnlockState_LOCKED = UnlockState' "LOCKED"

pattern UnlockState_UNLOCKED :: UnlockState
pattern UnlockState_UNLOCKED = UnlockState' "UNLOCKED"

pattern UnlockState_UNLOCKING :: UnlockState
pattern UnlockState_UNLOCKING = UnlockState' "UNLOCKING"

{-# COMPLETE
  UnlockState_LOCKED,
  UnlockState_UNLOCKED,
  UnlockState_UNLOCKING,
  UnlockState'
  #-}
