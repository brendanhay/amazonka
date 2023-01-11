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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UnlockState = UnlockState'
  { fromUnlockState ::
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
