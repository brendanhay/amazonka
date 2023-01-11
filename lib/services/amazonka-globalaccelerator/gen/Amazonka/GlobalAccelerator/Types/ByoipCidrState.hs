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
-- Module      : Amazonka.GlobalAccelerator.Types.ByoipCidrState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.ByoipCidrState
  ( ByoipCidrState
      ( ..,
        ByoipCidrState_ADVERTISING,
        ByoipCidrState_DEPROVISIONED,
        ByoipCidrState_FAILED_ADVERTISING,
        ByoipCidrState_FAILED_DEPROVISION,
        ByoipCidrState_FAILED_PROVISION,
        ByoipCidrState_FAILED_WITHDRAW,
        ByoipCidrState_PENDING_ADVERTISING,
        ByoipCidrState_PENDING_DEPROVISIONING,
        ByoipCidrState_PENDING_PROVISIONING,
        ByoipCidrState_PENDING_WITHDRAWING,
        ByoipCidrState_READY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ByoipCidrState = ByoipCidrState'
  { fromByoipCidrState ::
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

pattern ByoipCidrState_ADVERTISING :: ByoipCidrState
pattern ByoipCidrState_ADVERTISING = ByoipCidrState' "ADVERTISING"

pattern ByoipCidrState_DEPROVISIONED :: ByoipCidrState
pattern ByoipCidrState_DEPROVISIONED = ByoipCidrState' "DEPROVISIONED"

pattern ByoipCidrState_FAILED_ADVERTISING :: ByoipCidrState
pattern ByoipCidrState_FAILED_ADVERTISING = ByoipCidrState' "FAILED_ADVERTISING"

pattern ByoipCidrState_FAILED_DEPROVISION :: ByoipCidrState
pattern ByoipCidrState_FAILED_DEPROVISION = ByoipCidrState' "FAILED_DEPROVISION"

pattern ByoipCidrState_FAILED_PROVISION :: ByoipCidrState
pattern ByoipCidrState_FAILED_PROVISION = ByoipCidrState' "FAILED_PROVISION"

pattern ByoipCidrState_FAILED_WITHDRAW :: ByoipCidrState
pattern ByoipCidrState_FAILED_WITHDRAW = ByoipCidrState' "FAILED_WITHDRAW"

pattern ByoipCidrState_PENDING_ADVERTISING :: ByoipCidrState
pattern ByoipCidrState_PENDING_ADVERTISING = ByoipCidrState' "PENDING_ADVERTISING"

pattern ByoipCidrState_PENDING_DEPROVISIONING :: ByoipCidrState
pattern ByoipCidrState_PENDING_DEPROVISIONING = ByoipCidrState' "PENDING_DEPROVISIONING"

pattern ByoipCidrState_PENDING_PROVISIONING :: ByoipCidrState
pattern ByoipCidrState_PENDING_PROVISIONING = ByoipCidrState' "PENDING_PROVISIONING"

pattern ByoipCidrState_PENDING_WITHDRAWING :: ByoipCidrState
pattern ByoipCidrState_PENDING_WITHDRAWING = ByoipCidrState' "PENDING_WITHDRAWING"

pattern ByoipCidrState_READY :: ByoipCidrState
pattern ByoipCidrState_READY = ByoipCidrState' "READY"

{-# COMPLETE
  ByoipCidrState_ADVERTISING,
  ByoipCidrState_DEPROVISIONED,
  ByoipCidrState_FAILED_ADVERTISING,
  ByoipCidrState_FAILED_DEPROVISION,
  ByoipCidrState_FAILED_PROVISION,
  ByoipCidrState_FAILED_WITHDRAW,
  ByoipCidrState_PENDING_ADVERTISING,
  ByoipCidrState_PENDING_DEPROVISIONING,
  ByoipCidrState_PENDING_PROVISIONING,
  ByoipCidrState_PENDING_WITHDRAWING,
  ByoipCidrState_READY,
  ByoipCidrState'
  #-}
