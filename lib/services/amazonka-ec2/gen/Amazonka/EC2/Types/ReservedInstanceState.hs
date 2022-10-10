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
-- Module      : Amazonka.EC2.Types.ReservedInstanceState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ReservedInstanceState
  ( ReservedInstanceState
      ( ..,
        ReservedInstanceState_Active,
        ReservedInstanceState_Payment_failed,
        ReservedInstanceState_Payment_pending,
        ReservedInstanceState_Queued,
        ReservedInstanceState_Queued_deleted,
        ReservedInstanceState_Retired
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ReservedInstanceState = ReservedInstanceState'
  { fromReservedInstanceState ::
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

pattern ReservedInstanceState_Active :: ReservedInstanceState
pattern ReservedInstanceState_Active = ReservedInstanceState' "active"

pattern ReservedInstanceState_Payment_failed :: ReservedInstanceState
pattern ReservedInstanceState_Payment_failed = ReservedInstanceState' "payment-failed"

pattern ReservedInstanceState_Payment_pending :: ReservedInstanceState
pattern ReservedInstanceState_Payment_pending = ReservedInstanceState' "payment-pending"

pattern ReservedInstanceState_Queued :: ReservedInstanceState
pattern ReservedInstanceState_Queued = ReservedInstanceState' "queued"

pattern ReservedInstanceState_Queued_deleted :: ReservedInstanceState
pattern ReservedInstanceState_Queued_deleted = ReservedInstanceState' "queued-deleted"

pattern ReservedInstanceState_Retired :: ReservedInstanceState
pattern ReservedInstanceState_Retired = ReservedInstanceState' "retired"

{-# COMPLETE
  ReservedInstanceState_Active,
  ReservedInstanceState_Payment_failed,
  ReservedInstanceState_Payment_pending,
  ReservedInstanceState_Queued,
  ReservedInstanceState_Queued_deleted,
  ReservedInstanceState_Retired,
  ReservedInstanceState'
  #-}
