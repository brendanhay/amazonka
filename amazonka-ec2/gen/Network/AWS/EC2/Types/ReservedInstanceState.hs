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
-- Module      : Network.AWS.EC2.Types.ReservedInstanceState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstanceState
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype ReservedInstanceState = ReservedInstanceState'
  { fromReservedInstanceState ::
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
