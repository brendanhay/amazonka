{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstanceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ReservedInstanceState
  ( ReservedInstanceState
    ( ReservedInstanceState'
    , ReservedInstanceStatePaymentPending
    , ReservedInstanceStateActive
    , ReservedInstanceStatePaymentFailed
    , ReservedInstanceStateRetired
    , ReservedInstanceStateQueued
    , ReservedInstanceStateQueuedDeleted
    , fromReservedInstanceState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ReservedInstanceState = ReservedInstanceState'{fromReservedInstanceState
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern ReservedInstanceStatePaymentPending :: ReservedInstanceState
pattern ReservedInstanceStatePaymentPending = ReservedInstanceState' "payment-pending"

pattern ReservedInstanceStateActive :: ReservedInstanceState
pattern ReservedInstanceStateActive = ReservedInstanceState' "active"

pattern ReservedInstanceStatePaymentFailed :: ReservedInstanceState
pattern ReservedInstanceStatePaymentFailed = ReservedInstanceState' "payment-failed"

pattern ReservedInstanceStateRetired :: ReservedInstanceState
pattern ReservedInstanceStateRetired = ReservedInstanceState' "retired"

pattern ReservedInstanceStateQueued :: ReservedInstanceState
pattern ReservedInstanceStateQueued = ReservedInstanceState' "queued"

pattern ReservedInstanceStateQueuedDeleted :: ReservedInstanceState
pattern ReservedInstanceStateQueuedDeleted = ReservedInstanceState' "queued-deleted"

{-# COMPLETE 
  ReservedInstanceStatePaymentPending,

  ReservedInstanceStateActive,

  ReservedInstanceStatePaymentFailed,

  ReservedInstanceStateRetired,

  ReservedInstanceStateQueued,

  ReservedInstanceStateQueuedDeleted,
  ReservedInstanceState'
  #-}
