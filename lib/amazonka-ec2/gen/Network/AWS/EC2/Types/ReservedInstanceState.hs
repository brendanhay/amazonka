-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstanceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstanceState
  ( ReservedInstanceState
      ( ReservedInstanceState',
        Active,
        PaymentFailed,
        PaymentPending,
        Queued,
        QueuedDeleted,
        Retired
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ReservedInstanceState = ReservedInstanceState' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Active :: ReservedInstanceState
pattern Active = ReservedInstanceState' "active"

pattern PaymentFailed :: ReservedInstanceState
pattern PaymentFailed = ReservedInstanceState' "payment-failed"

pattern PaymentPending :: ReservedInstanceState
pattern PaymentPending = ReservedInstanceState' "payment-pending"

pattern Queued :: ReservedInstanceState
pattern Queued = ReservedInstanceState' "queued"

pattern QueuedDeleted :: ReservedInstanceState
pattern QueuedDeleted = ReservedInstanceState' "queued-deleted"

pattern Retired :: ReservedInstanceState
pattern Retired = ReservedInstanceState' "retired"

{-# COMPLETE
  Active,
  PaymentFailed,
  PaymentPending,
  Queued,
  QueuedDeleted,
  Retired,
  ReservedInstanceState'
  #-}
