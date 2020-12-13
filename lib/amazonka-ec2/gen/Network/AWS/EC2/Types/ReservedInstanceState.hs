{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        PaymentPending,
        Active,
        PaymentFailed,
        Retired,
        Queued,
        QueuedDeleted
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

pattern PaymentPending :: ReservedInstanceState
pattern PaymentPending = ReservedInstanceState' "payment-pending"

pattern Active :: ReservedInstanceState
pattern Active = ReservedInstanceState' "active"

pattern PaymentFailed :: ReservedInstanceState
pattern PaymentFailed = ReservedInstanceState' "payment-failed"

pattern Retired :: ReservedInstanceState
pattern Retired = ReservedInstanceState' "retired"

pattern Queued :: ReservedInstanceState
pattern Queued = ReservedInstanceState' "queued"

pattern QueuedDeleted :: ReservedInstanceState
pattern QueuedDeleted = ReservedInstanceState' "queued-deleted"

{-# COMPLETE
  PaymentPending,
  Active,
  PaymentFailed,
  Retired,
  Queued,
  QueuedDeleted,
  ReservedInstanceState'
  #-}
