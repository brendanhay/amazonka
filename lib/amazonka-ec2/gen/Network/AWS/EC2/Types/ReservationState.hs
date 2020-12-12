{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservationState
  ( ReservationState
      ( ReservationState',
        RSActive,
        RSPaymentFailed,
        RSPaymentPending,
        RSRetired
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ReservationState = ReservationState' Lude.Text
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

pattern RSActive :: ReservationState
pattern RSActive = ReservationState' "active"

pattern RSPaymentFailed :: ReservationState
pattern RSPaymentFailed = ReservationState' "payment-failed"

pattern RSPaymentPending :: ReservationState
pattern RSPaymentPending = ReservationState' "payment-pending"

pattern RSRetired :: ReservationState
pattern RSRetired = ReservationState' "retired"

{-# COMPLETE
  RSActive,
  RSPaymentFailed,
  RSPaymentPending,
  RSRetired,
  ReservationState'
  #-}
