{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationState
  ( CapacityReservationState
      ( CapacityReservationState',
        CRSActive,
        CRSCancelled,
        CRSExpired,
        CRSFailed,
        CRSPending
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CapacityReservationState = CapacityReservationState' Lude.Text
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

pattern CRSActive :: CapacityReservationState
pattern CRSActive = CapacityReservationState' "active"

pattern CRSCancelled :: CapacityReservationState
pattern CRSCancelled = CapacityReservationState' "cancelled"

pattern CRSExpired :: CapacityReservationState
pattern CRSExpired = CapacityReservationState' "expired"

pattern CRSFailed :: CapacityReservationState
pattern CRSFailed = CapacityReservationState' "failed"

pattern CRSPending :: CapacityReservationState
pattern CRSPending = CapacityReservationState' "pending"

{-# COMPLETE
  CRSActive,
  CRSCancelled,
  CRSExpired,
  CRSFailed,
  CRSPending,
  CapacityReservationState'
  #-}
