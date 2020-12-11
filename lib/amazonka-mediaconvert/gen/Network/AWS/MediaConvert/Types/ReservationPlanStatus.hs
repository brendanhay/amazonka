-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ReservationPlanStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ReservationPlanStatus
  ( ReservationPlanStatus
      ( ReservationPlanStatus',
        Active,
        Expired
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specifies whether the pricing plan for your reserved queue is ACTIVE or EXPIRED.
newtype ReservationPlanStatus = ReservationPlanStatus' Lude.Text
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

pattern Active :: ReservationPlanStatus
pattern Active = ReservationPlanStatus' "ACTIVE"

pattern Expired :: ReservationPlanStatus
pattern Expired = ReservationPlanStatus' "EXPIRED"

{-# COMPLETE
  Active,
  Expired,
  ReservationPlanStatus'
  #-}
