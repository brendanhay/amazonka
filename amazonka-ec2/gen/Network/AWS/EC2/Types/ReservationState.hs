{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservationState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservationState
  ( ReservationState
      ( ..,
        ReservationState_Active,
        ReservationState_Payment_failed,
        ReservationState_Payment_pending,
        ReservationState_Retired
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype ReservationState = ReservationState'
  { fromReservationState ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern ReservationState_Active :: ReservationState
pattern ReservationState_Active = ReservationState' "active"

pattern ReservationState_Payment_failed :: ReservationState
pattern ReservationState_Payment_failed = ReservationState' "payment-failed"

pattern ReservationState_Payment_pending :: ReservationState
pattern ReservationState_Payment_pending = ReservationState' "payment-pending"

pattern ReservationState_Retired :: ReservationState
pattern ReservationState_Retired = ReservationState' "retired"

{-# COMPLETE
  ReservationState_Active,
  ReservationState_Payment_failed,
  ReservationState_Payment_pending,
  ReservationState_Retired,
  ReservationState'
  #-}
