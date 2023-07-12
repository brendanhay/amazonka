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
-- Module      : Amazonka.EC2.Types.ReservationState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ReservationState
  ( ReservationState
      ( ..,
        ReservationState_Active,
        ReservationState_Payment_failed,
        ReservationState_Payment_pending,
        ReservationState_Retired
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ReservationState = ReservationState'
  { fromReservationState ::
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
