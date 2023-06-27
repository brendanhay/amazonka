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
-- Module      : Amazonka.Athena.Types.CapacityReservationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.CapacityReservationStatus
  ( CapacityReservationStatus
      ( ..,
        CapacityReservationStatus_ACTIVE,
        CapacityReservationStatus_CANCELLED,
        CapacityReservationStatus_CANCELLING,
        CapacityReservationStatus_FAILED,
        CapacityReservationStatus_PENDING,
        CapacityReservationStatus_UPDATE_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CapacityReservationStatus = CapacityReservationStatus'
  { fromCapacityReservationStatus ::
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

pattern CapacityReservationStatus_ACTIVE :: CapacityReservationStatus
pattern CapacityReservationStatus_ACTIVE = CapacityReservationStatus' "ACTIVE"

pattern CapacityReservationStatus_CANCELLED :: CapacityReservationStatus
pattern CapacityReservationStatus_CANCELLED = CapacityReservationStatus' "CANCELLED"

pattern CapacityReservationStatus_CANCELLING :: CapacityReservationStatus
pattern CapacityReservationStatus_CANCELLING = CapacityReservationStatus' "CANCELLING"

pattern CapacityReservationStatus_FAILED :: CapacityReservationStatus
pattern CapacityReservationStatus_FAILED = CapacityReservationStatus' "FAILED"

pattern CapacityReservationStatus_PENDING :: CapacityReservationStatus
pattern CapacityReservationStatus_PENDING = CapacityReservationStatus' "PENDING"

pattern CapacityReservationStatus_UPDATE_PENDING :: CapacityReservationStatus
pattern CapacityReservationStatus_UPDATE_PENDING = CapacityReservationStatus' "UPDATE_PENDING"

{-# COMPLETE
  CapacityReservationStatus_ACTIVE,
  CapacityReservationStatus_CANCELLED,
  CapacityReservationStatus_CANCELLING,
  CapacityReservationStatus_FAILED,
  CapacityReservationStatus_PENDING,
  CapacityReservationStatus_UPDATE_PENDING,
  CapacityReservationStatus'
  #-}
