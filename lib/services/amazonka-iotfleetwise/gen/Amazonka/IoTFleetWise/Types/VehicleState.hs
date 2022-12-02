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
-- Module      : Amazonka.IoTFleetWise.Types.VehicleState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.VehicleState
  ( VehicleState
      ( ..,
        VehicleState_CREATED,
        VehicleState_DELETING,
        VehicleState_HEALTHY,
        VehicleState_READY,
        VehicleState_SUSPENDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VehicleState = VehicleState'
  { fromVehicleState ::
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

pattern VehicleState_CREATED :: VehicleState
pattern VehicleState_CREATED = VehicleState' "CREATED"

pattern VehicleState_DELETING :: VehicleState
pattern VehicleState_DELETING = VehicleState' "DELETING"

pattern VehicleState_HEALTHY :: VehicleState
pattern VehicleState_HEALTHY = VehicleState' "HEALTHY"

pattern VehicleState_READY :: VehicleState
pattern VehicleState_READY = VehicleState' "READY"

pattern VehicleState_SUSPENDED :: VehicleState
pattern VehicleState_SUSPENDED = VehicleState' "SUSPENDED"

{-# COMPLETE
  VehicleState_CREATED,
  VehicleState_DELETING,
  VehicleState_HEALTHY,
  VehicleState_READY,
  VehicleState_SUSPENDED,
  VehicleState'
  #-}
