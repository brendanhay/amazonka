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
-- Module      : Amazonka.EMR.Types.ComputeLimitsUnitType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.ComputeLimitsUnitType
  ( ComputeLimitsUnitType
      ( ..,
        ComputeLimitsUnitType_InstanceFleetUnits,
        ComputeLimitsUnitType_Instances,
        ComputeLimitsUnitType_VCPU
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ComputeLimitsUnitType = ComputeLimitsUnitType'
  { fromComputeLimitsUnitType ::
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

pattern ComputeLimitsUnitType_InstanceFleetUnits :: ComputeLimitsUnitType
pattern ComputeLimitsUnitType_InstanceFleetUnits = ComputeLimitsUnitType' "InstanceFleetUnits"

pattern ComputeLimitsUnitType_Instances :: ComputeLimitsUnitType
pattern ComputeLimitsUnitType_Instances = ComputeLimitsUnitType' "Instances"

pattern ComputeLimitsUnitType_VCPU :: ComputeLimitsUnitType
pattern ComputeLimitsUnitType_VCPU = ComputeLimitsUnitType' "VCPU"

{-# COMPLETE
  ComputeLimitsUnitType_InstanceFleetUnits,
  ComputeLimitsUnitType_Instances,
  ComputeLimitsUnitType_VCPU,
  ComputeLimitsUnitType'
  #-}
