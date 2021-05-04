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
-- Module      : Network.AWS.EMR.Types.ComputeLimitsUnitType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ComputeLimitsUnitType
  ( ComputeLimitsUnitType
      ( ..,
        ComputeLimitsUnitType_InstanceFleetUnits,
        ComputeLimitsUnitType_Instances,
        ComputeLimitsUnitType_VCPU
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ComputeLimitsUnitType = ComputeLimitsUnitType'
  { fromComputeLimitsUnitType ::
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
