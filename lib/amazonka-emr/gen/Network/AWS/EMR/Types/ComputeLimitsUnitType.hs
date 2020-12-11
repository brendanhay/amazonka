-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ComputeLimitsUnitType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ComputeLimitsUnitType
  ( ComputeLimitsUnitType
      ( ComputeLimitsUnitType',
        InstanceFleetUnits,
        Instances,
        Vcpu
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ComputeLimitsUnitType = ComputeLimitsUnitType' Lude.Text
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

pattern InstanceFleetUnits :: ComputeLimitsUnitType
pattern InstanceFleetUnits = ComputeLimitsUnitType' "InstanceFleetUnits"

pattern Instances :: ComputeLimitsUnitType
pattern Instances = ComputeLimitsUnitType' "Instances"

pattern Vcpu :: ComputeLimitsUnitType
pattern Vcpu = ComputeLimitsUnitType' "VCPU"

{-# COMPLETE
  InstanceFleetUnits,
  Instances,
  Vcpu,
  ComputeLimitsUnitType'
  #-}
