{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetCapacitySpecificationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetCapacitySpecificationRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DefaultTargetCapacityType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The number of units to request. You can choose to set the target
-- capacity as the number of instances. Or you can set the target capacity
-- to a performance characteristic that is important to your application
-- workload, such as vCPUs, memory, or I\/O. If the request type is
-- @maintain@, you can specify a target capacity of 0 and add capacity
-- later.
--
-- You can use the On-Demand Instance @MaxTotalPrice@ parameter, the Spot
-- Instance @MaxTotalPrice@ parameter, or both parameters to ensure that
-- your fleet cost does not exceed your budget. If you set a maximum price
-- per hour for the On-Demand Instances and Spot Instances in your request,
-- EC2 Fleet will launch instances until it reaches the maximum amount that
-- you\'re willing to pay. When the maximum amount you\'re willing to pay
-- is reached, the fleet stops launching instances even if it hasn’t met
-- the target capacity. The @MaxTotalPrice@ parameters are located in
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_OnDemandOptionsRequest OnDemandOptionsRequest>
-- and
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotOptionsRequest SpotOptionsRequest>.
--
-- /See:/ 'newTargetCapacitySpecificationRequest' smart constructor.
data TargetCapacitySpecificationRequest = TargetCapacitySpecificationRequest'
  { -- | The default @TotalTargetCapacity@, which is either @Spot@ or
    -- @On-Demand@.
    defaultTargetCapacityType :: Prelude.Maybe DefaultTargetCapacityType,
    -- | The number of On-Demand units to request.
    onDemandTargetCapacity :: Prelude.Maybe Prelude.Int,
    -- | The number of Spot units to request.
    spotTargetCapacity :: Prelude.Maybe Prelude.Int,
    -- | The number of units to request, filled using
    -- @DefaultTargetCapacityType@.
    totalTargetCapacity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TargetCapacitySpecificationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultTargetCapacityType', 'targetCapacitySpecificationRequest_defaultTargetCapacityType' - The default @TotalTargetCapacity@, which is either @Spot@ or
-- @On-Demand@.
--
-- 'onDemandTargetCapacity', 'targetCapacitySpecificationRequest_onDemandTargetCapacity' - The number of On-Demand units to request.
--
-- 'spotTargetCapacity', 'targetCapacitySpecificationRequest_spotTargetCapacity' - The number of Spot units to request.
--
-- 'totalTargetCapacity', 'targetCapacitySpecificationRequest_totalTargetCapacity' - The number of units to request, filled using
-- @DefaultTargetCapacityType@.
newTargetCapacitySpecificationRequest ::
  -- | 'totalTargetCapacity'
  Prelude.Int ->
  TargetCapacitySpecificationRequest
newTargetCapacitySpecificationRequest
  pTotalTargetCapacity_ =
    TargetCapacitySpecificationRequest'
      { defaultTargetCapacityType =
          Prelude.Nothing,
        onDemandTargetCapacity =
          Prelude.Nothing,
        spotTargetCapacity = Prelude.Nothing,
        totalTargetCapacity =
          pTotalTargetCapacity_
      }

-- | The default @TotalTargetCapacity@, which is either @Spot@ or
-- @On-Demand@.
targetCapacitySpecificationRequest_defaultTargetCapacityType :: Lens.Lens' TargetCapacitySpecificationRequest (Prelude.Maybe DefaultTargetCapacityType)
targetCapacitySpecificationRequest_defaultTargetCapacityType = Lens.lens (\TargetCapacitySpecificationRequest' {defaultTargetCapacityType} -> defaultTargetCapacityType) (\s@TargetCapacitySpecificationRequest' {} a -> s {defaultTargetCapacityType = a} :: TargetCapacitySpecificationRequest)

-- | The number of On-Demand units to request.
targetCapacitySpecificationRequest_onDemandTargetCapacity :: Lens.Lens' TargetCapacitySpecificationRequest (Prelude.Maybe Prelude.Int)
targetCapacitySpecificationRequest_onDemandTargetCapacity = Lens.lens (\TargetCapacitySpecificationRequest' {onDemandTargetCapacity} -> onDemandTargetCapacity) (\s@TargetCapacitySpecificationRequest' {} a -> s {onDemandTargetCapacity = a} :: TargetCapacitySpecificationRequest)

-- | The number of Spot units to request.
targetCapacitySpecificationRequest_spotTargetCapacity :: Lens.Lens' TargetCapacitySpecificationRequest (Prelude.Maybe Prelude.Int)
targetCapacitySpecificationRequest_spotTargetCapacity = Lens.lens (\TargetCapacitySpecificationRequest' {spotTargetCapacity} -> spotTargetCapacity) (\s@TargetCapacitySpecificationRequest' {} a -> s {spotTargetCapacity = a} :: TargetCapacitySpecificationRequest)

-- | The number of units to request, filled using
-- @DefaultTargetCapacityType@.
targetCapacitySpecificationRequest_totalTargetCapacity :: Lens.Lens' TargetCapacitySpecificationRequest Prelude.Int
targetCapacitySpecificationRequest_totalTargetCapacity = Lens.lens (\TargetCapacitySpecificationRequest' {totalTargetCapacity} -> totalTargetCapacity) (\s@TargetCapacitySpecificationRequest' {} a -> s {totalTargetCapacity = a} :: TargetCapacitySpecificationRequest)

instance
  Prelude.Hashable
    TargetCapacitySpecificationRequest

instance
  Prelude.NFData
    TargetCapacitySpecificationRequest

instance
  Prelude.ToQuery
    TargetCapacitySpecificationRequest
  where
  toQuery TargetCapacitySpecificationRequest' {..} =
    Prelude.mconcat
      [ "DefaultTargetCapacityType"
          Prelude.=: defaultTargetCapacityType,
        "OnDemandTargetCapacity"
          Prelude.=: onDemandTargetCapacity,
        "SpotTargetCapacity" Prelude.=: spotTargetCapacity,
        "TotalTargetCapacity" Prelude.=: totalTargetCapacity
      ]
