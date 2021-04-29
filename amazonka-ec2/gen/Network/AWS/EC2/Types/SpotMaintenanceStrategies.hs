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
-- Module      : Network.AWS.EC2.Types.SpotMaintenanceStrategies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotMaintenanceStrategies where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.SpotCapacityRebalance
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The strategies for managing your Spot Instances that are at an elevated
-- risk of being interrupted.
--
-- /See:/ 'newSpotMaintenanceStrategies' smart constructor.
data SpotMaintenanceStrategies = SpotMaintenanceStrategies'
  { -- | The strategy to use when Amazon EC2 emits a signal that your Spot
    -- Instance is at an elevated risk of being interrupted.
    capacityRebalance :: Prelude.Maybe SpotCapacityRebalance
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SpotMaintenanceStrategies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityRebalance', 'spotMaintenanceStrategies_capacityRebalance' - The strategy to use when Amazon EC2 emits a signal that your Spot
-- Instance is at an elevated risk of being interrupted.
newSpotMaintenanceStrategies ::
  SpotMaintenanceStrategies
newSpotMaintenanceStrategies =
  SpotMaintenanceStrategies'
    { capacityRebalance =
        Prelude.Nothing
    }

-- | The strategy to use when Amazon EC2 emits a signal that your Spot
-- Instance is at an elevated risk of being interrupted.
spotMaintenanceStrategies_capacityRebalance :: Lens.Lens' SpotMaintenanceStrategies (Prelude.Maybe SpotCapacityRebalance)
spotMaintenanceStrategies_capacityRebalance = Lens.lens (\SpotMaintenanceStrategies' {capacityRebalance} -> capacityRebalance) (\s@SpotMaintenanceStrategies' {} a -> s {capacityRebalance = a} :: SpotMaintenanceStrategies)

instance Prelude.FromXML SpotMaintenanceStrategies where
  parseXML x =
    SpotMaintenanceStrategies'
      Prelude.<$> (x Prelude..@? "capacityRebalance")

instance Prelude.Hashable SpotMaintenanceStrategies

instance Prelude.NFData SpotMaintenanceStrategies

instance Prelude.ToQuery SpotMaintenanceStrategies where
  toQuery SpotMaintenanceStrategies' {..} =
    Prelude.mconcat
      ["CapacityRebalance" Prelude.=: capacityRebalance]
