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
-- Module      : Amazonka.EC2.Types.SpotMaintenanceStrategies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SpotMaintenanceStrategies where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.SpotCapacityRebalance
import qualified Amazonka.Prelude as Prelude

-- | The strategies for managing your Spot Instances that are at an elevated
-- risk of being interrupted.
--
-- /See:/ 'newSpotMaintenanceStrategies' smart constructor.
data SpotMaintenanceStrategies = SpotMaintenanceStrategies'
  { -- | The Spot Instance replacement strategy to use when Amazon EC2 emits a
    -- signal that your Spot Instance is at an elevated risk of being
    -- interrupted. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-capacity-rebalance.html Capacity rebalancing>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    capacityRebalance :: Prelude.Maybe SpotCapacityRebalance
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpotMaintenanceStrategies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityRebalance', 'spotMaintenanceStrategies_capacityRebalance' - The Spot Instance replacement strategy to use when Amazon EC2 emits a
-- signal that your Spot Instance is at an elevated risk of being
-- interrupted. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-capacity-rebalance.html Capacity rebalancing>
-- in the /Amazon EC2 User Guide for Linux Instances/.
newSpotMaintenanceStrategies ::
  SpotMaintenanceStrategies
newSpotMaintenanceStrategies =
  SpotMaintenanceStrategies'
    { capacityRebalance =
        Prelude.Nothing
    }

-- | The Spot Instance replacement strategy to use when Amazon EC2 emits a
-- signal that your Spot Instance is at an elevated risk of being
-- interrupted. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-fleet-capacity-rebalance.html Capacity rebalancing>
-- in the /Amazon EC2 User Guide for Linux Instances/.
spotMaintenanceStrategies_capacityRebalance :: Lens.Lens' SpotMaintenanceStrategies (Prelude.Maybe SpotCapacityRebalance)
spotMaintenanceStrategies_capacityRebalance = Lens.lens (\SpotMaintenanceStrategies' {capacityRebalance} -> capacityRebalance) (\s@SpotMaintenanceStrategies' {} a -> s {capacityRebalance = a} :: SpotMaintenanceStrategies)

instance Data.FromXML SpotMaintenanceStrategies where
  parseXML x =
    SpotMaintenanceStrategies'
      Prelude.<$> (x Data..@? "capacityRebalance")

instance Prelude.Hashable SpotMaintenanceStrategies where
  hashWithSalt _salt SpotMaintenanceStrategies' {..} =
    _salt `Prelude.hashWithSalt` capacityRebalance

instance Prelude.NFData SpotMaintenanceStrategies where
  rnf SpotMaintenanceStrategies' {..} =
    Prelude.rnf capacityRebalance

instance Data.ToQuery SpotMaintenanceStrategies where
  toQuery SpotMaintenanceStrategies' {..} =
    Prelude.mconcat
      ["CapacityRebalance" Data.=: capacityRebalance]
