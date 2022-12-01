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
-- Module      : Amazonka.Discovery.Types.CustomerMeCollectorInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Types.CustomerMeCollectorInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The inventory data for installed Migration Evaluator collectors.
--
-- /See:/ 'newCustomerMeCollectorInfo' smart constructor.
data CustomerMeCollectorInfo = CustomerMeCollectorInfo'
  { -- | The number of active Migration Evaluator collectors.
    activeMeCollectors :: Prelude.Int,
    -- | The number of healthy Migration Evaluator collectors.
    healthyMeCollectors :: Prelude.Int,
    -- | The number of deny-listed Migration Evaluator collectors.
    denyListedMeCollectors :: Prelude.Int,
    -- | The number of Migration Evaluator collectors with @SHUTDOWN@ status.
    shutdownMeCollectors :: Prelude.Int,
    -- | The number of unhealthy Migration Evaluator collectors.
    unhealthyMeCollectors :: Prelude.Int,
    -- | The total number of Migration Evaluator collectors.
    totalMeCollectors :: Prelude.Int,
    -- | The number of unknown Migration Evaluator collectors.
    unknownMeCollectors :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomerMeCollectorInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeMeCollectors', 'customerMeCollectorInfo_activeMeCollectors' - The number of active Migration Evaluator collectors.
--
-- 'healthyMeCollectors', 'customerMeCollectorInfo_healthyMeCollectors' - The number of healthy Migration Evaluator collectors.
--
-- 'denyListedMeCollectors', 'customerMeCollectorInfo_denyListedMeCollectors' - The number of deny-listed Migration Evaluator collectors.
--
-- 'shutdownMeCollectors', 'customerMeCollectorInfo_shutdownMeCollectors' - The number of Migration Evaluator collectors with @SHUTDOWN@ status.
--
-- 'unhealthyMeCollectors', 'customerMeCollectorInfo_unhealthyMeCollectors' - The number of unhealthy Migration Evaluator collectors.
--
-- 'totalMeCollectors', 'customerMeCollectorInfo_totalMeCollectors' - The total number of Migration Evaluator collectors.
--
-- 'unknownMeCollectors', 'customerMeCollectorInfo_unknownMeCollectors' - The number of unknown Migration Evaluator collectors.
newCustomerMeCollectorInfo ::
  -- | 'activeMeCollectors'
  Prelude.Int ->
  -- | 'healthyMeCollectors'
  Prelude.Int ->
  -- | 'denyListedMeCollectors'
  Prelude.Int ->
  -- | 'shutdownMeCollectors'
  Prelude.Int ->
  -- | 'unhealthyMeCollectors'
  Prelude.Int ->
  -- | 'totalMeCollectors'
  Prelude.Int ->
  -- | 'unknownMeCollectors'
  Prelude.Int ->
  CustomerMeCollectorInfo
newCustomerMeCollectorInfo
  pActiveMeCollectors_
  pHealthyMeCollectors_
  pDenyListedMeCollectors_
  pShutdownMeCollectors_
  pUnhealthyMeCollectors_
  pTotalMeCollectors_
  pUnknownMeCollectors_ =
    CustomerMeCollectorInfo'
      { activeMeCollectors =
          pActiveMeCollectors_,
        healthyMeCollectors = pHealthyMeCollectors_,
        denyListedMeCollectors = pDenyListedMeCollectors_,
        shutdownMeCollectors = pShutdownMeCollectors_,
        unhealthyMeCollectors = pUnhealthyMeCollectors_,
        totalMeCollectors = pTotalMeCollectors_,
        unknownMeCollectors = pUnknownMeCollectors_
      }

-- | The number of active Migration Evaluator collectors.
customerMeCollectorInfo_activeMeCollectors :: Lens.Lens' CustomerMeCollectorInfo Prelude.Int
customerMeCollectorInfo_activeMeCollectors = Lens.lens (\CustomerMeCollectorInfo' {activeMeCollectors} -> activeMeCollectors) (\s@CustomerMeCollectorInfo' {} a -> s {activeMeCollectors = a} :: CustomerMeCollectorInfo)

-- | The number of healthy Migration Evaluator collectors.
customerMeCollectorInfo_healthyMeCollectors :: Lens.Lens' CustomerMeCollectorInfo Prelude.Int
customerMeCollectorInfo_healthyMeCollectors = Lens.lens (\CustomerMeCollectorInfo' {healthyMeCollectors} -> healthyMeCollectors) (\s@CustomerMeCollectorInfo' {} a -> s {healthyMeCollectors = a} :: CustomerMeCollectorInfo)

-- | The number of deny-listed Migration Evaluator collectors.
customerMeCollectorInfo_denyListedMeCollectors :: Lens.Lens' CustomerMeCollectorInfo Prelude.Int
customerMeCollectorInfo_denyListedMeCollectors = Lens.lens (\CustomerMeCollectorInfo' {denyListedMeCollectors} -> denyListedMeCollectors) (\s@CustomerMeCollectorInfo' {} a -> s {denyListedMeCollectors = a} :: CustomerMeCollectorInfo)

-- | The number of Migration Evaluator collectors with @SHUTDOWN@ status.
customerMeCollectorInfo_shutdownMeCollectors :: Lens.Lens' CustomerMeCollectorInfo Prelude.Int
customerMeCollectorInfo_shutdownMeCollectors = Lens.lens (\CustomerMeCollectorInfo' {shutdownMeCollectors} -> shutdownMeCollectors) (\s@CustomerMeCollectorInfo' {} a -> s {shutdownMeCollectors = a} :: CustomerMeCollectorInfo)

-- | The number of unhealthy Migration Evaluator collectors.
customerMeCollectorInfo_unhealthyMeCollectors :: Lens.Lens' CustomerMeCollectorInfo Prelude.Int
customerMeCollectorInfo_unhealthyMeCollectors = Lens.lens (\CustomerMeCollectorInfo' {unhealthyMeCollectors} -> unhealthyMeCollectors) (\s@CustomerMeCollectorInfo' {} a -> s {unhealthyMeCollectors = a} :: CustomerMeCollectorInfo)

-- | The total number of Migration Evaluator collectors.
customerMeCollectorInfo_totalMeCollectors :: Lens.Lens' CustomerMeCollectorInfo Prelude.Int
customerMeCollectorInfo_totalMeCollectors = Lens.lens (\CustomerMeCollectorInfo' {totalMeCollectors} -> totalMeCollectors) (\s@CustomerMeCollectorInfo' {} a -> s {totalMeCollectors = a} :: CustomerMeCollectorInfo)

-- | The number of unknown Migration Evaluator collectors.
customerMeCollectorInfo_unknownMeCollectors :: Lens.Lens' CustomerMeCollectorInfo Prelude.Int
customerMeCollectorInfo_unknownMeCollectors = Lens.lens (\CustomerMeCollectorInfo' {unknownMeCollectors} -> unknownMeCollectors) (\s@CustomerMeCollectorInfo' {} a -> s {unknownMeCollectors = a} :: CustomerMeCollectorInfo)

instance Core.FromJSON CustomerMeCollectorInfo where
  parseJSON =
    Core.withObject
      "CustomerMeCollectorInfo"
      ( \x ->
          CustomerMeCollectorInfo'
            Prelude.<$> (x Core..: "activeMeCollectors")
            Prelude.<*> (x Core..: "healthyMeCollectors")
            Prelude.<*> (x Core..: "denyListedMeCollectors")
            Prelude.<*> (x Core..: "shutdownMeCollectors")
            Prelude.<*> (x Core..: "unhealthyMeCollectors")
            Prelude.<*> (x Core..: "totalMeCollectors")
            Prelude.<*> (x Core..: "unknownMeCollectors")
      )

instance Prelude.Hashable CustomerMeCollectorInfo where
  hashWithSalt _salt CustomerMeCollectorInfo' {..} =
    _salt `Prelude.hashWithSalt` activeMeCollectors
      `Prelude.hashWithSalt` healthyMeCollectors
      `Prelude.hashWithSalt` denyListedMeCollectors
      `Prelude.hashWithSalt` shutdownMeCollectors
      `Prelude.hashWithSalt` unhealthyMeCollectors
      `Prelude.hashWithSalt` totalMeCollectors
      `Prelude.hashWithSalt` unknownMeCollectors

instance Prelude.NFData CustomerMeCollectorInfo where
  rnf CustomerMeCollectorInfo' {..} =
    Prelude.rnf activeMeCollectors
      `Prelude.seq` Prelude.rnf healthyMeCollectors
      `Prelude.seq` Prelude.rnf denyListedMeCollectors
      `Prelude.seq` Prelude.rnf shutdownMeCollectors
      `Prelude.seq` Prelude.rnf unhealthyMeCollectors
      `Prelude.seq` Prelude.rnf totalMeCollectors
      `Prelude.seq` Prelude.rnf unknownMeCollectors
