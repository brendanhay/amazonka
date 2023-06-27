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
-- Module      : Amazonka.Discovery.Types.CustomerAgentlessCollectorInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Types.CustomerAgentlessCollectorInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The inventory data for installed Agentless Collector collectors.
--
-- /See:/ 'newCustomerAgentlessCollectorInfo' smart constructor.
data CustomerAgentlessCollectorInfo = CustomerAgentlessCollectorInfo'
  { -- | The number of active Agentless Collector collectors.
    activeAgentlessCollectors :: Prelude.Int,
    -- | The number of healthy Agentless Collector collectors.
    healthyAgentlessCollectors :: Prelude.Int,
    -- | The number of deny-listed Agentless Collector collectors.
    denyListedAgentlessCollectors :: Prelude.Int,
    -- | The number of Agentless Collector collectors with @SHUTDOWN@ status.
    shutdownAgentlessCollectors :: Prelude.Int,
    -- | The number of unhealthy Agentless Collector collectors.
    unhealthyAgentlessCollectors :: Prelude.Int,
    -- | The total number of Agentless Collector collectors.
    totalAgentlessCollectors :: Prelude.Int,
    -- | The number of unknown Agentless Collector collectors.
    unknownAgentlessCollectors :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomerAgentlessCollectorInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeAgentlessCollectors', 'customerAgentlessCollectorInfo_activeAgentlessCollectors' - The number of active Agentless Collector collectors.
--
-- 'healthyAgentlessCollectors', 'customerAgentlessCollectorInfo_healthyAgentlessCollectors' - The number of healthy Agentless Collector collectors.
--
-- 'denyListedAgentlessCollectors', 'customerAgentlessCollectorInfo_denyListedAgentlessCollectors' - The number of deny-listed Agentless Collector collectors.
--
-- 'shutdownAgentlessCollectors', 'customerAgentlessCollectorInfo_shutdownAgentlessCollectors' - The number of Agentless Collector collectors with @SHUTDOWN@ status.
--
-- 'unhealthyAgentlessCollectors', 'customerAgentlessCollectorInfo_unhealthyAgentlessCollectors' - The number of unhealthy Agentless Collector collectors.
--
-- 'totalAgentlessCollectors', 'customerAgentlessCollectorInfo_totalAgentlessCollectors' - The total number of Agentless Collector collectors.
--
-- 'unknownAgentlessCollectors', 'customerAgentlessCollectorInfo_unknownAgentlessCollectors' - The number of unknown Agentless Collector collectors.
newCustomerAgentlessCollectorInfo ::
  -- | 'activeAgentlessCollectors'
  Prelude.Int ->
  -- | 'healthyAgentlessCollectors'
  Prelude.Int ->
  -- | 'denyListedAgentlessCollectors'
  Prelude.Int ->
  -- | 'shutdownAgentlessCollectors'
  Prelude.Int ->
  -- | 'unhealthyAgentlessCollectors'
  Prelude.Int ->
  -- | 'totalAgentlessCollectors'
  Prelude.Int ->
  -- | 'unknownAgentlessCollectors'
  Prelude.Int ->
  CustomerAgentlessCollectorInfo
newCustomerAgentlessCollectorInfo
  pActiveAgentlessCollectors_
  pHealthyAgentlessCollectors_
  pDenyListedAgentlessCollectors_
  pShutdownAgentlessCollectors_
  pUnhealthyAgentlessCollectors_
  pTotalAgentlessCollectors_
  pUnknownAgentlessCollectors_ =
    CustomerAgentlessCollectorInfo'
      { activeAgentlessCollectors =
          pActiveAgentlessCollectors_,
        healthyAgentlessCollectors =
          pHealthyAgentlessCollectors_,
        denyListedAgentlessCollectors =
          pDenyListedAgentlessCollectors_,
        shutdownAgentlessCollectors =
          pShutdownAgentlessCollectors_,
        unhealthyAgentlessCollectors =
          pUnhealthyAgentlessCollectors_,
        totalAgentlessCollectors =
          pTotalAgentlessCollectors_,
        unknownAgentlessCollectors =
          pUnknownAgentlessCollectors_
      }

-- | The number of active Agentless Collector collectors.
customerAgentlessCollectorInfo_activeAgentlessCollectors :: Lens.Lens' CustomerAgentlessCollectorInfo Prelude.Int
customerAgentlessCollectorInfo_activeAgentlessCollectors = Lens.lens (\CustomerAgentlessCollectorInfo' {activeAgentlessCollectors} -> activeAgentlessCollectors) (\s@CustomerAgentlessCollectorInfo' {} a -> s {activeAgentlessCollectors = a} :: CustomerAgentlessCollectorInfo)

-- | The number of healthy Agentless Collector collectors.
customerAgentlessCollectorInfo_healthyAgentlessCollectors :: Lens.Lens' CustomerAgentlessCollectorInfo Prelude.Int
customerAgentlessCollectorInfo_healthyAgentlessCollectors = Lens.lens (\CustomerAgentlessCollectorInfo' {healthyAgentlessCollectors} -> healthyAgentlessCollectors) (\s@CustomerAgentlessCollectorInfo' {} a -> s {healthyAgentlessCollectors = a} :: CustomerAgentlessCollectorInfo)

-- | The number of deny-listed Agentless Collector collectors.
customerAgentlessCollectorInfo_denyListedAgentlessCollectors :: Lens.Lens' CustomerAgentlessCollectorInfo Prelude.Int
customerAgentlessCollectorInfo_denyListedAgentlessCollectors = Lens.lens (\CustomerAgentlessCollectorInfo' {denyListedAgentlessCollectors} -> denyListedAgentlessCollectors) (\s@CustomerAgentlessCollectorInfo' {} a -> s {denyListedAgentlessCollectors = a} :: CustomerAgentlessCollectorInfo)

-- | The number of Agentless Collector collectors with @SHUTDOWN@ status.
customerAgentlessCollectorInfo_shutdownAgentlessCollectors :: Lens.Lens' CustomerAgentlessCollectorInfo Prelude.Int
customerAgentlessCollectorInfo_shutdownAgentlessCollectors = Lens.lens (\CustomerAgentlessCollectorInfo' {shutdownAgentlessCollectors} -> shutdownAgentlessCollectors) (\s@CustomerAgentlessCollectorInfo' {} a -> s {shutdownAgentlessCollectors = a} :: CustomerAgentlessCollectorInfo)

-- | The number of unhealthy Agentless Collector collectors.
customerAgentlessCollectorInfo_unhealthyAgentlessCollectors :: Lens.Lens' CustomerAgentlessCollectorInfo Prelude.Int
customerAgentlessCollectorInfo_unhealthyAgentlessCollectors = Lens.lens (\CustomerAgentlessCollectorInfo' {unhealthyAgentlessCollectors} -> unhealthyAgentlessCollectors) (\s@CustomerAgentlessCollectorInfo' {} a -> s {unhealthyAgentlessCollectors = a} :: CustomerAgentlessCollectorInfo)

-- | The total number of Agentless Collector collectors.
customerAgentlessCollectorInfo_totalAgentlessCollectors :: Lens.Lens' CustomerAgentlessCollectorInfo Prelude.Int
customerAgentlessCollectorInfo_totalAgentlessCollectors = Lens.lens (\CustomerAgentlessCollectorInfo' {totalAgentlessCollectors} -> totalAgentlessCollectors) (\s@CustomerAgentlessCollectorInfo' {} a -> s {totalAgentlessCollectors = a} :: CustomerAgentlessCollectorInfo)

-- | The number of unknown Agentless Collector collectors.
customerAgentlessCollectorInfo_unknownAgentlessCollectors :: Lens.Lens' CustomerAgentlessCollectorInfo Prelude.Int
customerAgentlessCollectorInfo_unknownAgentlessCollectors = Lens.lens (\CustomerAgentlessCollectorInfo' {unknownAgentlessCollectors} -> unknownAgentlessCollectors) (\s@CustomerAgentlessCollectorInfo' {} a -> s {unknownAgentlessCollectors = a} :: CustomerAgentlessCollectorInfo)

instance Data.FromJSON CustomerAgentlessCollectorInfo where
  parseJSON =
    Data.withObject
      "CustomerAgentlessCollectorInfo"
      ( \x ->
          CustomerAgentlessCollectorInfo'
            Prelude.<$> (x Data..: "activeAgentlessCollectors")
            Prelude.<*> (x Data..: "healthyAgentlessCollectors")
            Prelude.<*> (x Data..: "denyListedAgentlessCollectors")
            Prelude.<*> (x Data..: "shutdownAgentlessCollectors")
            Prelude.<*> (x Data..: "unhealthyAgentlessCollectors")
            Prelude.<*> (x Data..: "totalAgentlessCollectors")
            Prelude.<*> (x Data..: "unknownAgentlessCollectors")
      )

instance
  Prelude.Hashable
    CustomerAgentlessCollectorInfo
  where
  hashWithSalt
    _salt
    CustomerAgentlessCollectorInfo' {..} =
      _salt
        `Prelude.hashWithSalt` activeAgentlessCollectors
        `Prelude.hashWithSalt` healthyAgentlessCollectors
        `Prelude.hashWithSalt` denyListedAgentlessCollectors
        `Prelude.hashWithSalt` shutdownAgentlessCollectors
        `Prelude.hashWithSalt` unhealthyAgentlessCollectors
        `Prelude.hashWithSalt` totalAgentlessCollectors
        `Prelude.hashWithSalt` unknownAgentlessCollectors

instance
  Prelude.NFData
    CustomerAgentlessCollectorInfo
  where
  rnf CustomerAgentlessCollectorInfo' {..} =
    Prelude.rnf activeAgentlessCollectors
      `Prelude.seq` Prelude.rnf healthyAgentlessCollectors
      `Prelude.seq` Prelude.rnf denyListedAgentlessCollectors
      `Prelude.seq` Prelude.rnf shutdownAgentlessCollectors
      `Prelude.seq` Prelude.rnf unhealthyAgentlessCollectors
      `Prelude.seq` Prelude.rnf totalAgentlessCollectors
      `Prelude.seq` Prelude.rnf unknownAgentlessCollectors
