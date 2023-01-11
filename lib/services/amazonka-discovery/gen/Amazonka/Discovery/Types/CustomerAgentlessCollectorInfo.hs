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

-- | /See:/ 'newCustomerAgentlessCollectorInfo' smart constructor.
data CustomerAgentlessCollectorInfo = CustomerAgentlessCollectorInfo'
  { activeAgentlessCollectors :: Prelude.Int,
    healthyAgentlessCollectors :: Prelude.Int,
    denyListedAgentlessCollectors :: Prelude.Int,
    shutdownAgentlessCollectors :: Prelude.Int,
    unhealthyAgentlessCollectors :: Prelude.Int,
    totalAgentlessCollectors :: Prelude.Int,
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
-- 'activeAgentlessCollectors', 'customerAgentlessCollectorInfo_activeAgentlessCollectors' - Undocumented member.
--
-- 'healthyAgentlessCollectors', 'customerAgentlessCollectorInfo_healthyAgentlessCollectors' - Undocumented member.
--
-- 'denyListedAgentlessCollectors', 'customerAgentlessCollectorInfo_denyListedAgentlessCollectors' - Undocumented member.
--
-- 'shutdownAgentlessCollectors', 'customerAgentlessCollectorInfo_shutdownAgentlessCollectors' - Undocumented member.
--
-- 'unhealthyAgentlessCollectors', 'customerAgentlessCollectorInfo_unhealthyAgentlessCollectors' - Undocumented member.
--
-- 'totalAgentlessCollectors', 'customerAgentlessCollectorInfo_totalAgentlessCollectors' - Undocumented member.
--
-- 'unknownAgentlessCollectors', 'customerAgentlessCollectorInfo_unknownAgentlessCollectors' - Undocumented member.
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

-- | Undocumented member.
customerAgentlessCollectorInfo_activeAgentlessCollectors :: Lens.Lens' CustomerAgentlessCollectorInfo Prelude.Int
customerAgentlessCollectorInfo_activeAgentlessCollectors = Lens.lens (\CustomerAgentlessCollectorInfo' {activeAgentlessCollectors} -> activeAgentlessCollectors) (\s@CustomerAgentlessCollectorInfo' {} a -> s {activeAgentlessCollectors = a} :: CustomerAgentlessCollectorInfo)

-- | Undocumented member.
customerAgentlessCollectorInfo_healthyAgentlessCollectors :: Lens.Lens' CustomerAgentlessCollectorInfo Prelude.Int
customerAgentlessCollectorInfo_healthyAgentlessCollectors = Lens.lens (\CustomerAgentlessCollectorInfo' {healthyAgentlessCollectors} -> healthyAgentlessCollectors) (\s@CustomerAgentlessCollectorInfo' {} a -> s {healthyAgentlessCollectors = a} :: CustomerAgentlessCollectorInfo)

-- | Undocumented member.
customerAgentlessCollectorInfo_denyListedAgentlessCollectors :: Lens.Lens' CustomerAgentlessCollectorInfo Prelude.Int
customerAgentlessCollectorInfo_denyListedAgentlessCollectors = Lens.lens (\CustomerAgentlessCollectorInfo' {denyListedAgentlessCollectors} -> denyListedAgentlessCollectors) (\s@CustomerAgentlessCollectorInfo' {} a -> s {denyListedAgentlessCollectors = a} :: CustomerAgentlessCollectorInfo)

-- | Undocumented member.
customerAgentlessCollectorInfo_shutdownAgentlessCollectors :: Lens.Lens' CustomerAgentlessCollectorInfo Prelude.Int
customerAgentlessCollectorInfo_shutdownAgentlessCollectors = Lens.lens (\CustomerAgentlessCollectorInfo' {shutdownAgentlessCollectors} -> shutdownAgentlessCollectors) (\s@CustomerAgentlessCollectorInfo' {} a -> s {shutdownAgentlessCollectors = a} :: CustomerAgentlessCollectorInfo)

-- | Undocumented member.
customerAgentlessCollectorInfo_unhealthyAgentlessCollectors :: Lens.Lens' CustomerAgentlessCollectorInfo Prelude.Int
customerAgentlessCollectorInfo_unhealthyAgentlessCollectors = Lens.lens (\CustomerAgentlessCollectorInfo' {unhealthyAgentlessCollectors} -> unhealthyAgentlessCollectors) (\s@CustomerAgentlessCollectorInfo' {} a -> s {unhealthyAgentlessCollectors = a} :: CustomerAgentlessCollectorInfo)

-- | Undocumented member.
customerAgentlessCollectorInfo_totalAgentlessCollectors :: Lens.Lens' CustomerAgentlessCollectorInfo Prelude.Int
customerAgentlessCollectorInfo_totalAgentlessCollectors = Lens.lens (\CustomerAgentlessCollectorInfo' {totalAgentlessCollectors} -> totalAgentlessCollectors) (\s@CustomerAgentlessCollectorInfo' {} a -> s {totalAgentlessCollectors = a} :: CustomerAgentlessCollectorInfo)

-- | Undocumented member.
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
