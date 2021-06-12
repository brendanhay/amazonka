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
-- Module      : Network.AWS.Discovery.Types.CustomerConnectorInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.CustomerConnectorInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Inventory data for installed discovery connectors.
--
-- /See:/ 'newCustomerConnectorInfo' smart constructor.
data CustomerConnectorInfo = CustomerConnectorInfo'
  { -- | Number of active discovery connectors.
    activeConnectors :: Core.Int,
    -- | Number of healthy discovery connectors.
    healthyConnectors :: Core.Int,
    -- | Number of blacklisted discovery connectors.
    blackListedConnectors :: Core.Int,
    -- | Number of discovery connectors with status SHUTDOWN,
    shutdownConnectors :: Core.Int,
    -- | Number of unhealthy discovery connectors.
    unhealthyConnectors :: Core.Int,
    -- | Total number of discovery connectors.
    totalConnectors :: Core.Int,
    -- | Number of unknown discovery connectors.
    unknownConnectors :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CustomerConnectorInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeConnectors', 'customerConnectorInfo_activeConnectors' - Number of active discovery connectors.
--
-- 'healthyConnectors', 'customerConnectorInfo_healthyConnectors' - Number of healthy discovery connectors.
--
-- 'blackListedConnectors', 'customerConnectorInfo_blackListedConnectors' - Number of blacklisted discovery connectors.
--
-- 'shutdownConnectors', 'customerConnectorInfo_shutdownConnectors' - Number of discovery connectors with status SHUTDOWN,
--
-- 'unhealthyConnectors', 'customerConnectorInfo_unhealthyConnectors' - Number of unhealthy discovery connectors.
--
-- 'totalConnectors', 'customerConnectorInfo_totalConnectors' - Total number of discovery connectors.
--
-- 'unknownConnectors', 'customerConnectorInfo_unknownConnectors' - Number of unknown discovery connectors.
newCustomerConnectorInfo ::
  -- | 'activeConnectors'
  Core.Int ->
  -- | 'healthyConnectors'
  Core.Int ->
  -- | 'blackListedConnectors'
  Core.Int ->
  -- | 'shutdownConnectors'
  Core.Int ->
  -- | 'unhealthyConnectors'
  Core.Int ->
  -- | 'totalConnectors'
  Core.Int ->
  -- | 'unknownConnectors'
  Core.Int ->
  CustomerConnectorInfo
newCustomerConnectorInfo
  pActiveConnectors_
  pHealthyConnectors_
  pBlackListedConnectors_
  pShutdownConnectors_
  pUnhealthyConnectors_
  pTotalConnectors_
  pUnknownConnectors_ =
    CustomerConnectorInfo'
      { activeConnectors =
          pActiveConnectors_,
        healthyConnectors = pHealthyConnectors_,
        blackListedConnectors = pBlackListedConnectors_,
        shutdownConnectors = pShutdownConnectors_,
        unhealthyConnectors = pUnhealthyConnectors_,
        totalConnectors = pTotalConnectors_,
        unknownConnectors = pUnknownConnectors_
      }

-- | Number of active discovery connectors.
customerConnectorInfo_activeConnectors :: Lens.Lens' CustomerConnectorInfo Core.Int
customerConnectorInfo_activeConnectors = Lens.lens (\CustomerConnectorInfo' {activeConnectors} -> activeConnectors) (\s@CustomerConnectorInfo' {} a -> s {activeConnectors = a} :: CustomerConnectorInfo)

-- | Number of healthy discovery connectors.
customerConnectorInfo_healthyConnectors :: Lens.Lens' CustomerConnectorInfo Core.Int
customerConnectorInfo_healthyConnectors = Lens.lens (\CustomerConnectorInfo' {healthyConnectors} -> healthyConnectors) (\s@CustomerConnectorInfo' {} a -> s {healthyConnectors = a} :: CustomerConnectorInfo)

-- | Number of blacklisted discovery connectors.
customerConnectorInfo_blackListedConnectors :: Lens.Lens' CustomerConnectorInfo Core.Int
customerConnectorInfo_blackListedConnectors = Lens.lens (\CustomerConnectorInfo' {blackListedConnectors} -> blackListedConnectors) (\s@CustomerConnectorInfo' {} a -> s {blackListedConnectors = a} :: CustomerConnectorInfo)

-- | Number of discovery connectors with status SHUTDOWN,
customerConnectorInfo_shutdownConnectors :: Lens.Lens' CustomerConnectorInfo Core.Int
customerConnectorInfo_shutdownConnectors = Lens.lens (\CustomerConnectorInfo' {shutdownConnectors} -> shutdownConnectors) (\s@CustomerConnectorInfo' {} a -> s {shutdownConnectors = a} :: CustomerConnectorInfo)

-- | Number of unhealthy discovery connectors.
customerConnectorInfo_unhealthyConnectors :: Lens.Lens' CustomerConnectorInfo Core.Int
customerConnectorInfo_unhealthyConnectors = Lens.lens (\CustomerConnectorInfo' {unhealthyConnectors} -> unhealthyConnectors) (\s@CustomerConnectorInfo' {} a -> s {unhealthyConnectors = a} :: CustomerConnectorInfo)

-- | Total number of discovery connectors.
customerConnectorInfo_totalConnectors :: Lens.Lens' CustomerConnectorInfo Core.Int
customerConnectorInfo_totalConnectors = Lens.lens (\CustomerConnectorInfo' {totalConnectors} -> totalConnectors) (\s@CustomerConnectorInfo' {} a -> s {totalConnectors = a} :: CustomerConnectorInfo)

-- | Number of unknown discovery connectors.
customerConnectorInfo_unknownConnectors :: Lens.Lens' CustomerConnectorInfo Core.Int
customerConnectorInfo_unknownConnectors = Lens.lens (\CustomerConnectorInfo' {unknownConnectors} -> unknownConnectors) (\s@CustomerConnectorInfo' {} a -> s {unknownConnectors = a} :: CustomerConnectorInfo)

instance Core.FromJSON CustomerConnectorInfo where
  parseJSON =
    Core.withObject
      "CustomerConnectorInfo"
      ( \x ->
          CustomerConnectorInfo'
            Core.<$> (x Core..: "activeConnectors")
            Core.<*> (x Core..: "healthyConnectors")
            Core.<*> (x Core..: "blackListedConnectors")
            Core.<*> (x Core..: "shutdownConnectors")
            Core.<*> (x Core..: "unhealthyConnectors")
            Core.<*> (x Core..: "totalConnectors")
            Core.<*> (x Core..: "unknownConnectors")
      )

instance Core.Hashable CustomerConnectorInfo

instance Core.NFData CustomerConnectorInfo
