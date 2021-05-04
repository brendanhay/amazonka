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
-- Module      : Network.AWS.Discovery.Types.CustomerConnectorInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.CustomerConnectorInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Inventory data for installed discovery connectors.
--
-- /See:/ 'newCustomerConnectorInfo' smart constructor.
data CustomerConnectorInfo = CustomerConnectorInfo'
  { -- | Number of active discovery connectors.
    activeConnectors :: Prelude.Int,
    -- | Number of healthy discovery connectors.
    healthyConnectors :: Prelude.Int,
    -- | Number of blacklisted discovery connectors.
    blackListedConnectors :: Prelude.Int,
    -- | Number of discovery connectors with status SHUTDOWN,
    shutdownConnectors :: Prelude.Int,
    -- | Number of unhealthy discovery connectors.
    unhealthyConnectors :: Prelude.Int,
    -- | Total number of discovery connectors.
    totalConnectors :: Prelude.Int,
    -- | Number of unknown discovery connectors.
    unknownConnectors :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'healthyConnectors'
  Prelude.Int ->
  -- | 'blackListedConnectors'
  Prelude.Int ->
  -- | 'shutdownConnectors'
  Prelude.Int ->
  -- | 'unhealthyConnectors'
  Prelude.Int ->
  -- | 'totalConnectors'
  Prelude.Int ->
  -- | 'unknownConnectors'
  Prelude.Int ->
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
customerConnectorInfo_activeConnectors :: Lens.Lens' CustomerConnectorInfo Prelude.Int
customerConnectorInfo_activeConnectors = Lens.lens (\CustomerConnectorInfo' {activeConnectors} -> activeConnectors) (\s@CustomerConnectorInfo' {} a -> s {activeConnectors = a} :: CustomerConnectorInfo)

-- | Number of healthy discovery connectors.
customerConnectorInfo_healthyConnectors :: Lens.Lens' CustomerConnectorInfo Prelude.Int
customerConnectorInfo_healthyConnectors = Lens.lens (\CustomerConnectorInfo' {healthyConnectors} -> healthyConnectors) (\s@CustomerConnectorInfo' {} a -> s {healthyConnectors = a} :: CustomerConnectorInfo)

-- | Number of blacklisted discovery connectors.
customerConnectorInfo_blackListedConnectors :: Lens.Lens' CustomerConnectorInfo Prelude.Int
customerConnectorInfo_blackListedConnectors = Lens.lens (\CustomerConnectorInfo' {blackListedConnectors} -> blackListedConnectors) (\s@CustomerConnectorInfo' {} a -> s {blackListedConnectors = a} :: CustomerConnectorInfo)

-- | Number of discovery connectors with status SHUTDOWN,
customerConnectorInfo_shutdownConnectors :: Lens.Lens' CustomerConnectorInfo Prelude.Int
customerConnectorInfo_shutdownConnectors = Lens.lens (\CustomerConnectorInfo' {shutdownConnectors} -> shutdownConnectors) (\s@CustomerConnectorInfo' {} a -> s {shutdownConnectors = a} :: CustomerConnectorInfo)

-- | Number of unhealthy discovery connectors.
customerConnectorInfo_unhealthyConnectors :: Lens.Lens' CustomerConnectorInfo Prelude.Int
customerConnectorInfo_unhealthyConnectors = Lens.lens (\CustomerConnectorInfo' {unhealthyConnectors} -> unhealthyConnectors) (\s@CustomerConnectorInfo' {} a -> s {unhealthyConnectors = a} :: CustomerConnectorInfo)

-- | Total number of discovery connectors.
customerConnectorInfo_totalConnectors :: Lens.Lens' CustomerConnectorInfo Prelude.Int
customerConnectorInfo_totalConnectors = Lens.lens (\CustomerConnectorInfo' {totalConnectors} -> totalConnectors) (\s@CustomerConnectorInfo' {} a -> s {totalConnectors = a} :: CustomerConnectorInfo)

-- | Number of unknown discovery connectors.
customerConnectorInfo_unknownConnectors :: Lens.Lens' CustomerConnectorInfo Prelude.Int
customerConnectorInfo_unknownConnectors = Lens.lens (\CustomerConnectorInfo' {unknownConnectors} -> unknownConnectors) (\s@CustomerConnectorInfo' {} a -> s {unknownConnectors = a} :: CustomerConnectorInfo)

instance Prelude.FromJSON CustomerConnectorInfo where
  parseJSON =
    Prelude.withObject
      "CustomerConnectorInfo"
      ( \x ->
          CustomerConnectorInfo'
            Prelude.<$> (x Prelude..: "activeConnectors")
            Prelude.<*> (x Prelude..: "healthyConnectors")
            Prelude.<*> (x Prelude..: "blackListedConnectors")
            Prelude.<*> (x Prelude..: "shutdownConnectors")
            Prelude.<*> (x Prelude..: "unhealthyConnectors")
            Prelude.<*> (x Prelude..: "totalConnectors")
            Prelude.<*> (x Prelude..: "unknownConnectors")
      )

instance Prelude.Hashable CustomerConnectorInfo

instance Prelude.NFData CustomerConnectorInfo
