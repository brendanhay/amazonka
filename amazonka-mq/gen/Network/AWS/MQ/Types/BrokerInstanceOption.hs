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
-- Module      : Network.AWS.MQ.Types.BrokerInstanceOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.BrokerInstanceOption where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.AvailabilityZone
import Network.AWS.MQ.Types.BrokerStorageType
import Network.AWS.MQ.Types.DeploymentMode
import Network.AWS.MQ.Types.EngineType

-- | Option for host instance type.
--
-- /See:/ 'newBrokerInstanceOption' smart constructor.
data BrokerInstanceOption = BrokerInstanceOption'
  { -- | The list of available az.
    availabilityZones :: Core.Maybe [AvailabilityZone],
    -- | The broker\'s storage type.
    storageType :: Core.Maybe BrokerStorageType,
    -- | The type of broker engine.
    engineType :: Core.Maybe EngineType,
    -- | The list of supported deployment modes.
    supportedDeploymentModes :: Core.Maybe [DeploymentMode],
    -- | The list of supported engine versions.
    supportedEngineVersions :: Core.Maybe [Core.Text],
    -- | The type of broker instance.
    hostInstanceType :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BrokerInstanceOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'brokerInstanceOption_availabilityZones' - The list of available az.
--
-- 'storageType', 'brokerInstanceOption_storageType' - The broker\'s storage type.
--
-- 'engineType', 'brokerInstanceOption_engineType' - The type of broker engine.
--
-- 'supportedDeploymentModes', 'brokerInstanceOption_supportedDeploymentModes' - The list of supported deployment modes.
--
-- 'supportedEngineVersions', 'brokerInstanceOption_supportedEngineVersions' - The list of supported engine versions.
--
-- 'hostInstanceType', 'brokerInstanceOption_hostInstanceType' - The type of broker instance.
newBrokerInstanceOption ::
  BrokerInstanceOption
newBrokerInstanceOption =
  BrokerInstanceOption'
    { availabilityZones =
        Core.Nothing,
      storageType = Core.Nothing,
      engineType = Core.Nothing,
      supportedDeploymentModes = Core.Nothing,
      supportedEngineVersions = Core.Nothing,
      hostInstanceType = Core.Nothing
    }

-- | The list of available az.
brokerInstanceOption_availabilityZones :: Lens.Lens' BrokerInstanceOption (Core.Maybe [AvailabilityZone])
brokerInstanceOption_availabilityZones = Lens.lens (\BrokerInstanceOption' {availabilityZones} -> availabilityZones) (\s@BrokerInstanceOption' {} a -> s {availabilityZones = a} :: BrokerInstanceOption) Core.. Lens.mapping Lens._Coerce

-- | The broker\'s storage type.
brokerInstanceOption_storageType :: Lens.Lens' BrokerInstanceOption (Core.Maybe BrokerStorageType)
brokerInstanceOption_storageType = Lens.lens (\BrokerInstanceOption' {storageType} -> storageType) (\s@BrokerInstanceOption' {} a -> s {storageType = a} :: BrokerInstanceOption)

-- | The type of broker engine.
brokerInstanceOption_engineType :: Lens.Lens' BrokerInstanceOption (Core.Maybe EngineType)
brokerInstanceOption_engineType = Lens.lens (\BrokerInstanceOption' {engineType} -> engineType) (\s@BrokerInstanceOption' {} a -> s {engineType = a} :: BrokerInstanceOption)

-- | The list of supported deployment modes.
brokerInstanceOption_supportedDeploymentModes :: Lens.Lens' BrokerInstanceOption (Core.Maybe [DeploymentMode])
brokerInstanceOption_supportedDeploymentModes = Lens.lens (\BrokerInstanceOption' {supportedDeploymentModes} -> supportedDeploymentModes) (\s@BrokerInstanceOption' {} a -> s {supportedDeploymentModes = a} :: BrokerInstanceOption) Core.. Lens.mapping Lens._Coerce

-- | The list of supported engine versions.
brokerInstanceOption_supportedEngineVersions :: Lens.Lens' BrokerInstanceOption (Core.Maybe [Core.Text])
brokerInstanceOption_supportedEngineVersions = Lens.lens (\BrokerInstanceOption' {supportedEngineVersions} -> supportedEngineVersions) (\s@BrokerInstanceOption' {} a -> s {supportedEngineVersions = a} :: BrokerInstanceOption) Core.. Lens.mapping Lens._Coerce

-- | The type of broker instance.
brokerInstanceOption_hostInstanceType :: Lens.Lens' BrokerInstanceOption (Core.Maybe Core.Text)
brokerInstanceOption_hostInstanceType = Lens.lens (\BrokerInstanceOption' {hostInstanceType} -> hostInstanceType) (\s@BrokerInstanceOption' {} a -> s {hostInstanceType = a} :: BrokerInstanceOption)

instance Core.FromJSON BrokerInstanceOption where
  parseJSON =
    Core.withObject
      "BrokerInstanceOption"
      ( \x ->
          BrokerInstanceOption'
            Core.<$> (x Core..:? "availabilityZones" Core..!= Core.mempty)
            Core.<*> (x Core..:? "storageType")
            Core.<*> (x Core..:? "engineType")
            Core.<*> ( x Core..:? "supportedDeploymentModes"
                         Core..!= Core.mempty
                     )
            Core.<*> ( x Core..:? "supportedEngineVersions"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "hostInstanceType")
      )

instance Core.Hashable BrokerInstanceOption

instance Core.NFData BrokerInstanceOption
