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
-- Module      : Network.AWS.MQ.Types.BrokerInstanceOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.BrokerInstanceOption where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types.AvailabilityZone
import Network.AWS.MQ.Types.BrokerStorageType
import Network.AWS.MQ.Types.DeploymentMode
import Network.AWS.MQ.Types.EngineType
import qualified Network.AWS.Prelude as Prelude

-- | Option for host instance type.
--
-- /See:/ 'newBrokerInstanceOption' smart constructor.
data BrokerInstanceOption = BrokerInstanceOption'
  { -- | The list of available az.
    availabilityZones :: Prelude.Maybe [AvailabilityZone],
    -- | The broker\'s storage type.
    storageType :: Prelude.Maybe BrokerStorageType,
    -- | The type of broker engine.
    engineType :: Prelude.Maybe EngineType,
    -- | The list of supported deployment modes.
    supportedDeploymentModes :: Prelude.Maybe [DeploymentMode],
    -- | The list of supported engine versions.
    supportedEngineVersions :: Prelude.Maybe [Prelude.Text],
    -- | The type of broker instance.
    hostInstanceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      storageType = Prelude.Nothing,
      engineType = Prelude.Nothing,
      supportedDeploymentModes = Prelude.Nothing,
      supportedEngineVersions = Prelude.Nothing,
      hostInstanceType = Prelude.Nothing
    }

-- | The list of available az.
brokerInstanceOption_availabilityZones :: Lens.Lens' BrokerInstanceOption (Prelude.Maybe [AvailabilityZone])
brokerInstanceOption_availabilityZones = Lens.lens (\BrokerInstanceOption' {availabilityZones} -> availabilityZones) (\s@BrokerInstanceOption' {} a -> s {availabilityZones = a} :: BrokerInstanceOption) Prelude.. Lens.mapping Prelude._Coerce

-- | The broker\'s storage type.
brokerInstanceOption_storageType :: Lens.Lens' BrokerInstanceOption (Prelude.Maybe BrokerStorageType)
brokerInstanceOption_storageType = Lens.lens (\BrokerInstanceOption' {storageType} -> storageType) (\s@BrokerInstanceOption' {} a -> s {storageType = a} :: BrokerInstanceOption)

-- | The type of broker engine.
brokerInstanceOption_engineType :: Lens.Lens' BrokerInstanceOption (Prelude.Maybe EngineType)
brokerInstanceOption_engineType = Lens.lens (\BrokerInstanceOption' {engineType} -> engineType) (\s@BrokerInstanceOption' {} a -> s {engineType = a} :: BrokerInstanceOption)

-- | The list of supported deployment modes.
brokerInstanceOption_supportedDeploymentModes :: Lens.Lens' BrokerInstanceOption (Prelude.Maybe [DeploymentMode])
brokerInstanceOption_supportedDeploymentModes = Lens.lens (\BrokerInstanceOption' {supportedDeploymentModes} -> supportedDeploymentModes) (\s@BrokerInstanceOption' {} a -> s {supportedDeploymentModes = a} :: BrokerInstanceOption) Prelude.. Lens.mapping Prelude._Coerce

-- | The list of supported engine versions.
brokerInstanceOption_supportedEngineVersions :: Lens.Lens' BrokerInstanceOption (Prelude.Maybe [Prelude.Text])
brokerInstanceOption_supportedEngineVersions = Lens.lens (\BrokerInstanceOption' {supportedEngineVersions} -> supportedEngineVersions) (\s@BrokerInstanceOption' {} a -> s {supportedEngineVersions = a} :: BrokerInstanceOption) Prelude.. Lens.mapping Prelude._Coerce

-- | The type of broker instance.
brokerInstanceOption_hostInstanceType :: Lens.Lens' BrokerInstanceOption (Prelude.Maybe Prelude.Text)
brokerInstanceOption_hostInstanceType = Lens.lens (\BrokerInstanceOption' {hostInstanceType} -> hostInstanceType) (\s@BrokerInstanceOption' {} a -> s {hostInstanceType = a} :: BrokerInstanceOption)

instance Prelude.FromJSON BrokerInstanceOption where
  parseJSON =
    Prelude.withObject
      "BrokerInstanceOption"
      ( \x ->
          BrokerInstanceOption'
            Prelude.<$> ( x Prelude..:? "availabilityZones"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "storageType")
            Prelude.<*> (x Prelude..:? "engineType")
            Prelude.<*> ( x Prelude..:? "supportedDeploymentModes"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "supportedEngineVersions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "hostInstanceType")
      )

instance Prelude.Hashable BrokerInstanceOption

instance Prelude.NFData BrokerInstanceOption
