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
-- Module      : Amazonka.StorageGateway.Types.GatewayInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.GatewayInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.StorageGateway.Types.HostEnvironment

-- | Describes a gateway object.
--
-- /See:/ 'newGatewayInfo' smart constructor.
data GatewayInfo = GatewayInfo'
  { -- | The ID of the Amazon EC2 instance that was used to launch the gateway.
    ec2InstanceId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region where the Amazon EC2 instance is located.
    ec2InstanceRegion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
    -- operation to return a list of gateways for your account and Amazon Web
    -- Services Region.
    gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier assigned to your gateway during activation. This
    -- ID becomes part of the gateway Amazon Resource Name (ARN), which you use
    -- as input for other operations.
    gatewayId :: Prelude.Maybe Prelude.Text,
    -- | The name of the gateway.
    gatewayName :: Prelude.Maybe Prelude.Text,
    -- | The state of the gateway.
    --
    -- Valid Values: @DISABLED@ | @ACTIVE@
    gatewayOperationalState :: Prelude.Maybe Prelude.Text,
    -- | The type of the gateway.
    gatewayType :: Prelude.Maybe Prelude.Text,
    -- | The type of hardware or software platform on which the gateway is
    -- running.
    hostEnvironment :: Prelude.Maybe HostEnvironment,
    -- | A unique identifier for the specific instance of the host platform
    -- running the gateway. This value is only available for certain host
    -- environments, and its format depends on the host environment type.
    hostEnvironmentId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GatewayInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ec2InstanceId', 'gatewayInfo_ec2InstanceId' - The ID of the Amazon EC2 instance that was used to launch the gateway.
--
-- 'ec2InstanceRegion', 'gatewayInfo_ec2InstanceRegion' - The Amazon Web Services Region where the Amazon EC2 instance is located.
--
-- 'gatewayARN', 'gatewayInfo_gatewayARN' - The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and Amazon Web
-- Services Region.
--
-- 'gatewayId', 'gatewayInfo_gatewayId' - The unique identifier assigned to your gateway during activation. This
-- ID becomes part of the gateway Amazon Resource Name (ARN), which you use
-- as input for other operations.
--
-- 'gatewayName', 'gatewayInfo_gatewayName' - The name of the gateway.
--
-- 'gatewayOperationalState', 'gatewayInfo_gatewayOperationalState' - The state of the gateway.
--
-- Valid Values: @DISABLED@ | @ACTIVE@
--
-- 'gatewayType', 'gatewayInfo_gatewayType' - The type of the gateway.
--
-- 'hostEnvironment', 'gatewayInfo_hostEnvironment' - The type of hardware or software platform on which the gateway is
-- running.
--
-- 'hostEnvironmentId', 'gatewayInfo_hostEnvironmentId' - A unique identifier for the specific instance of the host platform
-- running the gateway. This value is only available for certain host
-- environments, and its format depends on the host environment type.
newGatewayInfo ::
  GatewayInfo
newGatewayInfo =
  GatewayInfo'
    { ec2InstanceId = Prelude.Nothing,
      ec2InstanceRegion = Prelude.Nothing,
      gatewayARN = Prelude.Nothing,
      gatewayId = Prelude.Nothing,
      gatewayName = Prelude.Nothing,
      gatewayOperationalState = Prelude.Nothing,
      gatewayType = Prelude.Nothing,
      hostEnvironment = Prelude.Nothing,
      hostEnvironmentId = Prelude.Nothing
    }

-- | The ID of the Amazon EC2 instance that was used to launch the gateway.
gatewayInfo_ec2InstanceId :: Lens.Lens' GatewayInfo (Prelude.Maybe Prelude.Text)
gatewayInfo_ec2InstanceId = Lens.lens (\GatewayInfo' {ec2InstanceId} -> ec2InstanceId) (\s@GatewayInfo' {} a -> s {ec2InstanceId = a} :: GatewayInfo)

-- | The Amazon Web Services Region where the Amazon EC2 instance is located.
gatewayInfo_ec2InstanceRegion :: Lens.Lens' GatewayInfo (Prelude.Maybe Prelude.Text)
gatewayInfo_ec2InstanceRegion = Lens.lens (\GatewayInfo' {ec2InstanceRegion} -> ec2InstanceRegion) (\s@GatewayInfo' {} a -> s {ec2InstanceRegion = a} :: GatewayInfo)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and Amazon Web
-- Services Region.
gatewayInfo_gatewayARN :: Lens.Lens' GatewayInfo (Prelude.Maybe Prelude.Text)
gatewayInfo_gatewayARN = Lens.lens (\GatewayInfo' {gatewayARN} -> gatewayARN) (\s@GatewayInfo' {} a -> s {gatewayARN = a} :: GatewayInfo)

-- | The unique identifier assigned to your gateway during activation. This
-- ID becomes part of the gateway Amazon Resource Name (ARN), which you use
-- as input for other operations.
gatewayInfo_gatewayId :: Lens.Lens' GatewayInfo (Prelude.Maybe Prelude.Text)
gatewayInfo_gatewayId = Lens.lens (\GatewayInfo' {gatewayId} -> gatewayId) (\s@GatewayInfo' {} a -> s {gatewayId = a} :: GatewayInfo)

-- | The name of the gateway.
gatewayInfo_gatewayName :: Lens.Lens' GatewayInfo (Prelude.Maybe Prelude.Text)
gatewayInfo_gatewayName = Lens.lens (\GatewayInfo' {gatewayName} -> gatewayName) (\s@GatewayInfo' {} a -> s {gatewayName = a} :: GatewayInfo)

-- | The state of the gateway.
--
-- Valid Values: @DISABLED@ | @ACTIVE@
gatewayInfo_gatewayOperationalState :: Lens.Lens' GatewayInfo (Prelude.Maybe Prelude.Text)
gatewayInfo_gatewayOperationalState = Lens.lens (\GatewayInfo' {gatewayOperationalState} -> gatewayOperationalState) (\s@GatewayInfo' {} a -> s {gatewayOperationalState = a} :: GatewayInfo)

-- | The type of the gateway.
gatewayInfo_gatewayType :: Lens.Lens' GatewayInfo (Prelude.Maybe Prelude.Text)
gatewayInfo_gatewayType = Lens.lens (\GatewayInfo' {gatewayType} -> gatewayType) (\s@GatewayInfo' {} a -> s {gatewayType = a} :: GatewayInfo)

-- | The type of hardware or software platform on which the gateway is
-- running.
gatewayInfo_hostEnvironment :: Lens.Lens' GatewayInfo (Prelude.Maybe HostEnvironment)
gatewayInfo_hostEnvironment = Lens.lens (\GatewayInfo' {hostEnvironment} -> hostEnvironment) (\s@GatewayInfo' {} a -> s {hostEnvironment = a} :: GatewayInfo)

-- | A unique identifier for the specific instance of the host platform
-- running the gateway. This value is only available for certain host
-- environments, and its format depends on the host environment type.
gatewayInfo_hostEnvironmentId :: Lens.Lens' GatewayInfo (Prelude.Maybe Prelude.Text)
gatewayInfo_hostEnvironmentId = Lens.lens (\GatewayInfo' {hostEnvironmentId} -> hostEnvironmentId) (\s@GatewayInfo' {} a -> s {hostEnvironmentId = a} :: GatewayInfo)

instance Data.FromJSON GatewayInfo where
  parseJSON =
    Data.withObject
      "GatewayInfo"
      ( \x ->
          GatewayInfo'
            Prelude.<$> (x Data..:? "Ec2InstanceId")
            Prelude.<*> (x Data..:? "Ec2InstanceRegion")
            Prelude.<*> (x Data..:? "GatewayARN")
            Prelude.<*> (x Data..:? "GatewayId")
            Prelude.<*> (x Data..:? "GatewayName")
            Prelude.<*> (x Data..:? "GatewayOperationalState")
            Prelude.<*> (x Data..:? "GatewayType")
            Prelude.<*> (x Data..:? "HostEnvironment")
            Prelude.<*> (x Data..:? "HostEnvironmentId")
      )

instance Prelude.Hashable GatewayInfo where
  hashWithSalt _salt GatewayInfo' {..} =
    _salt
      `Prelude.hashWithSalt` ec2InstanceId
      `Prelude.hashWithSalt` ec2InstanceRegion
      `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` gatewayId
      `Prelude.hashWithSalt` gatewayName
      `Prelude.hashWithSalt` gatewayOperationalState
      `Prelude.hashWithSalt` gatewayType
      `Prelude.hashWithSalt` hostEnvironment
      `Prelude.hashWithSalt` hostEnvironmentId

instance Prelude.NFData GatewayInfo where
  rnf GatewayInfo' {..} =
    Prelude.rnf ec2InstanceId
      `Prelude.seq` Prelude.rnf ec2InstanceRegion
      `Prelude.seq` Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf gatewayId
      `Prelude.seq` Prelude.rnf gatewayName
      `Prelude.seq` Prelude.rnf gatewayOperationalState
      `Prelude.seq` Prelude.rnf gatewayType
      `Prelude.seq` Prelude.rnf hostEnvironment
      `Prelude.seq` Prelude.rnf hostEnvironmentId
