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
-- Module      : Network.AWS.StorageGateway.Types.GatewayInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.GatewayInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a gateway object.
--
-- /See:/ 'newGatewayInfo' smart constructor.
data GatewayInfo = GatewayInfo'
  { -- | The state of the gateway.
    --
    -- Valid Values: @DISABLED@ | @ACTIVE@
    gatewayOperationalState :: Core.Maybe Core.Text,
    -- | The name of the gateway.
    gatewayName :: Core.Maybe Core.Text,
    -- | The type of the gateway.
    gatewayType :: Core.Maybe Core.Text,
    -- | The AWS Region where the Amazon EC2 instance is located.
    ec2InstanceRegion :: Core.Maybe Core.Text,
    -- | The ID of the Amazon EC2 instance that was used to launch the gateway.
    ec2InstanceId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
    -- operation to return a list of gateways for your account and AWS Region.
    gatewayARN :: Core.Maybe Core.Text,
    -- | The unique identifier assigned to your gateway during activation. This
    -- ID becomes part of the gateway Amazon Resource Name (ARN), which you use
    -- as input for other operations.
    gatewayId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GatewayInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayOperationalState', 'gatewayInfo_gatewayOperationalState' - The state of the gateway.
--
-- Valid Values: @DISABLED@ | @ACTIVE@
--
-- 'gatewayName', 'gatewayInfo_gatewayName' - The name of the gateway.
--
-- 'gatewayType', 'gatewayInfo_gatewayType' - The type of the gateway.
--
-- 'ec2InstanceRegion', 'gatewayInfo_ec2InstanceRegion' - The AWS Region where the Amazon EC2 instance is located.
--
-- 'ec2InstanceId', 'gatewayInfo_ec2InstanceId' - The ID of the Amazon EC2 instance that was used to launch the gateway.
--
-- 'gatewayARN', 'gatewayInfo_gatewayARN' - The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and AWS Region.
--
-- 'gatewayId', 'gatewayInfo_gatewayId' - The unique identifier assigned to your gateway during activation. This
-- ID becomes part of the gateway Amazon Resource Name (ARN), which you use
-- as input for other operations.
newGatewayInfo ::
  GatewayInfo
newGatewayInfo =
  GatewayInfo'
    { gatewayOperationalState =
        Core.Nothing,
      gatewayName = Core.Nothing,
      gatewayType = Core.Nothing,
      ec2InstanceRegion = Core.Nothing,
      ec2InstanceId = Core.Nothing,
      gatewayARN = Core.Nothing,
      gatewayId = Core.Nothing
    }

-- | The state of the gateway.
--
-- Valid Values: @DISABLED@ | @ACTIVE@
gatewayInfo_gatewayOperationalState :: Lens.Lens' GatewayInfo (Core.Maybe Core.Text)
gatewayInfo_gatewayOperationalState = Lens.lens (\GatewayInfo' {gatewayOperationalState} -> gatewayOperationalState) (\s@GatewayInfo' {} a -> s {gatewayOperationalState = a} :: GatewayInfo)

-- | The name of the gateway.
gatewayInfo_gatewayName :: Lens.Lens' GatewayInfo (Core.Maybe Core.Text)
gatewayInfo_gatewayName = Lens.lens (\GatewayInfo' {gatewayName} -> gatewayName) (\s@GatewayInfo' {} a -> s {gatewayName = a} :: GatewayInfo)

-- | The type of the gateway.
gatewayInfo_gatewayType :: Lens.Lens' GatewayInfo (Core.Maybe Core.Text)
gatewayInfo_gatewayType = Lens.lens (\GatewayInfo' {gatewayType} -> gatewayType) (\s@GatewayInfo' {} a -> s {gatewayType = a} :: GatewayInfo)

-- | The AWS Region where the Amazon EC2 instance is located.
gatewayInfo_ec2InstanceRegion :: Lens.Lens' GatewayInfo (Core.Maybe Core.Text)
gatewayInfo_ec2InstanceRegion = Lens.lens (\GatewayInfo' {ec2InstanceRegion} -> ec2InstanceRegion) (\s@GatewayInfo' {} a -> s {ec2InstanceRegion = a} :: GatewayInfo)

-- | The ID of the Amazon EC2 instance that was used to launch the gateway.
gatewayInfo_ec2InstanceId :: Lens.Lens' GatewayInfo (Core.Maybe Core.Text)
gatewayInfo_ec2InstanceId = Lens.lens (\GatewayInfo' {ec2InstanceId} -> ec2InstanceId) (\s@GatewayInfo' {} a -> s {ec2InstanceId = a} :: GatewayInfo)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and AWS Region.
gatewayInfo_gatewayARN :: Lens.Lens' GatewayInfo (Core.Maybe Core.Text)
gatewayInfo_gatewayARN = Lens.lens (\GatewayInfo' {gatewayARN} -> gatewayARN) (\s@GatewayInfo' {} a -> s {gatewayARN = a} :: GatewayInfo)

-- | The unique identifier assigned to your gateway during activation. This
-- ID becomes part of the gateway Amazon Resource Name (ARN), which you use
-- as input for other operations.
gatewayInfo_gatewayId :: Lens.Lens' GatewayInfo (Core.Maybe Core.Text)
gatewayInfo_gatewayId = Lens.lens (\GatewayInfo' {gatewayId} -> gatewayId) (\s@GatewayInfo' {} a -> s {gatewayId = a} :: GatewayInfo)

instance Core.FromJSON GatewayInfo where
  parseJSON =
    Core.withObject
      "GatewayInfo"
      ( \x ->
          GatewayInfo'
            Core.<$> (x Core..:? "GatewayOperationalState")
            Core.<*> (x Core..:? "GatewayName")
            Core.<*> (x Core..:? "GatewayType")
            Core.<*> (x Core..:? "Ec2InstanceRegion")
            Core.<*> (x Core..:? "Ec2InstanceId")
            Core.<*> (x Core..:? "GatewayARN")
            Core.<*> (x Core..:? "GatewayId")
      )

instance Core.Hashable GatewayInfo

instance Core.NFData GatewayInfo
