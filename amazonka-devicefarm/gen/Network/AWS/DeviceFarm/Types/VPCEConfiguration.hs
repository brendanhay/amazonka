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
-- Module      : Network.AWS.DeviceFarm.Types.VPCEConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.VPCEConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents an Amazon Virtual Private Cloud (VPC) endpoint configuration.
--
-- /See:/ 'newVPCEConfiguration' smart constructor.
data VPCEConfiguration = VPCEConfiguration'
  { -- | The friendly name you give to your VPC endpoint configuration to manage
    -- your configurations more easily.
    vpceConfigurationName :: Core.Maybe Core.Text,
    -- | An optional description that provides details about your VPC endpoint
    -- configuration.
    vpceConfigurationDescription :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the VPC endpoint configuration.
    arn :: Core.Maybe Core.Text,
    -- | The DNS name that maps to the private IP address of the service you want
    -- to access.
    serviceDnsName :: Core.Maybe Core.Text,
    -- | The name of the VPC endpoint service running in your AWS account that
    -- you want Device Farm to test.
    vpceServiceName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VPCEConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpceConfigurationName', 'vPCEConfiguration_vpceConfigurationName' - The friendly name you give to your VPC endpoint configuration to manage
-- your configurations more easily.
--
-- 'vpceConfigurationDescription', 'vPCEConfiguration_vpceConfigurationDescription' - An optional description that provides details about your VPC endpoint
-- configuration.
--
-- 'arn', 'vPCEConfiguration_arn' - The Amazon Resource Name (ARN) of the VPC endpoint configuration.
--
-- 'serviceDnsName', 'vPCEConfiguration_serviceDnsName' - The DNS name that maps to the private IP address of the service you want
-- to access.
--
-- 'vpceServiceName', 'vPCEConfiguration_vpceServiceName' - The name of the VPC endpoint service running in your AWS account that
-- you want Device Farm to test.
newVPCEConfiguration ::
  VPCEConfiguration
newVPCEConfiguration =
  VPCEConfiguration'
    { vpceConfigurationName =
        Core.Nothing,
      vpceConfigurationDescription = Core.Nothing,
      arn = Core.Nothing,
      serviceDnsName = Core.Nothing,
      vpceServiceName = Core.Nothing
    }

-- | The friendly name you give to your VPC endpoint configuration to manage
-- your configurations more easily.
vPCEConfiguration_vpceConfigurationName :: Lens.Lens' VPCEConfiguration (Core.Maybe Core.Text)
vPCEConfiguration_vpceConfigurationName = Lens.lens (\VPCEConfiguration' {vpceConfigurationName} -> vpceConfigurationName) (\s@VPCEConfiguration' {} a -> s {vpceConfigurationName = a} :: VPCEConfiguration)

-- | An optional description that provides details about your VPC endpoint
-- configuration.
vPCEConfiguration_vpceConfigurationDescription :: Lens.Lens' VPCEConfiguration (Core.Maybe Core.Text)
vPCEConfiguration_vpceConfigurationDescription = Lens.lens (\VPCEConfiguration' {vpceConfigurationDescription} -> vpceConfigurationDescription) (\s@VPCEConfiguration' {} a -> s {vpceConfigurationDescription = a} :: VPCEConfiguration)

-- | The Amazon Resource Name (ARN) of the VPC endpoint configuration.
vPCEConfiguration_arn :: Lens.Lens' VPCEConfiguration (Core.Maybe Core.Text)
vPCEConfiguration_arn = Lens.lens (\VPCEConfiguration' {arn} -> arn) (\s@VPCEConfiguration' {} a -> s {arn = a} :: VPCEConfiguration)

-- | The DNS name that maps to the private IP address of the service you want
-- to access.
vPCEConfiguration_serviceDnsName :: Lens.Lens' VPCEConfiguration (Core.Maybe Core.Text)
vPCEConfiguration_serviceDnsName = Lens.lens (\VPCEConfiguration' {serviceDnsName} -> serviceDnsName) (\s@VPCEConfiguration' {} a -> s {serviceDnsName = a} :: VPCEConfiguration)

-- | The name of the VPC endpoint service running in your AWS account that
-- you want Device Farm to test.
vPCEConfiguration_vpceServiceName :: Lens.Lens' VPCEConfiguration (Core.Maybe Core.Text)
vPCEConfiguration_vpceServiceName = Lens.lens (\VPCEConfiguration' {vpceServiceName} -> vpceServiceName) (\s@VPCEConfiguration' {} a -> s {vpceServiceName = a} :: VPCEConfiguration)

instance Core.FromJSON VPCEConfiguration where
  parseJSON =
    Core.withObject
      "VPCEConfiguration"
      ( \x ->
          VPCEConfiguration'
            Core.<$> (x Core..:? "vpceConfigurationName")
            Core.<*> (x Core..:? "vpceConfigurationDescription")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "serviceDnsName")
            Core.<*> (x Core..:? "vpceServiceName")
      )

instance Core.Hashable VPCEConfiguration

instance Core.NFData VPCEConfiguration
