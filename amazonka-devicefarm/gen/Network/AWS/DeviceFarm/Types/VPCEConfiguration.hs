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
-- Module      : Network.AWS.DeviceFarm.Types.VPCEConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.VPCEConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents an Amazon Virtual Private Cloud (VPC) endpoint configuration.
--
-- /See:/ 'newVPCEConfiguration' smart constructor.
data VPCEConfiguration = VPCEConfiguration'
  { -- | The friendly name you give to your VPC endpoint configuration to manage
    -- your configurations more easily.
    vpceConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | An optional description that provides details about your VPC endpoint
    -- configuration.
    vpceConfigurationDescription :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the VPC endpoint configuration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The DNS name that maps to the private IP address of the service you want
    -- to access.
    serviceDnsName :: Prelude.Maybe Prelude.Text,
    -- | The name of the VPC endpoint service running in your AWS account that
    -- you want Device Farm to test.
    vpceServiceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      vpceConfigurationDescription = Prelude.Nothing,
      arn = Prelude.Nothing,
      serviceDnsName = Prelude.Nothing,
      vpceServiceName = Prelude.Nothing
    }

-- | The friendly name you give to your VPC endpoint configuration to manage
-- your configurations more easily.
vPCEConfiguration_vpceConfigurationName :: Lens.Lens' VPCEConfiguration (Prelude.Maybe Prelude.Text)
vPCEConfiguration_vpceConfigurationName = Lens.lens (\VPCEConfiguration' {vpceConfigurationName} -> vpceConfigurationName) (\s@VPCEConfiguration' {} a -> s {vpceConfigurationName = a} :: VPCEConfiguration)

-- | An optional description that provides details about your VPC endpoint
-- configuration.
vPCEConfiguration_vpceConfigurationDescription :: Lens.Lens' VPCEConfiguration (Prelude.Maybe Prelude.Text)
vPCEConfiguration_vpceConfigurationDescription = Lens.lens (\VPCEConfiguration' {vpceConfigurationDescription} -> vpceConfigurationDescription) (\s@VPCEConfiguration' {} a -> s {vpceConfigurationDescription = a} :: VPCEConfiguration)

-- | The Amazon Resource Name (ARN) of the VPC endpoint configuration.
vPCEConfiguration_arn :: Lens.Lens' VPCEConfiguration (Prelude.Maybe Prelude.Text)
vPCEConfiguration_arn = Lens.lens (\VPCEConfiguration' {arn} -> arn) (\s@VPCEConfiguration' {} a -> s {arn = a} :: VPCEConfiguration)

-- | The DNS name that maps to the private IP address of the service you want
-- to access.
vPCEConfiguration_serviceDnsName :: Lens.Lens' VPCEConfiguration (Prelude.Maybe Prelude.Text)
vPCEConfiguration_serviceDnsName = Lens.lens (\VPCEConfiguration' {serviceDnsName} -> serviceDnsName) (\s@VPCEConfiguration' {} a -> s {serviceDnsName = a} :: VPCEConfiguration)

-- | The name of the VPC endpoint service running in your AWS account that
-- you want Device Farm to test.
vPCEConfiguration_vpceServiceName :: Lens.Lens' VPCEConfiguration (Prelude.Maybe Prelude.Text)
vPCEConfiguration_vpceServiceName = Lens.lens (\VPCEConfiguration' {vpceServiceName} -> vpceServiceName) (\s@VPCEConfiguration' {} a -> s {vpceServiceName = a} :: VPCEConfiguration)

instance Prelude.FromJSON VPCEConfiguration where
  parseJSON =
    Prelude.withObject
      "VPCEConfiguration"
      ( \x ->
          VPCEConfiguration'
            Prelude.<$> (x Prelude..:? "vpceConfigurationName")
            Prelude.<*> (x Prelude..:? "vpceConfigurationDescription")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "serviceDnsName")
            Prelude.<*> (x Prelude..:? "vpceServiceName")
      )

instance Prelude.Hashable VPCEConfiguration

instance Prelude.NFData VPCEConfiguration
