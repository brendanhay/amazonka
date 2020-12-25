{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.VPCEConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.VPCEConfiguration
  ( VPCEConfiguration (..),

    -- * Smart constructor
    mkVPCEConfiguration,

    -- * Lenses
    vpcecArn,
    vpcecServiceDnsName,
    vpcecVpceConfigurationDescription,
    vpcecVpceConfigurationName,
    vpcecVpceServiceName,
  )
where

import qualified Network.AWS.DeviceFarm.Types.Arn as Types
import qualified Network.AWS.DeviceFarm.Types.ServiceDnsName as Types
import qualified Network.AWS.DeviceFarm.Types.VPCEServiceName as Types
import qualified Network.AWS.DeviceFarm.Types.VpceConfigurationDescription as Types
import qualified Network.AWS.DeviceFarm.Types.VpceConfigurationName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents an Amazon Virtual Private Cloud (VPC) endpoint configuration.
--
-- /See:/ 'mkVPCEConfiguration' smart constructor.
data VPCEConfiguration = VPCEConfiguration'
  { -- | The Amazon Resource Name (ARN) of the VPC endpoint configuration.
    arn :: Core.Maybe Types.Arn,
    -- | The DNS name that maps to the private IP address of the service you want to access.
    serviceDnsName :: Core.Maybe Types.ServiceDnsName,
    -- | An optional description that provides details about your VPC endpoint configuration.
    vpceConfigurationDescription :: Core.Maybe Types.VpceConfigurationDescription,
    -- | The friendly name you give to your VPC endpoint configuration to manage your configurations more easily.
    vpceConfigurationName :: Core.Maybe Types.VpceConfigurationName,
    -- | The name of the VPC endpoint service running in your AWS account that you want Device Farm to test.
    vpceServiceName :: Core.Maybe Types.VPCEServiceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VPCEConfiguration' value with any optional fields omitted.
mkVPCEConfiguration ::
  VPCEConfiguration
mkVPCEConfiguration =
  VPCEConfiguration'
    { arn = Core.Nothing,
      serviceDnsName = Core.Nothing,
      vpceConfigurationDescription = Core.Nothing,
      vpceConfigurationName = Core.Nothing,
      vpceServiceName = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the VPC endpoint configuration.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcecArn :: Lens.Lens' VPCEConfiguration (Core.Maybe Types.Arn)
vpcecArn = Lens.field @"arn"
{-# DEPRECATED vpcecArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The DNS name that maps to the private IP address of the service you want to access.
--
-- /Note:/ Consider using 'serviceDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcecServiceDnsName :: Lens.Lens' VPCEConfiguration (Core.Maybe Types.ServiceDnsName)
vpcecServiceDnsName = Lens.field @"serviceDnsName"
{-# DEPRECATED vpcecServiceDnsName "Use generic-lens or generic-optics with 'serviceDnsName' instead." #-}

-- | An optional description that provides details about your VPC endpoint configuration.
--
-- /Note:/ Consider using 'vpceConfigurationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcecVpceConfigurationDescription :: Lens.Lens' VPCEConfiguration (Core.Maybe Types.VpceConfigurationDescription)
vpcecVpceConfigurationDescription = Lens.field @"vpceConfigurationDescription"
{-# DEPRECATED vpcecVpceConfigurationDescription "Use generic-lens or generic-optics with 'vpceConfigurationDescription' instead." #-}

-- | The friendly name you give to your VPC endpoint configuration to manage your configurations more easily.
--
-- /Note:/ Consider using 'vpceConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcecVpceConfigurationName :: Lens.Lens' VPCEConfiguration (Core.Maybe Types.VpceConfigurationName)
vpcecVpceConfigurationName = Lens.field @"vpceConfigurationName"
{-# DEPRECATED vpcecVpceConfigurationName "Use generic-lens or generic-optics with 'vpceConfigurationName' instead." #-}

-- | The name of the VPC endpoint service running in your AWS account that you want Device Farm to test.
--
-- /Note:/ Consider using 'vpceServiceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcecVpceServiceName :: Lens.Lens' VPCEConfiguration (Core.Maybe Types.VPCEServiceName)
vpcecVpceServiceName = Lens.field @"vpceServiceName"
{-# DEPRECATED vpcecVpceServiceName "Use generic-lens or generic-optics with 'vpceServiceName' instead." #-}

instance Core.FromJSON VPCEConfiguration where
  parseJSON =
    Core.withObject "VPCEConfiguration" Core.$
      \x ->
        VPCEConfiguration'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "serviceDnsName")
          Core.<*> (x Core..:? "vpceConfigurationDescription")
          Core.<*> (x Core..:? "vpceConfigurationName")
          Core.<*> (x Core..:? "vpceServiceName")
