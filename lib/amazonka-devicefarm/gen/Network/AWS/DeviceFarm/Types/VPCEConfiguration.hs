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
    vecVpceServiceName,
    vecArn,
    vecVpceConfigurationName,
    vecServiceDNSName,
    vecVpceConfigurationDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an Amazon Virtual Private Cloud (VPC) endpoint configuration.
--
-- /See:/ 'mkVPCEConfiguration' smart constructor.
data VPCEConfiguration = VPCEConfiguration'
  { vpceServiceName ::
      Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    vpceConfigurationName :: Lude.Maybe Lude.Text,
    serviceDNSName :: Lude.Maybe Lude.Text,
    vpceConfigurationDescription :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCEConfiguration' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the VPC endpoint configuration.
-- * 'serviceDNSName' - The DNS name that maps to the private IP address of the service you want to access.
-- * 'vpceConfigurationDescription' - An optional description that provides details about your VPC endpoint configuration.
-- * 'vpceConfigurationName' - The friendly name you give to your VPC endpoint configuration to manage your configurations more easily.
-- * 'vpceServiceName' - The name of the VPC endpoint service running in your AWS account that you want Device Farm to test.
mkVPCEConfiguration ::
  VPCEConfiguration
mkVPCEConfiguration =
  VPCEConfiguration'
    { vpceServiceName = Lude.Nothing,
      arn = Lude.Nothing,
      vpceConfigurationName = Lude.Nothing,
      serviceDNSName = Lude.Nothing,
      vpceConfigurationDescription = Lude.Nothing
    }

-- | The name of the VPC endpoint service running in your AWS account that you want Device Farm to test.
--
-- /Note:/ Consider using 'vpceServiceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vecVpceServiceName :: Lens.Lens' VPCEConfiguration (Lude.Maybe Lude.Text)
vecVpceServiceName = Lens.lens (vpceServiceName :: VPCEConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {vpceServiceName = a} :: VPCEConfiguration)
{-# DEPRECATED vecVpceServiceName "Use generic-lens or generic-optics with 'vpceServiceName' instead." #-}

-- | The Amazon Resource Name (ARN) of the VPC endpoint configuration.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vecArn :: Lens.Lens' VPCEConfiguration (Lude.Maybe Lude.Text)
vecArn = Lens.lens (arn :: VPCEConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: VPCEConfiguration)
{-# DEPRECATED vecArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name you give to your VPC endpoint configuration to manage your configurations more easily.
--
-- /Note:/ Consider using 'vpceConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vecVpceConfigurationName :: Lens.Lens' VPCEConfiguration (Lude.Maybe Lude.Text)
vecVpceConfigurationName = Lens.lens (vpceConfigurationName :: VPCEConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {vpceConfigurationName = a} :: VPCEConfiguration)
{-# DEPRECATED vecVpceConfigurationName "Use generic-lens or generic-optics with 'vpceConfigurationName' instead." #-}

-- | The DNS name that maps to the private IP address of the service you want to access.
--
-- /Note:/ Consider using 'serviceDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vecServiceDNSName :: Lens.Lens' VPCEConfiguration (Lude.Maybe Lude.Text)
vecServiceDNSName = Lens.lens (serviceDNSName :: VPCEConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {serviceDNSName = a} :: VPCEConfiguration)
{-# DEPRECATED vecServiceDNSName "Use generic-lens or generic-optics with 'serviceDNSName' instead." #-}

-- | An optional description that provides details about your VPC endpoint configuration.
--
-- /Note:/ Consider using 'vpceConfigurationDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vecVpceConfigurationDescription :: Lens.Lens' VPCEConfiguration (Lude.Maybe Lude.Text)
vecVpceConfigurationDescription = Lens.lens (vpceConfigurationDescription :: VPCEConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {vpceConfigurationDescription = a} :: VPCEConfiguration)
{-# DEPRECATED vecVpceConfigurationDescription "Use generic-lens or generic-optics with 'vpceConfigurationDescription' instead." #-}

instance Lude.FromJSON VPCEConfiguration where
  parseJSON =
    Lude.withObject
      "VPCEConfiguration"
      ( \x ->
          VPCEConfiguration'
            Lude.<$> (x Lude..:? "vpceServiceName")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "vpceConfigurationName")
            Lude.<*> (x Lude..:? "serviceDnsName")
            Lude.<*> (x Lude..:? "vpceConfigurationDescription")
      )
