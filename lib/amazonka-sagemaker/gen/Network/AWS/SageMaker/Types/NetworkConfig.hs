{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NetworkConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NetworkConfig
  ( NetworkConfig (..),

    -- * Smart constructor
    mkNetworkConfig,

    -- * Lenses
    ncEnableInterContainerTrafficEncryption,
    ncEnableNetworkIsolation,
    ncVpcConfig,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.VpcConfig as Types

-- | Networking options for a job, such as network traffic encryption between containers, whether to allow inbound and outbound network calls to and from containers, and the VPC subnets and security groups to use for VPC-enabled jobs.
--
-- /See:/ 'mkNetworkConfig' smart constructor.
data NetworkConfig = NetworkConfig'
  { -- | Whether to encrypt all communications between distributed processing jobs. Choose @True@ to encrypt communications. Encryption provides greater security for distributed processing jobs, but the processing might take longer.
    enableInterContainerTrafficEncryption :: Core.Maybe Core.Bool,
    -- | Whether to allow inbound and outbound network calls to and from the containers used for the processing job.
    enableNetworkIsolation :: Core.Maybe Core.Bool,
    vpcConfig :: Core.Maybe Types.VpcConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkConfig' value with any optional fields omitted.
mkNetworkConfig ::
  NetworkConfig
mkNetworkConfig =
  NetworkConfig'
    { enableInterContainerTrafficEncryption =
        Core.Nothing,
      enableNetworkIsolation = Core.Nothing,
      vpcConfig = Core.Nothing
    }

-- | Whether to encrypt all communications between distributed processing jobs. Choose @True@ to encrypt communications. Encryption provides greater security for distributed processing jobs, but the processing might take longer.
--
-- /Note:/ Consider using 'enableInterContainerTrafficEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncEnableInterContainerTrafficEncryption :: Lens.Lens' NetworkConfig (Core.Maybe Core.Bool)
ncEnableInterContainerTrafficEncryption = Lens.field @"enableInterContainerTrafficEncryption"
{-# DEPRECATED ncEnableInterContainerTrafficEncryption "Use generic-lens or generic-optics with 'enableInterContainerTrafficEncryption' instead." #-}

-- | Whether to allow inbound and outbound network calls to and from the containers used for the processing job.
--
-- /Note:/ Consider using 'enableNetworkIsolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncEnableNetworkIsolation :: Lens.Lens' NetworkConfig (Core.Maybe Core.Bool)
ncEnableNetworkIsolation = Lens.field @"enableNetworkIsolation"
{-# DEPRECATED ncEnableNetworkIsolation "Use generic-lens or generic-optics with 'enableNetworkIsolation' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncVpcConfig :: Lens.Lens' NetworkConfig (Core.Maybe Types.VpcConfig)
ncVpcConfig = Lens.field @"vpcConfig"
{-# DEPRECATED ncVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

instance Core.FromJSON NetworkConfig where
  toJSON NetworkConfig {..} =
    Core.object
      ( Core.catMaybes
          [ ("EnableInterContainerTrafficEncryption" Core..=)
              Core.<$> enableInterContainerTrafficEncryption,
            ("EnableNetworkIsolation" Core..=) Core.<$> enableNetworkIsolation,
            ("VpcConfig" Core..=) Core.<$> vpcConfig
          ]
      )

instance Core.FromJSON NetworkConfig where
  parseJSON =
    Core.withObject "NetworkConfig" Core.$
      \x ->
        NetworkConfig'
          Core.<$> (x Core..:? "EnableInterContainerTrafficEncryption")
          Core.<*> (x Core..:? "EnableNetworkIsolation")
          Core.<*> (x Core..:? "VpcConfig")
