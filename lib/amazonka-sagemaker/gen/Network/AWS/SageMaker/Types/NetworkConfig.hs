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
    ncEnableNetworkIsolation,
    ncVPCConfig,
    ncEnableInterContainerTrafficEncryption,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.VPCConfig

-- | Networking options for a job, such as network traffic encryption between containers, whether to allow inbound and outbound network calls to and from containers, and the VPC subnets and security groups to use for VPC-enabled jobs.
--
-- /See:/ 'mkNetworkConfig' smart constructor.
data NetworkConfig = NetworkConfig'
  { -- | Whether to allow inbound and outbound network calls to and from the containers used for the processing job.
    enableNetworkIsolation :: Lude.Maybe Lude.Bool,
    vpcConfig :: Lude.Maybe VPCConfig,
    -- | Whether to encrypt all communications between distributed processing jobs. Choose @True@ to encrypt communications. Encryption provides greater security for distributed processing jobs, but the processing might take longer.
    enableInterContainerTrafficEncryption :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkConfig' with the minimum fields required to make a request.
--
-- * 'enableNetworkIsolation' - Whether to allow inbound and outbound network calls to and from the containers used for the processing job.
-- * 'vpcConfig' -
-- * 'enableInterContainerTrafficEncryption' - Whether to encrypt all communications between distributed processing jobs. Choose @True@ to encrypt communications. Encryption provides greater security for distributed processing jobs, but the processing might take longer.
mkNetworkConfig ::
  NetworkConfig
mkNetworkConfig =
  NetworkConfig'
    { enableNetworkIsolation = Lude.Nothing,
      vpcConfig = Lude.Nothing,
      enableInterContainerTrafficEncryption = Lude.Nothing
    }

-- | Whether to allow inbound and outbound network calls to and from the containers used for the processing job.
--
-- /Note:/ Consider using 'enableNetworkIsolation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncEnableNetworkIsolation :: Lens.Lens' NetworkConfig (Lude.Maybe Lude.Bool)
ncEnableNetworkIsolation = Lens.lens (enableNetworkIsolation :: NetworkConfig -> Lude.Maybe Lude.Bool) (\s a -> s {enableNetworkIsolation = a} :: NetworkConfig)
{-# DEPRECATED ncEnableNetworkIsolation "Use generic-lens or generic-optics with 'enableNetworkIsolation' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncVPCConfig :: Lens.Lens' NetworkConfig (Lude.Maybe VPCConfig)
ncVPCConfig = Lens.lens (vpcConfig :: NetworkConfig -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: NetworkConfig)
{-# DEPRECATED ncVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | Whether to encrypt all communications between distributed processing jobs. Choose @True@ to encrypt communications. Encryption provides greater security for distributed processing jobs, but the processing might take longer.
--
-- /Note:/ Consider using 'enableInterContainerTrafficEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncEnableInterContainerTrafficEncryption :: Lens.Lens' NetworkConfig (Lude.Maybe Lude.Bool)
ncEnableInterContainerTrafficEncryption = Lens.lens (enableInterContainerTrafficEncryption :: NetworkConfig -> Lude.Maybe Lude.Bool) (\s a -> s {enableInterContainerTrafficEncryption = a} :: NetworkConfig)
{-# DEPRECATED ncEnableInterContainerTrafficEncryption "Use generic-lens or generic-optics with 'enableInterContainerTrafficEncryption' instead." #-}

instance Lude.FromJSON NetworkConfig where
  parseJSON =
    Lude.withObject
      "NetworkConfig"
      ( \x ->
          NetworkConfig'
            Lude.<$> (x Lude..:? "EnableNetworkIsolation")
            Lude.<*> (x Lude..:? "VpcConfig")
            Lude.<*> (x Lude..:? "EnableInterContainerTrafficEncryption")
      )

instance Lude.ToJSON NetworkConfig where
  toJSON NetworkConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EnableNetworkIsolation" Lude..=)
              Lude.<$> enableNetworkIsolation,
            ("VpcConfig" Lude..=) Lude.<$> vpcConfig,
            ("EnableInterContainerTrafficEncryption" Lude..=)
              Lude.<$> enableInterContainerTrafficEncryption
          ]
      )
