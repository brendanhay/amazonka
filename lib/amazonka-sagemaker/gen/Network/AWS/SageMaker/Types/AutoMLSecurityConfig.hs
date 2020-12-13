{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLSecurityConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLSecurityConfig
  ( AutoMLSecurityConfig (..),

    -- * Smart constructor
    mkAutoMLSecurityConfig,

    -- * Lenses
    amlscVPCConfig,
    amlscVolumeKMSKeyId,
    amlscEnableInterContainerTrafficEncryption,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.VPCConfig

-- | Security options.
--
-- /See:/ 'mkAutoMLSecurityConfig' smart constructor.
data AutoMLSecurityConfig = AutoMLSecurityConfig'
  { -- | VPC configuration.
    vpcConfig :: Lude.Maybe VPCConfig,
    -- | The key used to encrypt stored data.
    volumeKMSKeyId :: Lude.Maybe Lude.Text,
    -- | Whether to use traffic encryption between the container layers.
    enableInterContainerTrafficEncryption :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoMLSecurityConfig' with the minimum fields required to make a request.
--
-- * 'vpcConfig' - VPC configuration.
-- * 'volumeKMSKeyId' - The key used to encrypt stored data.
-- * 'enableInterContainerTrafficEncryption' - Whether to use traffic encryption between the container layers.
mkAutoMLSecurityConfig ::
  AutoMLSecurityConfig
mkAutoMLSecurityConfig =
  AutoMLSecurityConfig'
    { vpcConfig = Lude.Nothing,
      volumeKMSKeyId = Lude.Nothing,
      enableInterContainerTrafficEncryption = Lude.Nothing
    }

-- | VPC configuration.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlscVPCConfig :: Lens.Lens' AutoMLSecurityConfig (Lude.Maybe VPCConfig)
amlscVPCConfig = Lens.lens (vpcConfig :: AutoMLSecurityConfig -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: AutoMLSecurityConfig)
{-# DEPRECATED amlscVPCConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | The key used to encrypt stored data.
--
-- /Note:/ Consider using 'volumeKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlscVolumeKMSKeyId :: Lens.Lens' AutoMLSecurityConfig (Lude.Maybe Lude.Text)
amlscVolumeKMSKeyId = Lens.lens (volumeKMSKeyId :: AutoMLSecurityConfig -> Lude.Maybe Lude.Text) (\s a -> s {volumeKMSKeyId = a} :: AutoMLSecurityConfig)
{-# DEPRECATED amlscVolumeKMSKeyId "Use generic-lens or generic-optics with 'volumeKMSKeyId' instead." #-}

-- | Whether to use traffic encryption between the container layers.
--
-- /Note:/ Consider using 'enableInterContainerTrafficEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlscEnableInterContainerTrafficEncryption :: Lens.Lens' AutoMLSecurityConfig (Lude.Maybe Lude.Bool)
amlscEnableInterContainerTrafficEncryption = Lens.lens (enableInterContainerTrafficEncryption :: AutoMLSecurityConfig -> Lude.Maybe Lude.Bool) (\s a -> s {enableInterContainerTrafficEncryption = a} :: AutoMLSecurityConfig)
{-# DEPRECATED amlscEnableInterContainerTrafficEncryption "Use generic-lens or generic-optics with 'enableInterContainerTrafficEncryption' instead." #-}

instance Lude.FromJSON AutoMLSecurityConfig where
  parseJSON =
    Lude.withObject
      "AutoMLSecurityConfig"
      ( \x ->
          AutoMLSecurityConfig'
            Lude.<$> (x Lude..:? "VpcConfig")
            Lude.<*> (x Lude..:? "VolumeKmsKeyId")
            Lude.<*> (x Lude..:? "EnableInterContainerTrafficEncryption")
      )

instance Lude.ToJSON AutoMLSecurityConfig where
  toJSON AutoMLSecurityConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VpcConfig" Lude..=) Lude.<$> vpcConfig,
            ("VolumeKmsKeyId" Lude..=) Lude.<$> volumeKMSKeyId,
            ("EnableInterContainerTrafficEncryption" Lude..=)
              Lude.<$> enableInterContainerTrafficEncryption
          ]
      )
