{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLSecurityConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AutoMLSecurityConfig
  ( AutoMLSecurityConfig (..)
  -- * Smart constructor
  , mkAutoMLSecurityConfig
  -- * Lenses
  , amlscEnableInterContainerTrafficEncryption
  , amlscVolumeKmsKeyId
  , amlscVpcConfig
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.KmsKeyId as Types
import qualified Network.AWS.SageMaker.Types.VpcConfig as Types

-- | Security options.
--
-- /See:/ 'mkAutoMLSecurityConfig' smart constructor.
data AutoMLSecurityConfig = AutoMLSecurityConfig'
  { enableInterContainerTrafficEncryption :: Core.Maybe Core.Bool
    -- ^ Whether to use traffic encryption between the container layers.
  , volumeKmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ The key used to encrypt stored data.
  , vpcConfig :: Core.Maybe Types.VpcConfig
    -- ^ VPC configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoMLSecurityConfig' value with any optional fields omitted.
mkAutoMLSecurityConfig
    :: AutoMLSecurityConfig
mkAutoMLSecurityConfig
  = AutoMLSecurityConfig'{enableInterContainerTrafficEncryption =
                            Core.Nothing,
                          volumeKmsKeyId = Core.Nothing, vpcConfig = Core.Nothing}

-- | Whether to use traffic encryption between the container layers.
--
-- /Note:/ Consider using 'enableInterContainerTrafficEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlscEnableInterContainerTrafficEncryption :: Lens.Lens' AutoMLSecurityConfig (Core.Maybe Core.Bool)
amlscEnableInterContainerTrafficEncryption = Lens.field @"enableInterContainerTrafficEncryption"
{-# INLINEABLE amlscEnableInterContainerTrafficEncryption #-}
{-# DEPRECATED enableInterContainerTrafficEncryption "Use generic-lens or generic-optics with 'enableInterContainerTrafficEncryption' instead"  #-}

-- | The key used to encrypt stored data.
--
-- /Note:/ Consider using 'volumeKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlscVolumeKmsKeyId :: Lens.Lens' AutoMLSecurityConfig (Core.Maybe Types.KmsKeyId)
amlscVolumeKmsKeyId = Lens.field @"volumeKmsKeyId"
{-# INLINEABLE amlscVolumeKmsKeyId #-}
{-# DEPRECATED volumeKmsKeyId "Use generic-lens or generic-optics with 'volumeKmsKeyId' instead"  #-}

-- | VPC configuration.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlscVpcConfig :: Lens.Lens' AutoMLSecurityConfig (Core.Maybe Types.VpcConfig)
amlscVpcConfig = Lens.field @"vpcConfig"
{-# INLINEABLE amlscVpcConfig #-}
{-# DEPRECATED vpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead"  #-}

instance Core.FromJSON AutoMLSecurityConfig where
        toJSON AutoMLSecurityConfig{..}
          = Core.object
              (Core.catMaybes
                 [("EnableInterContainerTrafficEncryption" Core..=) Core.<$>
                    enableInterContainerTrafficEncryption,
                  ("VolumeKmsKeyId" Core..=) Core.<$> volumeKmsKeyId,
                  ("VpcConfig" Core..=) Core.<$> vpcConfig])

instance Core.FromJSON AutoMLSecurityConfig where
        parseJSON
          = Core.withObject "AutoMLSecurityConfig" Core.$
              \ x ->
                AutoMLSecurityConfig' Core.<$>
                  (x Core..:? "EnableInterContainerTrafficEncryption") Core.<*>
                    x Core..:? "VolumeKmsKeyId"
                    Core.<*> x Core..:? "VpcConfig"
