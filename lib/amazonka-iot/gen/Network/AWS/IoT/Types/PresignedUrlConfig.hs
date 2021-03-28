{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.PresignedUrlConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.PresignedUrlConfig
  ( PresignedUrlConfig (..)
  -- * Smart constructor
  , mkPresignedUrlConfig
  -- * Lenses
  , pucExpiresInSec
  , pucRoleArn
  ) where

import qualified Network.AWS.IoT.Types.RoleArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration for pre-signed S3 URLs.
--
-- /See:/ 'mkPresignedUrlConfig' smart constructor.
data PresignedUrlConfig = PresignedUrlConfig'
  { expiresInSec :: Core.Maybe Core.Natural
    -- ^ How long (in seconds) pre-signed URLs are valid. Valid values are 60 - 3600, the default value is 3600 seconds. Pre-signed URLs are generated when Jobs receives an MQTT request for the job document.
  , roleArn :: Core.Maybe Types.RoleArn
    -- ^ The ARN of an IAM role that grants grants permission to download files from the S3 bucket where the job data/updates are stored. The role must also grant permission for IoT to download the files.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PresignedUrlConfig' value with any optional fields omitted.
mkPresignedUrlConfig
    :: PresignedUrlConfig
mkPresignedUrlConfig
  = PresignedUrlConfig'{expiresInSec = Core.Nothing,
                        roleArn = Core.Nothing}

-- | How long (in seconds) pre-signed URLs are valid. Valid values are 60 - 3600, the default value is 3600 seconds. Pre-signed URLs are generated when Jobs receives an MQTT request for the job document.
--
-- /Note:/ Consider using 'expiresInSec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pucExpiresInSec :: Lens.Lens' PresignedUrlConfig (Core.Maybe Core.Natural)
pucExpiresInSec = Lens.field @"expiresInSec"
{-# INLINEABLE pucExpiresInSec #-}
{-# DEPRECATED expiresInSec "Use generic-lens or generic-optics with 'expiresInSec' instead"  #-}

-- | The ARN of an IAM role that grants grants permission to download files from the S3 bucket where the job data/updates are stored. The role must also grant permission for IoT to download the files.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pucRoleArn :: Lens.Lens' PresignedUrlConfig (Core.Maybe Types.RoleArn)
pucRoleArn = Lens.field @"roleArn"
{-# INLINEABLE pucRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.FromJSON PresignedUrlConfig where
        toJSON PresignedUrlConfig{..}
          = Core.object
              (Core.catMaybes
                 [("expiresInSec" Core..=) Core.<$> expiresInSec,
                  ("roleArn" Core..=) Core.<$> roleArn])

instance Core.FromJSON PresignedUrlConfig where
        parseJSON
          = Core.withObject "PresignedUrlConfig" Core.$
              \ x ->
                PresignedUrlConfig' Core.<$>
                  (x Core..:? "expiresInSec") Core.<*> x Core..:? "roleArn"
