{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.S3LogsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.S3LogsConfig
  ( S3LogsConfig (..)
  -- * Smart constructor
  , mkS3LogsConfig
  -- * Lenses
  , slcStatus
  , slcEncryptionDisabled
  , slcLocation
  ) where

import qualified Network.AWS.CodeBuild.Types.LogsConfigStatusType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about S3 logs for a build project. 
--
-- /See:/ 'mkS3LogsConfig' smart constructor.
data S3LogsConfig = S3LogsConfig'
  { status :: Types.LogsConfigStatusType
    -- ^ The current status of the S3 build logs. Valid values are:
--
--
--     * @ENABLED@ : S3 build logs are enabled for this build project.
--
--
--     * @DISABLED@ : S3 build logs are not enabled for this build project.
--
--
  , encryptionDisabled :: Core.Maybe Core.Bool
    -- ^ Set to true if you do not want your S3 build log output encrypted. By default S3 build logs are encrypted. 
  , location :: Core.Maybe Core.Text
    -- ^ The ARN of an S3 bucket and the path prefix for S3 logs. If your Amazon S3 bucket name is @my-bucket@ , and your path prefix is @build-log@ , then acceptable formats are @my-bucket/build-log@ or @arn:aws:s3:::my-bucket/build-log@ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3LogsConfig' value with any optional fields omitted.
mkS3LogsConfig
    :: Types.LogsConfigStatusType -- ^ 'status'
    -> S3LogsConfig
mkS3LogsConfig status
  = S3LogsConfig'{status, encryptionDisabled = Core.Nothing,
                  location = Core.Nothing}

-- | The current status of the S3 build logs. Valid values are:
--
--
--     * @ENABLED@ : S3 build logs are enabled for this build project.
--
--
--     * @DISABLED@ : S3 build logs are not enabled for this build project.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcStatus :: Lens.Lens' S3LogsConfig Types.LogsConfigStatusType
slcStatus = Lens.field @"status"
{-# INLINEABLE slcStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Set to true if you do not want your S3 build log output encrypted. By default S3 build logs are encrypted. 
--
-- /Note:/ Consider using 'encryptionDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcEncryptionDisabled :: Lens.Lens' S3LogsConfig (Core.Maybe Core.Bool)
slcEncryptionDisabled = Lens.field @"encryptionDisabled"
{-# INLINEABLE slcEncryptionDisabled #-}
{-# DEPRECATED encryptionDisabled "Use generic-lens or generic-optics with 'encryptionDisabled' instead"  #-}

-- | The ARN of an S3 bucket and the path prefix for S3 logs. If your Amazon S3 bucket name is @my-bucket@ , and your path prefix is @build-log@ , then acceptable formats are @my-bucket/build-log@ or @arn:aws:s3:::my-bucket/build-log@ . 
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcLocation :: Lens.Lens' S3LogsConfig (Core.Maybe Core.Text)
slcLocation = Lens.field @"location"
{-# INLINEABLE slcLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

instance Core.FromJSON S3LogsConfig where
        toJSON S3LogsConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("status" Core..= status),
                  ("encryptionDisabled" Core..=) Core.<$> encryptionDisabled,
                  ("location" Core..=) Core.<$> location])

instance Core.FromJSON S3LogsConfig where
        parseJSON
          = Core.withObject "S3LogsConfig" Core.$
              \ x ->
                S3LogsConfig' Core.<$>
                  (x Core..: "status") Core.<*> x Core..:? "encryptionDisabled"
                    Core.<*> x Core..:? "location"
