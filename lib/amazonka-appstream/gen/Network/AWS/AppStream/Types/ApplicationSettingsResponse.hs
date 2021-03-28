{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ApplicationSettingsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.ApplicationSettingsResponse
  ( ApplicationSettingsResponse (..)
  -- * Smart constructor
  , mkApplicationSettingsResponse
  -- * Lenses
  , asrEnabled
  , asrS3BucketName
  , asrSettingsGroup
  ) where

import qualified Network.AWS.AppStream.Types.SettingsGroup as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the persistent application settings for users of a stack.
--
-- /See:/ 'mkApplicationSettingsResponse' smart constructor.
data ApplicationSettingsResponse = ApplicationSettingsResponse'
  { enabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether persistent application settings are enabled for users during their streaming sessions.
  , s3BucketName :: Core.Maybe Core.Text
    -- ^ The S3 bucket where users’ persistent application settings are stored. When persistent application settings are enabled for the first time for an account in an AWS Region, an S3 bucket is created. The bucket is unique to the AWS account and the Region. 
  , settingsGroup :: Core.Maybe Types.SettingsGroup
    -- ^ The path prefix for the S3 bucket where users’ persistent application settings are stored.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplicationSettingsResponse' value with any optional fields omitted.
mkApplicationSettingsResponse
    :: ApplicationSettingsResponse
mkApplicationSettingsResponse
  = ApplicationSettingsResponse'{enabled = Core.Nothing,
                                 s3BucketName = Core.Nothing, settingsGroup = Core.Nothing}

-- | Specifies whether persistent application settings are enabled for users during their streaming sessions.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrEnabled :: Lens.Lens' ApplicationSettingsResponse (Core.Maybe Core.Bool)
asrEnabled = Lens.field @"enabled"
{-# INLINEABLE asrEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The S3 bucket where users’ persistent application settings are stored. When persistent application settings are enabled for the first time for an account in an AWS Region, an S3 bucket is created. The bucket is unique to the AWS account and the Region. 
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrS3BucketName :: Lens.Lens' ApplicationSettingsResponse (Core.Maybe Core.Text)
asrS3BucketName = Lens.field @"s3BucketName"
{-# INLINEABLE asrS3BucketName #-}
{-# DEPRECATED s3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead"  #-}

-- | The path prefix for the S3 bucket where users’ persistent application settings are stored.
--
-- /Note:/ Consider using 'settingsGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrSettingsGroup :: Lens.Lens' ApplicationSettingsResponse (Core.Maybe Types.SettingsGroup)
asrSettingsGroup = Lens.field @"settingsGroup"
{-# INLINEABLE asrSettingsGroup #-}
{-# DEPRECATED settingsGroup "Use generic-lens or generic-optics with 'settingsGroup' instead"  #-}

instance Core.FromJSON ApplicationSettingsResponse where
        parseJSON
          = Core.withObject "ApplicationSettingsResponse" Core.$
              \ x ->
                ApplicationSettingsResponse' Core.<$>
                  (x Core..:? "Enabled") Core.<*> x Core..:? "S3BucketName" Core.<*>
                    x Core..:? "SettingsGroup"
