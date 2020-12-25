{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.LoggingInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.LoggingInfo
  ( LoggingInfo (..),

    -- * Smart constructor
    mkLoggingInfo,

    -- * Lenses
    liS3BucketName,
    liS3Region,
    liS3KeyPrefix,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.S3BucketName as Types
import qualified Network.AWS.SSM.Types.S3KeyPrefix as Types
import qualified Network.AWS.SSM.Types.S3Region as Types

-- | Information about an S3 bucket to write instance-level logs to.
--
-- /See:/ 'mkLoggingInfo' smart constructor.
data LoggingInfo = LoggingInfo'
  { -- | The name of an S3 bucket where execution logs are stored .
    s3BucketName :: Types.S3BucketName,
    -- | The Region where the S3 bucket is located.
    s3Region :: Types.S3Region,
    -- | (Optional) The S3 bucket subfolder.
    s3KeyPrefix :: Core.Maybe Types.S3KeyPrefix
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoggingInfo' value with any optional fields omitted.
mkLoggingInfo ::
  -- | 's3BucketName'
  Types.S3BucketName ->
  -- | 's3Region'
  Types.S3Region ->
  LoggingInfo
mkLoggingInfo s3BucketName s3Region =
  LoggingInfo' {s3BucketName, s3Region, s3KeyPrefix = Core.Nothing}

-- | The name of an S3 bucket where execution logs are stored .
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liS3BucketName :: Lens.Lens' LoggingInfo Types.S3BucketName
liS3BucketName = Lens.field @"s3BucketName"
{-# DEPRECATED liS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | The Region where the S3 bucket is located.
--
-- /Note:/ Consider using 's3Region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liS3Region :: Lens.Lens' LoggingInfo Types.S3Region
liS3Region = Lens.field @"s3Region"
{-# DEPRECATED liS3Region "Use generic-lens or generic-optics with 's3Region' instead." #-}

-- | (Optional) The S3 bucket subfolder.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liS3KeyPrefix :: Lens.Lens' LoggingInfo (Core.Maybe Types.S3KeyPrefix)
liS3KeyPrefix = Lens.field @"s3KeyPrefix"
{-# DEPRECATED liS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

instance Core.FromJSON LoggingInfo where
  toJSON LoggingInfo {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("S3BucketName" Core..= s3BucketName),
            Core.Just ("S3Region" Core..= s3Region),
            ("S3KeyPrefix" Core..=) Core.<$> s3KeyPrefix
          ]
      )

instance Core.FromJSON LoggingInfo where
  parseJSON =
    Core.withObject "LoggingInfo" Core.$
      \x ->
        LoggingInfo'
          Core.<$> (x Core..: "S3BucketName")
          Core.<*> (x Core..: "S3Region")
          Core.<*> (x Core..:? "S3KeyPrefix")
