{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.AccessLog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.AccessLog
  ( AccessLog (..),

    -- * Smart constructor
    mkAccessLog,

    -- * Lenses
    alEnabled,
    alEmitInterval,
    alS3BucketName,
    alS3BucketPrefix,
  )
where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.ELB.Types.S3BucketName as Types
import qualified Network.AWS.ELB.Types.S3BucketPrefix as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the @AccessLog@ attribute.
--
-- /See:/ 'mkAccessLog' smart constructor.
data AccessLog = AccessLog'
  { -- | Specifies whether access logs are enabled for the load balancer.
    enabled :: Core.Bool,
    -- | The interval for publishing the access logs. You can specify an interval of either 5 minutes or 60 minutes.
    --
    -- Default: 60 minutes
    emitInterval :: Core.Maybe Core.Int,
    -- | The name of the Amazon S3 bucket where the access logs are stored.
    s3BucketName :: Core.Maybe Types.S3BucketName,
    -- | The logical hierarchy you created for your Amazon S3 bucket, for example @my-bucket-prefix/prod@ . If the prefix is not provided, the log is placed at the root level of the bucket.
    s3BucketPrefix :: Core.Maybe Types.S3BucketPrefix
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AccessLog' value with any optional fields omitted.
mkAccessLog ::
  -- | 'enabled'
  Core.Bool ->
  AccessLog
mkAccessLog enabled =
  AccessLog'
    { enabled,
      emitInterval = Core.Nothing,
      s3BucketName = Core.Nothing,
      s3BucketPrefix = Core.Nothing
    }

-- | Specifies whether access logs are enabled for the load balancer.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alEnabled :: Lens.Lens' AccessLog Core.Bool
alEnabled = Lens.field @"enabled"
{-# DEPRECATED alEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The interval for publishing the access logs. You can specify an interval of either 5 minutes or 60 minutes.
--
-- Default: 60 minutes
--
-- /Note:/ Consider using 'emitInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alEmitInterval :: Lens.Lens' AccessLog (Core.Maybe Core.Int)
alEmitInterval = Lens.field @"emitInterval"
{-# DEPRECATED alEmitInterval "Use generic-lens or generic-optics with 'emitInterval' instead." #-}

-- | The name of the Amazon S3 bucket where the access logs are stored.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alS3BucketName :: Lens.Lens' AccessLog (Core.Maybe Types.S3BucketName)
alS3BucketName = Lens.field @"s3BucketName"
{-# DEPRECATED alS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | The logical hierarchy you created for your Amazon S3 bucket, for example @my-bucket-prefix/prod@ . If the prefix is not provided, the log is placed at the root level of the bucket.
--
-- /Note:/ Consider using 's3BucketPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
alS3BucketPrefix :: Lens.Lens' AccessLog (Core.Maybe Types.S3BucketPrefix)
alS3BucketPrefix = Lens.field @"s3BucketPrefix"
{-# DEPRECATED alS3BucketPrefix "Use generic-lens or generic-optics with 's3BucketPrefix' instead." #-}

instance Core.FromXML AccessLog where
  parseXML x =
    AccessLog'
      Core.<$> (x Core..@ "Enabled")
      Core.<*> (x Core..@? "EmitInterval")
      Core.<*> (x Core..@? "S3BucketName")
      Core.<*> (x Core..@? "S3BucketPrefix")
