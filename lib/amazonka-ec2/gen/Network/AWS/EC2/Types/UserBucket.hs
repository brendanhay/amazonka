{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UserBucket
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.UserBucket
  ( UserBucket (..)
  -- * Smart constructor
  , mkUserBucket
  -- * Lenses
  , ubS3Bucket
  , ubS3Key
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the Amazon S3 bucket for the disk image.
--
-- /See:/ 'mkUserBucket' smart constructor.
data UserBucket = UserBucket'
  { s3Bucket :: Core.Maybe Core.Text
    -- ^ The name of the Amazon S3 bucket where the disk image is located.
  , s3Key :: Core.Maybe Core.Text
    -- ^ The file name of the disk image.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserBucket' value with any optional fields omitted.
mkUserBucket
    :: UserBucket
mkUserBucket
  = UserBucket'{s3Bucket = Core.Nothing, s3Key = Core.Nothing}

-- | The name of the Amazon S3 bucket where the disk image is located.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubS3Bucket :: Lens.Lens' UserBucket (Core.Maybe Core.Text)
ubS3Bucket = Lens.field @"s3Bucket"
{-# INLINEABLE ubS3Bucket #-}
{-# DEPRECATED s3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead"  #-}

-- | The file name of the disk image.
--
-- /Note:/ Consider using 's3Key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubS3Key :: Lens.Lens' UserBucket (Core.Maybe Core.Text)
ubS3Key = Lens.field @"s3Key"
{-# INLINEABLE ubS3Key #-}
{-# DEPRECATED s3Key "Use generic-lens or generic-optics with 's3Key' instead"  #-}

instance Core.ToQuery UserBucket where
        toQuery UserBucket{..}
          = Core.maybe Core.mempty (Core.toQueryPair "S3Bucket") s3Bucket
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "S3Key") s3Key
