{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UserBucketDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UserBucketDetails
  ( UserBucketDetails (..),

    -- * Smart constructor
    mkUserBucketDetails,

    -- * Lenses
    ubdS3Bucket,
    ubdS3Key,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the Amazon S3 bucket for the disk image.
--
-- /See:/ 'mkUserBucketDetails' smart constructor.
data UserBucketDetails = UserBucketDetails'
  { -- | The Amazon S3 bucket from which the disk image was created.
    s3Bucket :: Core.Maybe Types.String,
    -- | The file name of the disk image.
    s3Key :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserBucketDetails' value with any optional fields omitted.
mkUserBucketDetails ::
  UserBucketDetails
mkUserBucketDetails =
  UserBucketDetails' {s3Bucket = Core.Nothing, s3Key = Core.Nothing}

-- | The Amazon S3 bucket from which the disk image was created.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubdS3Bucket :: Lens.Lens' UserBucketDetails (Core.Maybe Types.String)
ubdS3Bucket = Lens.field @"s3Bucket"
{-# DEPRECATED ubdS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

-- | The file name of the disk image.
--
-- /Note:/ Consider using 's3Key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubdS3Key :: Lens.Lens' UserBucketDetails (Core.Maybe Types.String)
ubdS3Key = Lens.field @"s3Key"
{-# DEPRECATED ubdS3Key "Use generic-lens or generic-optics with 's3Key' instead." #-}

instance Core.FromXML UserBucketDetails where
  parseXML x =
    UserBucketDetails'
      Core.<$> (x Core..@? "s3Bucket") Core.<*> (x Core..@? "s3Key")
