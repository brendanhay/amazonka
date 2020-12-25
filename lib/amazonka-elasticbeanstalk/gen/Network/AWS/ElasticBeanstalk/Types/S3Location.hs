{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.S3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.S3Location
  ( S3Location (..),

    -- * Smart constructor
    mkS3Location,

    -- * Lenses
    slS3Bucket,
    slS3Key,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.S3Bucket as Types
import qualified Network.AWS.ElasticBeanstalk.Types.S3Key as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The bucket and key of an item stored in Amazon S3.
--
-- /See:/ 'mkS3Location' smart constructor.
data S3Location = S3Location'
  { -- | The Amazon S3 bucket where the data is located.
    s3Bucket :: Core.Maybe Types.S3Bucket,
    -- | The Amazon S3 key where the data is located.
    s3Key :: Core.Maybe Types.S3Key
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3Location' value with any optional fields omitted.
mkS3Location ::
  S3Location
mkS3Location =
  S3Location' {s3Bucket = Core.Nothing, s3Key = Core.Nothing}

-- | The Amazon S3 bucket where the data is located.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slS3Bucket :: Lens.Lens' S3Location (Core.Maybe Types.S3Bucket)
slS3Bucket = Lens.field @"s3Bucket"
{-# DEPRECATED slS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

-- | The Amazon S3 key where the data is located.
--
-- /Note:/ Consider using 's3Key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slS3Key :: Lens.Lens' S3Location (Core.Maybe Types.S3Key)
slS3Key = Lens.field @"s3Key"
{-# DEPRECATED slS3Key "Use generic-lens or generic-optics with 's3Key' instead." #-}

instance Core.FromXML S3Location where
  parseXML x =
    S3Location'
      Core.<$> (x Core..@? "S3Bucket") Core.<*> (x Core..@? "S3Key")
