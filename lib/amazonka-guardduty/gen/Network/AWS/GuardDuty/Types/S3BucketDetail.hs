{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.S3BucketDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.S3BucketDetail
  ( S3BucketDetail (..)
  -- * Smart constructor
  , mkS3BucketDetail
  -- * Lenses
  , sbdArn
  , sbdCreatedAt
  , sbdDefaultServerSideEncryption
  , sbdName
  , sbdOwner
  , sbdPublicAccess
  , sbdTags
  , sbdType
  ) where

import qualified Network.AWS.GuardDuty.Types.DefaultServerSideEncryption as Types
import qualified Network.AWS.GuardDuty.Types.Owner as Types
import qualified Network.AWS.GuardDuty.Types.PublicAccess as Types
import qualified Network.AWS.GuardDuty.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information on the S3 bucket.
--
-- /See:/ 'mkS3BucketDetail' smart constructor.
data S3BucketDetail = S3BucketDetail'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the S3 bucket.
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time the bucket was created at.
  , defaultServerSideEncryption :: Core.Maybe Types.DefaultServerSideEncryption
    -- ^ Describes the server side encryption method used in the S3 bucket.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the S3 bucket.
  , owner :: Core.Maybe Types.Owner
    -- ^ The owner of the S3 bucket.
  , publicAccess :: Core.Maybe Types.PublicAccess
    -- ^ Describes the public access policies that apply to the S3 bucket.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ All tags attached to the S3 bucket
  , type' :: Core.Maybe Core.Text
    -- ^ Describes whether the bucket is a source or destination bucket.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'S3BucketDetail' value with any optional fields omitted.
mkS3BucketDetail
    :: S3BucketDetail
mkS3BucketDetail
  = S3BucketDetail'{arn = Core.Nothing, createdAt = Core.Nothing,
                    defaultServerSideEncryption = Core.Nothing, name = Core.Nothing,
                    owner = Core.Nothing, publicAccess = Core.Nothing,
                    tags = Core.Nothing, type' = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the S3 bucket.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdArn :: Lens.Lens' S3BucketDetail (Core.Maybe Core.Text)
sbdArn = Lens.field @"arn"
{-# INLINEABLE sbdArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The date and time the bucket was created at.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdCreatedAt :: Lens.Lens' S3BucketDetail (Core.Maybe Core.NominalDiffTime)
sbdCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE sbdCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | Describes the server side encryption method used in the S3 bucket.
--
-- /Note:/ Consider using 'defaultServerSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdDefaultServerSideEncryption :: Lens.Lens' S3BucketDetail (Core.Maybe Types.DefaultServerSideEncryption)
sbdDefaultServerSideEncryption = Lens.field @"defaultServerSideEncryption"
{-# INLINEABLE sbdDefaultServerSideEncryption #-}
{-# DEPRECATED defaultServerSideEncryption "Use generic-lens or generic-optics with 'defaultServerSideEncryption' instead"  #-}

-- | The name of the S3 bucket.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdName :: Lens.Lens' S3BucketDetail (Core.Maybe Core.Text)
sbdName = Lens.field @"name"
{-# INLINEABLE sbdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The owner of the S3 bucket.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdOwner :: Lens.Lens' S3BucketDetail (Core.Maybe Types.Owner)
sbdOwner = Lens.field @"owner"
{-# INLINEABLE sbdOwner #-}
{-# DEPRECATED owner "Use generic-lens or generic-optics with 'owner' instead"  #-}

-- | Describes the public access policies that apply to the S3 bucket.
--
-- /Note:/ Consider using 'publicAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdPublicAccess :: Lens.Lens' S3BucketDetail (Core.Maybe Types.PublicAccess)
sbdPublicAccess = Lens.field @"publicAccess"
{-# INLINEABLE sbdPublicAccess #-}
{-# DEPRECATED publicAccess "Use generic-lens or generic-optics with 'publicAccess' instead"  #-}

-- | All tags attached to the S3 bucket
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdTags :: Lens.Lens' S3BucketDetail (Core.Maybe [Types.Tag])
sbdTags = Lens.field @"tags"
{-# INLINEABLE sbdTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Describes whether the bucket is a source or destination bucket.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdType :: Lens.Lens' S3BucketDetail (Core.Maybe Core.Text)
sbdType = Lens.field @"type'"
{-# INLINEABLE sbdType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON S3BucketDetail where
        parseJSON
          = Core.withObject "S3BucketDetail" Core.$
              \ x ->
                S3BucketDetail' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "createdAt" Core.<*>
                    x Core..:? "defaultServerSideEncryption"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "owner"
                    Core.<*> x Core..:? "publicAccess"
                    Core.<*> x Core..:? "tags"
                    Core.<*> x Core..:? "type"
