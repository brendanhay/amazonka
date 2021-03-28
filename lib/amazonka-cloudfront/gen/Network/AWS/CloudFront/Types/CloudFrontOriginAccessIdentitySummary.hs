{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentitySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentitySummary
  ( CloudFrontOriginAccessIdentitySummary (..)
  -- * Smart constructor
  , mkCloudFrontOriginAccessIdentitySummary
  -- * Lenses
  , cfoaisId
  , cfoaisS3CanonicalUserId
  , cfoaisComment
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Summary of the information about a CloudFront origin access identity.
--
-- /See:/ 'mkCloudFrontOriginAccessIdentitySummary' smart constructor.
data CloudFrontOriginAccessIdentitySummary = CloudFrontOriginAccessIdentitySummary'
  { id :: Core.Text
    -- ^ The ID for the origin access identity. For example: @E74FTE3AJFJ256A@ .
  , s3CanonicalUserId :: Core.Text
    -- ^ The Amazon S3 canonical user ID for the origin access identity, which you use when giving the origin access identity read permission to an object in Amazon S3.
  , comment :: Core.Text
    -- ^ The comment for this origin access identity, as originally specified when created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudFrontOriginAccessIdentitySummary' value with any optional fields omitted.
mkCloudFrontOriginAccessIdentitySummary
    :: Core.Text -- ^ 'id'
    -> Core.Text -- ^ 's3CanonicalUserId'
    -> Core.Text -- ^ 'comment'
    -> CloudFrontOriginAccessIdentitySummary
mkCloudFrontOriginAccessIdentitySummary id s3CanonicalUserId
  comment
  = CloudFrontOriginAccessIdentitySummary'{id, s3CanonicalUserId,
                                           comment}

-- | The ID for the origin access identity. For example: @E74FTE3AJFJ256A@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoaisId :: Lens.Lens' CloudFrontOriginAccessIdentitySummary Core.Text
cfoaisId = Lens.field @"id"
{-# INLINEABLE cfoaisId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The Amazon S3 canonical user ID for the origin access identity, which you use when giving the origin access identity read permission to an object in Amazon S3.
--
-- /Note:/ Consider using 's3CanonicalUserId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoaisS3CanonicalUserId :: Lens.Lens' CloudFrontOriginAccessIdentitySummary Core.Text
cfoaisS3CanonicalUserId = Lens.field @"s3CanonicalUserId"
{-# INLINEABLE cfoaisS3CanonicalUserId #-}
{-# DEPRECATED s3CanonicalUserId "Use generic-lens or generic-optics with 's3CanonicalUserId' instead"  #-}

-- | The comment for this origin access identity, as originally specified when created.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoaisComment :: Lens.Lens' CloudFrontOriginAccessIdentitySummary Core.Text
cfoaisComment = Lens.field @"comment"
{-# INLINEABLE cfoaisComment #-}
{-# DEPRECATED comment "Use generic-lens or generic-optics with 'comment' instead"  #-}

instance Core.FromXML CloudFrontOriginAccessIdentitySummary where
        parseXML x
          = CloudFrontOriginAccessIdentitySummary' Core.<$>
              (x Core..@ "Id") Core.<*> x Core..@ "S3CanonicalUserId" Core.<*>
                x Core..@ "Comment"
