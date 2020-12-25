{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentitySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentitySummary
  ( CloudFrontOriginAccessIdentitySummary (..),

    -- * Smart constructor
    mkCloudFrontOriginAccessIdentitySummary,

    -- * Lenses
    cfoaisId,
    cfoaisS3CanonicalUserId,
    cfoaisComment,
  )
where

import qualified Network.AWS.CloudFront.Types.Comment as Types
import qualified Network.AWS.CloudFront.Types.Id as Types
import qualified Network.AWS.CloudFront.Types.S3CanonicalUserId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Summary of the information about a CloudFront origin access identity.
--
-- /See:/ 'mkCloudFrontOriginAccessIdentitySummary' smart constructor.
data CloudFrontOriginAccessIdentitySummary = CloudFrontOriginAccessIdentitySummary'
  { -- | The ID for the origin access identity. For example: @E74FTE3AJFJ256A@ .
    id :: Types.Id,
    -- | The Amazon S3 canonical user ID for the origin access identity, which you use when giving the origin access identity read permission to an object in Amazon S3.
    s3CanonicalUserId :: Types.S3CanonicalUserId,
    -- | The comment for this origin access identity, as originally specified when created.
    comment :: Types.Comment
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudFrontOriginAccessIdentitySummary' value with any optional fields omitted.
mkCloudFrontOriginAccessIdentitySummary ::
  -- | 'id'
  Types.Id ->
  -- | 's3CanonicalUserId'
  Types.S3CanonicalUserId ->
  -- | 'comment'
  Types.Comment ->
  CloudFrontOriginAccessIdentitySummary
mkCloudFrontOriginAccessIdentitySummary
  id
  s3CanonicalUserId
  comment =
    CloudFrontOriginAccessIdentitySummary'
      { id,
        s3CanonicalUserId,
        comment
      }

-- | The ID for the origin access identity. For example: @E74FTE3AJFJ256A@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoaisId :: Lens.Lens' CloudFrontOriginAccessIdentitySummary Types.Id
cfoaisId = Lens.field @"id"
{-# DEPRECATED cfoaisId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The Amazon S3 canonical user ID for the origin access identity, which you use when giving the origin access identity read permission to an object in Amazon S3.
--
-- /Note:/ Consider using 's3CanonicalUserId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoaisS3CanonicalUserId :: Lens.Lens' CloudFrontOriginAccessIdentitySummary Types.S3CanonicalUserId
cfoaisS3CanonicalUserId = Lens.field @"s3CanonicalUserId"
{-# DEPRECATED cfoaisS3CanonicalUserId "Use generic-lens or generic-optics with 's3CanonicalUserId' instead." #-}

-- | The comment for this origin access identity, as originally specified when created.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoaisComment :: Lens.Lens' CloudFrontOriginAccessIdentitySummary Types.Comment
cfoaisComment = Lens.field @"comment"
{-# DEPRECATED cfoaisComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Core.FromXML CloudFrontOriginAccessIdentitySummary where
  parseXML x =
    CloudFrontOriginAccessIdentitySummary'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@ "S3CanonicalUserId")
      Core.<*> (x Core..@ "Comment")
