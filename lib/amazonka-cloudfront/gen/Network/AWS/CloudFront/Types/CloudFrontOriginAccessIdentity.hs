{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentity
  ( CloudFrontOriginAccessIdentity (..),

    -- * Smart constructor
    mkCloudFrontOriginAccessIdentity,

    -- * Lenses
    cfoaiId,
    cfoaiS3CanonicalUserId,
    cfoaiCloudFrontOriginAccessIdentityConfig,
  )
where

import qualified Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityConfig as Types
import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | CloudFront origin access identity.
--
-- /See:/ 'mkCloudFrontOriginAccessIdentity' smart constructor.
data CloudFrontOriginAccessIdentity = CloudFrontOriginAccessIdentity'
  { -- | The ID for the origin access identity, for example, @E74FTE3AJFJ256A@ .
    id :: Types.String,
    -- | The Amazon S3 canonical user ID for the origin access identity, used when giving the origin access identity read permission to an object in Amazon S3.
    s3CanonicalUserId :: Types.String,
    -- | The current configuration information for the identity.
    cloudFrontOriginAccessIdentityConfig :: Core.Maybe Types.CloudFrontOriginAccessIdentityConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudFrontOriginAccessIdentity' value with any optional fields omitted.
mkCloudFrontOriginAccessIdentity ::
  -- | 'id'
  Types.String ->
  -- | 's3CanonicalUserId'
  Types.String ->
  CloudFrontOriginAccessIdentity
mkCloudFrontOriginAccessIdentity id s3CanonicalUserId =
  CloudFrontOriginAccessIdentity'
    { id,
      s3CanonicalUserId,
      cloudFrontOriginAccessIdentityConfig = Core.Nothing
    }

-- | The ID for the origin access identity, for example, @E74FTE3AJFJ256A@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoaiId :: Lens.Lens' CloudFrontOriginAccessIdentity Types.String
cfoaiId = Lens.field @"id"
{-# DEPRECATED cfoaiId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The Amazon S3 canonical user ID for the origin access identity, used when giving the origin access identity read permission to an object in Amazon S3.
--
-- /Note:/ Consider using 's3CanonicalUserId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoaiS3CanonicalUserId :: Lens.Lens' CloudFrontOriginAccessIdentity Types.String
cfoaiS3CanonicalUserId = Lens.field @"s3CanonicalUserId"
{-# DEPRECATED cfoaiS3CanonicalUserId "Use generic-lens or generic-optics with 's3CanonicalUserId' instead." #-}

-- | The current configuration information for the identity.
--
-- /Note:/ Consider using 'cloudFrontOriginAccessIdentityConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoaiCloudFrontOriginAccessIdentityConfig :: Lens.Lens' CloudFrontOriginAccessIdentity (Core.Maybe Types.CloudFrontOriginAccessIdentityConfig)
cfoaiCloudFrontOriginAccessIdentityConfig = Lens.field @"cloudFrontOriginAccessIdentityConfig"
{-# DEPRECATED cfoaiCloudFrontOriginAccessIdentityConfig "Use generic-lens or generic-optics with 'cloudFrontOriginAccessIdentityConfig' instead." #-}

instance Core.FromXML CloudFrontOriginAccessIdentity where
  parseXML x =
    CloudFrontOriginAccessIdentity'
      Core.<$> (x Core..@ "Id")
      Core.<*> (x Core..@ "S3CanonicalUserId")
      Core.<*> (x Core..@? "CloudFrontOriginAccessIdentityConfig")
