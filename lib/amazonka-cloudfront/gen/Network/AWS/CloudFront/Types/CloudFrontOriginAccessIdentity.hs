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
    cfoaiCloudFrontOriginAccessIdentityConfig,
    cfoaiId,
    cfoaiS3CanonicalUserId,
  )
where

import Network.AWS.CloudFront.Types.CloudFrontOriginAccessIdentityConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | CloudFront origin access identity.
--
-- /See:/ 'mkCloudFrontOriginAccessIdentity' smart constructor.
data CloudFrontOriginAccessIdentity = CloudFrontOriginAccessIdentity'
  { cloudFrontOriginAccessIdentityConfig ::
      Lude.Maybe
        CloudFrontOriginAccessIdentityConfig,
    id :: Lude.Text,
    s3CanonicalUserId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudFrontOriginAccessIdentity' with the minimum fields required to make a request.
--
-- * 'cloudFrontOriginAccessIdentityConfig' - The current configuration information for the identity.
-- * 'id' - The ID for the origin access identity, for example, @E74FTE3AJFJ256A@ .
-- * 's3CanonicalUserId' - The Amazon S3 canonical user ID for the origin access identity, used when giving the origin access identity read permission to an object in Amazon S3.
mkCloudFrontOriginAccessIdentity ::
  -- | 'id'
  Lude.Text ->
  -- | 's3CanonicalUserId'
  Lude.Text ->
  CloudFrontOriginAccessIdentity
mkCloudFrontOriginAccessIdentity pId_ pS3CanonicalUserId_ =
  CloudFrontOriginAccessIdentity'
    { cloudFrontOriginAccessIdentityConfig =
        Lude.Nothing,
      id = pId_,
      s3CanonicalUserId = pS3CanonicalUserId_
    }

-- | The current configuration information for the identity.
--
-- /Note:/ Consider using 'cloudFrontOriginAccessIdentityConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoaiCloudFrontOriginAccessIdentityConfig :: Lens.Lens' CloudFrontOriginAccessIdentity (Lude.Maybe CloudFrontOriginAccessIdentityConfig)
cfoaiCloudFrontOriginAccessIdentityConfig = Lens.lens (cloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentity -> Lude.Maybe CloudFrontOriginAccessIdentityConfig) (\s a -> s {cloudFrontOriginAccessIdentityConfig = a} :: CloudFrontOriginAccessIdentity)
{-# DEPRECATED cfoaiCloudFrontOriginAccessIdentityConfig "Use generic-lens or generic-optics with 'cloudFrontOriginAccessIdentityConfig' instead." #-}

-- | The ID for the origin access identity, for example, @E74FTE3AJFJ256A@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoaiId :: Lens.Lens' CloudFrontOriginAccessIdentity Lude.Text
cfoaiId = Lens.lens (id :: CloudFrontOriginAccessIdentity -> Lude.Text) (\s a -> s {id = a} :: CloudFrontOriginAccessIdentity)
{-# DEPRECATED cfoaiId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The Amazon S3 canonical user ID for the origin access identity, used when giving the origin access identity read permission to an object in Amazon S3.
--
-- /Note:/ Consider using 's3CanonicalUserId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfoaiS3CanonicalUserId :: Lens.Lens' CloudFrontOriginAccessIdentity Lude.Text
cfoaiS3CanonicalUserId = Lens.lens (s3CanonicalUserId :: CloudFrontOriginAccessIdentity -> Lude.Text) (\s a -> s {s3CanonicalUserId = a} :: CloudFrontOriginAccessIdentity)
{-# DEPRECATED cfoaiS3CanonicalUserId "Use generic-lens or generic-optics with 's3CanonicalUserId' instead." #-}

instance Lude.FromXML CloudFrontOriginAccessIdentity where
  parseXML x =
    CloudFrontOriginAccessIdentity'
      Lude.<$> (x Lude..@? "CloudFrontOriginAccessIdentityConfig")
      Lude.<*> (x Lude..@ "Id")
      Lude.<*> (x Lude..@ "S3CanonicalUserId")
