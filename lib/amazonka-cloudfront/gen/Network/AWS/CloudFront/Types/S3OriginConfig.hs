-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.S3OriginConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.S3OriginConfig
  ( S3OriginConfig (..),

    -- * Smart constructor
    mkS3OriginConfig,

    -- * Lenses
    socOriginAccessIdentity,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that contains information about the Amazon S3 origin. If the origin is a custom origin or an S3 bucket that is configured as a website endpoint, use the @CustomOriginConfig@ element instead.
--
-- /See:/ 'mkS3OriginConfig' smart constructor.
newtype S3OriginConfig = S3OriginConfig'
  { originAccessIdentity ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3OriginConfig' with the minimum fields required to make a request.
--
-- * 'originAccessIdentity' - The CloudFront origin access identity to associate with the origin. Use an origin access identity to configure the origin so that viewers can /only/ access objects in an Amazon S3 bucket through CloudFront. The format of the value is:
--
-- origin-access-identity/cloudfront//ID-of-origin-access-identity/
-- where @/ID-of-origin-access-identity/ @ is the value that CloudFront returned in the @ID@ element when you created the origin access identity.
-- If you want viewers to be able to access objects using either the CloudFront URL or the Amazon S3 URL, specify an empty @OriginAccessIdentity@ element.
-- To delete the origin access identity from an existing distribution, update the distribution configuration and include an empty @OriginAccessIdentity@ element.
-- To replace the origin access identity, update the distribution configuration and specify the new origin access identity.
-- For more information about the origin access identity, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
mkS3OriginConfig ::
  -- | 'originAccessIdentity'
  Lude.Text ->
  S3OriginConfig
mkS3OriginConfig pOriginAccessIdentity_ =
  S3OriginConfig' {originAccessIdentity = pOriginAccessIdentity_}

-- | The CloudFront origin access identity to associate with the origin. Use an origin access identity to configure the origin so that viewers can /only/ access objects in an Amazon S3 bucket through CloudFront. The format of the value is:
--
-- origin-access-identity/cloudfront//ID-of-origin-access-identity/
-- where @/ID-of-origin-access-identity/ @ is the value that CloudFront returned in the @ID@ element when you created the origin access identity.
-- If you want viewers to be able to access objects using either the CloudFront URL or the Amazon S3 URL, specify an empty @OriginAccessIdentity@ element.
-- To delete the origin access identity from an existing distribution, update the distribution configuration and include an empty @OriginAccessIdentity@ element.
-- To replace the origin access identity, update the distribution configuration and specify the new origin access identity.
-- For more information about the origin access identity, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'originAccessIdentity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
socOriginAccessIdentity :: Lens.Lens' S3OriginConfig Lude.Text
socOriginAccessIdentity = Lens.lens (originAccessIdentity :: S3OriginConfig -> Lude.Text) (\s a -> s {originAccessIdentity = a} :: S3OriginConfig)
{-# DEPRECATED socOriginAccessIdentity "Use generic-lens or generic-optics with 'originAccessIdentity' instead." #-}

instance Lude.FromXML S3OriginConfig where
  parseXML x =
    S3OriginConfig' Lude.<$> (x Lude..@ "OriginAccessIdentity")

instance Lude.ToXML S3OriginConfig where
  toXML S3OriginConfig' {..} =
    Lude.mconcat
      ["OriginAccessIdentity" Lude.@= originAccessIdentity]
