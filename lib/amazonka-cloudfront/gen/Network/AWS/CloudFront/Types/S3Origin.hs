-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.S3Origin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.S3Origin
  ( S3Origin (..),

    -- * Smart constructor
    mkS3Origin,

    -- * Lenses
    soDomainName,
    soOriginAccessIdentity,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.
--
-- /See:/ 'mkS3Origin' smart constructor.
data S3Origin = S3Origin'
  { domainName :: Lude.Text,
    originAccessIdentity :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3Origin' with the minimum fields required to make a request.
--
-- * 'domainName' - The DNS name of the Amazon S3 origin.
-- * 'originAccessIdentity' - The CloudFront origin access identity to associate with the distribution. Use an origin access identity to configure the distribution so that end users can only access objects in an Amazon S3 bucket through CloudFront.
--
-- If you want end users to be able to access objects using either the CloudFront URL or the Amazon S3 URL, specify an empty @OriginAccessIdentity@ element.
-- To delete the origin access identity from an existing distribution, update the distribution configuration and include an empty @OriginAccessIdentity@ element.
-- To replace the origin access identity, update the distribution configuration and specify the new origin access identity.
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-restricting-access-to-s3.html Using an Origin Access Identity to Restrict Access to Your Amazon S3 Content> in the /Amazon CloudFront Developer Guide/ .
mkS3Origin ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'originAccessIdentity'
  Lude.Text ->
  S3Origin
mkS3Origin pDomainName_ pOriginAccessIdentity_ =
  S3Origin'
    { domainName = pDomainName_,
      originAccessIdentity = pOriginAccessIdentity_
    }

-- | The DNS name of the Amazon S3 origin.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soDomainName :: Lens.Lens' S3Origin Lude.Text
soDomainName = Lens.lens (domainName :: S3Origin -> Lude.Text) (\s a -> s {domainName = a} :: S3Origin)
{-# DEPRECATED soDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The CloudFront origin access identity to associate with the distribution. Use an origin access identity to configure the distribution so that end users can only access objects in an Amazon S3 bucket through CloudFront.
--
-- If you want end users to be able to access objects using either the CloudFront URL or the Amazon S3 URL, specify an empty @OriginAccessIdentity@ element.
-- To delete the origin access identity from an existing distribution, update the distribution configuration and include an empty @OriginAccessIdentity@ element.
-- To replace the origin access identity, update the distribution configuration and specify the new origin access identity.
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-restricting-access-to-s3.html Using an Origin Access Identity to Restrict Access to Your Amazon S3 Content> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'originAccessIdentity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soOriginAccessIdentity :: Lens.Lens' S3Origin Lude.Text
soOriginAccessIdentity = Lens.lens (originAccessIdentity :: S3Origin -> Lude.Text) (\s a -> s {originAccessIdentity = a} :: S3Origin)
{-# DEPRECATED soOriginAccessIdentity "Use generic-lens or generic-optics with 'originAccessIdentity' instead." #-}

instance Lude.FromXML S3Origin where
  parseXML x =
    S3Origin'
      Lude.<$> (x Lude..@ "DomainName")
      Lude.<*> (x Lude..@ "OriginAccessIdentity")

instance Lude.ToXML S3Origin where
  toXML S3Origin' {..} =
    Lude.mconcat
      [ "DomainName" Lude.@= domainName,
        "OriginAccessIdentity" Lude.@= originAccessIdentity
      ]
