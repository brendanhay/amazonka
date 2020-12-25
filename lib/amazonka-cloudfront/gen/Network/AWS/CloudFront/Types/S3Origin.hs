{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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

import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.
--
-- /See:/ 'mkS3Origin' smart constructor.
data S3Origin = S3Origin'
  { -- | The DNS name of the Amazon S3 origin.
    domainName :: Types.String,
    -- | The CloudFront origin access identity to associate with the distribution. Use an origin access identity to configure the distribution so that end users can only access objects in an Amazon S3 bucket through CloudFront.
    --
    -- If you want end users to be able to access objects using either the CloudFront URL or the Amazon S3 URL, specify an empty @OriginAccessIdentity@ element.
    -- To delete the origin access identity from an existing distribution, update the distribution configuration and include an empty @OriginAccessIdentity@ element.
    -- To replace the origin access identity, update the distribution configuration and specify the new origin access identity.
    -- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-restricting-access-to-s3.html Using an Origin Access Identity to Restrict Access to Your Amazon S3 Content> in the /Amazon CloudFront Developer Guide/ .
    originAccessIdentity :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3Origin' value with any optional fields omitted.
mkS3Origin ::
  -- | 'domainName'
  Types.String ->
  -- | 'originAccessIdentity'
  Types.String ->
  S3Origin
mkS3Origin domainName originAccessIdentity =
  S3Origin' {domainName, originAccessIdentity}

-- | The DNS name of the Amazon S3 origin.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soDomainName :: Lens.Lens' S3Origin Types.String
soDomainName = Lens.field @"domainName"
{-# DEPRECATED soDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The CloudFront origin access identity to associate with the distribution. Use an origin access identity to configure the distribution so that end users can only access objects in an Amazon S3 bucket through CloudFront.
--
-- If you want end users to be able to access objects using either the CloudFront URL or the Amazon S3 URL, specify an empty @OriginAccessIdentity@ element.
-- To delete the origin access identity from an existing distribution, update the distribution configuration and include an empty @OriginAccessIdentity@ element.
-- To replace the origin access identity, update the distribution configuration and specify the new origin access identity.
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-restricting-access-to-s3.html Using an Origin Access Identity to Restrict Access to Your Amazon S3 Content> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'originAccessIdentity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soOriginAccessIdentity :: Lens.Lens' S3Origin Types.String
soOriginAccessIdentity = Lens.field @"originAccessIdentity"
{-# DEPRECATED soOriginAccessIdentity "Use generic-lens or generic-optics with 'originAccessIdentity' instead." #-}

instance Core.ToXML S3Origin where
  toXML S3Origin {..} =
    Core.toXMLNode "DomainName" domainName
      Core.<> Core.toXMLNode "OriginAccessIdentity" originAccessIdentity

instance Core.FromXML S3Origin where
  parseXML x =
    S3Origin'
      Core.<$> (x Core..@ "DomainName")
      Core.<*> (x Core..@ "OriginAccessIdentity")
