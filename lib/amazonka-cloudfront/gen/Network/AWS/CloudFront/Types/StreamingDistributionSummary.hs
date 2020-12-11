-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.StreamingDistributionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.StreamingDistributionSummary
  ( StreamingDistributionSummary (..),

    -- * Smart constructor
    mkStreamingDistributionSummary,

    -- * Lenses
    sdsId,
    sdsARN,
    sdsStatus,
    sdsLastModifiedTime,
    sdsDomainName,
    sdsS3Origin,
    sdsAliases,
    sdsTrustedSigners,
    sdsComment,
    sdsPriceClass,
    sdsEnabled,
  )
where

import Network.AWS.CloudFront.Types.Aliases
import Network.AWS.CloudFront.Types.PriceClass
import Network.AWS.CloudFront.Types.S3Origin
import Network.AWS.CloudFront.Types.TrustedSigners
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A summary of the information for a CloudFront streaming distribution.
--
-- /See:/ 'mkStreamingDistributionSummary' smart constructor.
data StreamingDistributionSummary = StreamingDistributionSummary'
  { id ::
      Lude.Text,
    arn :: Lude.Text,
    status :: Lude.Text,
    lastModifiedTime :: Lude.ISO8601,
    domainName :: Lude.Text,
    s3Origin :: S3Origin,
    aliases :: Aliases,
    trustedSigners :: TrustedSigners,
    comment :: Lude.Text,
    priceClass :: PriceClass,
    enabled :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StreamingDistributionSummary' with the minimum fields required to make a request.
--
-- * 'aliases' - A complex type that contains information about CNAMEs (alternate domain names), if any, for this streaming distribution.
-- * 'arn' - The ARN (Amazon Resource Name) for the streaming distribution. For example: @arn:aws:cloudfront::123456789012:streaming-distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
-- * 'comment' - The comment originally specified when this distribution was created.
-- * 'domainName' - The domain name corresponding to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
-- * 'enabled' - Whether the distribution is enabled to accept end user requests for content.
-- * 'id' - The identifier for the distribution, for example, @EDFDVBD632BHDS5@ .
-- * 'lastModifiedTime' - The date and time the distribution was last modified.
-- * 'priceClass' - A complex type that contains information about price class for this streaming distribution.
-- * 's3Origin' - A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.
-- * 'status' - Indicates the current status of the distribution. When the status is @Deployed@ , the distribution's information is fully propagated throughout the Amazon CloudFront system.
-- * 'trustedSigners' - A complex type that specifies the AWS accounts, if any, that you want to allow to create signed URLs for private content. If you want to require signed URLs in requests for objects in the target origin that match the @PathPattern@ for this cache behavior, specify @true@ for @Enabled@ , and specify the applicable values for @Quantity@ and @Items@ .If you don't want to require signed URLs in requests for objects that match @PathPattern@ , specify @false@ for @Enabled@ and @0@ for @Quantity@ . Omit @Items@ . To add, change, or remove one or more trusted signers, change @Enabled@ to @true@ (if it's currently @false@ ), change @Quantity@ as applicable, and specify all of the trusted signers that you want to include in the updated distribution.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
mkStreamingDistributionSummary ::
  -- | 'id'
  Lude.Text ->
  -- | 'arn'
  Lude.Text ->
  -- | 'status'
  Lude.Text ->
  -- | 'lastModifiedTime'
  Lude.ISO8601 ->
  -- | 'domainName'
  Lude.Text ->
  -- | 's3Origin'
  S3Origin ->
  -- | 'aliases'
  Aliases ->
  -- | 'trustedSigners'
  TrustedSigners ->
  -- | 'comment'
  Lude.Text ->
  -- | 'priceClass'
  PriceClass ->
  -- | 'enabled'
  Lude.Bool ->
  StreamingDistributionSummary
mkStreamingDistributionSummary
  pId_
  pARN_
  pStatus_
  pLastModifiedTime_
  pDomainName_
  pS3Origin_
  pAliases_
  pTrustedSigners_
  pComment_
  pPriceClass_
  pEnabled_ =
    StreamingDistributionSummary'
      { id = pId_,
        arn = pARN_,
        status = pStatus_,
        lastModifiedTime = pLastModifiedTime_,
        domainName = pDomainName_,
        s3Origin = pS3Origin_,
        aliases = pAliases_,
        trustedSigners = pTrustedSigners_,
        comment = pComment_,
        priceClass = pPriceClass_,
        enabled = pEnabled_
      }

-- | The identifier for the distribution, for example, @EDFDVBD632BHDS5@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsId :: Lens.Lens' StreamingDistributionSummary Lude.Text
sdsId = Lens.lens (id :: StreamingDistributionSummary -> Lude.Text) (\s a -> s {id = a} :: StreamingDistributionSummary)
{-# DEPRECATED sdsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ARN (Amazon Resource Name) for the streaming distribution. For example: @arn:aws:cloudfront::123456789012:streaming-distribution/EDFDVBD632BHDS5@ , where @123456789012@ is your AWS account ID.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsARN :: Lens.Lens' StreamingDistributionSummary Lude.Text
sdsARN = Lens.lens (arn :: StreamingDistributionSummary -> Lude.Text) (\s a -> s {arn = a} :: StreamingDistributionSummary)
{-# DEPRECATED sdsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Indicates the current status of the distribution. When the status is @Deployed@ , the distribution's information is fully propagated throughout the Amazon CloudFront system.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsStatus :: Lens.Lens' StreamingDistributionSummary Lude.Text
sdsStatus = Lens.lens (status :: StreamingDistributionSummary -> Lude.Text) (\s a -> s {status = a} :: StreamingDistributionSummary)
{-# DEPRECATED sdsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time the distribution was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsLastModifiedTime :: Lens.Lens' StreamingDistributionSummary Lude.ISO8601
sdsLastModifiedTime = Lens.lens (lastModifiedTime :: StreamingDistributionSummary -> Lude.ISO8601) (\s a -> s {lastModifiedTime = a} :: StreamingDistributionSummary)
{-# DEPRECATED sdsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The domain name corresponding to the distribution, for example, @d111111abcdef8.cloudfront.net@ .
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsDomainName :: Lens.Lens' StreamingDistributionSummary Lude.Text
sdsDomainName = Lens.lens (domainName :: StreamingDistributionSummary -> Lude.Text) (\s a -> s {domainName = a} :: StreamingDistributionSummary)
{-# DEPRECATED sdsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | A complex type that contains information about the Amazon S3 bucket from which you want CloudFront to get your media files for distribution.
--
-- /Note:/ Consider using 's3Origin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsS3Origin :: Lens.Lens' StreamingDistributionSummary S3Origin
sdsS3Origin = Lens.lens (s3Origin :: StreamingDistributionSummary -> S3Origin) (\s a -> s {s3Origin = a} :: StreamingDistributionSummary)
{-# DEPRECATED sdsS3Origin "Use generic-lens or generic-optics with 's3Origin' instead." #-}

-- | A complex type that contains information about CNAMEs (alternate domain names), if any, for this streaming distribution.
--
-- /Note:/ Consider using 'aliases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsAliases :: Lens.Lens' StreamingDistributionSummary Aliases
sdsAliases = Lens.lens (aliases :: StreamingDistributionSummary -> Aliases) (\s a -> s {aliases = a} :: StreamingDistributionSummary)
{-# DEPRECATED sdsAliases "Use generic-lens or generic-optics with 'aliases' instead." #-}

-- | A complex type that specifies the AWS accounts, if any, that you want to allow to create signed URLs for private content. If you want to require signed URLs in requests for objects in the target origin that match the @PathPattern@ for this cache behavior, specify @true@ for @Enabled@ , and specify the applicable values for @Quantity@ and @Items@ .If you don't want to require signed URLs in requests for objects that match @PathPattern@ , specify @false@ for @Enabled@ and @0@ for @Quantity@ . Omit @Items@ . To add, change, or remove one or more trusted signers, change @Enabled@ to @true@ (if it's currently @false@ ), change @Quantity@ as applicable, and specify all of the trusted signers that you want to include in the updated distribution.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'trustedSigners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsTrustedSigners :: Lens.Lens' StreamingDistributionSummary TrustedSigners
sdsTrustedSigners = Lens.lens (trustedSigners :: StreamingDistributionSummary -> TrustedSigners) (\s a -> s {trustedSigners = a} :: StreamingDistributionSummary)
{-# DEPRECATED sdsTrustedSigners "Use generic-lens or generic-optics with 'trustedSigners' instead." #-}

-- | The comment originally specified when this distribution was created.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsComment :: Lens.Lens' StreamingDistributionSummary Lude.Text
sdsComment = Lens.lens (comment :: StreamingDistributionSummary -> Lude.Text) (\s a -> s {comment = a} :: StreamingDistributionSummary)
{-# DEPRECATED sdsComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | A complex type that contains information about price class for this streaming distribution.
--
-- /Note:/ Consider using 'priceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsPriceClass :: Lens.Lens' StreamingDistributionSummary PriceClass
sdsPriceClass = Lens.lens (priceClass :: StreamingDistributionSummary -> PriceClass) (\s a -> s {priceClass = a} :: StreamingDistributionSummary)
{-# DEPRECATED sdsPriceClass "Use generic-lens or generic-optics with 'priceClass' instead." #-}

-- | Whether the distribution is enabled to accept end user requests for content.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsEnabled :: Lens.Lens' StreamingDistributionSummary Lude.Bool
sdsEnabled = Lens.lens (enabled :: StreamingDistributionSummary -> Lude.Bool) (\s a -> s {enabled = a} :: StreamingDistributionSummary)
{-# DEPRECATED sdsEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.FromXML StreamingDistributionSummary where
  parseXML x =
    StreamingDistributionSummary'
      Lude.<$> (x Lude..@ "Id")
      Lude.<*> (x Lude..@ "ARN")
      Lude.<*> (x Lude..@ "Status")
      Lude.<*> (x Lude..@ "LastModifiedTime")
      Lude.<*> (x Lude..@ "DomainName")
      Lude.<*> (x Lude..@ "S3Origin")
      Lude.<*> (x Lude..@ "Aliases")
      Lude.<*> (x Lude..@ "TrustedSigners")
      Lude.<*> (x Lude..@ "Comment")
      Lude.<*> (x Lude..@ "PriceClass")
      Lude.<*> (x Lude..@ "Enabled")
