{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Origin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Origin
  ( Origin (..),

    -- * Smart constructor
    mkOrigin,

    -- * Lenses
    oCustomHeaders,
    oCustomOriginConfig,
    oConnectionTimeout,
    oConnectionAttempts,
    oS3OriginConfig,
    oOriginPath,
    oOriginShield,
    oId,
    oDomainName,
  )
where

import Network.AWS.CloudFront.Types.CustomHeaders
import Network.AWS.CloudFront.Types.CustomOriginConfig
import Network.AWS.CloudFront.Types.OriginShield
import Network.AWS.CloudFront.Types.S3OriginConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An origin.
--
-- An origin is the location where content is stored, and from which CloudFront gets content to serve to viewers. To specify an origin:
--
--     * Use @S3OriginConfig@ to specify an Amazon S3 bucket that is not configured with static website hosting.
--
--
--     * Use @CustomOriginConfig@ to specify all other kinds of origins, including:
--
--     * An Amazon S3 bucket that is configured with static website hosting
--
--
--     * An Elastic Load Balancing load balancer
--
--
--     * An AWS Elemental MediaPackage endpoint
--
--
--     * An AWS Elemental MediaStore container
--
--
--     * Any other HTTP server, running on an Amazon EC2 instance or any other kind of host
--
--
--
--
-- For the current maximum number of origins that you can specify per distribution, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/cloudfront-limits.html#limits-web-distributions General Quotas on Web Distributions> in the /Amazon CloudFront Developer Guide/ (quotas were formerly referred to as limits).
--
-- /See:/ 'mkOrigin' smart constructor.
data Origin = Origin'
  { customHeaders :: Lude.Maybe CustomHeaders,
    customOriginConfig :: Lude.Maybe CustomOriginConfig,
    connectionTimeout :: Lude.Maybe Lude.Int,
    connectionAttempts :: Lude.Maybe Lude.Int,
    s3OriginConfig :: Lude.Maybe S3OriginConfig,
    originPath :: Lude.Maybe Lude.Text,
    originShield :: Lude.Maybe OriginShield,
    id :: Lude.Text,
    domainName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Origin' with the minimum fields required to make a request.
--
-- * 'connectionAttempts' - The number of times that CloudFront attempts to connect to the origin. The minimum number is 1, the maximum is 3, and the default (if you don’t specify otherwise) is 3.
--
-- For a custom origin (including an Amazon S3 bucket that’s configured with static website hosting), this value also specifies the number of times that CloudFront attempts to get a response from the origin, in the case of an <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginResponseTimeout Origin Response Timeout> .
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#origin-connection-attempts Origin Connection Attempts> in the /Amazon CloudFront Developer Guide/ .
-- * 'connectionTimeout' - The number of seconds that CloudFront waits when trying to establish a connection to the origin. The minimum timeout is 1 second, the maximum is 10 seconds, and the default (if you don’t specify otherwise) is 10 seconds.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#origin-connection-timeout Origin Connection Timeout> in the /Amazon CloudFront Developer Guide/ .
-- * 'customHeaders' - A list of HTTP header names and values that CloudFront adds to the requests that it sends to the origin.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/add-origin-custom-headers.html Adding Custom Headers to Origin Requests> in the /Amazon CloudFront Developer Guide/ .
-- * 'customOriginConfig' - Use this type to specify an origin that is not an Amazon S3 bucket, with one exception. If the Amazon S3 bucket is configured with static website hosting, use this type. If the Amazon S3 bucket is not configured with static website hosting, use the @S3OriginConfig@ type instead.
-- * 'domainName' - The domain name for the origin.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesDomainName Origin Domain Name> in the /Amazon CloudFront Developer Guide/ .
-- * 'id' - A unique identifier for the origin. This value must be unique within the distribution.
--
-- Use this value to specify the @TargetOriginId@ in a @CacheBehavior@ or @DefaultCacheBehavior@ .
-- * 'originPath' - An optional path that CloudFront appends to the origin domain name when CloudFront requests content from the origin.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginPath Origin Path> in the /Amazon CloudFront Developer Guide/ .
-- * 'originShield' - CloudFront Origin Shield. Using Origin Shield can help reduce the load on your origin.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/origin-shield.html Using Origin Shield> in the /Amazon CloudFront Developer Guide/ .
-- * 's3OriginConfig' - Use this type to specify an origin that is an Amazon S3 bucket that is not configured with static website hosting. To specify any other type of origin, including an Amazon S3 bucket that is configured with static website hosting, use the @CustomOriginConfig@ type instead.
mkOrigin ::
  -- | 'id'
  Lude.Text ->
  -- | 'domainName'
  Lude.Text ->
  Origin
mkOrigin pId_ pDomainName_ =
  Origin'
    { customHeaders = Lude.Nothing,
      customOriginConfig = Lude.Nothing,
      connectionTimeout = Lude.Nothing,
      connectionAttempts = Lude.Nothing,
      s3OriginConfig = Lude.Nothing,
      originPath = Lude.Nothing,
      originShield = Lude.Nothing,
      id = pId_,
      domainName = pDomainName_
    }

-- | A list of HTTP header names and values that CloudFront adds to the requests that it sends to the origin.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/add-origin-custom-headers.html Adding Custom Headers to Origin Requests> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'customHeaders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oCustomHeaders :: Lens.Lens' Origin (Lude.Maybe CustomHeaders)
oCustomHeaders = Lens.lens (customHeaders :: Origin -> Lude.Maybe CustomHeaders) (\s a -> s {customHeaders = a} :: Origin)
{-# DEPRECATED oCustomHeaders "Use generic-lens or generic-optics with 'customHeaders' instead." #-}

-- | Use this type to specify an origin that is not an Amazon S3 bucket, with one exception. If the Amazon S3 bucket is configured with static website hosting, use this type. If the Amazon S3 bucket is not configured with static website hosting, use the @S3OriginConfig@ type instead.
--
-- /Note:/ Consider using 'customOriginConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oCustomOriginConfig :: Lens.Lens' Origin (Lude.Maybe CustomOriginConfig)
oCustomOriginConfig = Lens.lens (customOriginConfig :: Origin -> Lude.Maybe CustomOriginConfig) (\s a -> s {customOriginConfig = a} :: Origin)
{-# DEPRECATED oCustomOriginConfig "Use generic-lens or generic-optics with 'customOriginConfig' instead." #-}

-- | The number of seconds that CloudFront waits when trying to establish a connection to the origin. The minimum timeout is 1 second, the maximum is 10 seconds, and the default (if you don’t specify otherwise) is 10 seconds.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#origin-connection-timeout Origin Connection Timeout> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'connectionTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oConnectionTimeout :: Lens.Lens' Origin (Lude.Maybe Lude.Int)
oConnectionTimeout = Lens.lens (connectionTimeout :: Origin -> Lude.Maybe Lude.Int) (\s a -> s {connectionTimeout = a} :: Origin)
{-# DEPRECATED oConnectionTimeout "Use generic-lens or generic-optics with 'connectionTimeout' instead." #-}

-- | The number of times that CloudFront attempts to connect to the origin. The minimum number is 1, the maximum is 3, and the default (if you don’t specify otherwise) is 3.
--
-- For a custom origin (including an Amazon S3 bucket that’s configured with static website hosting), this value also specifies the number of times that CloudFront attempts to get a response from the origin, in the case of an <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginResponseTimeout Origin Response Timeout> .
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#origin-connection-attempts Origin Connection Attempts> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'connectionAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oConnectionAttempts :: Lens.Lens' Origin (Lude.Maybe Lude.Int)
oConnectionAttempts = Lens.lens (connectionAttempts :: Origin -> Lude.Maybe Lude.Int) (\s a -> s {connectionAttempts = a} :: Origin)
{-# DEPRECATED oConnectionAttempts "Use generic-lens or generic-optics with 'connectionAttempts' instead." #-}

-- | Use this type to specify an origin that is an Amazon S3 bucket that is not configured with static website hosting. To specify any other type of origin, including an Amazon S3 bucket that is configured with static website hosting, use the @CustomOriginConfig@ type instead.
--
-- /Note:/ Consider using 's3OriginConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oS3OriginConfig :: Lens.Lens' Origin (Lude.Maybe S3OriginConfig)
oS3OriginConfig = Lens.lens (s3OriginConfig :: Origin -> Lude.Maybe S3OriginConfig) (\s a -> s {s3OriginConfig = a} :: Origin)
{-# DEPRECATED oS3OriginConfig "Use generic-lens or generic-optics with 's3OriginConfig' instead." #-}

-- | An optional path that CloudFront appends to the origin domain name when CloudFront requests content from the origin.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginPath Origin Path> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'originPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOriginPath :: Lens.Lens' Origin (Lude.Maybe Lude.Text)
oOriginPath = Lens.lens (originPath :: Origin -> Lude.Maybe Lude.Text) (\s a -> s {originPath = a} :: Origin)
{-# DEPRECATED oOriginPath "Use generic-lens or generic-optics with 'originPath' instead." #-}

-- | CloudFront Origin Shield. Using Origin Shield can help reduce the load on your origin.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/origin-shield.html Using Origin Shield> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'originShield' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOriginShield :: Lens.Lens' Origin (Lude.Maybe OriginShield)
oOriginShield = Lens.lens (originShield :: Origin -> Lude.Maybe OriginShield) (\s a -> s {originShield = a} :: Origin)
{-# DEPRECATED oOriginShield "Use generic-lens or generic-optics with 'originShield' instead." #-}

-- | A unique identifier for the origin. This value must be unique within the distribution.
--
-- Use this value to specify the @TargetOriginId@ in a @CacheBehavior@ or @DefaultCacheBehavior@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oId :: Lens.Lens' Origin Lude.Text
oId = Lens.lens (id :: Origin -> Lude.Text) (\s a -> s {id = a} :: Origin)
{-# DEPRECATED oId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The domain name for the origin.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesDomainName Origin Domain Name> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oDomainName :: Lens.Lens' Origin Lude.Text
oDomainName = Lens.lens (domainName :: Origin -> Lude.Text) (\s a -> s {domainName = a} :: Origin)
{-# DEPRECATED oDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.FromXML Origin where
  parseXML x =
    Origin'
      Lude.<$> (x Lude..@? "CustomHeaders")
      Lude.<*> (x Lude..@? "CustomOriginConfig")
      Lude.<*> (x Lude..@? "ConnectionTimeout")
      Lude.<*> (x Lude..@? "ConnectionAttempts")
      Lude.<*> (x Lude..@? "S3OriginConfig")
      Lude.<*> (x Lude..@? "OriginPath")
      Lude.<*> (x Lude..@? "OriginShield")
      Lude.<*> (x Lude..@ "Id")
      Lude.<*> (x Lude..@ "DomainName")

instance Lude.ToXML Origin where
  toXML Origin' {..} =
    Lude.mconcat
      [ "CustomHeaders" Lude.@= customHeaders,
        "CustomOriginConfig" Lude.@= customOriginConfig,
        "ConnectionTimeout" Lude.@= connectionTimeout,
        "ConnectionAttempts" Lude.@= connectionAttempts,
        "S3OriginConfig" Lude.@= s3OriginConfig,
        "OriginPath" Lude.@= originPath,
        "OriginShield" Lude.@= originShield,
        "Id" Lude.@= id,
        "DomainName" Lude.@= domainName
      ]
