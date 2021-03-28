{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CustomOriginConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.CustomOriginConfig
  ( CustomOriginConfig (..)
  -- * Smart constructor
  , mkCustomOriginConfig
  -- * Lenses
  , cocHTTPPort
  , cocHTTPSPort
  , cocOriginProtocolPolicy
  , cocOriginKeepaliveTimeout
  , cocOriginReadTimeout
  , cocOriginSslProtocols
  ) where

import qualified Network.AWS.CloudFront.Types.OriginProtocolPolicy as Types
import qualified Network.AWS.CloudFront.Types.OriginSslProtocols as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A custom origin. A custom origin is any origin that is /not/ an Amazon S3 bucket, with one exception. An Amazon S3 bucket that is <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html configured with static website hosting> /is/ a custom origin.
--
-- /See:/ 'mkCustomOriginConfig' smart constructor.
data CustomOriginConfig = CustomOriginConfig'
  { hTTPPort :: Core.Int
    -- ^ The HTTP port that CloudFront uses to connect to the origin. Specify the HTTP port that the origin listens on.
  , hTTPSPort :: Core.Int
    -- ^ The HTTPS port that CloudFront uses to connect to the origin. Specify the HTTPS port that the origin listens on.
  , originProtocolPolicy :: Types.OriginProtocolPolicy
    -- ^ Specifies the protocol (HTTP or HTTPS) that CloudFront uses to connect to the origin. Valid values are:
--
--
--     * @http-only@ – CloudFront always uses HTTP to connect to the origin.
--
--
--     * @match-viewer@ – CloudFront connects to the origin using the same protocol that the viewer used to connect to CloudFront.
--
--
--     * @https-only@ – CloudFront always uses HTTPS to connect to the origin.
--
--
  , originKeepaliveTimeout :: Core.Maybe Core.Int
    -- ^ Specifies how long, in seconds, CloudFront persists its connection to the origin. The minimum timeout is 1 second, the maximum is 60 seconds, and the default (if you don’t specify otherwise) is 5 seconds.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginKeepaliveTimeout Origin Keep-alive Timeout> in the /Amazon CloudFront Developer Guide/ .
  , originReadTimeout :: Core.Maybe Core.Int
    -- ^ Specifies how long, in seconds, CloudFront waits for a response from the origin. This is also known as the /origin response timeout/ . The minimum timeout is 1 second, the maximum is 60 seconds, and the default (if you don’t specify otherwise) is 30 seconds.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginResponseTimeout Origin Response Timeout> in the /Amazon CloudFront Developer Guide/ .
  , originSslProtocols :: Core.Maybe Types.OriginSslProtocols
    -- ^ Specifies the minimum SSL/TLS protocol that CloudFront uses when connecting to your origin over HTTPS. Valid values include @SSLv3@ , @TLSv1@ , @TLSv1.1@ , and @TLSv1.2@ .
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginSSLProtocols Minimum Origin SSL Protocol> in the /Amazon CloudFront Developer Guide/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomOriginConfig' value with any optional fields omitted.
mkCustomOriginConfig
    :: Core.Int -- ^ 'hTTPPort'
    -> Core.Int -- ^ 'hTTPSPort'
    -> Types.OriginProtocolPolicy -- ^ 'originProtocolPolicy'
    -> CustomOriginConfig
mkCustomOriginConfig hTTPPort hTTPSPort originProtocolPolicy
  = CustomOriginConfig'{hTTPPort, hTTPSPort, originProtocolPolicy,
                        originKeepaliveTimeout = Core.Nothing,
                        originReadTimeout = Core.Nothing,
                        originSslProtocols = Core.Nothing}

-- | The HTTP port that CloudFront uses to connect to the origin. Specify the HTTP port that the origin listens on.
--
-- /Note:/ Consider using 'hTTPPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cocHTTPPort :: Lens.Lens' CustomOriginConfig Core.Int
cocHTTPPort = Lens.field @"hTTPPort"
{-# INLINEABLE cocHTTPPort #-}
{-# DEPRECATED hTTPPort "Use generic-lens or generic-optics with 'hTTPPort' instead"  #-}

-- | The HTTPS port that CloudFront uses to connect to the origin. Specify the HTTPS port that the origin listens on.
--
-- /Note:/ Consider using 'hTTPSPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cocHTTPSPort :: Lens.Lens' CustomOriginConfig Core.Int
cocHTTPSPort = Lens.field @"hTTPSPort"
{-# INLINEABLE cocHTTPSPort #-}
{-# DEPRECATED hTTPSPort "Use generic-lens or generic-optics with 'hTTPSPort' instead"  #-}

-- | Specifies the protocol (HTTP or HTTPS) that CloudFront uses to connect to the origin. Valid values are:
--
--
--     * @http-only@ – CloudFront always uses HTTP to connect to the origin.
--
--
--     * @match-viewer@ – CloudFront connects to the origin using the same protocol that the viewer used to connect to CloudFront.
--
--
--     * @https-only@ – CloudFront always uses HTTPS to connect to the origin.
--
--
--
-- /Note:/ Consider using 'originProtocolPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cocOriginProtocolPolicy :: Lens.Lens' CustomOriginConfig Types.OriginProtocolPolicy
cocOriginProtocolPolicy = Lens.field @"originProtocolPolicy"
{-# INLINEABLE cocOriginProtocolPolicy #-}
{-# DEPRECATED originProtocolPolicy "Use generic-lens or generic-optics with 'originProtocolPolicy' instead"  #-}

-- | Specifies how long, in seconds, CloudFront persists its connection to the origin. The minimum timeout is 1 second, the maximum is 60 seconds, and the default (if you don’t specify otherwise) is 5 seconds.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginKeepaliveTimeout Origin Keep-alive Timeout> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'originKeepaliveTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cocOriginKeepaliveTimeout :: Lens.Lens' CustomOriginConfig (Core.Maybe Core.Int)
cocOriginKeepaliveTimeout = Lens.field @"originKeepaliveTimeout"
{-# INLINEABLE cocOriginKeepaliveTimeout #-}
{-# DEPRECATED originKeepaliveTimeout "Use generic-lens or generic-optics with 'originKeepaliveTimeout' instead"  #-}

-- | Specifies how long, in seconds, CloudFront waits for a response from the origin. This is also known as the /origin response timeout/ . The minimum timeout is 1 second, the maximum is 60 seconds, and the default (if you don’t specify otherwise) is 30 seconds.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginResponseTimeout Origin Response Timeout> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'originReadTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cocOriginReadTimeout :: Lens.Lens' CustomOriginConfig (Core.Maybe Core.Int)
cocOriginReadTimeout = Lens.field @"originReadTimeout"
{-# INLINEABLE cocOriginReadTimeout #-}
{-# DEPRECATED originReadTimeout "Use generic-lens or generic-optics with 'originReadTimeout' instead"  #-}

-- | Specifies the minimum SSL/TLS protocol that CloudFront uses when connecting to your origin over HTTPS. Valid values include @SSLv3@ , @TLSv1@ , @TLSv1.1@ , and @TLSv1.2@ .
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginSSLProtocols Minimum Origin SSL Protocol> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'originSslProtocols' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cocOriginSslProtocols :: Lens.Lens' CustomOriginConfig (Core.Maybe Types.OriginSslProtocols)
cocOriginSslProtocols = Lens.field @"originSslProtocols"
{-# INLINEABLE cocOriginSslProtocols #-}
{-# DEPRECATED originSslProtocols "Use generic-lens or generic-optics with 'originSslProtocols' instead"  #-}

instance Core.ToXML CustomOriginConfig where
        toXML CustomOriginConfig{..}
          = Core.toXMLElement "HTTPPort" hTTPPort Core.<>
              Core.toXMLElement "HTTPSPort" hTTPSPort
              Core.<>
              Core.toXMLElement "OriginProtocolPolicy" originProtocolPolicy
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "OriginKeepaliveTimeout")
                originKeepaliveTimeout
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "OriginReadTimeout")
                originReadTimeout
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "OriginSslProtocols")
                originSslProtocols

instance Core.FromXML CustomOriginConfig where
        parseXML x
          = CustomOriginConfig' Core.<$>
              (x Core..@ "HTTPPort") Core.<*> x Core..@ "HTTPSPort" Core.<*>
                x Core..@ "OriginProtocolPolicy"
                Core.<*> x Core..@? "OriginKeepaliveTimeout"
                Core.<*> x Core..@? "OriginReadTimeout"
                Core.<*> x Core..@? "OriginSslProtocols"
