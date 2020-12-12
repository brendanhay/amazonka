{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CustomOriginConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CustomOriginConfig
  ( CustomOriginConfig (..),

    -- * Smart constructor
    mkCustomOriginConfig,

    -- * Lenses
    cocOriginKeepaliveTimeout,
    cocOriginReadTimeout,
    cocOriginSSLProtocols,
    cocHTTPPort,
    cocHTTPSPort,
    cocOriginProtocolPolicy,
  )
where

import Network.AWS.CloudFront.Types.OriginProtocolPolicy
import Network.AWS.CloudFront.Types.OriginSSLProtocols
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A custom origin. A custom origin is any origin that is /not/ an Amazon S3 bucket, with one exception. An Amazon S3 bucket that is <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html configured with static website hosting> /is/ a custom origin.
--
-- /See:/ 'mkCustomOriginConfig' smart constructor.
data CustomOriginConfig = CustomOriginConfig'
  { originKeepaliveTimeout ::
      Lude.Maybe Lude.Int,
    originReadTimeout :: Lude.Maybe Lude.Int,
    originSSLProtocols :: Lude.Maybe OriginSSLProtocols,
    hTTPPort :: Lude.Int,
    httpsPort :: Lude.Int,
    originProtocolPolicy :: OriginProtocolPolicy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomOriginConfig' with the minimum fields required to make a request.
--
-- * 'hTTPPort' - The HTTP port that CloudFront uses to connect to the origin. Specify the HTTP port that the origin listens on.
-- * 'httpsPort' - The HTTPS port that CloudFront uses to connect to the origin. Specify the HTTPS port that the origin listens on.
-- * 'originKeepaliveTimeout' - Specifies how long, in seconds, CloudFront persists its connection to the origin. The minimum timeout is 1 second, the maximum is 60 seconds, and the default (if you don’t specify otherwise) is 5 seconds.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginKeepaliveTimeout Origin Keep-alive Timeout> in the /Amazon CloudFront Developer Guide/ .
-- * 'originProtocolPolicy' - Specifies the protocol (HTTP or HTTPS) that CloudFront uses to connect to the origin. Valid values are:
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
-- * 'originReadTimeout' - Specifies how long, in seconds, CloudFront waits for a response from the origin. This is also known as the /origin response timeout/ . The minimum timeout is 1 second, the maximum is 60 seconds, and the default (if you don’t specify otherwise) is 30 seconds.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginResponseTimeout Origin Response Timeout> in the /Amazon CloudFront Developer Guide/ .
-- * 'originSSLProtocols' - Specifies the minimum SSL/TLS protocol that CloudFront uses when connecting to your origin over HTTPS. Valid values include @SSLv3@ , @TLSv1@ , @TLSv1.1@ , and @TLSv1.2@ .
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginSSLProtocols Minimum Origin SSL Protocol> in the /Amazon CloudFront Developer Guide/ .
mkCustomOriginConfig ::
  -- | 'hTTPPort'
  Lude.Int ->
  -- | 'httpsPort'
  Lude.Int ->
  -- | 'originProtocolPolicy'
  OriginProtocolPolicy ->
  CustomOriginConfig
mkCustomOriginConfig pHTTPPort_ pHTTPSPort_ pOriginProtocolPolicy_ =
  CustomOriginConfig'
    { originKeepaliveTimeout = Lude.Nothing,
      originReadTimeout = Lude.Nothing,
      originSSLProtocols = Lude.Nothing,
      hTTPPort = pHTTPPort_,
      httpsPort = pHTTPSPort_,
      originProtocolPolicy = pOriginProtocolPolicy_
    }

-- | Specifies how long, in seconds, CloudFront persists its connection to the origin. The minimum timeout is 1 second, the maximum is 60 seconds, and the default (if you don’t specify otherwise) is 5 seconds.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginKeepaliveTimeout Origin Keep-alive Timeout> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'originKeepaliveTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cocOriginKeepaliveTimeout :: Lens.Lens' CustomOriginConfig (Lude.Maybe Lude.Int)
cocOriginKeepaliveTimeout = Lens.lens (originKeepaliveTimeout :: CustomOriginConfig -> Lude.Maybe Lude.Int) (\s a -> s {originKeepaliveTimeout = a} :: CustomOriginConfig)
{-# DEPRECATED cocOriginKeepaliveTimeout "Use generic-lens or generic-optics with 'originKeepaliveTimeout' instead." #-}

-- | Specifies how long, in seconds, CloudFront waits for a response from the origin. This is also known as the /origin response timeout/ . The minimum timeout is 1 second, the maximum is 60 seconds, and the default (if you don’t specify otherwise) is 30 seconds.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginResponseTimeout Origin Response Timeout> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'originReadTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cocOriginReadTimeout :: Lens.Lens' CustomOriginConfig (Lude.Maybe Lude.Int)
cocOriginReadTimeout = Lens.lens (originReadTimeout :: CustomOriginConfig -> Lude.Maybe Lude.Int) (\s a -> s {originReadTimeout = a} :: CustomOriginConfig)
{-# DEPRECATED cocOriginReadTimeout "Use generic-lens or generic-optics with 'originReadTimeout' instead." #-}

-- | Specifies the minimum SSL/TLS protocol that CloudFront uses when connecting to your origin over HTTPS. Valid values include @SSLv3@ , @TLSv1@ , @TLSv1.1@ , and @TLSv1.2@ .
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-values-specify.html#DownloadDistValuesOriginSSLProtocols Minimum Origin SSL Protocol> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'originSSLProtocols' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cocOriginSSLProtocols :: Lens.Lens' CustomOriginConfig (Lude.Maybe OriginSSLProtocols)
cocOriginSSLProtocols = Lens.lens (originSSLProtocols :: CustomOriginConfig -> Lude.Maybe OriginSSLProtocols) (\s a -> s {originSSLProtocols = a} :: CustomOriginConfig)
{-# DEPRECATED cocOriginSSLProtocols "Use generic-lens or generic-optics with 'originSSLProtocols' instead." #-}

-- | The HTTP port that CloudFront uses to connect to the origin. Specify the HTTP port that the origin listens on.
--
-- /Note:/ Consider using 'hTTPPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cocHTTPPort :: Lens.Lens' CustomOriginConfig Lude.Int
cocHTTPPort = Lens.lens (hTTPPort :: CustomOriginConfig -> Lude.Int) (\s a -> s {hTTPPort = a} :: CustomOriginConfig)
{-# DEPRECATED cocHTTPPort "Use generic-lens or generic-optics with 'hTTPPort' instead." #-}

-- | The HTTPS port that CloudFront uses to connect to the origin. Specify the HTTPS port that the origin listens on.
--
-- /Note:/ Consider using 'httpsPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cocHTTPSPort :: Lens.Lens' CustomOriginConfig Lude.Int
cocHTTPSPort = Lens.lens (httpsPort :: CustomOriginConfig -> Lude.Int) (\s a -> s {httpsPort = a} :: CustomOriginConfig)
{-# DEPRECATED cocHTTPSPort "Use generic-lens or generic-optics with 'httpsPort' instead." #-}

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
cocOriginProtocolPolicy :: Lens.Lens' CustomOriginConfig OriginProtocolPolicy
cocOriginProtocolPolicy = Lens.lens (originProtocolPolicy :: CustomOriginConfig -> OriginProtocolPolicy) (\s a -> s {originProtocolPolicy = a} :: CustomOriginConfig)
{-# DEPRECATED cocOriginProtocolPolicy "Use generic-lens or generic-optics with 'originProtocolPolicy' instead." #-}

instance Lude.FromXML CustomOriginConfig where
  parseXML x =
    CustomOriginConfig'
      Lude.<$> (x Lude..@? "OriginKeepaliveTimeout")
      Lude.<*> (x Lude..@? "OriginReadTimeout")
      Lude.<*> (x Lude..@? "OriginSslProtocols")
      Lude.<*> (x Lude..@ "HTTPPort")
      Lude.<*> (x Lude..@ "HTTPSPort")
      Lude.<*> (x Lude..@ "OriginProtocolPolicy")

instance Lude.ToXML CustomOriginConfig where
  toXML CustomOriginConfig' {..} =
    Lude.mconcat
      [ "OriginKeepaliveTimeout" Lude.@= originKeepaliveTimeout,
        "OriginReadTimeout" Lude.@= originReadTimeout,
        "OriginSslProtocols" Lude.@= originSSLProtocols,
        "HTTPPort" Lude.@= hTTPPort,
        "HTTPSPort" Lude.@= httpsPort,
        "OriginProtocolPolicy" Lude.@= originProtocolPolicy
      ]
