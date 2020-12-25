{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.HTTPRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.HTTPRequest
  ( HTTPRequest (..),

    -- * Smart constructor
    mkHTTPRequest,

    -- * Lenses
    httprClientIP,
    httprCountry,
    httprHTTPVersion,
    httprHeaders,
    httprMethod,
    httprURI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WAF.Types.Country as Types
import qualified Network.AWS.WAF.Types.HTTPHeader as Types
import qualified Network.AWS.WAF.Types.HTTPMethod as Types
import qualified Network.AWS.WAF.Types.HTTPVersion as Types
import qualified Network.AWS.WAF.Types.IPString as Types
import qualified Network.AWS.WAF.Types.URI as Types

-- | The response from a 'GetSampledRequests' request includes an @HTTPRequest@ complex type that appears as @Request@ in the response syntax. @HTTPRequest@ contains information about one of the web requests that were returned by @GetSampledRequests@ .
--
-- /See:/ 'mkHTTPRequest' smart constructor.
data HTTPRequest = HTTPRequest'
  { -- | The IP address that the request originated from. If the @WebACL@ is associated with a CloudFront distribution, this is the value of one of the following fields in CloudFront access logs:
    --
    --
    --     * @c-ip@ , if the viewer did not use an HTTP proxy or a load balancer to send the request
    --
    --
    --     * @x-forwarded-for@ , if the viewer did use an HTTP proxy or a load balancer to send the request
    clientIP :: Core.Maybe Types.IPString,
    -- | The two-letter country code for the country that the request originated from. For a current list of country codes, see the Wikipedia entry <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO 3166-1 alpha-2> .
    country :: Core.Maybe Types.Country,
    -- | The HTTP version specified in the sampled web request, for example, @HTTP/1.1@ .
    hTTPVersion :: Core.Maybe Types.HTTPVersion,
    -- | A complex type that contains two values for each header in the sampled web request: the name of the header and the value of the header.
    headers :: Core.Maybe [Types.HTTPHeader],
    -- | The HTTP method specified in the sampled web request. CloudFront supports the following methods: @DELETE@ , @GET@ , @HEAD@ , @OPTIONS@ , @PATCH@ , @POST@ , and @PUT@ .
    method :: Core.Maybe Types.HTTPMethod,
    -- | The part of a web request that identifies the resource, for example, @/images/daily-ad.jpg@ .
    uri :: Core.Maybe Types.URI
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HTTPRequest' value with any optional fields omitted.
mkHTTPRequest ::
  HTTPRequest
mkHTTPRequest =
  HTTPRequest'
    { clientIP = Core.Nothing,
      country = Core.Nothing,
      hTTPVersion = Core.Nothing,
      headers = Core.Nothing,
      method = Core.Nothing,
      uri = Core.Nothing
    }

-- | The IP address that the request originated from. If the @WebACL@ is associated with a CloudFront distribution, this is the value of one of the following fields in CloudFront access logs:
--
--
--     * @c-ip@ , if the viewer did not use an HTTP proxy or a load balancer to send the request
--
--
--     * @x-forwarded-for@ , if the viewer did use an HTTP proxy or a load balancer to send the request
--
--
--
-- /Note:/ Consider using 'clientIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httprClientIP :: Lens.Lens' HTTPRequest (Core.Maybe Types.IPString)
httprClientIP = Lens.field @"clientIP"
{-# DEPRECATED httprClientIP "Use generic-lens or generic-optics with 'clientIP' instead." #-}

-- | The two-letter country code for the country that the request originated from. For a current list of country codes, see the Wikipedia entry <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO 3166-1 alpha-2> .
--
-- /Note:/ Consider using 'country' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httprCountry :: Lens.Lens' HTTPRequest (Core.Maybe Types.Country)
httprCountry = Lens.field @"country"
{-# DEPRECATED httprCountry "Use generic-lens or generic-optics with 'country' instead." #-}

-- | The HTTP version specified in the sampled web request, for example, @HTTP/1.1@ .
--
-- /Note:/ Consider using 'hTTPVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httprHTTPVersion :: Lens.Lens' HTTPRequest (Core.Maybe Types.HTTPVersion)
httprHTTPVersion = Lens.field @"hTTPVersion"
{-# DEPRECATED httprHTTPVersion "Use generic-lens or generic-optics with 'hTTPVersion' instead." #-}

-- | A complex type that contains two values for each header in the sampled web request: the name of the header and the value of the header.
--
-- /Note:/ Consider using 'headers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httprHeaders :: Lens.Lens' HTTPRequest (Core.Maybe [Types.HTTPHeader])
httprHeaders = Lens.field @"headers"
{-# DEPRECATED httprHeaders "Use generic-lens or generic-optics with 'headers' instead." #-}

-- | The HTTP method specified in the sampled web request. CloudFront supports the following methods: @DELETE@ , @GET@ , @HEAD@ , @OPTIONS@ , @PATCH@ , @POST@ , and @PUT@ .
--
-- /Note:/ Consider using 'method' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httprMethod :: Lens.Lens' HTTPRequest (Core.Maybe Types.HTTPMethod)
httprMethod = Lens.field @"method"
{-# DEPRECATED httprMethod "Use generic-lens or generic-optics with 'method' instead." #-}

-- | The part of a web request that identifies the resource, for example, @/images/daily-ad.jpg@ .
--
-- /Note:/ Consider using 'uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httprURI :: Lens.Lens' HTTPRequest (Core.Maybe Types.URI)
httprURI = Lens.field @"uri"
{-# DEPRECATED httprURI "Use generic-lens or generic-optics with 'uri' instead." #-}

instance Core.FromJSON HTTPRequest where
  parseJSON =
    Core.withObject "HTTPRequest" Core.$
      \x ->
        HTTPRequest'
          Core.<$> (x Core..:? "ClientIP")
          Core.<*> (x Core..:? "Country")
          Core.<*> (x Core..:? "HTTPVersion")
          Core.<*> (x Core..:? "Headers")
          Core.<*> (x Core..:? "Method")
          Core.<*> (x Core..:? "URI")
