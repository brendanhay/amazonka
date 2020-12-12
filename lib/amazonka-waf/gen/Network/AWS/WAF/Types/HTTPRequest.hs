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
    httprHTTPVersion,
    httprCountry,
    httprURI,
    httprHeaders,
    httprMethod,
    httprClientIP,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAF.Types.HTTPHeader

-- | The response from a 'GetSampledRequests' request includes an @HTTPRequest@ complex type that appears as @Request@ in the response syntax. @HTTPRequest@ contains information about one of the web requests that were returned by @GetSampledRequests@ .
--
-- /See:/ 'mkHTTPRequest' smart constructor.
data HTTPRequest = HTTPRequest'
  { hTTPVersion ::
      Lude.Maybe Lude.Text,
    country :: Lude.Maybe Lude.Text,
    uri :: Lude.Maybe Lude.Text,
    headers :: Lude.Maybe [HTTPHeader],
    method :: Lude.Maybe Lude.Text,
    clientIP :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPRequest' with the minimum fields required to make a request.
--
-- * 'clientIP' - The IP address that the request originated from. If the @WebACL@ is associated with a CloudFront distribution, this is the value of one of the following fields in CloudFront access logs:
--
--
--     * @c-ip@ , if the viewer did not use an HTTP proxy or a load balancer to send the request
--
--
--     * @x-forwarded-for@ , if the viewer did use an HTTP proxy or a load balancer to send the request
--
--
-- * 'country' - The two-letter country code for the country that the request originated from. For a current list of country codes, see the Wikipedia entry <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO 3166-1 alpha-2> .
-- * 'hTTPVersion' - The HTTP version specified in the sampled web request, for example, @HTTP/1.1@ .
-- * 'headers' - A complex type that contains two values for each header in the sampled web request: the name of the header and the value of the header.
-- * 'method' - The HTTP method specified in the sampled web request. CloudFront supports the following methods: @DELETE@ , @GET@ , @HEAD@ , @OPTIONS@ , @PATCH@ , @POST@ , and @PUT@ .
-- * 'uri' - The part of a web request that identifies the resource, for example, @/images/daily-ad.jpg@ .
mkHTTPRequest ::
  HTTPRequest
mkHTTPRequest =
  HTTPRequest'
    { hTTPVersion = Lude.Nothing,
      country = Lude.Nothing,
      uri = Lude.Nothing,
      headers = Lude.Nothing,
      method = Lude.Nothing,
      clientIP = Lude.Nothing
    }

-- | The HTTP version specified in the sampled web request, for example, @HTTP/1.1@ .
--
-- /Note:/ Consider using 'hTTPVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httprHTTPVersion :: Lens.Lens' HTTPRequest (Lude.Maybe Lude.Text)
httprHTTPVersion = Lens.lens (hTTPVersion :: HTTPRequest -> Lude.Maybe Lude.Text) (\s a -> s {hTTPVersion = a} :: HTTPRequest)
{-# DEPRECATED httprHTTPVersion "Use generic-lens or generic-optics with 'hTTPVersion' instead." #-}

-- | The two-letter country code for the country that the request originated from. For a current list of country codes, see the Wikipedia entry <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO 3166-1 alpha-2> .
--
-- /Note:/ Consider using 'country' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httprCountry :: Lens.Lens' HTTPRequest (Lude.Maybe Lude.Text)
httprCountry = Lens.lens (country :: HTTPRequest -> Lude.Maybe Lude.Text) (\s a -> s {country = a} :: HTTPRequest)
{-# DEPRECATED httprCountry "Use generic-lens or generic-optics with 'country' instead." #-}

-- | The part of a web request that identifies the resource, for example, @/images/daily-ad.jpg@ .
--
-- /Note:/ Consider using 'uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httprURI :: Lens.Lens' HTTPRequest (Lude.Maybe Lude.Text)
httprURI = Lens.lens (uri :: HTTPRequest -> Lude.Maybe Lude.Text) (\s a -> s {uri = a} :: HTTPRequest)
{-# DEPRECATED httprURI "Use generic-lens or generic-optics with 'uri' instead." #-}

-- | A complex type that contains two values for each header in the sampled web request: the name of the header and the value of the header.
--
-- /Note:/ Consider using 'headers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httprHeaders :: Lens.Lens' HTTPRequest (Lude.Maybe [HTTPHeader])
httprHeaders = Lens.lens (headers :: HTTPRequest -> Lude.Maybe [HTTPHeader]) (\s a -> s {headers = a} :: HTTPRequest)
{-# DEPRECATED httprHeaders "Use generic-lens or generic-optics with 'headers' instead." #-}

-- | The HTTP method specified in the sampled web request. CloudFront supports the following methods: @DELETE@ , @GET@ , @HEAD@ , @OPTIONS@ , @PATCH@ , @POST@ , and @PUT@ .
--
-- /Note:/ Consider using 'method' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httprMethod :: Lens.Lens' HTTPRequest (Lude.Maybe Lude.Text)
httprMethod = Lens.lens (method :: HTTPRequest -> Lude.Maybe Lude.Text) (\s a -> s {method = a} :: HTTPRequest)
{-# DEPRECATED httprMethod "Use generic-lens or generic-optics with 'method' instead." #-}

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
httprClientIP :: Lens.Lens' HTTPRequest (Lude.Maybe Lude.Text)
httprClientIP = Lens.lens (clientIP :: HTTPRequest -> Lude.Maybe Lude.Text) (\s a -> s {clientIP = a} :: HTTPRequest)
{-# DEPRECATED httprClientIP "Use generic-lens or generic-optics with 'clientIP' instead." #-}

instance Lude.FromJSON HTTPRequest where
  parseJSON =
    Lude.withObject
      "HTTPRequest"
      ( \x ->
          HTTPRequest'
            Lude.<$> (x Lude..:? "HTTPVersion")
            Lude.<*> (x Lude..:? "Country")
            Lude.<*> (x Lude..:? "URI")
            Lude.<*> (x Lude..:? "Headers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Method")
            Lude.<*> (x Lude..:? "ClientIP")
      )
