-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CustomErrorResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CustomErrorResponse
  ( CustomErrorResponse (..),

    -- * Smart constructor
    mkCustomErrorResponse,

    -- * Lenses
    ceResponsePagePath,
    ceResponseCode,
    ceErrorCachingMinTTL,
    ceErrorCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that controls:
--
--
--     * Whether CloudFront replaces HTTP status codes in the 4xx and 5xx range with custom error messages before returning the response to the viewer.
--
--
--     * How long CloudFront caches HTTP status codes in the 4xx and 5xx range.
--
--
-- For more information about custom error pages, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses> in the /Amazon CloudFront Developer Guide/ .
--
-- /See:/ 'mkCustomErrorResponse' smart constructor.
data CustomErrorResponse = CustomErrorResponse'
  { responsePagePath ::
      Lude.Maybe Lude.Text,
    responseCode :: Lude.Maybe Lude.Text,
    errorCachingMinTTL :: Lude.Maybe Lude.Integer,
    errorCode :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomErrorResponse' with the minimum fields required to make a request.
--
-- * 'errorCachingMinTTL' - The minimum amount of time, in seconds, that you want CloudFront to cache the HTTP status code specified in @ErrorCode@ . When this time period has elapsed, CloudFront queries your origin to see whether the problem that caused the error has been resolved and the requested object is now available.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses> in the /Amazon CloudFront Developer Guide/ .
-- * 'errorCode' - The HTTP status code for which you want to specify a custom error page and/or a caching duration.
-- * 'responseCode' - The HTTP status code that you want CloudFront to return to the viewer along with the custom error page. There are a variety of reasons that you might want CloudFront to return a status code different from the status code that your origin returned to CloudFront, for example:
--
--
--     * Some Internet devices (some firewalls and corporate proxies, for example) intercept HTTP 4xx and 5xx and prevent the response from being returned to the viewer. If you substitute @200@ , the response typically won't be intercepted.
--
--
--     * If you don't care about distinguishing among different client errors or server errors, you can specify @400@ or @500@ as the @ResponseCode@ for all 4xx or 5xx errors.
--
--
--     * You might want to return a @200@ status code (OK) and static website so your customers don't know that your website is down.
--
--
-- If you specify a value for @ResponseCode@ , you must also specify a value for @ResponsePagePath@ .
-- * 'responsePagePath' - The path to the custom error page that you want CloudFront to return to a viewer when your origin returns the HTTP status code specified by @ErrorCode@ , for example, @/4xx-errors/403-forbidden.html@ . If you want to store your objects and your custom error pages in different locations, your distribution must include a cache behavior for which the following is true:
--
--
--     * The value of @PathPattern@ matches the path to your custom error messages. For example, suppose you saved custom error pages for 4xx errors in an Amazon S3 bucket in a directory named @/4xx-errors@ . Your distribution must include a cache behavior for which the path pattern routes requests for your custom error pages to that location, for example, @/4xx-errors/*@ .
--
--
--     * The value of @TargetOriginId@ specifies the value of the @ID@ element for the origin that contains your custom error pages.
--
--
-- If you specify a value for @ResponsePagePath@ , you must also specify a value for @ResponseCode@ .
-- We recommend that you store custom error pages in an Amazon S3 bucket. If you store custom error pages on an HTTP server and the server starts to return 5xx errors, CloudFront can't get the files that you want to return to viewers because the origin server is unavailable.
mkCustomErrorResponse ::
  -- | 'errorCode'
  Lude.Int ->
  CustomErrorResponse
mkCustomErrorResponse pErrorCode_ =
  CustomErrorResponse'
    { responsePagePath = Lude.Nothing,
      responseCode = Lude.Nothing,
      errorCachingMinTTL = Lude.Nothing,
      errorCode = pErrorCode_
    }

-- | The path to the custom error page that you want CloudFront to return to a viewer when your origin returns the HTTP status code specified by @ErrorCode@ , for example, @/4xx-errors/403-forbidden.html@ . If you want to store your objects and your custom error pages in different locations, your distribution must include a cache behavior for which the following is true:
--
--
--     * The value of @PathPattern@ matches the path to your custom error messages. For example, suppose you saved custom error pages for 4xx errors in an Amazon S3 bucket in a directory named @/4xx-errors@ . Your distribution must include a cache behavior for which the path pattern routes requests for your custom error pages to that location, for example, @/4xx-errors/*@ .
--
--
--     * The value of @TargetOriginId@ specifies the value of the @ID@ element for the origin that contains your custom error pages.
--
--
-- If you specify a value for @ResponsePagePath@ , you must also specify a value for @ResponseCode@ .
-- We recommend that you store custom error pages in an Amazon S3 bucket. If you store custom error pages on an HTTP server and the server starts to return 5xx errors, CloudFront can't get the files that you want to return to viewers because the origin server is unavailable.
--
-- /Note:/ Consider using 'responsePagePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceResponsePagePath :: Lens.Lens' CustomErrorResponse (Lude.Maybe Lude.Text)
ceResponsePagePath = Lens.lens (responsePagePath :: CustomErrorResponse -> Lude.Maybe Lude.Text) (\s a -> s {responsePagePath = a} :: CustomErrorResponse)
{-# DEPRECATED ceResponsePagePath "Use generic-lens or generic-optics with 'responsePagePath' instead." #-}

-- | The HTTP status code that you want CloudFront to return to the viewer along with the custom error page. There are a variety of reasons that you might want CloudFront to return a status code different from the status code that your origin returned to CloudFront, for example:
--
--
--     * Some Internet devices (some firewalls and corporate proxies, for example) intercept HTTP 4xx and 5xx and prevent the response from being returned to the viewer. If you substitute @200@ , the response typically won't be intercepted.
--
--
--     * If you don't care about distinguishing among different client errors or server errors, you can specify @400@ or @500@ as the @ResponseCode@ for all 4xx or 5xx errors.
--
--
--     * You might want to return a @200@ status code (OK) and static website so your customers don't know that your website is down.
--
--
-- If you specify a value for @ResponseCode@ , you must also specify a value for @ResponsePagePath@ .
--
-- /Note:/ Consider using 'responseCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceResponseCode :: Lens.Lens' CustomErrorResponse (Lude.Maybe Lude.Text)
ceResponseCode = Lens.lens (responseCode :: CustomErrorResponse -> Lude.Maybe Lude.Text) (\s a -> s {responseCode = a} :: CustomErrorResponse)
{-# DEPRECATED ceResponseCode "Use generic-lens or generic-optics with 'responseCode' instead." #-}

-- | The minimum amount of time, in seconds, that you want CloudFront to cache the HTTP status code specified in @ErrorCode@ . When this time period has elapsed, CloudFront queries your origin to see whether the problem that caused the error has been resolved and the requested object is now available.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'errorCachingMinTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceErrorCachingMinTTL :: Lens.Lens' CustomErrorResponse (Lude.Maybe Lude.Integer)
ceErrorCachingMinTTL = Lens.lens (errorCachingMinTTL :: CustomErrorResponse -> Lude.Maybe Lude.Integer) (\s a -> s {errorCachingMinTTL = a} :: CustomErrorResponse)
{-# DEPRECATED ceErrorCachingMinTTL "Use generic-lens or generic-optics with 'errorCachingMinTTL' instead." #-}

-- | The HTTP status code for which you want to specify a custom error page and/or a caching duration.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceErrorCode :: Lens.Lens' CustomErrorResponse Lude.Int
ceErrorCode = Lens.lens (errorCode :: CustomErrorResponse -> Lude.Int) (\s a -> s {errorCode = a} :: CustomErrorResponse)
{-# DEPRECATED ceErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

instance Lude.FromXML CustomErrorResponse where
  parseXML x =
    CustomErrorResponse'
      Lude.<$> (x Lude..@? "ResponsePagePath")
      Lude.<*> (x Lude..@? "ResponseCode")
      Lude.<*> (x Lude..@? "ErrorCachingMinTTL")
      Lude.<*> (x Lude..@ "ErrorCode")

instance Lude.ToXML CustomErrorResponse where
  toXML CustomErrorResponse' {..} =
    Lude.mconcat
      [ "ResponsePagePath" Lude.@= responsePagePath,
        "ResponseCode" Lude.@= responseCode,
        "ErrorCachingMinTTL" Lude.@= errorCachingMinTTL,
        "ErrorCode" Lude.@= errorCode
      ]
