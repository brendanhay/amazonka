{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    cerErrorCode,
    cerErrorCachingMinTTL,
    cerResponseCode,
    cerResponsePagePath,
  )
where

import qualified Network.AWS.CloudFront.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { -- | The HTTP status code for which you want to specify a custom error page and/or a caching duration.
    errorCode :: Core.Int,
    -- | The minimum amount of time, in seconds, that you want CloudFront to cache the HTTP status code specified in @ErrorCode@ . When this time period has elapsed, CloudFront queries your origin to see whether the problem that caused the error has been resolved and the requested object is now available.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses> in the /Amazon CloudFront Developer Guide/ .
    errorCachingMinTTL :: Core.Maybe Core.Integer,
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
    responseCode :: Core.Maybe Types.String,
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
    responsePagePath :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomErrorResponse' value with any optional fields omitted.
mkCustomErrorResponse ::
  -- | 'errorCode'
  Core.Int ->
  CustomErrorResponse
mkCustomErrorResponse errorCode =
  CustomErrorResponse'
    { errorCode,
      errorCachingMinTTL = Core.Nothing,
      responseCode = Core.Nothing,
      responsePagePath = Core.Nothing
    }

-- | The HTTP status code for which you want to specify a custom error page and/or a caching duration.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerErrorCode :: Lens.Lens' CustomErrorResponse Core.Int
cerErrorCode = Lens.field @"errorCode"
{-# DEPRECATED cerErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The minimum amount of time, in seconds, that you want CloudFront to cache the HTTP status code specified in @ErrorCode@ . When this time period has elapsed, CloudFront queries your origin to see whether the problem that caused the error has been resolved and the requested object is now available.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'errorCachingMinTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cerErrorCachingMinTTL :: Lens.Lens' CustomErrorResponse (Core.Maybe Core.Integer)
cerErrorCachingMinTTL = Lens.field @"errorCachingMinTTL"
{-# DEPRECATED cerErrorCachingMinTTL "Use generic-lens or generic-optics with 'errorCachingMinTTL' instead." #-}

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
cerResponseCode :: Lens.Lens' CustomErrorResponse (Core.Maybe Types.String)
cerResponseCode = Lens.field @"responseCode"
{-# DEPRECATED cerResponseCode "Use generic-lens or generic-optics with 'responseCode' instead." #-}

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
cerResponsePagePath :: Lens.Lens' CustomErrorResponse (Core.Maybe Types.String)
cerResponsePagePath = Lens.field @"responsePagePath"
{-# DEPRECATED cerResponsePagePath "Use generic-lens or generic-optics with 'responsePagePath' instead." #-}

instance Core.ToXML CustomErrorResponse where
  toXML CustomErrorResponse {..} =
    Core.toXMLNode "ErrorCode" errorCode
      Core.<> Core.toXMLNode "ErrorCachingMinTTL" Core.<$> errorCachingMinTTL
      Core.<> Core.toXMLNode "ResponseCode" Core.<$> responseCode
      Core.<> Core.toXMLNode "ResponsePagePath" Core.<$> responsePagePath

instance Core.FromXML CustomErrorResponse where
  parseXML x =
    CustomErrorResponse'
      Core.<$> (x Core..@ "ErrorCode")
      Core.<*> (x Core..@? "ErrorCachingMinTTL")
      Core.<*> (x Core..@? "ResponseCode")
      Core.<*> (x Core..@? "ResponsePagePath")
