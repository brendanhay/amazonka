{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CustomErrorResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CustomErrorResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A complex type that controls:
--
-- -   Whether CloudFront replaces HTTP status codes in the 4xx and 5xx
--     range with custom error messages before returning the response to
--     the viewer.
--
-- -   How long CloudFront caches HTTP status codes in the 4xx and 5xx
--     range.
--
-- For more information about custom error pages, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses>
-- in the /Amazon CloudFront Developer Guide/.
--
-- /See:/ 'newCustomErrorResponse' smart constructor.
data CustomErrorResponse = CustomErrorResponse'
  { -- | The minimum amount of time, in seconds, that you want CloudFront to
    -- cache the HTTP status code specified in @ErrorCode@. When this time
    -- period has elapsed, CloudFront queries your origin to see whether the
    -- problem that caused the error has been resolved and the requested object
    -- is now available.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses>
    -- in the /Amazon CloudFront Developer Guide/.
    errorCachingMinTTL :: Prelude.Maybe Prelude.Integer,
    -- | The HTTP status code that you want CloudFront to return to the viewer
    -- along with the custom error page. There are a variety of reasons that
    -- you might want CloudFront to return a status code different from the
    -- status code that your origin returned to CloudFront, for example:
    --
    -- -   Some Internet devices (some firewalls and corporate proxies, for
    --     example) intercept HTTP 4xx and 5xx and prevent the response from
    --     being returned to the viewer. If you substitute @200@, the response
    --     typically won\'t be intercepted.
    --
    -- -   If you don\'t care about distinguishing among different client
    --     errors or server errors, you can specify @400@ or @500@ as the
    --     @ResponseCode@ for all 4xx or 5xx errors.
    --
    -- -   You might want to return a @200@ status code (OK) and static website
    --     so your customers don\'t know that your website is down.
    --
    -- If you specify a value for @ResponseCode@, you must also specify a value
    -- for @ResponsePagePath@.
    responseCode :: Prelude.Maybe Prelude.Text,
    -- | The path to the custom error page that you want CloudFront to return to
    -- a viewer when your origin returns the HTTP status code specified by
    -- @ErrorCode@, for example, @\/4xx-errors\/403-forbidden.html@. If you
    -- want to store your objects and your custom error pages in different
    -- locations, your distribution must include a cache behavior for which the
    -- following is true:
    --
    -- -   The value of @PathPattern@ matches the path to your custom error
    --     messages. For example, suppose you saved custom error pages for 4xx
    --     errors in an Amazon S3 bucket in a directory named @\/4xx-errors@.
    --     Your distribution must include a cache behavior for which the path
    --     pattern routes requests for your custom error pages to that
    --     location, for example, @\/4xx-errors\/*@.
    --
    -- -   The value of @TargetOriginId@ specifies the value of the @ID@
    --     element for the origin that contains your custom error pages.
    --
    -- If you specify a value for @ResponsePagePath@, you must also specify a
    -- value for @ResponseCode@.
    --
    -- We recommend that you store custom error pages in an Amazon S3 bucket.
    -- If you store custom error pages on an HTTP server and the server starts
    -- to return 5xx errors, CloudFront can\'t get the files that you want to
    -- return to viewers because the origin server is unavailable.
    responsePagePath :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status code for which you want to specify a custom error page
    -- and\/or a caching duration.
    errorCode :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CustomErrorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCachingMinTTL', 'customErrorResponse_errorCachingMinTTL' - The minimum amount of time, in seconds, that you want CloudFront to
-- cache the HTTP status code specified in @ErrorCode@. When this time
-- period has elapsed, CloudFront queries your origin to see whether the
-- problem that caused the error has been resolved and the requested object
-- is now available.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'responseCode', 'customErrorResponse_responseCode' - The HTTP status code that you want CloudFront to return to the viewer
-- along with the custom error page. There are a variety of reasons that
-- you might want CloudFront to return a status code different from the
-- status code that your origin returned to CloudFront, for example:
--
-- -   Some Internet devices (some firewalls and corporate proxies, for
--     example) intercept HTTP 4xx and 5xx and prevent the response from
--     being returned to the viewer. If you substitute @200@, the response
--     typically won\'t be intercepted.
--
-- -   If you don\'t care about distinguishing among different client
--     errors or server errors, you can specify @400@ or @500@ as the
--     @ResponseCode@ for all 4xx or 5xx errors.
--
-- -   You might want to return a @200@ status code (OK) and static website
--     so your customers don\'t know that your website is down.
--
-- If you specify a value for @ResponseCode@, you must also specify a value
-- for @ResponsePagePath@.
--
-- 'responsePagePath', 'customErrorResponse_responsePagePath' - The path to the custom error page that you want CloudFront to return to
-- a viewer when your origin returns the HTTP status code specified by
-- @ErrorCode@, for example, @\/4xx-errors\/403-forbidden.html@. If you
-- want to store your objects and your custom error pages in different
-- locations, your distribution must include a cache behavior for which the
-- following is true:
--
-- -   The value of @PathPattern@ matches the path to your custom error
--     messages. For example, suppose you saved custom error pages for 4xx
--     errors in an Amazon S3 bucket in a directory named @\/4xx-errors@.
--     Your distribution must include a cache behavior for which the path
--     pattern routes requests for your custom error pages to that
--     location, for example, @\/4xx-errors\/*@.
--
-- -   The value of @TargetOriginId@ specifies the value of the @ID@
--     element for the origin that contains your custom error pages.
--
-- If you specify a value for @ResponsePagePath@, you must also specify a
-- value for @ResponseCode@.
--
-- We recommend that you store custom error pages in an Amazon S3 bucket.
-- If you store custom error pages on an HTTP server and the server starts
-- to return 5xx errors, CloudFront can\'t get the files that you want to
-- return to viewers because the origin server is unavailable.
--
-- 'errorCode', 'customErrorResponse_errorCode' - The HTTP status code for which you want to specify a custom error page
-- and\/or a caching duration.
newCustomErrorResponse ::
  -- | 'errorCode'
  Prelude.Int ->
  CustomErrorResponse
newCustomErrorResponse pErrorCode_ =
  CustomErrorResponse'
    { errorCachingMinTTL =
        Prelude.Nothing,
      responseCode = Prelude.Nothing,
      responsePagePath = Prelude.Nothing,
      errorCode = pErrorCode_
    }

-- | The minimum amount of time, in seconds, that you want CloudFront to
-- cache the HTTP status code specified in @ErrorCode@. When this time
-- period has elapsed, CloudFront queries your origin to see whether the
-- problem that caused the error has been resolved and the requested object
-- is now available.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses>
-- in the /Amazon CloudFront Developer Guide/.
customErrorResponse_errorCachingMinTTL :: Lens.Lens' CustomErrorResponse (Prelude.Maybe Prelude.Integer)
customErrorResponse_errorCachingMinTTL = Lens.lens (\CustomErrorResponse' {errorCachingMinTTL} -> errorCachingMinTTL) (\s@CustomErrorResponse' {} a -> s {errorCachingMinTTL = a} :: CustomErrorResponse)

-- | The HTTP status code that you want CloudFront to return to the viewer
-- along with the custom error page. There are a variety of reasons that
-- you might want CloudFront to return a status code different from the
-- status code that your origin returned to CloudFront, for example:
--
-- -   Some Internet devices (some firewalls and corporate proxies, for
--     example) intercept HTTP 4xx and 5xx and prevent the response from
--     being returned to the viewer. If you substitute @200@, the response
--     typically won\'t be intercepted.
--
-- -   If you don\'t care about distinguishing among different client
--     errors or server errors, you can specify @400@ or @500@ as the
--     @ResponseCode@ for all 4xx or 5xx errors.
--
-- -   You might want to return a @200@ status code (OK) and static website
--     so your customers don\'t know that your website is down.
--
-- If you specify a value for @ResponseCode@, you must also specify a value
-- for @ResponsePagePath@.
customErrorResponse_responseCode :: Lens.Lens' CustomErrorResponse (Prelude.Maybe Prelude.Text)
customErrorResponse_responseCode = Lens.lens (\CustomErrorResponse' {responseCode} -> responseCode) (\s@CustomErrorResponse' {} a -> s {responseCode = a} :: CustomErrorResponse)

-- | The path to the custom error page that you want CloudFront to return to
-- a viewer when your origin returns the HTTP status code specified by
-- @ErrorCode@, for example, @\/4xx-errors\/403-forbidden.html@. If you
-- want to store your objects and your custom error pages in different
-- locations, your distribution must include a cache behavior for which the
-- following is true:
--
-- -   The value of @PathPattern@ matches the path to your custom error
--     messages. For example, suppose you saved custom error pages for 4xx
--     errors in an Amazon S3 bucket in a directory named @\/4xx-errors@.
--     Your distribution must include a cache behavior for which the path
--     pattern routes requests for your custom error pages to that
--     location, for example, @\/4xx-errors\/*@.
--
-- -   The value of @TargetOriginId@ specifies the value of the @ID@
--     element for the origin that contains your custom error pages.
--
-- If you specify a value for @ResponsePagePath@, you must also specify a
-- value for @ResponseCode@.
--
-- We recommend that you store custom error pages in an Amazon S3 bucket.
-- If you store custom error pages on an HTTP server and the server starts
-- to return 5xx errors, CloudFront can\'t get the files that you want to
-- return to viewers because the origin server is unavailable.
customErrorResponse_responsePagePath :: Lens.Lens' CustomErrorResponse (Prelude.Maybe Prelude.Text)
customErrorResponse_responsePagePath = Lens.lens (\CustomErrorResponse' {responsePagePath} -> responsePagePath) (\s@CustomErrorResponse' {} a -> s {responsePagePath = a} :: CustomErrorResponse)

-- | The HTTP status code for which you want to specify a custom error page
-- and\/or a caching duration.
customErrorResponse_errorCode :: Lens.Lens' CustomErrorResponse Prelude.Int
customErrorResponse_errorCode = Lens.lens (\CustomErrorResponse' {errorCode} -> errorCode) (\s@CustomErrorResponse' {} a -> s {errorCode = a} :: CustomErrorResponse)

instance Prelude.FromXML CustomErrorResponse where
  parseXML x =
    CustomErrorResponse'
      Prelude.<$> (x Prelude..@? "ErrorCachingMinTTL")
      Prelude.<*> (x Prelude..@? "ResponseCode")
      Prelude.<*> (x Prelude..@? "ResponsePagePath")
      Prelude.<*> (x Prelude..@ "ErrorCode")

instance Prelude.Hashable CustomErrorResponse

instance Prelude.NFData CustomErrorResponse

instance Prelude.ToXML CustomErrorResponse where
  toXML CustomErrorResponse' {..} =
    Prelude.mconcat
      [ "ErrorCachingMinTTL" Prelude.@= errorCachingMinTTL,
        "ResponseCode" Prelude.@= responseCode,
        "ResponsePagePath" Prelude.@= responsePagePath,
        "ErrorCode" Prelude.@= errorCode
      ]
