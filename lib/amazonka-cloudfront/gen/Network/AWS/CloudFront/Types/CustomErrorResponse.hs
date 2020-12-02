{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CustomErrorResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CustomErrorResponse where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that controls:
--
--
--     * Whether CloudFront replaces HTTP status codes in the 4xx and 5xx range with custom error messages before returning the response to the viewer.
--
--     * How long CloudFront caches HTTP status codes in the 4xx and 5xx range.
--
--
--
-- For more information about custom error pages, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses> in the /Amazon CloudFront Developer Guide/ .
--
--
-- /See:/ 'customErrorResponse' smart constructor.
data CustomErrorResponse = CustomErrorResponse'
  { _ceResponsePagePath ::
      !(Maybe Text),
    _ceResponseCode :: !(Maybe Text),
    _ceErrorCachingMinTTL :: !(Maybe Integer),
    _ceErrorCode :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CustomErrorResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceResponsePagePath' - The path to the custom error page that you want CloudFront to return to a viewer when your origin returns the HTTP status code specified by @ErrorCode@ , for example, @/4xx-errors/403-forbidden.html@ . If you want to store your objects and your custom error pages in different locations, your distribution must include a cache behavior for which the following is true:     * The value of @PathPattern@ matches the path to your custom error messages. For example, suppose you saved custom error pages for 4xx errors in an Amazon S3 bucket in a directory named @/4xx-errors@ . Your distribution must include a cache behavior for which the path pattern routes requests for your custom error pages to that location, for example, @/4xx-errors/*@ .      * The value of @TargetOriginId@ specifies the value of the @ID@ element for the origin that contains your custom error pages. If you specify a value for @ResponsePagePath@ , you must also specify a value for @ResponseCode@ . We recommend that you store custom error pages in an Amazon S3 bucket. If you store custom error pages on an HTTP server and the server starts to return 5xx errors, CloudFront can't get the files that you want to return to viewers because the origin server is unavailable.
--
-- * 'ceResponseCode' - The HTTP status code that you want CloudFront to return to the viewer along with the custom error page. There are a variety of reasons that you might want CloudFront to return a status code different from the status code that your origin returned to CloudFront, for example:     * Some Internet devices (some firewalls and corporate proxies, for example) intercept HTTP 4xx and 5xx and prevent the response from being returned to the viewer. If you substitute @200@ , the response typically won't be intercepted.     * If you don't care about distinguishing among different client errors or server errors, you can specify @400@ or @500@ as the @ResponseCode@ for all 4xx or 5xx errors.     * You might want to return a @200@ status code (OK) and static website so your customers don't know that your website is down. If you specify a value for @ResponseCode@ , you must also specify a value for @ResponsePagePath@ .
--
-- * 'ceErrorCachingMinTTL' - The minimum amount of time, in seconds, that you want CloudFront to cache the HTTP status code specified in @ErrorCode@ . When this time period has elapsed, CloudFront queries your origin to see whether the problem that caused the error has been resolved and the requested object is now available. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'ceErrorCode' - The HTTP status code for which you want to specify a custom error page and/or a caching duration.
customErrorResponse ::
  -- | 'ceErrorCode'
  Int ->
  CustomErrorResponse
customErrorResponse pErrorCode_ =
  CustomErrorResponse'
    { _ceResponsePagePath = Nothing,
      _ceResponseCode = Nothing,
      _ceErrorCachingMinTTL = Nothing,
      _ceErrorCode = pErrorCode_
    }

-- | The path to the custom error page that you want CloudFront to return to a viewer when your origin returns the HTTP status code specified by @ErrorCode@ , for example, @/4xx-errors/403-forbidden.html@ . If you want to store your objects and your custom error pages in different locations, your distribution must include a cache behavior for which the following is true:     * The value of @PathPattern@ matches the path to your custom error messages. For example, suppose you saved custom error pages for 4xx errors in an Amazon S3 bucket in a directory named @/4xx-errors@ . Your distribution must include a cache behavior for which the path pattern routes requests for your custom error pages to that location, for example, @/4xx-errors/*@ .      * The value of @TargetOriginId@ specifies the value of the @ID@ element for the origin that contains your custom error pages. If you specify a value for @ResponsePagePath@ , you must also specify a value for @ResponseCode@ . We recommend that you store custom error pages in an Amazon S3 bucket. If you store custom error pages on an HTTP server and the server starts to return 5xx errors, CloudFront can't get the files that you want to return to viewers because the origin server is unavailable.
ceResponsePagePath :: Lens' CustomErrorResponse (Maybe Text)
ceResponsePagePath = lens _ceResponsePagePath (\s a -> s {_ceResponsePagePath = a})

-- | The HTTP status code that you want CloudFront to return to the viewer along with the custom error page. There are a variety of reasons that you might want CloudFront to return a status code different from the status code that your origin returned to CloudFront, for example:     * Some Internet devices (some firewalls and corporate proxies, for example) intercept HTTP 4xx and 5xx and prevent the response from being returned to the viewer. If you substitute @200@ , the response typically won't be intercepted.     * If you don't care about distinguishing among different client errors or server errors, you can specify @400@ or @500@ as the @ResponseCode@ for all 4xx or 5xx errors.     * You might want to return a @200@ status code (OK) and static website so your customers don't know that your website is down. If you specify a value for @ResponseCode@ , you must also specify a value for @ResponsePagePath@ .
ceResponseCode :: Lens' CustomErrorResponse (Maybe Text)
ceResponseCode = lens _ceResponseCode (\s a -> s {_ceResponseCode = a})

-- | The minimum amount of time, in seconds, that you want CloudFront to cache the HTTP status code specified in @ErrorCode@ . When this time period has elapsed, CloudFront queries your origin to see whether the problem that caused the error has been resolved and the requested object is now available. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/custom-error-pages.html Customizing Error Responses> in the /Amazon CloudFront Developer Guide/ .
ceErrorCachingMinTTL :: Lens' CustomErrorResponse (Maybe Integer)
ceErrorCachingMinTTL = lens _ceErrorCachingMinTTL (\s a -> s {_ceErrorCachingMinTTL = a})

-- | The HTTP status code for which you want to specify a custom error page and/or a caching duration.
ceErrorCode :: Lens' CustomErrorResponse Int
ceErrorCode = lens _ceErrorCode (\s a -> s {_ceErrorCode = a})

instance FromXML CustomErrorResponse where
  parseXML x =
    CustomErrorResponse'
      <$> (x .@? "ResponsePagePath")
      <*> (x .@? "ResponseCode")
      <*> (x .@? "ErrorCachingMinTTL")
      <*> (x .@ "ErrorCode")

instance Hashable CustomErrorResponse

instance NFData CustomErrorResponse

instance ToXML CustomErrorResponse where
  toXML CustomErrorResponse' {..} =
    mconcat
      [ "ResponsePagePath" @= _ceResponsePagePath,
        "ResponseCode" @= _ceResponseCode,
        "ErrorCachingMinTTL" @= _ceErrorCachingMinTTL,
        "ErrorCode" @= _ceErrorCode
      ]
