{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.HeaderEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.HeaderEnum where

import Network.AWS.Prelude

data HeaderEnum
  = Accept
  | AcceptCharset
  | AcceptDatetime
  | AcceptEncoding
  | AcceptLanguage
  | Authorization
  | CloudFrontForwardedProto
  | CloudFrontIsDesktopViewer
  | CloudFrontIsMobileViewer
  | CloudFrontIsSmartTVViewer
  | CloudFrontIsTabletViewer
  | CloudFrontViewerCountry
  | Host
  | Origin
  | Referer
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText HeaderEnum where
  parser =
    takeLowerText >>= \case
      "accept" -> pure Accept
      "accept-charset" -> pure AcceptCharset
      "accept-datetime" -> pure AcceptDatetime
      "accept-encoding" -> pure AcceptEncoding
      "accept-language" -> pure AcceptLanguage
      "authorization" -> pure Authorization
      "cloudfront-forwarded-proto" -> pure CloudFrontForwardedProto
      "cloudfront-is-desktop-viewer" -> pure CloudFrontIsDesktopViewer
      "cloudfront-is-mobile-viewer" -> pure CloudFrontIsMobileViewer
      "cloudfront-is-smarttv-viewer" -> pure CloudFrontIsSmartTVViewer
      "cloudfront-is-tablet-viewer" -> pure CloudFrontIsTabletViewer
      "cloudfront-viewer-country" -> pure CloudFrontViewerCountry
      "host" -> pure Host
      "origin" -> pure Origin
      "referer" -> pure Referer
      e ->
        fromTextError $
          "Failure parsing HeaderEnum from value: '" <> e
            <> "'. Accepted values: accept, accept-charset, accept-datetime, accept-encoding, accept-language, authorization, cloudfront-forwarded-proto, cloudfront-is-desktop-viewer, cloudfront-is-mobile-viewer, cloudfront-is-smarttv-viewer, cloudfront-is-tablet-viewer, cloudfront-viewer-country, host, origin, referer"

instance ToText HeaderEnum where
  toText = \case
    Accept -> "Accept"
    AcceptCharset -> "Accept-Charset"
    AcceptDatetime -> "Accept-Datetime"
    AcceptEncoding -> "Accept-Encoding"
    AcceptLanguage -> "Accept-Language"
    Authorization -> "Authorization"
    CloudFrontForwardedProto -> "CloudFront-Forwarded-Proto"
    CloudFrontIsDesktopViewer -> "CloudFront-Is-Desktop-Viewer"
    CloudFrontIsMobileViewer -> "CloudFront-Is-Mobile-Viewer"
    CloudFrontIsSmartTVViewer -> "CloudFront-Is-SmartTV-Viewer"
    CloudFrontIsTabletViewer -> "CloudFront-Is-Tablet-Viewer"
    CloudFrontViewerCountry -> "CloudFront-Viewer-Country"
    Host -> "Host"
    Origin -> "Origin"
    Referer -> "Referer"

instance Hashable HeaderEnum

instance NFData HeaderEnum

instance ToByteString HeaderEnum

instance ToQuery HeaderEnum

instance ToHeader HeaderEnum

instance ToJSON HeaderEnum where
  toJSON = toJSONText

instance FromJSON HeaderEnum where
  parseJSON = parseJSONText "HeaderEnum"
