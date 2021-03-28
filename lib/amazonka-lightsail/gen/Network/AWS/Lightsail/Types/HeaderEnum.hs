{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.HeaderEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.HeaderEnum
  ( HeaderEnum
    ( HeaderEnum'
    , HeaderEnumAccept
    , HeaderEnumAcceptCharset
    , HeaderEnumAcceptDatetime
    , HeaderEnumAcceptEncoding
    , HeaderEnumAcceptLanguage
    , HeaderEnumAuthorization
    , HeaderEnumCloudFrontForwardedProto
    , HeaderEnumCloudFrontIsDesktopViewer
    , HeaderEnumCloudFrontIsMobileViewer
    , HeaderEnumCloudFrontIsSmartTVViewer
    , HeaderEnumCloudFrontIsTabletViewer
    , HeaderEnumCloudFrontViewerCountry
    , HeaderEnumHost
    , HeaderEnumOrigin
    , HeaderEnumReferer
    , fromHeaderEnum
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype HeaderEnum = HeaderEnum'{fromHeaderEnum :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern HeaderEnumAccept :: HeaderEnum
pattern HeaderEnumAccept = HeaderEnum' "Accept"

pattern HeaderEnumAcceptCharset :: HeaderEnum
pattern HeaderEnumAcceptCharset = HeaderEnum' "Accept-Charset"

pattern HeaderEnumAcceptDatetime :: HeaderEnum
pattern HeaderEnumAcceptDatetime = HeaderEnum' "Accept-Datetime"

pattern HeaderEnumAcceptEncoding :: HeaderEnum
pattern HeaderEnumAcceptEncoding = HeaderEnum' "Accept-Encoding"

pattern HeaderEnumAcceptLanguage :: HeaderEnum
pattern HeaderEnumAcceptLanguage = HeaderEnum' "Accept-Language"

pattern HeaderEnumAuthorization :: HeaderEnum
pattern HeaderEnumAuthorization = HeaderEnum' "Authorization"

pattern HeaderEnumCloudFrontForwardedProto :: HeaderEnum
pattern HeaderEnumCloudFrontForwardedProto = HeaderEnum' "CloudFront-Forwarded-Proto"

pattern HeaderEnumCloudFrontIsDesktopViewer :: HeaderEnum
pattern HeaderEnumCloudFrontIsDesktopViewer = HeaderEnum' "CloudFront-Is-Desktop-Viewer"

pattern HeaderEnumCloudFrontIsMobileViewer :: HeaderEnum
pattern HeaderEnumCloudFrontIsMobileViewer = HeaderEnum' "CloudFront-Is-Mobile-Viewer"

pattern HeaderEnumCloudFrontIsSmartTVViewer :: HeaderEnum
pattern HeaderEnumCloudFrontIsSmartTVViewer = HeaderEnum' "CloudFront-Is-SmartTV-Viewer"

pattern HeaderEnumCloudFrontIsTabletViewer :: HeaderEnum
pattern HeaderEnumCloudFrontIsTabletViewer = HeaderEnum' "CloudFront-Is-Tablet-Viewer"

pattern HeaderEnumCloudFrontViewerCountry :: HeaderEnum
pattern HeaderEnumCloudFrontViewerCountry = HeaderEnum' "CloudFront-Viewer-Country"

pattern HeaderEnumHost :: HeaderEnum
pattern HeaderEnumHost = HeaderEnum' "Host"

pattern HeaderEnumOrigin :: HeaderEnum
pattern HeaderEnumOrigin = HeaderEnum' "Origin"

pattern HeaderEnumReferer :: HeaderEnum
pattern HeaderEnumReferer = HeaderEnum' "Referer"

{-# COMPLETE 
  HeaderEnumAccept,

  HeaderEnumAcceptCharset,

  HeaderEnumAcceptDatetime,

  HeaderEnumAcceptEncoding,

  HeaderEnumAcceptLanguage,

  HeaderEnumAuthorization,

  HeaderEnumCloudFrontForwardedProto,

  HeaderEnumCloudFrontIsDesktopViewer,

  HeaderEnumCloudFrontIsMobileViewer,

  HeaderEnumCloudFrontIsSmartTVViewer,

  HeaderEnumCloudFrontIsTabletViewer,

  HeaderEnumCloudFrontViewerCountry,

  HeaderEnumHost,

  HeaderEnumOrigin,

  HeaderEnumReferer,
  HeaderEnum'
  #-}
