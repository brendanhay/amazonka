-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.HeaderEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.HeaderEnum
  ( HeaderEnum
      ( HeaderEnum',
        Accept,
        AcceptCharset,
        AcceptDatetime,
        AcceptEncoding,
        AcceptLanguage,
        Authorization,
        CloudFrontForwardedProto,
        CloudFrontIsDesktopViewer,
        CloudFrontIsMobileViewer,
        CloudFrontIsSmartTVViewer,
        CloudFrontIsTabletViewer,
        CloudFrontViewerCountry,
        Host,
        Origin,
        Referer
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype HeaderEnum = HeaderEnum' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Accept :: HeaderEnum
pattern Accept = HeaderEnum' "Accept"

pattern AcceptCharset :: HeaderEnum
pattern AcceptCharset = HeaderEnum' "Accept-Charset"

pattern AcceptDatetime :: HeaderEnum
pattern AcceptDatetime = HeaderEnum' "Accept-Datetime"

pattern AcceptEncoding :: HeaderEnum
pattern AcceptEncoding = HeaderEnum' "Accept-Encoding"

pattern AcceptLanguage :: HeaderEnum
pattern AcceptLanguage = HeaderEnum' "Accept-Language"

pattern Authorization :: HeaderEnum
pattern Authorization = HeaderEnum' "Authorization"

pattern CloudFrontForwardedProto :: HeaderEnum
pattern CloudFrontForwardedProto = HeaderEnum' "CloudFront-Forwarded-Proto"

pattern CloudFrontIsDesktopViewer :: HeaderEnum
pattern CloudFrontIsDesktopViewer = HeaderEnum' "CloudFront-Is-Desktop-Viewer"

pattern CloudFrontIsMobileViewer :: HeaderEnum
pattern CloudFrontIsMobileViewer = HeaderEnum' "CloudFront-Is-Mobile-Viewer"

pattern CloudFrontIsSmartTVViewer :: HeaderEnum
pattern CloudFrontIsSmartTVViewer = HeaderEnum' "CloudFront-Is-SmartTV-Viewer"

pattern CloudFrontIsTabletViewer :: HeaderEnum
pattern CloudFrontIsTabletViewer = HeaderEnum' "CloudFront-Is-Tablet-Viewer"

pattern CloudFrontViewerCountry :: HeaderEnum
pattern CloudFrontViewerCountry = HeaderEnum' "CloudFront-Viewer-Country"

pattern Host :: HeaderEnum
pattern Host = HeaderEnum' "Host"

pattern Origin :: HeaderEnum
pattern Origin = HeaderEnum' "Origin"

pattern Referer :: HeaderEnum
pattern Referer = HeaderEnum' "Referer"

{-# COMPLETE
  Accept,
  AcceptCharset,
  AcceptDatetime,
  AcceptEncoding,
  AcceptLanguage,
  Authorization,
  CloudFrontForwardedProto,
  CloudFrontIsDesktopViewer,
  CloudFrontIsMobileViewer,
  CloudFrontIsSmartTVViewer,
  CloudFrontIsTabletViewer,
  CloudFrontViewerCountry,
  Host,
  Origin,
  Referer,
  HeaderEnum'
  #-}
