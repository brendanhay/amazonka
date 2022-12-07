{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Lightsail.Types.HeaderEnum
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.HeaderEnum
  ( HeaderEnum
      ( ..,
        HeaderEnum_Accept,
        HeaderEnum_Accept_Charset,
        HeaderEnum_Accept_Datetime,
        HeaderEnum_Accept_Encoding,
        HeaderEnum_Accept_Language,
        HeaderEnum_Authorization,
        HeaderEnum_CloudFront_Forwarded_Proto,
        HeaderEnum_CloudFront_Is_Desktop_Viewer,
        HeaderEnum_CloudFront_Is_Mobile_Viewer,
        HeaderEnum_CloudFront_Is_SmartTV_Viewer,
        HeaderEnum_CloudFront_Is_Tablet_Viewer,
        HeaderEnum_CloudFront_Viewer_Country,
        HeaderEnum_Host,
        HeaderEnum_Origin,
        HeaderEnum_Referer
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HeaderEnum = HeaderEnum'
  { fromHeaderEnum ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern HeaderEnum_Accept :: HeaderEnum
pattern HeaderEnum_Accept = HeaderEnum' "Accept"

pattern HeaderEnum_Accept_Charset :: HeaderEnum
pattern HeaderEnum_Accept_Charset = HeaderEnum' "Accept-Charset"

pattern HeaderEnum_Accept_Datetime :: HeaderEnum
pattern HeaderEnum_Accept_Datetime = HeaderEnum' "Accept-Datetime"

pattern HeaderEnum_Accept_Encoding :: HeaderEnum
pattern HeaderEnum_Accept_Encoding = HeaderEnum' "Accept-Encoding"

pattern HeaderEnum_Accept_Language :: HeaderEnum
pattern HeaderEnum_Accept_Language = HeaderEnum' "Accept-Language"

pattern HeaderEnum_Authorization :: HeaderEnum
pattern HeaderEnum_Authorization = HeaderEnum' "Authorization"

pattern HeaderEnum_CloudFront_Forwarded_Proto :: HeaderEnum
pattern HeaderEnum_CloudFront_Forwarded_Proto = HeaderEnum' "CloudFront-Forwarded-Proto"

pattern HeaderEnum_CloudFront_Is_Desktop_Viewer :: HeaderEnum
pattern HeaderEnum_CloudFront_Is_Desktop_Viewer = HeaderEnum' "CloudFront-Is-Desktop-Viewer"

pattern HeaderEnum_CloudFront_Is_Mobile_Viewer :: HeaderEnum
pattern HeaderEnum_CloudFront_Is_Mobile_Viewer = HeaderEnum' "CloudFront-Is-Mobile-Viewer"

pattern HeaderEnum_CloudFront_Is_SmartTV_Viewer :: HeaderEnum
pattern HeaderEnum_CloudFront_Is_SmartTV_Viewer = HeaderEnum' "CloudFront-Is-SmartTV-Viewer"

pattern HeaderEnum_CloudFront_Is_Tablet_Viewer :: HeaderEnum
pattern HeaderEnum_CloudFront_Is_Tablet_Viewer = HeaderEnum' "CloudFront-Is-Tablet-Viewer"

pattern HeaderEnum_CloudFront_Viewer_Country :: HeaderEnum
pattern HeaderEnum_CloudFront_Viewer_Country = HeaderEnum' "CloudFront-Viewer-Country"

pattern HeaderEnum_Host :: HeaderEnum
pattern HeaderEnum_Host = HeaderEnum' "Host"

pattern HeaderEnum_Origin :: HeaderEnum
pattern HeaderEnum_Origin = HeaderEnum' "Origin"

pattern HeaderEnum_Referer :: HeaderEnum
pattern HeaderEnum_Referer = HeaderEnum' "Referer"

{-# COMPLETE
  HeaderEnum_Accept,
  HeaderEnum_Accept_Charset,
  HeaderEnum_Accept_Datetime,
  HeaderEnum_Accept_Encoding,
  HeaderEnum_Accept_Language,
  HeaderEnum_Authorization,
  HeaderEnum_CloudFront_Forwarded_Proto,
  HeaderEnum_CloudFront_Is_Desktop_Viewer,
  HeaderEnum_CloudFront_Is_Mobile_Viewer,
  HeaderEnum_CloudFront_Is_SmartTV_Viewer,
  HeaderEnum_CloudFront_Is_Tablet_Viewer,
  HeaderEnum_CloudFront_Viewer_Country,
  HeaderEnum_Host,
  HeaderEnum_Origin,
  HeaderEnum_Referer,
  HeaderEnum'
  #-}
