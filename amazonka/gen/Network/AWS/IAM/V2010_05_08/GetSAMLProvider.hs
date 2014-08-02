{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.V2010_05_08.GetSAMLProvider
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the SAML provider metadocument that was uploaded when the provider
-- was created or updated. This operation requires Signature Version 4.
-- https://iam.amazonaws.com/ ?Action=GetSAMLProvider
-- &Name=arn:aws:iam::123456789012:saml-metadata/MyUniversity
-- &Version=2010-05-08 &AUTHPARAMS 2012-05-09T16:27:11Z 2015-12-31T211:59:59Z
-- Pd9fexDssTkRgGNqs...DxptfEs== 29f47818-99f5-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.V2010_05_08.GetSAMLProvider where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

data GetSAMLProvider = GetSAMLProvider
    { _gsamlprSAMLProviderArn :: Text
      -- ^ The Amazon Resource Name (ARN) of the SAML provider to get
      -- information about.
    } deriving (Generic)

makeLenses ''GetSAMLProvider

instance ToQuery GetSAMLProvider where
    toQuery = genericToQuery def

data GetSAMLProviderResponse = GetSAMLProviderResponse
    { _gsamlpsCreateDate :: Maybe ISO8601
      -- ^ The date and time when the SAML provider was created.
    , _gsamlpsValidUntil :: Maybe ISO8601
      -- ^ The expiration date and time for the SAML provider.
    , _gsamlpsSAMLMetadataDocument :: Maybe Text
      -- ^ The XML metadata document that includes information about an
      -- identity provider.
    } deriving (Generic)

makeLenses ''GetSAMLProviderResponse

instance FromXML GetSAMLProviderResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetSAMLProvider where
    type Sv GetSAMLProvider = IAM
    type Rs GetSAMLProvider = GetSAMLProviderResponse

    request = post "GetSAMLProvider"
    response _ = xmlResponse
