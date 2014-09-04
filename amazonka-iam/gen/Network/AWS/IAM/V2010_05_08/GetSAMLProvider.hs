{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.IAM.V2010_05_08.GetSAMLProvider
    (
    -- * Request
      GetSAMLProvider
    -- ** Request constructor
    , getSAMLProvider
    -- ** Request lenses
    , gsamlprSAMLProviderArn

    -- * Response
    , GetSAMLProviderResponse
    -- ** Response lenses
    , gsamlpsCreateDate
    , gsamlpsValidUntil
    , gsamlpsSAMLMetadataDocument
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetSAMLProvider' request.
getSAMLProvider :: Text -- ^ 'gsamlprSAMLProviderArn'
                -> GetSAMLProvider
getSAMLProvider p1 = GetSAMLProvider
    { _gsamlprSAMLProviderArn = p1
    }
{-# INLINE getSAMLProvider #-}

data GetSAMLProvider = GetSAMLProvider
    { _gsamlprSAMLProviderArn :: Text
      -- ^ The Amazon Resource Name (ARN) of the SAML provider to get
      -- information about.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the SAML provider to get information
-- about.
gsamlprSAMLProviderArn :: Lens' GetSAMLProvider (Text)
gsamlprSAMLProviderArn f x =
    f (_gsamlprSAMLProviderArn x)
        <&> \y -> x { _gsamlprSAMLProviderArn = y }
{-# INLINE gsamlprSAMLProviderArn #-}

instance ToQuery GetSAMLProvider where
    toQuery = genericQuery def

data GetSAMLProviderResponse = GetSAMLProviderResponse
    { _gsamlpsCreateDate :: Maybe ISO8601
      -- ^ The date and time when the SAML provider was created.
    , _gsamlpsValidUntil :: Maybe ISO8601
      -- ^ The expiration date and time for the SAML provider.
    , _gsamlpsSAMLMetadataDocument :: Maybe Text
      -- ^ The XML metadata document that includes information about an
      -- identity provider.
    } deriving (Show, Generic)

-- | The date and time when the SAML provider was created.
gsamlpsCreateDate :: Lens' GetSAMLProviderResponse (Maybe ISO8601)
gsamlpsCreateDate f x =
    f (_gsamlpsCreateDate x)
        <&> \y -> x { _gsamlpsCreateDate = y }
{-# INLINE gsamlpsCreateDate #-}

-- | The expiration date and time for the SAML provider.
gsamlpsValidUntil :: Lens' GetSAMLProviderResponse (Maybe ISO8601)
gsamlpsValidUntil f x =
    f (_gsamlpsValidUntil x)
        <&> \y -> x { _gsamlpsValidUntil = y }
{-# INLINE gsamlpsValidUntil #-}

-- | The XML metadata document that includes information about an identity
-- provider.
gsamlpsSAMLMetadataDocument :: Lens' GetSAMLProviderResponse (Maybe Text)
gsamlpsSAMLMetadataDocument f x =
    f (_gsamlpsSAMLMetadataDocument x)
        <&> \y -> x { _gsamlpsSAMLMetadataDocument = y }
{-# INLINE gsamlpsSAMLMetadataDocument #-}

instance FromXML GetSAMLProviderResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetSAMLProvider where
    type Sv GetSAMLProvider = IAM
    type Rs GetSAMLProvider = GetSAMLProviderResponse

    request = post "GetSAMLProvider"
    response _ = xmlResponse
