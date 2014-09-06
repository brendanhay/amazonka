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
    , mkGetSAMLProvider
    -- ** Request lenses
    , gsamlpSAMLProviderArn

    -- * Response
    , GetSAMLProviderResponse
    -- ** Response lenses
    , gsamlprsSAMLMetadataDocument
    , gsamlprsCreateDate
    , gsamlprsValidUntil
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | 
newtype GetSAMLProvider = GetSAMLProvider
    { _gsamlpSAMLProviderArn :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetSAMLProvider' request.
mkGetSAMLProvider :: Text -- ^ 'gsamlpSAMLProviderArn'
                  -> GetSAMLProvider
mkGetSAMLProvider p1 = GetSAMLProvider
    { _gsamlpSAMLProviderArn = p1
    }
{-# INLINE mkGetSAMLProvider #-}

-- | The Amazon Resource Name (ARN) of the SAML provider to get information
-- about.
gsamlpSAMLProviderArn :: Lens' GetSAMLProvider Text
gsamlpSAMLProviderArn =
    lens _gsamlpSAMLProviderArn (\s a -> s { _gsamlpSAMLProviderArn = a })
{-# INLINE gsamlpSAMLProviderArn #-}

instance ToQuery GetSAMLProvider where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the GetSAMLProvider
-- action.
data GetSAMLProviderResponse = GetSAMLProviderResponse
    { _gsamlprsSAMLMetadataDocument :: Maybe Text
    , _gsamlprsCreateDate :: Maybe ISO8601
    , _gsamlprsValidUntil :: Maybe ISO8601
    } deriving (Show, Generic)

-- | The XML metadata document that includes information about an identity
-- provider.
gsamlprsSAMLMetadataDocument :: Lens' GetSAMLProviderResponse (Maybe Text)
gsamlprsSAMLMetadataDocument =
    lens _gsamlprsSAMLMetadataDocument
         (\s a -> s { _gsamlprsSAMLMetadataDocument = a })
{-# INLINE gsamlprsSAMLMetadataDocument #-}

-- | The date and time when the SAML provider was created.
gsamlprsCreateDate :: Lens' GetSAMLProviderResponse (Maybe ISO8601)
gsamlprsCreateDate =
    lens _gsamlprsCreateDate (\s a -> s { _gsamlprsCreateDate = a })
{-# INLINE gsamlprsCreateDate #-}

-- | The expiration date and time for the SAML provider.
gsamlprsValidUntil :: Lens' GetSAMLProviderResponse (Maybe ISO8601)
gsamlprsValidUntil =
    lens _gsamlprsValidUntil (\s a -> s { _gsamlprsValidUntil = a })
{-# INLINE gsamlprsValidUntil #-}

instance FromXML GetSAMLProviderResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetSAMLProvider where
    type Sv GetSAMLProvider = IAM
    type Rs GetSAMLProvider = GetSAMLProviderResponse

    request = post "GetSAMLProvider"
    response _ = xmlResponse
