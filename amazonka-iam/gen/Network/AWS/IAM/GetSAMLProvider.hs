{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.GetSAMLProvider
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
module Network.AWS.IAM.GetSAMLProvider
    (
    -- * Request
      GetSAMLProvider
    -- ** Request constructor
    , mkGetSAMLProvider
    -- ** Request lenses
    , gsamlpSAMLProviderArn

    -- * Response
    , GetSAMLProviderResponse
    -- ** Response constructor
    , mkGetSAMLProviderResponse
    -- ** Response lenses
    , gsamlprSAMLMetadataDocument
    , gsamlprCreateDate
    , gsamlprValidUntil
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

newtype GetSAMLProvider = GetSAMLProvider
    { _gsamlpSAMLProviderArn :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetSAMLProvider' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SAMLProviderArn ::@ @Text@
--
mkGetSAMLProvider :: Text -- ^ 'gsamlpSAMLProviderArn'
                  -> GetSAMLProvider
mkGetSAMLProvider p1 = GetSAMLProvider
    { _gsamlpSAMLProviderArn = p1
    }

-- | The Amazon Resource Name (ARN) of the SAML provider to get information
-- about.
gsamlpSAMLProviderArn :: Lens' GetSAMLProvider Text
gsamlpSAMLProviderArn =
    lens _gsamlpSAMLProviderArn (\s a -> s { _gsamlpSAMLProviderArn = a })

instance ToQuery GetSAMLProvider where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the GetSAMLProvider
-- action.
data GetSAMLProviderResponse = GetSAMLProviderResponse
    { _gsamlprSAMLMetadataDocument :: !(Maybe Text)
    , _gsamlprCreateDate :: !(Maybe ISO8601)
    , _gsamlprValidUntil :: !(Maybe ISO8601)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetSAMLProviderResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SAMLMetadataDocument ::@ @Maybe Text@
--
-- * @CreateDate ::@ @Maybe ISO8601@
--
-- * @ValidUntil ::@ @Maybe ISO8601@
--
mkGetSAMLProviderResponse :: GetSAMLProviderResponse
mkGetSAMLProviderResponse = GetSAMLProviderResponse
    { _gsamlprSAMLMetadataDocument = Nothing
    , _gsamlprCreateDate = Nothing
    , _gsamlprValidUntil = Nothing
    }

-- | The XML metadata document that includes information about an identity
-- provider.
gsamlprSAMLMetadataDocument :: Lens' GetSAMLProviderResponse (Maybe Text)
gsamlprSAMLMetadataDocument =
    lens _gsamlprSAMLMetadataDocument
         (\s a -> s { _gsamlprSAMLMetadataDocument = a })

-- | The date and time when the SAML provider was created.
gsamlprCreateDate :: Lens' GetSAMLProviderResponse (Maybe ISO8601)
gsamlprCreateDate =
    lens _gsamlprCreateDate (\s a -> s { _gsamlprCreateDate = a })

-- | The expiration date and time for the SAML provider.
gsamlprValidUntil :: Lens' GetSAMLProviderResponse (Maybe ISO8601)
gsamlprValidUntil =
    lens _gsamlprValidUntil (\s a -> s { _gsamlprValidUntil = a })

instance FromXML GetSAMLProviderResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetSAMLProvider where
    type Sv GetSAMLProvider = IAM
    type Rs GetSAMLProvider = GetSAMLProviderResponse

    request = post "GetSAMLProvider"
    response _ = xmlResponse
