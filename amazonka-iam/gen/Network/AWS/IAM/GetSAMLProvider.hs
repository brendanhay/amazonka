{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
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
-- was created or updated.
module Network.AWS.IAM.GetSAMLProvider
    (
    -- * Request
      GetSAMLProvider
    -- ** Request constructor
    , getSAMLProvider
    -- ** Request lenses
    , gsamlpSAMLProviderArn

    -- * Response
    , GetSAMLProviderResponse
    -- ** Response constructor
    , getSAMLProviderResponse
    -- ** Response lenses
    , gsamlprCreateDate
    , gsamlprSAMLMetadataDocument
    , gsamlprValidUntil
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

newtype GetSAMLProvider = GetSAMLProvider
    { _gsamlpSAMLProviderArn :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetSAMLProvider' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsamlpSAMLProviderArn' @::@ 'Text'
--
getSAMLProvider :: Text -- ^ 'gsamlpSAMLProviderArn'
                -> GetSAMLProvider
getSAMLProvider p1 = GetSAMLProvider
    { _gsamlpSAMLProviderArn = p1
    }

-- | The Amazon Resource Name (ARN) of the SAML provider to get information
-- about.
gsamlpSAMLProviderArn :: Lens' GetSAMLProvider Text
gsamlpSAMLProviderArn =
    lens _gsamlpSAMLProviderArn (\s a -> s { _gsamlpSAMLProviderArn = a })

data GetSAMLProviderResponse = GetSAMLProviderResponse
    { _gsamlprCreateDate           :: Maybe RFC822
    , _gsamlprSAMLMetadataDocument :: Maybe Text
    , _gsamlprValidUntil           :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetSAMLProviderResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsamlprCreateDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'gsamlprSAMLMetadataDocument' @::@ 'Maybe' 'Text'
--
-- * 'gsamlprValidUntil' @::@ 'Maybe' 'UTCTime'
--
getSAMLProviderResponse :: GetSAMLProviderResponse
getSAMLProviderResponse = GetSAMLProviderResponse
    { _gsamlprSAMLMetadataDocument = Nothing
    , _gsamlprCreateDate           = Nothing
    , _gsamlprValidUntil           = Nothing
    }

-- | The date and time when the SAML provider was created.
gsamlprCreateDate :: Lens' GetSAMLProviderResponse (Maybe UTCTime)
gsamlprCreateDate =
    lens _gsamlprCreateDate (\s a -> s { _gsamlprCreateDate = a })
        . mapping _Time

-- | The XML metadata document that includes information about an identity
-- provider.
gsamlprSAMLMetadataDocument :: Lens' GetSAMLProviderResponse (Maybe Text)
gsamlprSAMLMetadataDocument =
    lens _gsamlprSAMLMetadataDocument
        (\s a -> s { _gsamlprSAMLMetadataDocument = a })

-- | The expiration date and time for the SAML provider.
gsamlprValidUntil :: Lens' GetSAMLProviderResponse (Maybe UTCTime)
gsamlprValidUntil =
    lens _gsamlprValidUntil (\s a -> s { _gsamlprValidUntil = a })
        . mapping _Time

instance AWSRequest GetSAMLProvider where
    type Sv GetSAMLProvider = IAM
    type Rs GetSAMLProvider = GetSAMLProviderResponse

    request  = post "GetSAMLProvider"
    response = xmlResponse

instance FromXML GetSAMLProviderResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetSAMLProviderResponse"

instance ToPath GetSAMLProvider where
    toPath = const "/"

instance ToHeaders GetSAMLProvider

instance ToQuery GetSAMLProvider
