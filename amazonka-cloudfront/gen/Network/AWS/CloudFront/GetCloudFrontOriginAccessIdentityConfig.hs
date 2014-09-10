{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the configuration information about an origin access identity.
module Network.AWS.CloudFront
    (
    -- * Request
      GetCloudFrontOriginAccessIdentityConfig
    -- ** Request constructor
    , mkGetCloudFrontOriginAccessIdentityConfig
    -- ** Request lenses
    , gcfoaicId

    -- * Response
    , GetCloudFrontOriginAccessIdentityConfigResponse
    -- ** Response constructor
    , mkGetCloudFrontOriginAccessIdentityConfigResponse
    -- ** Response lenses
    , gcfoaicrCloudFrontOriginAccessIdentityConfig
    , gcfoaicrETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to get an origin access identity's configuration.
newtype GetCloudFrontOriginAccessIdentityConfig = GetCloudFrontOriginAccessIdentityConfig
    { _gcfoaicId :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetCloudFrontOriginAccessIdentityConfig' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
mkGetCloudFrontOriginAccessIdentityConfig :: Text -- ^ 'gcfoaicId'
                                          -> GetCloudFrontOriginAccessIdentityConfig
mkGetCloudFrontOriginAccessIdentityConfig p1 = GetCloudFrontOriginAccessIdentityConfig
    { _gcfoaicId = p1
    }

-- | The identity's id.
gcfoaicId :: Lens' GetCloudFrontOriginAccessIdentityConfig Text
gcfoaicId = lens _gcfoaicId (\s a -> s { _gcfoaicId = a })

instance ToPath GetCloudFrontOriginAccessIdentityConfig

instance ToQuery GetCloudFrontOriginAccessIdentityConfig

instance ToHeaders GetCloudFrontOriginAccessIdentityConfig

instance ToXML GetCloudFrontOriginAccessIdentityConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetCloudFrontOriginAccessIdentityConfigRequest"

-- | The returned result of the corresponding request.
data GetCloudFrontOriginAccessIdentityConfigResponse = GetCloudFrontOriginAccessIdentityConfigResponse
    { _gcfoaicrCloudFrontOriginAccessIdentityConfig :: Maybe CloudFrontOriginAccessIdentityConfig
    , _gcfoaicrETag :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetCloudFrontOriginAccessIdentityConfigResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CloudFrontOriginAccessIdentityConfig ::@ @Maybe CloudFrontOriginAccessIdentityConfig@
--
-- * @ETag ::@ @Maybe Text@
--
mkGetCloudFrontOriginAccessIdentityConfigResponse :: GetCloudFrontOriginAccessIdentityConfigResponse
mkGetCloudFrontOriginAccessIdentityConfigResponse = GetCloudFrontOriginAccessIdentityConfigResponse
    { _gcfoaicrCloudFrontOriginAccessIdentityConfig = Nothing
    , _gcfoaicrETag = Nothing
    }

-- | The origin access identity's configuration information.
gcfoaicrCloudFrontOriginAccessIdentityConfig :: Lens' GetCloudFrontOriginAccessIdentityConfigResponse (Maybe CloudFrontOriginAccessIdentityConfig)
gcfoaicrCloudFrontOriginAccessIdentityConfig =
    lens _gcfoaicrCloudFrontOriginAccessIdentityConfig
         (\s a -> s { _gcfoaicrCloudFrontOriginAccessIdentityConfig = a })

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gcfoaicrETag :: Lens' GetCloudFrontOriginAccessIdentityConfigResponse (Maybe Text)
gcfoaicrETag = lens _gcfoaicrETag (\s a -> s { _gcfoaicrETag = a })

instance AWSRequest GetCloudFrontOriginAccessIdentityConfig where
    type Sv GetCloudFrontOriginAccessIdentityConfig = CloudFront
    type Rs GetCloudFrontOriginAccessIdentityConfig = GetCloudFrontOriginAccessIdentityConfigResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure GetCloudFrontOriginAccessIdentityConfigResponse
            <*> xml %|? "CloudFrontOriginAccessIdentityConfig"
            <*> hs ~:? "ETag"
