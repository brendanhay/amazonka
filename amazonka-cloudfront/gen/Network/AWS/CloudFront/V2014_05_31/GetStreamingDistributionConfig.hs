{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.GetStreamingDistributionConfig
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the configuration information about a streaming distribution.
module Network.AWS.CloudFront.V2014_05_31.GetStreamingDistributionConfig
    (
    -- * Request
      GetStreamingDistributionConfig
    -- ** Request constructor
    , mkGetStreamingDistributionConfig
    -- ** Request lenses
    , gsdcId

    -- * Response
    , GetStreamingDistributionConfigResponse
    -- ** Response lenses
    , gsdcrsStreamingDistributionConfig
    , gsdcrsETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | To request to get a streaming distribution configuration.
newtype GetStreamingDistributionConfig = GetStreamingDistributionConfig
    { _gsdcId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetStreamingDistributionConfig' request.
mkGetStreamingDistributionConfig :: Text -- ^ 'gsdcId'
                                 -> GetStreamingDistributionConfig
mkGetStreamingDistributionConfig p1 = GetStreamingDistributionConfig
    { _gsdcId = p1
    }

-- | The streaming distribution's id.
gsdcId :: Lens' GetStreamingDistributionConfig Text
gsdcId = lens _gsdcId (\s a -> s { _gsdcId = a })

instance ToPath GetStreamingDistributionConfig where
    toPath GetStreamingDistributionConfig{..} = mconcat
        [ "/2014-05-31/streaming-distribution/"
        , toBS _gsdcId
        , "/config"
        ]

instance ToQuery GetStreamingDistributionConfig

instance ToHeaders GetStreamingDistributionConfig

instance ToXML GetStreamingDistributionConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetStreamingDistributionConfigRequest"

-- | The returned result of the corresponding request.
data GetStreamingDistributionConfigResponse = GetStreamingDistributionConfigResponse
    { _gsdcrsStreamingDistributionConfig :: Maybe StreamingDistributionConfig
    , _gsdcrsETag :: Maybe Text
    } deriving (Show, Generic)

-- | The streaming distribution's configuration information.
gsdcrsStreamingDistributionConfig :: Lens' GetStreamingDistributionConfigResponse (Maybe StreamingDistributionConfig)
gsdcrsStreamingDistributionConfig =
    lens _gsdcrsStreamingDistributionConfig
         (\s a -> s { _gsdcrsStreamingDistributionConfig = a })

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gsdcrsETag :: Lens' GetStreamingDistributionConfigResponse (Maybe Text)
gsdcrsETag = lens _gsdcrsETag (\s a -> s { _gsdcrsETag = a })

instance AWSRequest GetStreamingDistributionConfig where
    type Sv GetStreamingDistributionConfig = CloudFront
    type Rs GetStreamingDistributionConfig = GetStreamingDistributionConfigResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure GetStreamingDistributionConfigResponse
            <*> xml %|? "StreamingDistributionConfig"
            <*> hs ~:? "ETag"
