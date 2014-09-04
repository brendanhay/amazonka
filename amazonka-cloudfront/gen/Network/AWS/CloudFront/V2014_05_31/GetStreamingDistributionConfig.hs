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
    , getStreamingDistributionConfig
    -- ** Request lenses
    , gsdcrId

    -- * Response
    , GetStreamingDistributionConfigResponse
    -- ** Response lenses
    , gsdcsStreamingDistributionConfig
    , gsdcsETag
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetStreamingDistributionConfig' request.
getStreamingDistributionConfig :: Text -- ^ 'gsdcrId'
                               -> GetStreamingDistributionConfig
getStreamingDistributionConfig p1 = GetStreamingDistributionConfig
    { _gsdcrId = p1
    }
{-# INLINE getStreamingDistributionConfig #-}

data GetStreamingDistributionConfig = GetStreamingDistributionConfig
    { _gsdcrId :: Text
      -- ^ The streaming distribution's id.
    } deriving (Show, Generic)

-- | The streaming distribution's id.
gsdcrId :: Lens' GetStreamingDistributionConfig (Text)
gsdcrId f x =
    f (_gsdcrId x)
        <&> \y -> x { _gsdcrId = y }
{-# INLINE gsdcrId #-}

instance ToPath GetStreamingDistributionConfig where
    toPath GetStreamingDistributionConfig{..} = mconcat
        [ "/2014-05-31/streaming-distribution/"
        , toBS _gsdcrId
        , "/config"
        ]

instance ToQuery GetStreamingDistributionConfig

instance ToHeaders GetStreamingDistributionConfig

instance ToXML GetStreamingDistributionConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetStreamingDistributionConfigRequest"

data GetStreamingDistributionConfigResponse = GetStreamingDistributionConfigResponse
    { _gsdcsStreamingDistributionConfig :: Maybe StreamingDistributionConfig
      -- ^ The streaming distribution's configuration information.
    , _gsdcsETag :: Maybe Text
      -- ^ The current version of the configuration. For example:
      -- E2QWRUHAPOMQZL.
    } deriving (Show, Generic)

-- | The streaming distribution's configuration information.
gsdcsStreamingDistributionConfig :: Lens' GetStreamingDistributionConfigResponse (Maybe StreamingDistributionConfig)
gsdcsStreamingDistributionConfig f x =
    f (_gsdcsStreamingDistributionConfig x)
        <&> \y -> x { _gsdcsStreamingDistributionConfig = y }
{-# INLINE gsdcsStreamingDistributionConfig #-}

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gsdcsETag :: Lens' GetStreamingDistributionConfigResponse (Maybe Text)
gsdcsETag f x =
    f (_gsdcsETag x)
        <&> \y -> x { _gsdcsETag = y }
{-# INLINE gsdcsETag #-}

instance AWSRequest GetStreamingDistributionConfig where
    type Sv GetStreamingDistributionConfig = CloudFront
    type Rs GetStreamingDistributionConfig = GetStreamingDistributionConfigResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure GetStreamingDistributionConfigResponse
            <*> xml %|? "StreamingDistributionConfig"
            <*> hs ~:? "ETag"
