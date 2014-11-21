{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.GetStreamingDistributionConfig
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the configuration information about a streaming distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetStreamingDistributionConfig.html>
module Network.AWS.CloudFront.GetStreamingDistributionConfig
    (
    -- * Request
      GetStreamingDistributionConfig
    -- ** Request constructor
    , getStreamingDistributionConfig
    -- ** Request lenses
    , gsdcId

    -- * Response
    , GetStreamingDistributionConfigResponse
    -- ** Response constructor
    , getStreamingDistributionConfigResponse
    -- ** Response lenses
    , gsdcrETag
    , gsdcrStreamingDistributionConfig
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

newtype GetStreamingDistributionConfig = GetStreamingDistributionConfig
    { _gsdcId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'GetStreamingDistributionConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsdcId' @::@ 'Text'
--
getStreamingDistributionConfig :: Text -- ^ 'gsdcId'
                               -> GetStreamingDistributionConfig
getStreamingDistributionConfig p1 = GetStreamingDistributionConfig
    { _gsdcId = p1
    }

-- | The streaming distribution's id.
gsdcId :: Lens' GetStreamingDistributionConfig Text
gsdcId = lens _gsdcId (\s a -> s { _gsdcId = a })

data GetStreamingDistributionConfigResponse = GetStreamingDistributionConfigResponse
    { _gsdcrETag                        :: Maybe Text
    , _gsdcrStreamingDistributionConfig :: Maybe StreamingDistributionConfig
    } deriving (Eq, Show)

-- | 'GetStreamingDistributionConfigResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsdcrETag' @::@ 'Maybe' 'Text'
--
-- * 'gsdcrStreamingDistributionConfig' @::@ 'Maybe' 'StreamingDistributionConfig'
--
getStreamingDistributionConfigResponse :: GetStreamingDistributionConfigResponse
getStreamingDistributionConfigResponse = GetStreamingDistributionConfigResponse
    { _gsdcrStreamingDistributionConfig = Nothing
    , _gsdcrETag                        = Nothing
    }

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gsdcrETag :: Lens' GetStreamingDistributionConfigResponse (Maybe Text)
gsdcrETag = lens _gsdcrETag (\s a -> s { _gsdcrETag = a })

-- | The streaming distribution's configuration information.
gsdcrStreamingDistributionConfig :: Lens' GetStreamingDistributionConfigResponse (Maybe StreamingDistributionConfig)
gsdcrStreamingDistributionConfig =
    lens _gsdcrStreamingDistributionConfig
        (\s a -> s { _gsdcrStreamingDistributionConfig = a })

instance ToPath GetStreamingDistributionConfig where
    toPath GetStreamingDistributionConfig{..} = mconcat
        [ "/2014-05-31/streaming-distribution/"
        , toText _gsdcId
        , "/config"
        ]

instance ToQuery GetStreamingDistributionConfig where
    toQuery = const mempty

instance ToHeaders GetStreamingDistributionConfig

instance ToXMLRoot GetStreamingDistributionConfig where
    toXMLRoot = const (namespaced ns "GetStreamingDistributionConfig" [])

instance ToXML GetStreamingDistributionConfig

instance AWSRequest GetStreamingDistributionConfig where
    type Sv GetStreamingDistributionConfig = CloudFront
    type Rs GetStreamingDistributionConfig = GetStreamingDistributionConfigResponse

    request  = get
    response = xmlHeaderResponse $ \h x -> GetStreamingDistributionConfigResponse
        <$> h ~:? "ETag"
        <*> x .@? "StreamingDistributionConfig"
