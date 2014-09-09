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
    -- ** Response constructor
    , mkGetStreamingDistributionConfigResponse
    -- ** Response lenses
    , gsdcrStreamingDistributionConfig
    , gsdcrETag
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
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
mkGetStreamingDistributionConfig :: Text -- ^ 'gsdcId'
                                 -> GetStreamingDistributionConfig
mkGetStreamingDistributionConfig p1 = GetStreamingDistributionConfig
    { _gsdcId = p1
    }

-- | The streaming distribution's id.
gsdcId :: Lens' GetStreamingDistributionConfig Text
gsdcId = lens _gsdcId (\s a -> s { _gsdcId = a })

instance ToPath GetStreamingDistributionConfig

instance ToQuery GetStreamingDistributionConfig

instance ToHeaders GetStreamingDistributionConfig

instance ToXML GetStreamingDistributionConfig where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetStreamingDistributionConfigRequest"

-- | The returned result of the corresponding request.
data GetStreamingDistributionConfigResponse = GetStreamingDistributionConfigResponse
    { _gsdcrStreamingDistributionConfig :: Maybe StreamingDistributionConfig
    , _gsdcrETag :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetStreamingDistributionConfigResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StreamingDistributionConfig ::@ @Maybe StreamingDistributionConfig@
--
-- * @ETag ::@ @Maybe Text@
--
mkGetStreamingDistributionConfigResponse :: GetStreamingDistributionConfigResponse
mkGetStreamingDistributionConfigResponse = GetStreamingDistributionConfigResponse
    { _gsdcrStreamingDistributionConfig = Nothing
    , _gsdcrETag = Nothing
    }

-- | The streaming distribution's configuration information.
gsdcrStreamingDistributionConfig :: Lens' GetStreamingDistributionConfigResponse (Maybe StreamingDistributionConfig)
gsdcrStreamingDistributionConfig =
    lens _gsdcrStreamingDistributionConfig
         (\s a -> s { _gsdcrStreamingDistributionConfig = a })

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gsdcrETag :: Lens' GetStreamingDistributionConfigResponse (Maybe Text)
gsdcrETag = lens _gsdcrETag (\s a -> s { _gsdcrETag = a })

instance AWSRequest GetStreamingDistributionConfig where
    type Sv GetStreamingDistributionConfig = CloudFront
    type Rs GetStreamingDistributionConfig = GetStreamingDistributionConfigResponse

    request = get
    response _ = cursorResponse $ \hs xml ->
        pure GetStreamingDistributionConfigResponse
            <*> xml %|? "StreamingDistributionConfig"
            <*> hs ~:? "ETag"
