{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.UpdateStreamingDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Update a streaming distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/UpdateStreamingDistribution.html>
module Network.AWS.CloudFront.UpdateStreamingDistribution
    (
    -- * Request
      UpdateStreamingDistribution
    -- ** Request constructor
    , updateStreamingDistribution
    -- ** Request lenses
    , usdId
    , usdIfMatch
    , usdStreamingDistributionConfig

    -- * Response
    , UpdateStreamingDistributionResponse
    -- ** Response constructor
    , updateStreamingDistributionResponse
    -- ** Response lenses
    , usdrETag
    , usdrStreamingDistribution
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.XML
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

data UpdateStreamingDistribution = UpdateStreamingDistribution
    { _usdId                          :: Text
    , _usdIfMatch                     :: Maybe Text
    , _usdStreamingDistributionConfig :: StreamingDistributionConfig
    } deriving (Eq, Show, Generic)

-- | 'UpdateStreamingDistribution' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usdId' @::@ 'Text'
--
-- * 'usdIfMatch' @::@ 'Maybe' 'Text'
--
-- * 'usdStreamingDistributionConfig' @::@ 'StreamingDistributionConfig'
--
updateStreamingDistribution :: StreamingDistributionConfig -- ^ 'usdStreamingDistributionConfig'
                            -> Text -- ^ 'usdId'
                            -> UpdateStreamingDistribution
updateStreamingDistribution p1 p2 = UpdateStreamingDistribution
    { _usdStreamingDistributionConfig = p1
    , _usdId                          = p2
    , _usdIfMatch                     = Nothing
    }

-- | The streaming distribution's id.
usdId :: Lens' UpdateStreamingDistribution Text
usdId = lens _usdId (\s a -> s { _usdId = a })

-- | The value of the ETag header you received when retrieving the streaming
-- distribution's configuration. For example: E2QWRUHAPOMQZL.
usdIfMatch :: Lens' UpdateStreamingDistribution (Maybe Text)
usdIfMatch = lens _usdIfMatch (\s a -> s { _usdIfMatch = a })

-- | The streaming distribution's configuration information.
usdStreamingDistributionConfig :: Lens' UpdateStreamingDistribution StreamingDistributionConfig
usdStreamingDistributionConfig =
    lens _usdStreamingDistributionConfig
        (\s a -> s { _usdStreamingDistributionConfig = a })

data UpdateStreamingDistributionResponse = UpdateStreamingDistributionResponse
    { _usdrETag                  :: Maybe Text
    , _usdrStreamingDistribution :: Maybe StreamingDistribution
    } deriving (Eq, Show, Generic)

-- | 'UpdateStreamingDistributionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usdrETag' @::@ 'Maybe' 'Text'
--
-- * 'usdrStreamingDistribution' @::@ 'Maybe' 'StreamingDistribution'
--
updateStreamingDistributionResponse :: UpdateStreamingDistributionResponse
updateStreamingDistributionResponse = UpdateStreamingDistributionResponse
    { _usdrStreamingDistribution = Nothing
    , _usdrETag                  = Nothing
    }

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
usdrETag :: Lens' UpdateStreamingDistributionResponse (Maybe Text)
usdrETag = lens _usdrETag (\s a -> s { _usdrETag = a })

-- | The streaming distribution's information.
usdrStreamingDistribution :: Lens' UpdateStreamingDistributionResponse (Maybe StreamingDistribution)
usdrStreamingDistribution =
    lens _usdrStreamingDistribution
        (\s a -> s { _usdrStreamingDistribution = a })

instance AWSRequest UpdateStreamingDistribution where
    type Sv UpdateStreamingDistribution = CloudFront
    type Rs UpdateStreamingDistribution = UpdateStreamingDistributionResponse

    request  = put
    response = xmlHeaderResponse $ \h x -> UpdateStreamingDistributionResponse
        <$> h ~:? "ETag"
        <*> x %| "StreamingDistribution"

instance ToPath UpdateStreamingDistribution where
    toPath UpdateStreamingDistribution{..} = mconcat
        [ "/2014-05-31/streaming-distribution/"
        , toText _usdId
        , "/config"
        ]

instance ToHeaders UpdateStreamingDistribution where
    toHeaders UpdateStreamingDistribution{..} = mconcat
        [ "If-Match" =: _usdIfMatch
        ]

instance ToQuery UpdateStreamingDistribution where
    toQuery = const mempty

instance ToXML UpdateStreamingDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "UpdateStreamingDistribution"
