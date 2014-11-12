{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
    , UpdateStreamingDistributionResult
    -- ** Response constructor
    , updateStreamingDistributionResult
    -- ** Response lenses
    , usdrETag
    , usdrStreamingDistribution
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

data UpdateStreamingDistribution = UpdateStreamingDistribution
    { _usdId                          :: Text
    , _usdIfMatch                     :: Maybe Text
    , _usdStreamingDistributionConfig :: StreamingDistributionConfig
    } (Eq, Show, Generic)

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

instance ToPath UpdateStreamingDistribution where
    toPath UpdateStreamingDistribution{..} = mconcat
        [ "/2014-05-31/streaming-distribution/"
        , toText _usdId
        , "/config"
        ]

instance ToQuery UpdateStreamingDistribution where
    toQuery = const mempty

instance ToHeaders UpdateStreamingDistribution where
    toHeaders UpdateStreamingDistribution{..} = mconcat
        [ "If-Match" =: _usdIfMatch
        ]

instance ToBody UpdateStreamingDistribution where
    toBody = toBody . encodeXML . _usdStreamingDistributionConfig

data UpdateStreamingDistributionResult = UpdateStreamingDistributionResult
    { _usdrETag                  :: Maybe Text
    , _usdrStreamingDistribution :: Maybe StreamingDistribution
    } (Eq, Show, Generic)

-- | 'UpdateStreamingDistributionResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usdrETag' @::@ 'Maybe' 'Text'
--
-- * 'usdrStreamingDistribution' @::@ 'Maybe' 'StreamingDistribution'
--
updateStreamingDistributionResult :: UpdateStreamingDistributionResult
updateStreamingDistributionResult = UpdateStreamingDistributionResult
    { _usdrStreamingDistribution = Nothing
    , _usdrETag                  = Nothing
    }

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
usdrETag :: Lens' UpdateStreamingDistributionResult (Maybe Text)
usdrETag = lens _usdrETag (\s a -> s { _usdrETag = a })

-- | The streaming distribution's information.
usdrStreamingDistribution :: Lens' UpdateStreamingDistributionResult (Maybe StreamingDistribution)
usdrStreamingDistribution =
    lens _usdrStreamingDistribution
        (\s a -> s { _usdrStreamingDistribution = a })

instance FromXML UpdateStreamingDistributionResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "UpdateStreamingDistributionResult"
instance AWSRequest UpdateStreamingDistribution where
    type Sv UpdateStreamingDistribution = CloudFront
    type Rs UpdateStreamingDistribution = UpdateStreamingDistributionResult

    request  = put
    response = xmlResponse $ \h x -> UpdateStreamingDistributionResult
        <$> h ~:? "ETag"
        <*> x %| "StreamingDistribution"
