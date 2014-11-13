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
      UpdateStreamingDistribution2014_05_31
    -- ** Request constructor
    , updateStreamingDistribution2014_05_31
    -- ** Request lenses
    , usdId
    , usdIfMatch
    , usdStreamingDistributionConfig

    -- * Response
    , UpdateStreamingDistribution2014_05_31Response
    -- ** Response constructor
    , updateStreamingDistribution2014_05_31Response
    -- ** Response lenses
    , usdrETag
    , usdrStreamingDistribution
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

data UpdateStreamingDistribution2014_05_31 = UpdateStreamingDistribution2014_05_31
    { _usdId                          :: Text
    , _usdIfMatch                     :: Maybe Text
    , _usdStreamingDistributionConfig :: StreamingDistributionConfig
    } deriving (Eq, Show, Generic)

-- | 'UpdateStreamingDistribution2014_05_31' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usdId' @::@ 'Text'
--
-- * 'usdIfMatch' @::@ 'Maybe' 'Text'
--
-- * 'usdStreamingDistributionConfig' @::@ 'StreamingDistributionConfig'
--
updateStreamingDistribution2014_05_31 :: StreamingDistributionConfig -- ^ 'usdStreamingDistributionConfig'
                                      -> Text -- ^ 'usdId'
                                      -> UpdateStreamingDistribution2014_05_31
updateStreamingDistribution2014_05_31 p1 p2 = UpdateStreamingDistribution2014_05_31
    { _usdStreamingDistributionConfig = p1
    , _usdId                          = p2
    , _usdIfMatch                     = Nothing
    }

-- | The streaming distribution's id.
usdId :: Lens' UpdateStreamingDistribution2014_05_31 Text
usdId = lens _usdId (\s a -> s { _usdId = a })

-- | The value of the ETag header you received when retrieving the streaming
-- distribution's configuration. For example: E2QWRUHAPOMQZL.
usdIfMatch :: Lens' UpdateStreamingDistribution2014_05_31 (Maybe Text)
usdIfMatch = lens _usdIfMatch (\s a -> s { _usdIfMatch = a })

-- | The streaming distribution's configuration information.
usdStreamingDistributionConfig :: Lens' UpdateStreamingDistribution2014_05_31 StreamingDistributionConfig
usdStreamingDistributionConfig =
    lens _usdStreamingDistributionConfig
        (\s a -> s { _usdStreamingDistributionConfig = a })

instance ToPath UpdateStreamingDistribution2014_05_31 where
    toPath UpdateStreamingDistribution2014_05_31{..} = mconcat
        [ "/2014-05-31/streaming-distribution/"
        , toText _usdId
        , "/config"
        ]

instance ToQuery UpdateStreamingDistribution2014_05_31 where
    toQuery = const mempty

instance ToHeaders UpdateStreamingDistribution2014_05_31 where
    toHeaders UpdateStreamingDistribution2014_05_31{..} = mconcat
        [ "If-Match" =: _usdIfMatch
        ]

instance ToBody UpdateStreamingDistribution2014_05_31 where
    toBody = toBody . encodeXML . _usdStreamingDistributionConfig

data UpdateStreamingDistribution2014_05_31Response = UpdateStreamingDistribution2014_05_31Response
    { _usdrETag                  :: Maybe Text
    , _usdrStreamingDistribution :: Maybe StreamingDistribution
    } deriving (Eq, Show, Generic)

-- | 'UpdateStreamingDistribution2014_05_31Response' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usdrETag' @::@ 'Maybe' 'Text'
--
-- * 'usdrStreamingDistribution' @::@ 'Maybe' 'StreamingDistribution'
--
updateStreamingDistribution2014_05_31Response :: UpdateStreamingDistribution2014_05_31Response
updateStreamingDistribution2014_05_31Response = UpdateStreamingDistribution2014_05_31Response
    { _usdrStreamingDistribution = Nothing
    , _usdrETag                  = Nothing
    }

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
usdrETag :: Lens' UpdateStreamingDistribution2014_05_31Response (Maybe Text)
usdrETag = lens _usdrETag (\s a -> s { _usdrETag = a })

-- | The streaming distribution's information.
usdrStreamingDistribution :: Lens' UpdateStreamingDistribution2014_05_31Response (Maybe StreamingDistribution)
usdrStreamingDistribution =
    lens _usdrStreamingDistribution
        (\s a -> s { _usdrStreamingDistribution = a })

instance FromXML UpdateStreamingDistribution2014_05_31Response where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "UpdateStreamingDistribution2014_05_31Response"
instance AWSRequest UpdateStreamingDistribution2014_05_31 where
    type Sv UpdateStreamingDistribution2014_05_31 = CloudFront
    type Rs UpdateStreamingDistribution2014_05_31 = UpdateStreamingDistribution2014_05_31Response

    request  = put
    response = xmlResponse $ \h x -> UpdateStreamingDistribution2014_05_31Response
        <$> h ~:? "ETag"
        <*> x %| "StreamingDistribution"
