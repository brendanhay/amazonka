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
module Network.AWS.CloudFront.GetStreamingDistributionConfig
    (
    -- * Request
      GetStreamingDistributionConfig2014_05_31
    -- ** Request constructor
    , getStreamingDistributionConfig2014_05_31
    -- ** Request lenses
    , gsdcId

    -- * Response
    , GetStreamingDistributionConfig2014_05_31Response
    -- ** Response constructor
    , getStreamingDistributionConfig2014_05_31Response
    -- ** Response lenses
    , gsdcrETag
    , gsdcrStreamingDistributionConfig
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

newtype GetStreamingDistributionConfig2014_05_31 = GetStreamingDistributionConfig2014_05_31
    { _gsdcId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetStreamingDistributionConfig2014_05_31' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsdcId' @::@ 'Text'
--
getStreamingDistributionConfig2014_05_31 :: Text -- ^ 'gsdcId'
                                         -> GetStreamingDistributionConfig2014_05_31
getStreamingDistributionConfig2014_05_31 p1 = GetStreamingDistributionConfig2014_05_31
    { _gsdcId = p1
    }

-- | The streaming distribution's id.
gsdcId :: Lens' GetStreamingDistributionConfig2014_05_31 Text
gsdcId = lens _gsdcId (\s a -> s { _gsdcId = a })

instance ToPath GetStreamingDistributionConfig2014_05_31 where
    toPath GetStreamingDistributionConfig2014_05_31{..} = mconcat
        [ "/2014-05-31/streaming-distribution/"
        , toText _gsdcId
        , "/config"
        ]

instance ToQuery GetStreamingDistributionConfig2014_05_31 where
    toQuery = const mempty

instance ToHeaders GetStreamingDistributionConfig2014_05_31

data GetStreamingDistributionConfig2014_05_31Response = GetStreamingDistributionConfig2014_05_31Response
    { _gsdcrETag                        :: Maybe Text
    , _gsdcrStreamingDistributionConfig :: Maybe StreamingDistributionConfig
    } deriving (Eq, Show, Generic)

-- | 'GetStreamingDistributionConfig2014_05_31Response' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsdcrETag' @::@ 'Maybe' 'Text'
--
-- * 'gsdcrStreamingDistributionConfig' @::@ 'Maybe' 'StreamingDistributionConfig'
--
getStreamingDistributionConfig2014_05_31Response :: GetStreamingDistributionConfig2014_05_31Response
getStreamingDistributionConfig2014_05_31Response = GetStreamingDistributionConfig2014_05_31Response
    { _gsdcrStreamingDistributionConfig = Nothing
    , _gsdcrETag                        = Nothing
    }

-- | The current version of the configuration. For example: E2QWRUHAPOMQZL.
gsdcrETag :: Lens' GetStreamingDistributionConfig2014_05_31Response (Maybe Text)
gsdcrETag = lens _gsdcrETag (\s a -> s { _gsdcrETag = a })

-- | The streaming distribution's configuration information.
gsdcrStreamingDistributionConfig :: Lens' GetStreamingDistributionConfig2014_05_31Response (Maybe StreamingDistributionConfig)
gsdcrStreamingDistributionConfig =
    lens _gsdcrStreamingDistributionConfig
        (\s a -> s { _gsdcrStreamingDistributionConfig = a })

instance AWSRequest GetStreamingDistributionConfig2014_05_31 where
    type Sv GetStreamingDistributionConfig2014_05_31 = CloudFront
    type Rs GetStreamingDistributionConfig2014_05_31 = GetStreamingDistributionConfig2014_05_31Response

    request  = get
    response = xmlResponse $ \h x -> GetStreamingDistributionConfig2014_05_31Response
        <$> h ~:? "ETag"
        <*> x %| "StreamingDistributionConfig"
