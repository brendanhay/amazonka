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

-- Module      : Network.AWS.CloudFront.CreateStreamingDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Create a new streaming distribution.
module Network.AWS.CloudFront.CreateStreamingDistribution
    (
    -- * Request
      CreateStreamingDistribution2014_05_31
    -- ** Request constructor
    , createStreamingDistribution2014_05_31
    -- ** Request lenses
    , csdStreamingDistributionConfig

    -- * Response
    , CreateStreamingDistribution2014_05_31Response
    -- ** Response constructor
    , createStreamingDistribution2014_05_31Response
    -- ** Response lenses
    , csdrETag
    , csdrLocation
    , csdrStreamingDistribution
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

newtype CreateStreamingDistribution2014_05_31 = CreateStreamingDistribution2014_05_31
    { _csdStreamingDistributionConfig :: StreamingDistributionConfig
    } deriving (Eq, Show, Generic)

-- | 'CreateStreamingDistribution2014_05_31' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csdStreamingDistributionConfig' @::@ 'StreamingDistributionConfig'
--
createStreamingDistribution2014_05_31 :: StreamingDistributionConfig -- ^ 'csdStreamingDistributionConfig'
                                      -> CreateStreamingDistribution2014_05_31
createStreamingDistribution2014_05_31 p1 = CreateStreamingDistribution2014_05_31
    { _csdStreamingDistributionConfig = p1
    }

-- | The streaming distribution's configuration information.
csdStreamingDistributionConfig :: Lens' CreateStreamingDistribution2014_05_31 StreamingDistributionConfig
csdStreamingDistributionConfig =
    lens _csdStreamingDistributionConfig
        (\s a -> s { _csdStreamingDistributionConfig = a })

instance ToPath CreateStreamingDistribution2014_05_31 where
    toPath = const "/2014-05-31/streaming-distribution"

instance ToQuery CreateStreamingDistribution2014_05_31 where
    toQuery = const mempty

instance ToHeaders CreateStreamingDistribution2014_05_31

instance ToBody CreateStreamingDistribution2014_05_31 where
    toBody = toBody . encodeXML . _csdStreamingDistributionConfig

data CreateStreamingDistribution2014_05_31Response = CreateStreamingDistribution2014_05_31Response
    { _csdrETag                  :: Maybe Text
    , _csdrLocation              :: Maybe Text
    , _csdrStreamingDistribution :: Maybe StreamingDistribution
    } deriving (Eq, Show, Generic)

-- | 'CreateStreamingDistribution2014_05_31Response' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csdrETag' @::@ 'Maybe' 'Text'
--
-- * 'csdrLocation' @::@ 'Maybe' 'Text'
--
-- * 'csdrStreamingDistribution' @::@ 'Maybe' 'StreamingDistribution'
--
createStreamingDistribution2014_05_31Response :: CreateStreamingDistribution2014_05_31Response
createStreamingDistribution2014_05_31Response = CreateStreamingDistribution2014_05_31Response
    { _csdrStreamingDistribution = Nothing
    , _csdrLocation              = Nothing
    , _csdrETag                  = Nothing
    }

-- | The current version of the streaming distribution created.
csdrETag :: Lens' CreateStreamingDistribution2014_05_31Response (Maybe Text)
csdrETag = lens _csdrETag (\s a -> s { _csdrETag = a })

-- | The fully qualified URI of the new streaming distribution resource just
-- created. For example:
-- https://cloudfront.amazonaws.com/2010-11-01/streaming-distribution/EGTXBD79H29TRA8.
-- 
csdrLocation :: Lens' CreateStreamingDistribution2014_05_31Response (Maybe Text)
csdrLocation = lens _csdrLocation (\s a -> s { _csdrLocation = a })

-- | The streaming distribution's information.
csdrStreamingDistribution :: Lens' CreateStreamingDistribution2014_05_31Response (Maybe StreamingDistribution)
csdrStreamingDistribution =
    lens _csdrStreamingDistribution
        (\s a -> s { _csdrStreamingDistribution = a })

instance AWSRequest CreateStreamingDistribution2014_05_31 where
    type Sv CreateStreamingDistribution2014_05_31 = CloudFront
    type Rs CreateStreamingDistribution2014_05_31 = CreateStreamingDistribution2014_05_31Response

    request  = post
    response = xmlResponse $ \h x -> CreateStreamingDistribution2014_05_31Response
        <$> h ~:? "ETag"
        <*> h ~:? "Location"
        <*> x %| "StreamingDistribution"
