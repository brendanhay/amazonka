{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/CreateStreamingDistribution.html>
module Network.AWS.CloudFront.CreateStreamingDistribution
    (
    -- * Request
      CreateStreamingDistribution
    -- ** Request constructor
    , createStreamingDistribution
    -- ** Request lenses
    , csdStreamingDistributionConfig

    -- * Response
    , CreateStreamingDistributionResponse
    -- ** Response constructor
    , createStreamingDistributionResponse
    -- ** Response lenses
    , csdrETag
    , csdrLocation
    , csdrStreamingDistribution
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.XML
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

newtype CreateStreamingDistribution = CreateStreamingDistribution
    { _csdStreamingDistributionConfig :: StreamingDistributionConfig
    } deriving (Eq, Show, Generic)

-- | 'CreateStreamingDistribution' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csdStreamingDistributionConfig' @::@ 'StreamingDistributionConfig'
--
createStreamingDistribution :: StreamingDistributionConfig -- ^ 'csdStreamingDistributionConfig'
                            -> CreateStreamingDistribution
createStreamingDistribution p1 = CreateStreamingDistribution
    { _csdStreamingDistributionConfig = p1
    }

-- | The streaming distribution's configuration information.
csdStreamingDistributionConfig :: Lens' CreateStreamingDistribution StreamingDistributionConfig
csdStreamingDistributionConfig =
    lens _csdStreamingDistributionConfig
        (\s a -> s { _csdStreamingDistributionConfig = a })

data CreateStreamingDistributionResponse = CreateStreamingDistributionResponse
    { _csdrETag                  :: Maybe Text
    , _csdrLocation              :: Maybe Text
    , _csdrStreamingDistribution :: Maybe StreamingDistribution
    } deriving (Eq, Show, Generic)

-- | 'CreateStreamingDistributionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csdrETag' @::@ 'Maybe' 'Text'
--
-- * 'csdrLocation' @::@ 'Maybe' 'Text'
--
-- * 'csdrStreamingDistribution' @::@ 'Maybe' 'StreamingDistribution'
--
createStreamingDistributionResponse :: CreateStreamingDistributionResponse
createStreamingDistributionResponse = CreateStreamingDistributionResponse
    { _csdrStreamingDistribution = Nothing
    , _csdrLocation              = Nothing
    , _csdrETag                  = Nothing
    }

-- | The current version of the streaming distribution created.
csdrETag :: Lens' CreateStreamingDistributionResponse (Maybe Text)
csdrETag = lens _csdrETag (\s a -> s { _csdrETag = a })

-- | The fully qualified URI of the new streaming distribution resource just
-- created. For example:
-- https://cloudfront.amazonaws.com/2010-11-01/streaming-distribution/EGTXBD79H29TRA8.
-- 
csdrLocation :: Lens' CreateStreamingDistributionResponse (Maybe Text)
csdrLocation = lens _csdrLocation (\s a -> s { _csdrLocation = a })

-- | The streaming distribution's information.
csdrStreamingDistribution :: Lens' CreateStreamingDistributionResponse (Maybe StreamingDistribution)
csdrStreamingDistribution =
    lens _csdrStreamingDistribution
        (\s a -> s { _csdrStreamingDistribution = a })

instance AWSRequest CreateStreamingDistribution where
    type Sv CreateStreamingDistribution = CloudFront
    type Rs CreateStreamingDistribution = CreateStreamingDistributionResponse

    request  = post
    response = xmlHeaderResponse $ \h x -> CreateStreamingDistributionResponse
        <*> h ~:? "ETag"
        <*> h ~:? "Location"
        <*> x %| "StreamingDistribution"

instance ToPath CreateStreamingDistribution where
    toPath = const "/2014-05-31/streaming-distribution"

instance ToHeaders CreateStreamingDistribution

instance ToQuery CreateStreamingDistribution where
    toQuery = const mempty

instance ToXML CreateStreamingDistribution where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "CreateStreamingDistribution"
