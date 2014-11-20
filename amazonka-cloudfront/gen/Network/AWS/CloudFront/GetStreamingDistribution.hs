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

-- Module      : Network.AWS.CloudFront.GetStreamingDistribution
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Get the information about a streaming distribution.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/GetStreamingDistribution.html>
module Network.AWS.CloudFront.GetStreamingDistribution
    (
    -- * Request
      GetStreamingDistribution
    -- ** Request constructor
    , getStreamingDistribution
    -- ** Request lenses
    , gsdId

    -- * Response
    , GetStreamingDistributionResponse
    -- ** Response constructor
    , getStreamingDistributionResponse
    -- ** Response lenses
    , gsdrETag
    , gsdrStreamingDistribution
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

newtype GetStreamingDistribution = GetStreamingDistribution
    { _gsdId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'GetStreamingDistribution' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsdId' @::@ 'Text'
--
getStreamingDistribution :: Text -- ^ 'gsdId'
                         -> GetStreamingDistribution
getStreamingDistribution p1 = GetStreamingDistribution
    { _gsdId = p1
    }

-- | The streaming distribution's id.
gsdId :: Lens' GetStreamingDistribution Text
gsdId = lens _gsdId (\s a -> s { _gsdId = a })

data GetStreamingDistributionResponse = GetStreamingDistributionResponse
    { _gsdrETag                  :: Maybe Text
    , _gsdrStreamingDistribution :: Maybe StreamingDistribution
    } deriving (Eq, Show)

-- | 'GetStreamingDistributionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsdrETag' @::@ 'Maybe' 'Text'
--
-- * 'gsdrStreamingDistribution' @::@ 'Maybe' 'StreamingDistribution'
--
getStreamingDistributionResponse :: GetStreamingDistributionResponse
getStreamingDistributionResponse = GetStreamingDistributionResponse
    { _gsdrStreamingDistribution = Nothing
    , _gsdrETag                  = Nothing
    }

-- | The current version of the streaming distribution's information. For
-- example: E2QWRUHAPOMQZL.
gsdrETag :: Lens' GetStreamingDistributionResponse (Maybe Text)
gsdrETag = lens _gsdrETag (\s a -> s { _gsdrETag = a })

-- | The streaming distribution's information.
gsdrStreamingDistribution :: Lens' GetStreamingDistributionResponse (Maybe StreamingDistribution)
gsdrStreamingDistribution =
    lens _gsdrStreamingDistribution
        (\s a -> s { _gsdrStreamingDistribution = a })

instance ToPath GetStreamingDistribution where
    toPath GetStreamingDistribution{..} = mconcat
        [ "/2014-05-31/streaming-distribution/"
        , toText _gsdId
        ]

instance ToQuery GetStreamingDistribution where
    toQuery = const mempty

instance ToHeaders GetStreamingDistribution

instance ToXMLRoot GetStreamingDistribution where
    toXMLRoot = const (element "GetStreamingDistribution" [])

instance ToXML GetStreamingDistribution

xml

instance AWSRequest GetStreamingDistribution where
    type Sv GetStreamingDistribution = CloudFront
    type Rs GetStreamingDistribution = GetStreamingDistributionResponse

    request  = get
    response = xmlHeaderResponse $ \h x -> GetStreamingDistributionResponse
        <$> h ~:? "ETag"
        <*> x .@? "StreamingDistribution"
