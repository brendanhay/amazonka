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
module Network.AWS.CloudFront.GetStreamingDistribution
    (
    -- * Request
      GetStreamingDistribution
    -- ** Request constructor
    , getStreamingDistribution
    -- ** Request lenses
    , gsdId

    -- * Response
    , GetStreamingDistributionResult
    -- ** Response constructor
    , getStreamingDistributionResult
    -- ** Response lenses
    , gsdrETag
    , gsdrStreamingDistribution
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudFront.Types

newtype GetStreamingDistribution = GetStreamingDistribution
    { _gsdId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

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

instance ToPath GetStreamingDistribution where
    toPath GetStreamingDistribution{..} = mconcat
        [ "/2014-05-31/streaming-distribution/"
        , toText _gsdId
        ]

instance ToQuery GetStreamingDistribution where
    toQuery = const mempty

instance ToHeaders GetStreamingDistribution

data GetStreamingDistributionResult = GetStreamingDistributionResult
    { _gsdrETag                  :: Maybe Text
    , _gsdrStreamingDistribution :: Maybe StreamingDistribution
    } deriving (Eq, Show, Generic)

-- | 'GetStreamingDistributionResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsdrETag' @::@ 'Maybe' 'Text'
--
-- * 'gsdrStreamingDistribution' @::@ 'Maybe' 'StreamingDistribution'
--
getStreamingDistributionResult :: GetStreamingDistributionResult
getStreamingDistributionResult = GetStreamingDistributionResult
    { _gsdrStreamingDistribution = Nothing
    , _gsdrETag                  = Nothing
    }

-- | The current version of the streaming distribution's information. For
-- example: E2QWRUHAPOMQZL.
gsdrETag :: Lens' GetStreamingDistributionResult (Maybe Text)
gsdrETag = lens _gsdrETag (\s a -> s { _gsdrETag = a })

-- | The streaming distribution's information.
gsdrStreamingDistribution :: Lens' GetStreamingDistributionResult (Maybe StreamingDistribution)
gsdrStreamingDistribution =
    lens _gsdrStreamingDistribution
        (\s a -> s { _gsdrStreamingDistribution = a })

instance FromXML GetStreamingDistributionResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetStreamingDistributionResult"
instance AWSRequest GetStreamingDistribution where
    type Sv GetStreamingDistribution = CloudFront
    type Rs GetStreamingDistribution = GetStreamingDistributionResult

    request  = get
    response = xmlResponse $ \h x -> GetStreamingDistributionResult
        <$> h ~:? "ETag"
        <*> x %| "StreamingDistribution"
