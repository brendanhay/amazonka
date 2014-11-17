{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.ListStreamingDistributions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | List streaming distributions.
--
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/ListStreamingDistributions.html>
module Network.AWS.CloudFront.ListStreamingDistributions
    (
    -- * Request
      ListStreamingDistributions
    -- ** Request constructor
    , listStreamingDistributions
    -- ** Request lenses
    , lsdMarker
    , lsdMaxItems

    -- * Response
    , ListStreamingDistributionsResponse
    -- ** Response constructor
    , listStreamingDistributionsResponse
    -- ** Response lenses
    , lsdrStreamingDistributionList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

data ListStreamingDistributions = ListStreamingDistributions
    { _lsdMarker   :: Maybe Text
    , _lsdMaxItems :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListStreamingDistributions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsdMarker' @::@ 'Maybe' 'Text'
--
-- * 'lsdMaxItems' @::@ 'Maybe' 'Text'
--
listStreamingDistributions :: ListStreamingDistributions
listStreamingDistributions = ListStreamingDistributions
    { _lsdMarker   = Nothing
    , _lsdMaxItems = Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list
-- of streaming distributions. The results include distributions in the list
-- that occur after the marker. To get the next page of results, set the
-- Marker to the value of the NextMarker from the current page's response
-- (which is also the ID of the last distribution on that page).
lsdMarker :: Lens' ListStreamingDistributions (Maybe Text)
lsdMarker = lens _lsdMarker (\s a -> s { _lsdMarker = a })

-- | The maximum number of streaming distributions you want in the response
-- body.
lsdMaxItems :: Lens' ListStreamingDistributions (Maybe Text)
lsdMaxItems = lens _lsdMaxItems (\s a -> s { _lsdMaxItems = a })

newtype ListStreamingDistributionsResponse = ListStreamingDistributionsResponse
    { _lsdrStreamingDistributionList :: Maybe StreamingDistributionList
    } deriving (Eq, Show, Generic)

-- | 'ListStreamingDistributionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsdrStreamingDistributionList' @::@ 'Maybe' 'StreamingDistributionList'
--
listStreamingDistributionsResponse :: ListStreamingDistributionsResponse
listStreamingDistributionsResponse = ListStreamingDistributionsResponse
    { _lsdrStreamingDistributionList = Nothing
    }

-- | The StreamingDistributionList type.
lsdrStreamingDistributionList :: Lens' ListStreamingDistributionsResponse (Maybe StreamingDistributionList)
lsdrStreamingDistributionList =
    lens _lsdrStreamingDistributionList
        (\s a -> s { _lsdrStreamingDistributionList = a })

instance ToPath ListStreamingDistributions where
    toPath = const "/2014-05-31/streaming-distribution"

instance ToQuery ListStreamingDistributions

instance ToHeaders ListStreamingDistributions

instance ToXML ListStreamingDistributions where
    toXML = const (node "ListStreamingDistributions" [])

instance AWSRequest ListStreamingDistributions where
    type Sv ListStreamingDistributions = CloudFront
    type Rs ListStreamingDistributions = ListStreamingDistributionsResponse

    request  = get
    response = xmlResponse

instance FromXML ListStreamingDistributionsResponse where
    parseXML c = ListStreamingDistributionsResponse
        <$> c .:? "StreamingDistributionList"
