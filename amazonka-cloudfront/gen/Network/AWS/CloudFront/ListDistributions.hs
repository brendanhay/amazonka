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

-- Module      : Network.AWS.CloudFront.ListDistributions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | List distributions.
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/ListDistributions.html>
module Network.AWS.CloudFront.ListDistributions
    (
    -- * Request
      ListDistributions
    -- ** Request constructor
    , listDistributions
    -- ** Request lenses
    , ldMarker
    , ldMaxItems

    -- * Response
    , ListDistributionsResponse
    -- ** Response constructor
    , listDistributionsResponse
    -- ** Response lenses
    , ldrDistributionList
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.Types
import qualified GHC.Exts

data ListDistributions = ListDistributions
    { _ldMarker   :: Maybe Text
    , _ldMaxItems :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'ListDistributions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldMarker' @::@ 'Maybe' 'Text'
--
-- * 'ldMaxItems' @::@ 'Maybe' 'Text'
--
listDistributions :: ListDistributions
listDistributions = ListDistributions
    { _ldMarker   = Nothing
    , _ldMaxItems = Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list of
-- distributions. The results include distributions in the list that occur after
-- the marker. To get the next page of results, set the Marker to the value of
-- the NextMarker from the current page's response (which is also the ID of the
-- last distribution on that page).
ldMarker :: Lens' ListDistributions (Maybe Text)
ldMarker = lens _ldMarker (\s a -> s { _ldMarker = a })

-- | The maximum number of distributions you want in the response body.
ldMaxItems :: Lens' ListDistributions (Maybe Text)
ldMaxItems = lens _ldMaxItems (\s a -> s { _ldMaxItems = a })

newtype ListDistributionsResponse = ListDistributionsResponse
    { _ldrDistributionList :: DistributionList
    } deriving (Eq, Show)

-- | 'ListDistributionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrDistributionList' @::@ 'DistributionList'
--
listDistributionsResponse :: DistributionList -- ^ 'ldrDistributionList'
                          -> ListDistributionsResponse
listDistributionsResponse p1 = ListDistributionsResponse
    { _ldrDistributionList = p1
    }

-- | The DistributionList type.
ldrDistributionList :: Lens' ListDistributionsResponse DistributionList
ldrDistributionList =
    lens _ldrDistributionList (\s a -> s { _ldrDistributionList = a })

instance ToPath ListDistributions where
    toPath = const "/2014-05-31/distribution"

instance ToQuery ListDistributions where
    toQuery ListDistributions{..} = mconcat
        [ "Marker"   =? _ldMarker
        , "MaxItems" =? _ldMaxItems
        ]

instance ToHeaders ListDistributions

instance ToXMLRoot ListDistributions where
    toXMLRoot = const (namespaced ns "ListDistributions" [])

instance ToXML ListDistributions

instance AWSRequest ListDistributions where
    type Sv ListDistributions = CloudFront
    type Rs ListDistributions = ListDistributionsResponse

    request  = get
    response = xmlResponse

instance FromXML ListDistributionsResponse where
    parseXML x = ListDistributionsResponse
        <$> x .@  "DistributionList"

instance AWSPager ListDistributions where
    page rq rs
        | stop (rs ^. ldrDistributionList . dlIsTruncated) = Nothing
        | otherwise = Just $ rq
            & ldMarker .~ rs ^. ldrDistributionList . dlNextMarker
