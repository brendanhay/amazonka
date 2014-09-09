{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.ListDistributions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | List distributions.
module Network.AWS.CloudFront.V2014_05_31.ListDistributions
    (
    -- * Request
      ListDistributions
    -- ** Request constructor
    , mkListDistributions
    -- ** Request lenses
    , ldMarker
    , ldMaxItems

    -- * Response
    , ListDistributionsResponse
    -- ** Response constructor
    , mkListDistributionsResponse
    -- ** Response lenses
    , ldrDistributionList
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to list your distributions.
data ListDistributions = ListDistributions
    { _ldMarker :: Maybe Text
    , _ldMaxItems :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListDistributions' request.
mkListDistributions :: ListDistributions
mkListDistributions = ListDistributions
    { _ldMarker = Nothing
    , _ldMaxItems = Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list of
-- distributions. The results include distributions in the list that occur
-- after the marker. To get the next page of results, set the Marker to the
-- value of the NextMarker from the current page's response (which is also the
-- ID of the last distribution on that page).
ldMarker :: Lens' ListDistributions (Maybe Text)
ldMarker = lens _ldMarker (\s a -> s { _ldMarker = a })

-- | The maximum number of distributions you want in the response body.
ldMaxItems :: Lens' ListDistributions (Maybe Text)
ldMaxItems = lens _ldMaxItems (\s a -> s { _ldMaxItems = a })

instance ToPath ListDistributions

instance ToQuery ListDistributions

instance ToHeaders ListDistributions

instance ToXML ListDistributions where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ListDistributionsRequest"

-- | The returned result of the corresponding request.
newtype ListDistributionsResponse = ListDistributionsResponse
    { _ldrDistributionList :: DistributionList
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListDistributionsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkListDistributionsResponse :: DistributionList -- ^ 'ldrDistributionList'
                            -> ListDistributionsResponse
mkListDistributionsResponse p1 = ListDistributionsResponse
    { _ldrDistributionList = p1
    }

-- | The DistributionList type.
ldrDistributionList :: Lens' ListDistributionsResponse DistributionList
ldrDistributionList =
    lens _ldrDistributionList (\s a -> s { _ldrDistributionList = a })

instance FromXML ListDistributionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListDistributions where
    type Sv ListDistributions = CloudFront
    type Rs ListDistributions = ListDistributionsResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListDistributions where
    next rq rs
        | not (rs ^. ldrDistributionList . dlIsTruncated) = Nothing
        | otherwise = Just $
            rq & ldMarker .~ rs ^. ldrDistributionList . dlNextMarker
