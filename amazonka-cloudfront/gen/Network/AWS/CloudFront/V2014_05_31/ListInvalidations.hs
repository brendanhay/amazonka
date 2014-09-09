{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.ListInvalidations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | List invalidation batches.
module Network.AWS.CloudFront.V2014_05_31.ListInvalidations
    (
    -- * Request
      ListInvalidations
    -- ** Request constructor
    , mkListInvalidations
    -- ** Request lenses
    , liDistributionId
    , liMarker
    , liMaxItems

    -- * Response
    , ListInvalidationsResponse
    -- ** Response constructor
    , mkListInvalidationsResponse
    -- ** Response lenses
    , lirInvalidationList
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to list invalidations.
data ListInvalidations = ListInvalidations
    { _liDistributionId :: Text
    , _liMarker :: Maybe Text
    , _liMaxItems :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListInvalidations' request.
mkListInvalidations :: Text -- ^ 'liDistributionId'
                    -> ListInvalidations
mkListInvalidations p1 = ListInvalidations
    { _liDistributionId = p1
    , _liMarker = Nothing
    , _liMaxItems = Nothing
    }

-- | The distribution's id.
liDistributionId :: Lens' ListInvalidations Text
liDistributionId =
    lens _liDistributionId (\s a -> s { _liDistributionId = a })

-- | Use this parameter when paginating results to indicate where to begin in
-- your list of invalidation batches. Because the results are returned in
-- decreasing order from most recent to oldest, the most recent results are on
-- the first page, the second page will contain earlier results, and so on. To
-- get the next page of results, set the Marker to the value of the NextMarker
-- from the current page's response. This value is the same as the ID of the
-- last invalidation batch on that page.
liMarker :: Lens' ListInvalidations (Maybe Text)
liMarker = lens _liMarker (\s a -> s { _liMarker = a })

-- | The maximum number of invalidation batches you want in the response body.
liMaxItems :: Lens' ListInvalidations (Maybe Text)
liMaxItems = lens _liMaxItems (\s a -> s { _liMaxItems = a })

instance ToPath ListInvalidations

instance ToQuery ListInvalidations

instance ToHeaders ListInvalidations

instance ToXML ListInvalidations where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ListInvalidationsRequest"

-- | The returned result of the corresponding request.
newtype ListInvalidationsResponse = ListInvalidationsResponse
    { _lirInvalidationList :: InvalidationList
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListInvalidationsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkListInvalidationsResponse :: InvalidationList -- ^ 'lirInvalidationList'
                            -> ListInvalidationsResponse
mkListInvalidationsResponse p1 = ListInvalidationsResponse
    { _lirInvalidationList = p1
    }

-- | Information about invalidation batches.
lirInvalidationList :: Lens' ListInvalidationsResponse InvalidationList
lirInvalidationList =
    lens _lirInvalidationList (\s a -> s { _lirInvalidationList = a })

instance FromXML ListInvalidationsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListInvalidations where
    type Sv ListInvalidations = CloudFront
    type Rs ListInvalidations = ListInvalidationsResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListInvalidations where
    next rq rs
        | not (rs ^. lirInvalidationList . ilIsTruncated) = Nothing
        | otherwise = Just $
            rq & liMarker .~ rs ^. lirInvalidationList . ilNextMarker
