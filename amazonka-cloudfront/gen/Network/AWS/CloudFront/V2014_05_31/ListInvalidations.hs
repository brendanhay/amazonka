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
    , mkListInvalidationsRequest
    -- ** Request lenses
    , lirDistributionId
    , lirMarker
    , lirMaxItems

    -- * Response
    , ListInvalidationsResponse
    -- ** Response lenses
    , lisInvalidationList
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListInvalidations' request.
mkListInvalidationsRequest :: Text -- ^ 'lirDistributionId'
                           -> ListInvalidations
mkListInvalidationsRequest p1 = ListInvalidations
    { _lirDistributionId = p1
    , _lirMarker = Nothing
    , _lirMaxItems = Nothing
    }
{-# INLINE mkListInvalidationsRequest #-}

data ListInvalidations = ListInvalidations
    { _lirDistributionId :: Text
      -- ^ The distribution's id.
    , _lirMarker :: Maybe Text
      -- ^ Use this parameter when paginating results to indicate where to
      -- begin in your list of invalidation batches. Because the results
      -- are returned in decreasing order from most recent to oldest, the
      -- most recent results are on the first page, the second page will
      -- contain earlier results, and so on. To get the next page of
      -- results, set the Marker to the value of the NextMarker from the
      -- current page's response. This value is the same as the ID of the
      -- last invalidation batch on that page.
    , _lirMaxItems :: Maybe Text
      -- ^ The maximum number of invalidation batches you want in the
      -- response body.
    } deriving (Show, Generic)

-- | The distribution's id.
lirDistributionId :: Lens' ListInvalidations (Text)
lirDistributionId = lens _lirDistributionId (\s a -> s { _lirDistributionId = a })
{-# INLINE lirDistributionId #-}

-- | Use this parameter when paginating results to indicate where to begin in
-- your list of invalidation batches. Because the results are returned in
-- decreasing order from most recent to oldest, the most recent results are on
-- the first page, the second page will contain earlier results, and so on. To
-- get the next page of results, set the Marker to the value of the NextMarker
-- from the current page's response. This value is the same as the ID of the
-- last invalidation batch on that page.
lirMarker :: Lens' ListInvalidations (Maybe Text)
lirMarker = lens _lirMarker (\s a -> s { _lirMarker = a })
{-# INLINE lirMarker #-}

-- | The maximum number of invalidation batches you want in the response body.
lirMaxItems :: Lens' ListInvalidations (Maybe Text)
lirMaxItems = lens _lirMaxItems (\s a -> s { _lirMaxItems = a })
{-# INLINE lirMaxItems #-}

instance ToPath ListInvalidations where
    toPath ListInvalidations{..} = mconcat
        [ "/2014-05-31/distribution/"
        , toBS _lirDistributionId
        , "/invalidation"
        ]

instance ToQuery ListInvalidations where
    toQuery ListInvalidations{..} = mconcat
        [ "Marker" =? _lirMarker
        , "MaxItems" =? _lirMaxItems
        ]

instance ToHeaders ListInvalidations

instance ToXML ListInvalidations where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ListInvalidationsRequest"

newtype ListInvalidationsResponse = ListInvalidationsResponse
    { _lisInvalidationList :: Maybe InvalidationList
      -- ^ Information about invalidation batches.
    } deriving (Show, Generic)

-- | Information about invalidation batches.
lisInvalidationList :: Lens' ListInvalidationsResponse (Maybe InvalidationList)
lisInvalidationList = lens _lisInvalidationList (\s a -> s { _lisInvalidationList = a })
{-# INLINE lisInvalidationList #-}

instance FromXML ListInvalidationsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListInvalidations where
    type Sv ListInvalidations = CloudFront
    type Rs ListInvalidations = ListInvalidationsResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListInvalidations where
    next rq rs
        | not (_ilIsTruncated $ _lisInvalidationList rs) = Nothing
        | otherwise = Just $ rq
            { _lirMarker = _ilNextMarker $ _lisInvalidationList rs
            }
