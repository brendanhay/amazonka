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
    -- ** Response lenses
    , ldrsDistributionList
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
{-# INLINE mkListDistributions #-}

-- | Use this when paginating results to indicate where to begin in your list of
-- distributions. The results include distributions in the list that occur
-- after the marker. To get the next page of results, set the Marker to the
-- value of the NextMarker from the current page's response (which is also the
-- ID of the last distribution on that page).
ldMarker :: Lens' ListDistributions (Maybe Text)
ldMarker = lens _ldMarker (\s a -> s { _ldMarker = a })
{-# INLINE ldMarker #-}

-- | The maximum number of distributions you want in the response body.
ldMaxItems :: Lens' ListDistributions (Maybe Text)
ldMaxItems = lens _ldMaxItems (\s a -> s { _ldMaxItems = a })
{-# INLINE ldMaxItems #-}

instance ToPath ListDistributions where
    toPath = const "/2014-05-31/distribution"

instance ToQuery ListDistributions where
    toQuery ListDistributions{..} = mconcat
        [ "Marker" =? _ldMarker
        , "MaxItems" =? _ldMaxItems
        ]

instance ToHeaders ListDistributions

instance ToXML ListDistributions where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ListDistributionsRequest"

-- | The returned result of the corresponding request.
newtype ListDistributionsResponse = ListDistributionsResponse
    { _ldrsDistributionList :: Maybe DistributionList
    } deriving (Show, Generic)

-- | The DistributionList type.
ldrsDistributionList :: Lens' ListDistributionsResponse (Maybe DistributionList)
ldrsDistributionList =
    lens _ldrsDistributionList (\s a -> s { _ldrsDistributionList = a })
{-# INLINE ldrsDistributionList #-}

instance FromXML ListDistributionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListDistributions where
    type Sv ListDistributions = CloudFront
    type Rs ListDistributions = ListDistributionsResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListDistributions where
    next rq rs
        | not (_dlIsTruncated $ _ldrsDistributionList rs) = Nothing
        | otherwise = Just $ rq
            { _ldMarker = _dlNextMarker $ _ldrsDistributionList rs
            }
