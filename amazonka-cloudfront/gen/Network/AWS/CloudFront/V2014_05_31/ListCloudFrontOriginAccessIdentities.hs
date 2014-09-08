{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFront.V2014_05_31.ListCloudFrontOriginAccessIdentities
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | List origin access identities.
module Network.AWS.CloudFront.V2014_05_31.ListCloudFrontOriginAccessIdentities
    (
    -- * Request
      ListCloudFrontOriginAccessIdentities
    -- ** Request constructor
    , mkListCloudFrontOriginAccessIdentities
    -- ** Request lenses
    , lcfoaiMarker
    , lcfoaiMaxItems

    -- * Response
    , ListCloudFrontOriginAccessIdentitiesResponse
    -- ** Response lenses
    , lcfoairCloudFrontOriginAccessIdentityList
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The request to list origin access identities.
data ListCloudFrontOriginAccessIdentities = ListCloudFrontOriginAccessIdentities
    { _lcfoaiMarker :: Maybe Text
    , _lcfoaiMaxItems :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListCloudFrontOriginAccessIdentities' request.
mkListCloudFrontOriginAccessIdentities :: ListCloudFrontOriginAccessIdentities
mkListCloudFrontOriginAccessIdentities = ListCloudFrontOriginAccessIdentities
    { _lcfoaiMarker = Nothing
    , _lcfoaiMaxItems = Nothing
    }

-- | Use this when paginating results to indicate where to begin in your list of
-- origin access identities. The results include identities in the list that
-- occur after the marker. To get the next page of results, set the Marker to
-- the value of the NextMarker from the current page's response (which is also
-- the ID of the last identity on that page).
lcfoaiMarker :: Lens' ListCloudFrontOriginAccessIdentities (Maybe Text)
lcfoaiMarker = lens _lcfoaiMarker (\s a -> s { _lcfoaiMarker = a })

-- | The maximum number of origin access identities you want in the response
-- body.
lcfoaiMaxItems :: Lens' ListCloudFrontOriginAccessIdentities (Maybe Text)
lcfoaiMaxItems = lens _lcfoaiMaxItems (\s a -> s { _lcfoaiMaxItems = a })

instance ToPath ListCloudFrontOriginAccessIdentities where
    toPath = const "/2014-05-31/origin-access-identity/cloudfront"

instance ToQuery ListCloudFrontOriginAccessIdentities where
    toQuery ListCloudFrontOriginAccessIdentities{..} = mconcat
        [ "Marker" =? _lcfoaiMarker
        , "MaxItems" =? _lcfoaiMaxItems
        ]

instance ToHeaders ListCloudFrontOriginAccessIdentities

instance ToXML ListCloudFrontOriginAccessIdentities where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ListCloudFrontOriginAccessIdentitiesRequest"

-- | The returned result of the corresponding request.
newtype ListCloudFrontOriginAccessIdentitiesResponse = ListCloudFrontOriginAccessIdentitiesResponse
    { _lcfoairCloudFrontOriginAccessIdentityList :: CloudFrontOriginAccessIdentityList
    } deriving (Show, Generic)

-- | The CloudFrontOriginAccessIdentityList type.
lcfoairCloudFrontOriginAccessIdentityList :: Lens' ListCloudFrontOriginAccessIdentitiesResponse CloudFrontOriginAccessIdentityList
lcfoairCloudFrontOriginAccessIdentityList =
    lens _lcfoairCloudFrontOriginAccessIdentityList
         (\s a -> s { _lcfoairCloudFrontOriginAccessIdentityList = a })

instance FromXML ListCloudFrontOriginAccessIdentitiesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListCloudFrontOriginAccessIdentities where
    type Sv ListCloudFrontOriginAccessIdentities = CloudFront
    type Rs ListCloudFrontOriginAccessIdentities = ListCloudFrontOriginAccessIdentitiesResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListCloudFrontOriginAccessIdentities where
    next rq rs
        | not (rs ^. lcfoairCloudFrontOriginAccessIdentityList . cfoailIsTruncated) = Nothing
        | otherwise = Just $
            rq & lcfoaiMarker .~ rs ^. lcfoairCloudFrontOriginAccessIdentityList . cfoailNextMarker
