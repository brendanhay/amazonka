{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.CloudFront.V2014_05_31.ListCloudFrontOriginAccessIdentities where

import Network.AWS.Request.RestXML
import Network.AWS.CloudFront.V2014_05_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListCloudFrontOriginAccessIdentities' request.
listCloudFrontOriginAccessIdentities :: ListCloudFrontOriginAccessIdentities
listCloudFrontOriginAccessIdentities = ListCloudFrontOriginAccessIdentities
    { _lcfoairMarker = Nothing
    , _lcfoairMaxItems = Nothing
    }

data ListCloudFrontOriginAccessIdentities = ListCloudFrontOriginAccessIdentities
    { _lcfoairMarker :: Maybe Text
      -- ^ Use this when paginating results to indicate where to begin in
      -- your list of origin access identities. The results include
      -- identities in the list that occur after the marker. To get the
      -- next page of results, set the Marker to the value of the
      -- NextMarker from the current page's response (which is also the ID
      -- of the last identity on that page).
    , _lcfoairMaxItems :: Maybe Text
      -- ^ The maximum number of origin access identities you want in the
      -- response body.
    } deriving (Show, Generic)

makeLenses ''ListCloudFrontOriginAccessIdentities

instance ToPath ListCloudFrontOriginAccessIdentities where
    toPath = const "/2014-05-31/origin-access-identity/cloudfront"

instance ToQuery ListCloudFrontOriginAccessIdentities where
    toQuery ListCloudFrontOriginAccessIdentities{..} = mconcat
        [ "Marker" =? _lcfoairMarker
        , "MaxItems" =? _lcfoairMaxItems
        ]

instance ToHeaders ListCloudFrontOriginAccessIdentities

instance ToXML ListCloudFrontOriginAccessIdentities where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ListCloudFrontOriginAccessIdentitiesRequest"

data ListCloudFrontOriginAccessIdentitiesResponse = ListCloudFrontOriginAccessIdentitiesResponse
    { _lcfoaisCloudFrontOriginAccessIdentityList :: CloudFrontOriginAccessIdentityList
      -- ^ The CloudFrontOriginAccessIdentityList type.
    } deriving (Show, Generic)

makeLenses ''ListCloudFrontOriginAccessIdentitiesResponse

instance FromXML ListCloudFrontOriginAccessIdentitiesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListCloudFrontOriginAccessIdentities where
    type Sv ListCloudFrontOriginAccessIdentities = CloudFront
    type Rs ListCloudFrontOriginAccessIdentities = ListCloudFrontOriginAccessIdentitiesResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListCloudFrontOriginAccessIdentities where
    next rq rs
        | not (_cfoailIsTruncated $ _lcfoaisCloudFrontOriginAccessIdentityList rs) = Nothing
        | otherwise = Just $ rq
            { _lcfoairMarker = _cfoailNextMarker $ _lcfoaisCloudFrontOriginAccessIdentityList rs
            }
