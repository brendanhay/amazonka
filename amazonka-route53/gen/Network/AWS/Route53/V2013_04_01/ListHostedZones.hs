{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.V2013_04_01.ListHostedZones
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | To retrieve a list of your hosted zones, send a GET request to the
-- 2013-04-01/hostedzone resource. The response to this request includes a
-- HostedZones element with zero, one, or multiple HostedZone child elements.
-- By default, the list of hosted zones is displayed on a single page. You can
-- control the length of the page that is displayed by using the MaxItems
-- parameter. You can use the Marker parameter to control the hosted zone that
-- the list begins with. Amazon Route 53 returns a maximum of 100 items. If
-- you set MaxItems to a value greater than 100, Amazon Route 53 returns only
-- the first 100.
module Network.AWS.Route53.V2013_04_01.ListHostedZones
    (
    -- * Request
      ListHostedZones
    -- ** Request constructor
    , mkListHostedZones
    -- ** Request lenses
    , lhzMarker
    , lhzMaxItems

    -- * Response
    , ListHostedZonesResponse
    -- ** Response lenses
    , lhzrsHostedZones
    , lhzrsMarker
    , lhzrsIsTruncated
    , lhzrsNextMarker
    , lhzrsMaxItems
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | To retrieve a list of your hosted zones, send a GET request to the
-- 2013-04-01/hostedzone resource. The response to this request includes a
-- HostedZones element with zero or more HostedZone child elements. By
-- default, the list of hosted zones is displayed on a single page. You can
-- control the length of the page that is displayed by using the MaxItems
-- parameter. You can use the Marker parameter to control the hosted zone that
-- the list begins with. For more information about listing hosted zones, see
-- Listing the Hosted Zones for an AWS Account in the Amazon Route 53
-- Developer Guide. Route 53 returns a maximum of 100 items. If you set
-- MaxItems to a value greater than 100, Route 53 returns only the first 100.
data ListHostedZones = ListHostedZones
    { _lhzMarker :: Maybe Text
    , _lhzMaxItems :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListHostedZones' request.
mkListHostedZones :: ListHostedZones
mkListHostedZones = ListHostedZones
    { _lhzMarker = Nothing
    , _lhzMaxItems = Nothing
    }

-- | If the request returned more than one page of results, submit another
-- request and specify the value of NextMarker from the last response in the
-- marker parameter to get the next page of results.
lhzMarker :: Lens' ListHostedZones (Maybe Text)
lhzMarker = lens _lhzMarker (\s a -> s { _lhzMarker = a })

-- | Specify the maximum number of hosted zones to return per page of results.
lhzMaxItems :: Lens' ListHostedZones (Maybe Text)
lhzMaxItems = lens _lhzMaxItems (\s a -> s { _lhzMaxItems = a })

instance ToPath ListHostedZones where
    toPath = const "/2013-04-01/hostedzone"

instance ToQuery ListHostedZones where
    toQuery ListHostedZones{..} = mconcat
        [ "marker" =? _lhzMarker
        , "maxitems" =? _lhzMaxItems
        ]

instance ToHeaders ListHostedZones

instance ToXML ListHostedZones where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ListHostedZonesRequest"

-- | A complex type that contains the response for the request.
data ListHostedZonesResponse = ListHostedZonesResponse
    { _lhzrsHostedZones :: [HostedZone]
    , _lhzrsMarker :: Text
    , _lhzrsIsTruncated :: Bool
    , _lhzrsNextMarker :: Maybe Text
    , _lhzrsMaxItems :: Text
    } deriving (Show, Generic)

-- | A complex type that contains information about the hosted zones associated
-- with the current AWS account.
lhzrsHostedZones :: Lens' ListHostedZonesResponse [HostedZone]
lhzrsHostedZones =
    lens _lhzrsHostedZones (\s a -> s { _lhzrsHostedZones = a })

-- | If the request returned more than one page of results, submit another
-- request and specify the value of NextMarker from the last response in the
-- marker parameter to get the next page of results.
lhzrsMarker :: Lens' ListHostedZonesResponse Text
lhzrsMarker = lens _lhzrsMarker (\s a -> s { _lhzrsMarker = a })

-- | A flag indicating whether there are more hosted zones to be listed. If your
-- results were truncated, you can make a follow-up request for the next page
-- of results by using the Marker element. Valid Values: true | false.
lhzrsIsTruncated :: Lens' ListHostedZonesResponse Bool
lhzrsIsTruncated =
    lens _lhzrsIsTruncated (\s a -> s { _lhzrsIsTruncated = a })

-- | Indicates where to continue listing hosted zones. If
-- ListHostedZonesResponse$IsTruncated is true, make another request to
-- ListHostedZones and include the value of the NextMarker element in the
-- Marker element to get the next page of results.
lhzrsNextMarker :: Lens' ListHostedZonesResponse (Maybe Text)
lhzrsNextMarker = lens _lhzrsNextMarker (\s a -> s { _lhzrsNextMarker = a })

-- | The maximum number of hosted zones to be included in the response body. If
-- the number of hosted zones associated with this AWS account exceeds
-- MaxItems, the value of ListHostedZonesResponse$IsTruncated in the response
-- is true. Call ListHostedZones again and specify the value of
-- ListHostedZonesResponse$NextMarker in the ListHostedZonesRequest$Marker
-- element to get the next page of results.
lhzrsMaxItems :: Lens' ListHostedZonesResponse Text
lhzrsMaxItems = lens _lhzrsMaxItems (\s a -> s { _lhzrsMaxItems = a })

instance FromXML ListHostedZonesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListHostedZones where
    type Sv ListHostedZones = Route53
    type Rs ListHostedZones = ListHostedZonesResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListHostedZones where
    next rq rs
        | not (rs ^. lhzrsIsTruncated) = Nothing
        | otherwise = Just $
            rq & lhzMarker .~ rs ^. lhzrsNextMarker
