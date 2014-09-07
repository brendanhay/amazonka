{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.V2013_04_01.ListHealthChecks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | To retrieve a list of your health checks, send a GET request to the
-- 2013-04-01/healthcheck resource. The response to this request includes a
-- HealthChecks element with zero, one, or multiple HealthCheck child
-- elements. By default, the list of health checks is displayed on a single
-- page. You can control the length of the page that is displayed by using the
-- MaxItems parameter. You can use the Marker parameter to control the health
-- check that the list begins with. Amazon Route 53 returns a maximum of 100
-- items. If you set MaxItems to a value greater than 100, Amazon Route 53
-- returns only the first 100.
module Network.AWS.Route53.V2013_04_01.ListHealthChecks
    (
    -- * Request
      ListHealthChecks
    -- ** Request constructor
    , mkListHealthChecks
    -- ** Request lenses
    , lhcMarker
    , lhcMaxItems

    -- * Response
    , ListHealthChecksResponse
    -- ** Response lenses
    , lhcrsHealthChecks
    , lhcrsMarker
    , lhcrsIsTruncated
    , lhcrsNextMarker
    , lhcrsMaxItems
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | To retrieve a list of your health checks, send a GET request to the
-- 2013-04-01/healthcheck resource. The response to this request includes a
-- HealthChecks element with zero or more HealthCheck child elements. By
-- default, the list of health checks is displayed on a single page. You can
-- control the length of the page that is displayed by using the MaxItems
-- parameter. You can use the Marker parameter to control the health check
-- that the list begins with. Route 53 returns a maximum of 100 items. If you
-- set MaxItems to a value greater than 100, Route 53 returns only the first
-- 100.
data ListHealthChecks = ListHealthChecks
    { _lhcMarker :: Maybe Text
    , _lhcMaxItems :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListHealthChecks' request.
mkListHealthChecks :: ListHealthChecks
mkListHealthChecks = ListHealthChecks
    { _lhcMarker = Nothing
    , _lhcMaxItems = Nothing
    }

-- | If the request returned more than one page of results, submit another
-- request and specify the value of NextMarker from the last response in the
-- marker parameter to get the next page of results.
lhcMarker :: Lens' ListHealthChecks (Maybe Text)
lhcMarker = lens _lhcMarker (\s a -> s { _lhcMarker = a })

-- | Specify the maximum number of health checks to return per page of results.
lhcMaxItems :: Lens' ListHealthChecks (Maybe Text)
lhcMaxItems = lens _lhcMaxItems (\s a -> s { _lhcMaxItems = a })

instance ToPath ListHealthChecks where
    toPath = const "/2013-04-01/healthcheck"

instance ToQuery ListHealthChecks where
    toQuery ListHealthChecks{..} = mconcat
        [ "marker" =? _lhcMarker
        , "maxitems" =? _lhcMaxItems
        ]

instance ToHeaders ListHealthChecks

instance ToXML ListHealthChecks where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ListHealthChecksRequest"

-- | A complex type that contains the response for the request.
data ListHealthChecksResponse = ListHealthChecksResponse
    { _lhcrsHealthChecks :: [HealthCheck]
    , _lhcrsMarker :: Text
    , _lhcrsIsTruncated :: Bool
    , _lhcrsNextMarker :: Maybe Text
    , _lhcrsMaxItems :: Text
    } deriving (Show, Generic)

-- | A complex type that contains information about the health checks associated
-- with the current AWS account.
lhcrsHealthChecks :: Lens' ListHealthChecksResponse [HealthCheck]
lhcrsHealthChecks =
    lens _lhcrsHealthChecks (\s a -> s { _lhcrsHealthChecks = a })

-- | If the request returned more than one page of results, submit another
-- request and specify the value of NextMarker from the last response in the
-- marker parameter to get the next page of results.
lhcrsMarker :: Lens' ListHealthChecksResponse Text
lhcrsMarker = lens _lhcrsMarker (\s a -> s { _lhcrsMarker = a })

-- | A flag indicating whether there are more health checks to be listed. If
-- your results were truncated, you can make a follow-up request for the next
-- page of results by using the Marker element. Valid Values: true | false.
lhcrsIsTruncated :: Lens' ListHealthChecksResponse Bool
lhcrsIsTruncated =
    lens _lhcrsIsTruncated (\s a -> s { _lhcrsIsTruncated = a })

-- | Indicates where to continue listing health checks. If
-- ListHealthChecksResponse$IsTruncated is true, make another request to
-- ListHealthChecks and include the value of the NextMarker element in the
-- Marker element to get the next page of results.
lhcrsNextMarker :: Lens' ListHealthChecksResponse (Maybe Text)
lhcrsNextMarker = lens _lhcrsNextMarker (\s a -> s { _lhcrsNextMarker = a })

-- | The maximum number of health checks to be included in the response body. If
-- the number of health checks associated with this AWS account exceeds
-- MaxItems, the value of ListHealthChecksResponse$IsTruncated in the response
-- is true. Call ListHealthChecks again and specify the value of
-- ListHealthChecksResponse$NextMarker in the ListHostedZonesRequest$Marker
-- element to get the next page of results.
lhcrsMaxItems :: Lens' ListHealthChecksResponse Text
lhcrsMaxItems = lens _lhcrsMaxItems (\s a -> s { _lhcrsMaxItems = a })

instance FromXML ListHealthChecksResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListHealthChecks where
    type Sv ListHealthChecks = Route53
    type Rs ListHealthChecks = ListHealthChecksResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListHealthChecks where
    next rq rs
        | not (rs ^. lhcrsIsTruncated) = Nothing
        | otherwise = Just $
            rq & lhcMarker .~ rs ^. lhcrsNextMarker
