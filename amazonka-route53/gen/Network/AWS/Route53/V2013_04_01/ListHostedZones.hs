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
    , listHostedZones
    -- ** Request lenses
    , lhzrMarker
    , lhzrMaxItems

    -- * Response
    , ListHostedZonesResponse
    -- ** Response lenses
    , lhzsHostedZones
    , lhzsMarker
    , lhzsMaxItems
    , lhzsIsTruncated
    , lhzsNextMarker
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListHostedZones' request.
listHostedZones :: ListHostedZones
listHostedZones = ListHostedZones
    { _lhzrMarker = Nothing
    , _lhzrMaxItems = Nothing
    }
{-# INLINE listHostedZones #-}

data ListHostedZones = ListHostedZones
    { _lhzrMarker :: Maybe Text
      -- ^ If the request returned more than one page of results, submit
      -- another request and specify the value of NextMarker from the last
      -- response in the marker parameter to get the next page of results.
    , _lhzrMaxItems :: Maybe Text
      -- ^ Specify the maximum number of hosted zones to return per page of
      -- results.
    } deriving (Show, Generic)

-- | If the request returned more than one page of results, submit another
-- request and specify the value of NextMarker from the last response in the
-- marker parameter to get the next page of results.
lhzrMarker :: Lens' ListHostedZones (Maybe Text)
lhzrMarker f x =
    f (_lhzrMarker x)
        <&> \y -> x { _lhzrMarker = y }
{-# INLINE lhzrMarker #-}

-- | Specify the maximum number of hosted zones to return per page of results.
lhzrMaxItems :: Lens' ListHostedZones (Maybe Text)
lhzrMaxItems f x =
    f (_lhzrMaxItems x)
        <&> \y -> x { _lhzrMaxItems = y }
{-# INLINE lhzrMaxItems #-}

instance ToPath ListHostedZones where
    toPath = const "/2013-04-01/hostedzone"

instance ToQuery ListHostedZones where
    toQuery ListHostedZones{..} = mconcat
        [ "marker" =? _lhzrMarker
        , "maxitems" =? _lhzrMaxItems
        ]

instance ToHeaders ListHostedZones

instance ToXML ListHostedZones where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ListHostedZonesRequest"

data ListHostedZonesResponse = ListHostedZonesResponse
    { _lhzsHostedZones :: [HostedZone]
      -- ^ A complex type that contains information about the hosted zones
      -- associated with the current AWS account.
    , _lhzsMarker :: Text
      -- ^ If the request returned more than one page of results, submit
      -- another request and specify the value of NextMarker from the last
      -- response in the marker parameter to get the next page of results.
    , _lhzsMaxItems :: Text
      -- ^ The maximum number of hosted zones to be included in the response
      -- body. If the number of hosted zones associated with this AWS
      -- account exceeds MaxItems, the value of
      -- ListHostedZonesResponse$IsTruncated in the response is true. Call
      -- ListHostedZones again and specify the value of
      -- ListHostedZonesResponse$NextMarker in the
      -- ListHostedZonesRequest$Marker element to get the next page of
      -- results.
    , _lhzsIsTruncated :: Bool
      -- ^ A flag indicating whether there are more hosted zones to be
      -- listed. If your results were truncated, you can make a follow-up
      -- request for the next page of results by using the Marker element.
      -- Valid Values: true | false.
    , _lhzsNextMarker :: Maybe Text
      -- ^ Indicates where to continue listing hosted zones. If
      -- ListHostedZonesResponse$IsTruncated is true, make another request
      -- to ListHostedZones and include the value of the NextMarker
      -- element in the Marker element to get the next page of results.
    } deriving (Show, Generic)

-- | A complex type that contains information about the hosted zones associated
-- with the current AWS account.
lhzsHostedZones :: Lens' ListHostedZonesResponse ([HostedZone])
lhzsHostedZones f x =
    f (_lhzsHostedZones x)
        <&> \y -> x { _lhzsHostedZones = y }
{-# INLINE lhzsHostedZones #-}

-- | If the request returned more than one page of results, submit another
-- request and specify the value of NextMarker from the last response in the
-- marker parameter to get the next page of results.
lhzsMarker :: Lens' ListHostedZonesResponse (Text)
lhzsMarker f x =
    f (_lhzsMarker x)
        <&> \y -> x { _lhzsMarker = y }
{-# INLINE lhzsMarker #-}

-- | The maximum number of hosted zones to be included in the response body. If
-- the number of hosted zones associated with this AWS account exceeds
-- MaxItems, the value of ListHostedZonesResponse$IsTruncated in the response
-- is true. Call ListHostedZones again and specify the value of
-- ListHostedZonesResponse$NextMarker in the ListHostedZonesRequest$Marker
-- element to get the next page of results.
lhzsMaxItems :: Lens' ListHostedZonesResponse (Text)
lhzsMaxItems f x =
    f (_lhzsMaxItems x)
        <&> \y -> x { _lhzsMaxItems = y }
{-# INLINE lhzsMaxItems #-}

-- | A flag indicating whether there are more hosted zones to be listed. If your
-- results were truncated, you can make a follow-up request for the next page
-- of results by using the Marker element. Valid Values: true | false.
lhzsIsTruncated :: Lens' ListHostedZonesResponse (Bool)
lhzsIsTruncated f x =
    f (_lhzsIsTruncated x)
        <&> \y -> x { _lhzsIsTruncated = y }
{-# INLINE lhzsIsTruncated #-}

-- | Indicates where to continue listing hosted zones. If
-- ListHostedZonesResponse$IsTruncated is true, make another request to
-- ListHostedZones and include the value of the NextMarker element in the
-- Marker element to get the next page of results.
lhzsNextMarker :: Lens' ListHostedZonesResponse (Maybe Text)
lhzsNextMarker f x =
    f (_lhzsNextMarker x)
        <&> \y -> x { _lhzsNextMarker = y }
{-# INLINE lhzsNextMarker #-}

instance FromXML ListHostedZonesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListHostedZones where
    type Sv ListHostedZones = Route53
    type Rs ListHostedZones = ListHostedZonesResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListHostedZones where
    next rq rs
        | not (_lhzsIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _lhzrMarker = _lhzsNextMarker rs
            }
