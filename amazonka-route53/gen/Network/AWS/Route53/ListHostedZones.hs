{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.Route53.ListHostedZones
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
module Network.AWS.Route53.ListHostedZones
    (
    -- * Request
      ListHostedZones
    -- ** Request constructor
    , listHostedZones
    -- ** Request lenses
    , lhzDelegationSetId
    , lhzMarker
    , lhzMaxItems

    -- * Response
    , ListHostedZonesResponse
    -- ** Response constructor
    , listHostedZonesResponse
    -- ** Response lenses
    , lhzrHostedZones
    , lhzrIsTruncated
    , lhzrMarker
    , lhzrMaxItems
    , lhzrNextMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Route53.Types

data ListHostedZones = ListHostedZones
    { _lhzDelegationSetId :: Maybe Text
    , _lhzMarker          :: Maybe Text
    , _lhzMaxItems        :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListHostedZones' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lhzDelegationSetId' @::@ 'Maybe' 'Text'
--
-- * 'lhzMarker' @::@ 'Maybe' 'Text'
--
-- * 'lhzMaxItems' @::@ 'Maybe' 'Text'
--
listHostedZones :: ListHostedZones
listHostedZones = ListHostedZones
    { _lhzMarker          = Nothing
    , _lhzMaxItems        = Nothing
    , _lhzDelegationSetId = Nothing
    }

lhzDelegationSetId :: Lens' ListHostedZones (Maybe Text)
lhzDelegationSetId =
    lens _lhzDelegationSetId (\s a -> s { _lhzDelegationSetId = a })

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
        [ "marker"          =? _lhzMarker
        , "maxitems"        =? _lhzMaxItems
        , "delegationsetid" =? _lhzDelegationSetId
        ]

instance ToHeaders ListHostedZones

data ListHostedZonesResponse = ListHostedZonesResponse
    { _lhzrHostedZones :: [HostedZone]
    , _lhzrIsTruncated :: Bool
    , _lhzrMarker      :: Text
    , _lhzrMaxItems    :: Text
    , _lhzrNextMarker  :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ListHostedZonesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lhzrHostedZones' @::@ ['HostedZone']
--
-- * 'lhzrIsTruncated' @::@ 'Bool'
--
-- * 'lhzrMarker' @::@ 'Text'
--
-- * 'lhzrMaxItems' @::@ 'Text'
--
-- * 'lhzrNextMarker' @::@ 'Maybe' 'Text'
--
listHostedZonesResponse :: Text -- ^ 'lhzrMarker'
                        -> Bool -- ^ 'lhzrIsTruncated'
                        -> Text -- ^ 'lhzrMaxItems'
                        -> ListHostedZonesResponse
listHostedZonesResponse p1 p2 p3 = ListHostedZonesResponse
    { _lhzrMarker      = p1
    , _lhzrIsTruncated = p2
    , _lhzrMaxItems    = p3
    , _lhzrHostedZones = mempty
    , _lhzrNextMarker  = Nothing
    }

-- | A complex type that contains information about the hosted zones
-- associated with the current AWS account.
lhzrHostedZones :: Lens' ListHostedZonesResponse [HostedZone]
lhzrHostedZones = lens _lhzrHostedZones (\s a -> s { _lhzrHostedZones = a })

-- | A flag indicating whether there are more hosted zones to be listed. If
-- your results were truncated, you can make a follow-up request for the
-- next page of results by using the Marker element. Valid Values: true |
-- false.
lhzrIsTruncated :: Lens' ListHostedZonesResponse Bool
lhzrIsTruncated = lens _lhzrIsTruncated (\s a -> s { _lhzrIsTruncated = a })

-- | If the request returned more than one page of results, submit another
-- request and specify the value of NextMarker from the last response in the
-- marker parameter to get the next page of results.
lhzrMarker :: Lens' ListHostedZonesResponse Text
lhzrMarker = lens _lhzrMarker (\s a -> s { _lhzrMarker = a })

-- | The maximum number of hosted zones to be included in the response body.
-- If the number of hosted zones associated with this AWS account exceeds
-- MaxItems, the value of ListHostedZonesResponse$IsTruncated in the
-- response is true. Call ListHostedZones again and specify the value of
-- ListHostedZonesResponse$NextMarker in the ListHostedZonesRequest$Marker
-- element to get the next page of results.
lhzrMaxItems :: Lens' ListHostedZonesResponse Text
lhzrMaxItems = lens _lhzrMaxItems (\s a -> s { _lhzrMaxItems = a })

-- | Indicates where to continue listing hosted zones. If
-- ListHostedZonesResponse$IsTruncated is true, make another request to
-- ListHostedZones and include the value of the NextMarker element in the
-- Marker element to get the next page of results.
lhzrNextMarker :: Lens' ListHostedZonesResponse (Maybe Text)
lhzrNextMarker = lens _lhzrNextMarker (\s a -> s { _lhzrNextMarker = a })

instance AWSRequest ListHostedZones where
    type Sv ListHostedZones = Route53
    type Rs ListHostedZones = ListHostedZonesResponse

    request  = get
    response = xmlResponse $ \h x -> ListHostedZonesResponse
        <$> x %| "HostedZones"
        <*> x %| "IsTruncated"
        <*> x %| "Marker"
        <*> x %| "MaxItems"
        <*> x %| "NextMarker"
