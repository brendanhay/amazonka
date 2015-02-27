{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.ListHostedZonesByName
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | To retrieve a list of your hosted zones in lexicographic order, send a 'GET'
-- request to the '2013-04-01/hostedzonesbyname' resource. The response to this
-- request includes a 'HostedZones' element with zero or more 'HostedZone' child
-- elements lexicographically ordered by DNS name. By default, the list of
-- hosted zones is displayed on a single page. You can control the length of the
-- page that is displayed by using the 'MaxItems' parameter. You can use the 'DNSName' and 'HostedZoneId' parameters to control the hosted zone that the list begins
-- with.
--
-- Amazon Route 53 returns a maximum of 100 items. If you set MaxItems to a
-- value greater than 100, Amazon Route 53 returns only the first 100.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListHostedZonesByName.html>
module Network.AWS.Route53.ListHostedZonesByName
    (
    -- * Request
      ListHostedZonesByName
    -- ** Request constructor
    , listHostedZonesByName
    -- ** Request lenses
    , lhzbnDNSName
    , lhzbnHostedZoneId
    , lhzbnMaxItems

    -- * Response
    , ListHostedZonesByNameResponse
    -- ** Response constructor
    , listHostedZonesByNameResponse
    -- ** Response lenses
    , lhzbnrDNSName
    , lhzbnrHostedZoneId
    , lhzbnrHostedZones
    , lhzbnrIsTruncated
    , lhzbnrMaxItems
    , lhzbnrNextDNSName
    , lhzbnrNextHostedZoneId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import qualified GHC.Exts

data ListHostedZonesByName = ListHostedZonesByName
    { _lhzbnDNSName      :: Maybe Text
    , _lhzbnHostedZoneId :: Maybe Text
    , _lhzbnMaxItems     :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ListHostedZonesByName' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lhzbnDNSName' @::@ 'Maybe' 'Text'
--
-- * 'lhzbnHostedZoneId' @::@ 'Maybe' 'Text'
--
-- * 'lhzbnMaxItems' @::@ 'Maybe' 'Text'
--
listHostedZonesByName :: ListHostedZonesByName
listHostedZonesByName = ListHostedZonesByName
    { _lhzbnDNSName      = Nothing
    , _lhzbnHostedZoneId = Nothing
    , _lhzbnMaxItems     = Nothing
    }

-- | The first name in the lexicographic ordering of domain names that you want
-- the 'ListHostedZonesByNameRequest' request to list.
--
-- If the request returned more than one page of results, submit another
-- request and specify the value of 'NextDNSName' and 'NextHostedZoneId' from the
-- last response in the 'DNSName' and 'HostedZoneId' parameters to get the next page
-- of results.
lhzbnDNSName :: Lens' ListHostedZonesByName (Maybe Text)
lhzbnDNSName = lens _lhzbnDNSName (\s a -> s { _lhzbnDNSName = a })

-- | If the request returned more than one page of results, submit another request
-- and specify the value of 'NextDNSName' and 'NextHostedZoneId' from the last
-- response in the 'DNSName' and 'HostedZoneId' parameters to get the next page of
-- results.
lhzbnHostedZoneId :: Lens' ListHostedZonesByName (Maybe Text)
lhzbnHostedZoneId =
    lens _lhzbnHostedZoneId (\s a -> s { _lhzbnHostedZoneId = a })

-- | Specify the maximum number of hosted zones to return per page of results.
lhzbnMaxItems :: Lens' ListHostedZonesByName (Maybe Text)
lhzbnMaxItems = lens _lhzbnMaxItems (\s a -> s { _lhzbnMaxItems = a })

data ListHostedZonesByNameResponse = ListHostedZonesByNameResponse
    { _lhzbnrDNSName          :: Maybe Text
    , _lhzbnrHostedZoneId     :: Maybe Text
    , _lhzbnrHostedZones      :: List "HostedZone" HostedZone
    , _lhzbnrIsTruncated      :: Bool
    , _lhzbnrMaxItems         :: Text
    , _lhzbnrNextDNSName      :: Maybe Text
    , _lhzbnrNextHostedZoneId :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ListHostedZonesByNameResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lhzbnrDNSName' @::@ 'Maybe' 'Text'
--
-- * 'lhzbnrHostedZoneId' @::@ 'Maybe' 'Text'
--
-- * 'lhzbnrHostedZones' @::@ ['HostedZone']
--
-- * 'lhzbnrIsTruncated' @::@ 'Bool'
--
-- * 'lhzbnrMaxItems' @::@ 'Text'
--
-- * 'lhzbnrNextDNSName' @::@ 'Maybe' 'Text'
--
-- * 'lhzbnrNextHostedZoneId' @::@ 'Maybe' 'Text'
--
listHostedZonesByNameResponse :: Bool -- ^ 'lhzbnrIsTruncated'
                              -> Text -- ^ 'lhzbnrMaxItems'
                              -> ListHostedZonesByNameResponse
listHostedZonesByNameResponse p1 p2 = ListHostedZonesByNameResponse
    { _lhzbnrIsTruncated      = p1
    , _lhzbnrMaxItems         = p2
    , _lhzbnrHostedZones      = mempty
    , _lhzbnrDNSName          = Nothing
    , _lhzbnrHostedZoneId     = Nothing
    , _lhzbnrNextDNSName      = Nothing
    , _lhzbnrNextHostedZoneId = Nothing
    }

-- | The 'DNSName' value sent in the request.
lhzbnrDNSName :: Lens' ListHostedZonesByNameResponse (Maybe Text)
lhzbnrDNSName = lens _lhzbnrDNSName (\s a -> s { _lhzbnrDNSName = a })

-- | The 'HostedZoneId' value sent in the request.
lhzbnrHostedZoneId :: Lens' ListHostedZonesByNameResponse (Maybe Text)
lhzbnrHostedZoneId =
    lens _lhzbnrHostedZoneId (\s a -> s { _lhzbnrHostedZoneId = a })

-- | A complex type that contains information about the hosted zones associated
-- with the current AWS account.
lhzbnrHostedZones :: Lens' ListHostedZonesByNameResponse [HostedZone]
lhzbnrHostedZones =
    lens _lhzbnrHostedZones (\s a -> s { _lhzbnrHostedZones = a })
        . _List

-- | A flag indicating whether there are more hosted zones to be listed. If your
-- results were truncated, you can make a follow-up request for the next page of
-- results by using the 'NextDNSName' and 'NextHostedZoneId' elements.
--
-- Valid Values: 'true' | 'false'
lhzbnrIsTruncated :: Lens' ListHostedZonesByNameResponse Bool
lhzbnrIsTruncated =
    lens _lhzbnrIsTruncated (\s a -> s { _lhzbnrIsTruncated = a })

-- | The maximum number of hosted zones to be included in the response body. If
-- the number of hosted zones associated with this AWS account exceeds 'MaxItems',
-- the value of 'ListHostedZonesByNameResponse$IsTruncated' in the response is 'true'
-- . Call 'ListHostedZonesByName' again and specify the value of 'ListHostedZonesByNameResponse$NextDNSName' and 'ListHostedZonesByNameResponse$NextHostedZoneId' elements respectively to
-- get the next page of results.
lhzbnrMaxItems :: Lens' ListHostedZonesByNameResponse Text
lhzbnrMaxItems = lens _lhzbnrMaxItems (\s a -> s { _lhzbnrMaxItems = a })

-- | If 'ListHostedZonesByNameResponse$IsTruncated' is 'true', there are more hosted
-- zones associated with the current AWS account. To get the next page of
-- results, make another request to 'ListHostedZonesByName'. Specify the value of 'ListHostedZonesByNameResponse$NextDNSName' in the 'ListHostedZonesByNameRequest$DNSName' element and 'ListHostedZonesByNameResponse$NextHostedZoneId' in the 'ListHostedZonesByNameRequest$HostedZoneId' element.
lhzbnrNextDNSName :: Lens' ListHostedZonesByNameResponse (Maybe Text)
lhzbnrNextDNSName =
    lens _lhzbnrNextDNSName (\s a -> s { _lhzbnrNextDNSName = a })

-- | If 'ListHostedZonesByNameResponse$IsTruncated' is 'true', there are more hosted
-- zones associated with the current AWS account. To get the next page of
-- results, make another request to 'ListHostedZonesByName'. Specify the value of 'ListHostedZonesByNameResponse$NextDNSName' in the 'ListHostedZonesByNameRequest$DNSName' element and 'ListHostedZonesByNameResponse$NextHostedZoneId' in the 'ListHostedZonesByNameRequest$HostedZoneId' element.
lhzbnrNextHostedZoneId :: Lens' ListHostedZonesByNameResponse (Maybe Text)
lhzbnrNextHostedZoneId =
    lens _lhzbnrNextHostedZoneId (\s a -> s { _lhzbnrNextHostedZoneId = a })

instance ToPath ListHostedZonesByName where
    toPath = const "/2013-04-01/hostedzonesbyname"

instance ToQuery ListHostedZonesByName where
    toQuery ListHostedZonesByName{..} = mconcat
        [ "dnsname"      =? _lhzbnDNSName
        , "hostedzoneid" =? _lhzbnHostedZoneId
        , "maxitems"     =? _lhzbnMaxItems
        ]

instance ToHeaders ListHostedZonesByName

instance ToXMLRoot ListHostedZonesByName where
    toXMLRoot = const (namespaced ns "ListHostedZonesByName" [])

instance ToXML ListHostedZonesByName

instance AWSRequest ListHostedZonesByName where
    type Sv ListHostedZonesByName = Route53
    type Rs ListHostedZonesByName = ListHostedZonesByNameResponse

    request  = get
    response = xmlResponse

instance FromXML ListHostedZonesByNameResponse where
    parseXML x = ListHostedZonesByNameResponse
        <$> x .@? "DNSName"
        <*> x .@? "HostedZoneId"
        <*> x .@? "HostedZones" .!@ mempty
        <*> x .@  "IsTruncated"
        <*> x .@  "MaxItems"
        <*> x .@? "NextDNSName"
        <*> x .@? "NextHostedZoneId"
