{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.V2013_04_01.ListResourceRecordSets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Imagine all the resource record sets in a zone listed out in front of you.
-- Imagine them sorted lexicographically first by DNS name (with the labels
-- reversed, like "com.amazon.www" for example), and secondarily,
-- lexicographically by record type. This operation retrieves at most MaxItems
-- resource record sets from this list, in order, starting at a position
-- specified by the Name and Type arguments: If both Name and Type are
-- omitted, this means start the results at the first RRSET in the HostedZone.
-- If Name is specified but Type is omitted, this means start the results at
-- the first RRSET in the list whose name is greater than or equal to Name. If
-- both Name and Type are specified, this means start the results at the first
-- RRSET in the list whose name is greater than or equal to Name and whose
-- type is greater than or equal to Type. It is an error to specify the Type
-- but not the Name. Use ListResourceRecordSets to retrieve a single known
-- record set by specifying the record set's name and type, and setting
-- MaxItems = 1 To retrieve all the records in a HostedZone, first pause any
-- processes making calls to ChangeResourceRecordSets. Initially call
-- ListResourceRecordSets without a Name and Type to get the first page of
-- record sets. For subsequent calls, set Name and Type to the NextName and
-- NextType values returned by the previous response. In the presence of
-- concurrent ChangeResourceRecordSets calls, there is no consistency of
-- results across calls to ListResourceRecordSets. The only way to get a
-- consistent multi-page snapshot of all RRSETs in a zone is to stop making
-- changes while pagination is in progress. However, the results from
-- ListResourceRecordSets are consistent within a page. If MakeChange calls
-- are taking place concurrently, the result of each one will either be
-- completely visible in your results or not at all. You will not see partial
-- changes, or changes that do not ultimately succeed. (This follows from the
-- fact that MakeChange is atomic) The results from ListResourceRecordSets are
-- strongly consistent with ChangeResourceRecordSets. To be precise, if a
-- single process makes a call to ChangeResourceRecordSets and receives a
-- successful response, the effects of that change will be visible in a
-- subsequent call to ListResourceRecordSets by that process.
module Network.AWS.Route53.V2013_04_01.ListResourceRecordSets
    (
    -- * Request
      ListResourceRecordSets
    -- ** Request constructor
    , mkListResourceRecordSets
    -- ** Request lenses
    , lrrsHostedZoneId
    , lrrsStartRecordName
    , lrrsStartRecordType
    , lrrsStartRecordIdentifier
    , lrrsMaxItems

    -- * Response
    , ListResourceRecordSetsResponse
    -- ** Response lenses
    , lrrsrsResourceRecordSets
    , lrrsrsIsTruncated
    , lrrsrsNextRecordName
    , lrrsrsNextRecordType
    , lrrsrsNextRecordIdentifier
    , lrrsrsMaxItems
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The input for a ListResourceRecordSets request.
data ListResourceRecordSets = ListResourceRecordSets
    { _lrrsHostedZoneId :: Text
    , _lrrsStartRecordName :: Maybe Text
    , _lrrsStartRecordType :: Maybe RecordType
    , _lrrsStartRecordIdentifier :: Maybe Text
    , _lrrsMaxItems :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListResourceRecordSets' request.
mkListResourceRecordSets :: Text -- ^ 'lrrsHostedZoneId'
                         -> ListResourceRecordSets
mkListResourceRecordSets p1 = ListResourceRecordSets
    { _lrrsHostedZoneId = p1
    , _lrrsStartRecordName = Nothing
    , _lrrsStartRecordType = Nothing
    , _lrrsStartRecordIdentifier = Nothing
    , _lrrsMaxItems = Nothing
    }

-- | The ID of the hosted zone that contains the resource record sets that you
-- want to get.
lrrsHostedZoneId :: Lens' ListResourceRecordSets Text
lrrsHostedZoneId =
    lens _lrrsHostedZoneId (\s a -> s { _lrrsHostedZoneId = a })

-- | The first name in the lexicographic ordering of domain names that you want
-- the ListResourceRecordSets request to list.
lrrsStartRecordName :: Lens' ListResourceRecordSets (Maybe Text)
lrrsStartRecordName =
    lens _lrrsStartRecordName (\s a -> s { _lrrsStartRecordName = a })

-- | The DNS type at which to begin the listing of resource record sets. Valid
-- values: A | AAAA | CNAME | MX | NS | PTR | SOA | SPF | SRV | TXT Values for
-- Weighted Resource Record Sets: A | AAAA | CNAME | TXT Values for Regional
-- Resource Record Sets: A | AAAA | CNAME | TXT Values for Alias Resource
-- Record Sets: A | AAAA Constraint: Specifying type without specifying name
-- returns an InvalidInput error.
lrrsStartRecordType :: Lens' ListResourceRecordSets (Maybe RecordType)
lrrsStartRecordType =
    lens _lrrsStartRecordType (\s a -> s { _lrrsStartRecordType = a })

-- | Weighted resource record sets only: If results were truncated for a given
-- DNS name and type, specify the value of
-- ListResourceRecordSetsResponse$NextRecordIdentifier from the previous
-- response to get the next resource record set that has the current DNS name
-- and type.
lrrsStartRecordIdentifier :: Lens' ListResourceRecordSets (Maybe Text)
lrrsStartRecordIdentifier =
    lens _lrrsStartRecordIdentifier
         (\s a -> s { _lrrsStartRecordIdentifier = a })

-- | The maximum number of records you want in the response body.
lrrsMaxItems :: Lens' ListResourceRecordSets (Maybe Text)
lrrsMaxItems = lens _lrrsMaxItems (\s a -> s { _lrrsMaxItems = a })

instance ToPath ListResourceRecordSets where
    toPath ListResourceRecordSets{..} = mconcat
        [ "/2013-04-01/hostedzone/"
        , toBS _lrrsHostedZoneId
        , "/rrset"
        ]

instance ToQuery ListResourceRecordSets where
    toQuery ListResourceRecordSets{..} = mconcat
        [ "identifier" =? _lrrsStartRecordIdentifier
        , "maxitems" =? _lrrsMaxItems
        , "name" =? _lrrsStartRecordName
        , "type" =? _lrrsStartRecordType
        ]

instance ToHeaders ListResourceRecordSets

instance ToXML ListResourceRecordSets where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ListResourceRecordSetsRequest"

-- | A complex type that contains information about the resource record sets
-- that are returned by the request and information about the response.
data ListResourceRecordSetsResponse = ListResourceRecordSetsResponse
    { _lrrsrsResourceRecordSets :: [ResourceRecordSet]
    , _lrrsrsIsTruncated :: Bool
    , _lrrsrsNextRecordName :: Maybe Text
    , _lrrsrsNextRecordType :: Maybe RecordType
    , _lrrsrsNextRecordIdentifier :: Maybe Text
    , _lrrsrsMaxItems :: Text
    } deriving (Show, Generic)

-- | A complex type that contains information about the resource record sets
-- that are returned by the request.
lrrsrsResourceRecordSets :: Lens' ListResourceRecordSetsResponse [ResourceRecordSet]
lrrsrsResourceRecordSets =
    lens _lrrsrsResourceRecordSets
         (\s a -> s { _lrrsrsResourceRecordSets = a })

-- | A flag that indicates whether there are more resource record sets to be
-- listed. If your results were truncated, you can make a follow-up request
-- for the next page of results by using the
-- ListResourceRecordSetsResponse$NextRecordName element. Valid Values: true |
-- false.
lrrsrsIsTruncated :: Lens' ListResourceRecordSetsResponse Bool
lrrsrsIsTruncated =
    lens _lrrsrsIsTruncated (\s a -> s { _lrrsrsIsTruncated = a })

-- | If the results were truncated, the name of the next record in the list.
-- This element is present only if ListResourceRecordSetsResponse$IsTruncated
-- is true.
lrrsrsNextRecordName :: Lens' ListResourceRecordSetsResponse (Maybe Text)
lrrsrsNextRecordName =
    lens _lrrsrsNextRecordName (\s a -> s { _lrrsrsNextRecordName = a })

-- | If the results were truncated, the type of the next record in the list.
-- This element is present only if ListResourceRecordSetsResponse$IsTruncated
-- is true.
lrrsrsNextRecordType :: Lens' ListResourceRecordSetsResponse (Maybe RecordType)
lrrsrsNextRecordType =
    lens _lrrsrsNextRecordType (\s a -> s { _lrrsrsNextRecordType = a })

-- | Weighted resource record sets only: If results were truncated for a given
-- DNS name and type, the value of SetIdentifier for the next resource record
-- set that has the current DNS name and type.
lrrsrsNextRecordIdentifier :: Lens' ListResourceRecordSetsResponse (Maybe Text)
lrrsrsNextRecordIdentifier =
    lens _lrrsrsNextRecordIdentifier
         (\s a -> s { _lrrsrsNextRecordIdentifier = a })

-- | The maximum number of records you requested. The maximum value of MaxItems
-- is 100.
lrrsrsMaxItems :: Lens' ListResourceRecordSetsResponse Text
lrrsrsMaxItems = lens _lrrsrsMaxItems (\s a -> s { _lrrsrsMaxItems = a })

instance FromXML ListResourceRecordSetsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListResourceRecordSets where
    type Sv ListResourceRecordSets = Route53
    type Rs ListResourceRecordSets = ListResourceRecordSetsResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListResourceRecordSets where
    next rq rs
        | not (rs ^. lrrsrsIsTruncated) = Nothing
        | isNothing p1 && isNothing p2 && isNothing p3 = Nothing
        | otherwise = Just $ rq
            & lrrsStartRecordName .~ p1
            & lrrsStartRecordType .~ p2
            & lrrsStartRecordIdentifier .~ p3
      where
        p1 = lrrsrsNextRecordName rs
        p2 = lrrsrsNextRecordType rs
        p3 = lrrsrsNextRecordIdentifier rs
