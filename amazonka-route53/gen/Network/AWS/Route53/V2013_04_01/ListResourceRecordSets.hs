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
    , mkListResourceRecordSetsRequest
    -- ** Request lenses
    , lrrsrHostedZoneId
    , lrrsrStartRecordName
    , lrrsrStartRecordType
    , lrrsrStartRecordIdentifier
    , lrrsrMaxItems

    -- * Response
    , ListResourceRecordSetsResponse
    -- ** Response lenses
    , lrrssResourceRecordSets
    , lrrssIsTruncated
    , lrrssNextRecordName
    , lrrssNextRecordType
    , lrrssNextRecordIdentifier
    , lrrssMaxItems
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListResourceRecordSets' request.
mkListResourceRecordSetsRequest :: Text -- ^ 'lrrsrHostedZoneId'
                                -> ListResourceRecordSets
mkListResourceRecordSetsRequest p1 = ListResourceRecordSets
    { _lrrsrHostedZoneId = p1
    , _lrrsrStartRecordName = Nothing
    , _lrrsrStartRecordType = Nothing
    , _lrrsrStartRecordIdentifier = Nothing
    , _lrrsrMaxItems = Nothing
    }
{-# INLINE mkListResourceRecordSetsRequest #-}

data ListResourceRecordSets = ListResourceRecordSets
    { _lrrsrHostedZoneId :: Text
      -- ^ The ID of the hosted zone that contains the resource record sets
      -- that you want to get.
    , _lrrsrStartRecordName :: Maybe Text
      -- ^ The first name in the lexicographic ordering of domain names that
      -- you want the ListResourceRecordSets request to list.
    , _lrrsrStartRecordType :: Maybe RecordType
      -- ^ The DNS type at which to begin the listing of resource record
      -- sets. Valid values: A | AAAA | CNAME | MX | NS | PTR | SOA | SPF
      -- | SRV | TXT Values for Weighted Resource Record Sets: A | AAAA |
      -- CNAME | TXT Values for Regional Resource Record Sets: A | AAAA |
      -- CNAME | TXT Values for Alias Resource Record Sets: A | AAAA
      -- Constraint: Specifying type without specifying name returns an
      -- InvalidInput error.
    , _lrrsrStartRecordIdentifier :: Maybe Text
      -- ^ Weighted resource record sets only: If results were truncated for
      -- a given DNS name and type, specify the value of
      -- ListResourceRecordSetsResponse$NextRecordIdentifier from the
      -- previous response to get the next resource record set that has
      -- the current DNS name and type.
    , _lrrsrMaxItems :: Maybe Text
      -- ^ The maximum number of records you want in the response body.
    } deriving (Show, Generic)

-- | The ID of the hosted zone that contains the resource record sets that you
-- want to get.
lrrsrHostedZoneId :: Lens' ListResourceRecordSets (Text)
lrrsrHostedZoneId = lens _lrrsrHostedZoneId (\s a -> s { _lrrsrHostedZoneId = a })
{-# INLINE lrrsrHostedZoneId #-}

-- | The first name in the lexicographic ordering of domain names that you want
-- the ListResourceRecordSets request to list.
lrrsrStartRecordName :: Lens' ListResourceRecordSets (Maybe Text)
lrrsrStartRecordName = lens _lrrsrStartRecordName (\s a -> s { _lrrsrStartRecordName = a })
{-# INLINE lrrsrStartRecordName #-}

-- | The DNS type at which to begin the listing of resource record sets. Valid
-- values: A | AAAA | CNAME | MX | NS | PTR | SOA | SPF | SRV | TXT Values for
-- Weighted Resource Record Sets: A | AAAA | CNAME | TXT Values for Regional
-- Resource Record Sets: A | AAAA | CNAME | TXT Values for Alias Resource
-- Record Sets: A | AAAA Constraint: Specifying type without specifying name
-- returns an InvalidInput error.
lrrsrStartRecordType :: Lens' ListResourceRecordSets (Maybe RecordType)
lrrsrStartRecordType = lens _lrrsrStartRecordType (\s a -> s { _lrrsrStartRecordType = a })
{-# INLINE lrrsrStartRecordType #-}

-- | Weighted resource record sets only: If results were truncated for a given
-- DNS name and type, specify the value of
-- ListResourceRecordSetsResponse$NextRecordIdentifier from the previous
-- response to get the next resource record set that has the current DNS name
-- and type.
lrrsrStartRecordIdentifier :: Lens' ListResourceRecordSets (Maybe Text)
lrrsrStartRecordIdentifier = lens _lrrsrStartRecordIdentifier (\s a -> s { _lrrsrStartRecordIdentifier = a })
{-# INLINE lrrsrStartRecordIdentifier #-}

-- | The maximum number of records you want in the response body.
lrrsrMaxItems :: Lens' ListResourceRecordSets (Maybe Text)
lrrsrMaxItems = lens _lrrsrMaxItems (\s a -> s { _lrrsrMaxItems = a })
{-# INLINE lrrsrMaxItems #-}

instance ToPath ListResourceRecordSets where
    toPath ListResourceRecordSets{..} = mconcat
        [ "/2013-04-01/hostedzone/"
        , toBS _lrrsrHostedZoneId
        , "/rrset"
        ]

instance ToQuery ListResourceRecordSets where
    toQuery ListResourceRecordSets{..} = mconcat
        [ "identifier" =? _lrrsrStartRecordIdentifier
        , "maxitems" =? _lrrsrMaxItems
        , "name" =? _lrrsrStartRecordName
        , "type" =? _lrrsrStartRecordType
        ]

instance ToHeaders ListResourceRecordSets

instance ToXML ListResourceRecordSets where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ListResourceRecordSetsRequest"

data ListResourceRecordSetsResponse = ListResourceRecordSetsResponse
    { _lrrssResourceRecordSets :: [ResourceRecordSet]
      -- ^ A complex type that contains information about the resource
      -- record sets that are returned by the request.
    , _lrrssIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more resource record sets
      -- to be listed. If your results were truncated, you can make a
      -- follow-up request for the next page of results by using the
      -- ListResourceRecordSetsResponse$NextRecordName element. Valid
      -- Values: true | false.
    , _lrrssNextRecordName :: Maybe Text
      -- ^ If the results were truncated, the name of the next record in the
      -- list. This element is present only if
      -- ListResourceRecordSetsResponse$IsTruncated is true.
    , _lrrssNextRecordType :: Maybe RecordType
      -- ^ If the results were truncated, the type of the next record in the
      -- list. This element is present only if
      -- ListResourceRecordSetsResponse$IsTruncated is true.
    , _lrrssNextRecordIdentifier :: Maybe Text
      -- ^ Weighted resource record sets only: If results were truncated for
      -- a given DNS name and type, the value of SetIdentifier for the
      -- next resource record set that has the current DNS name and type.
    , _lrrssMaxItems :: Text
      -- ^ The maximum number of records you requested. The maximum value of
      -- MaxItems is 100.
    } deriving (Show, Generic)

-- | A complex type that contains information about the resource record sets
-- that are returned by the request.
lrrssResourceRecordSets :: Lens' ListResourceRecordSetsResponse ([ResourceRecordSet])
lrrssResourceRecordSets = lens _lrrssResourceRecordSets (\s a -> s { _lrrssResourceRecordSets = a })
{-# INLINE lrrssResourceRecordSets #-}

-- | A flag that indicates whether there are more resource record sets to be
-- listed. If your results were truncated, you can make a follow-up request
-- for the next page of results by using the
-- ListResourceRecordSetsResponse$NextRecordName element. Valid Values: true |
-- false.
lrrssIsTruncated :: Lens' ListResourceRecordSetsResponse (Bool)
lrrssIsTruncated = lens _lrrssIsTruncated (\s a -> s { _lrrssIsTruncated = a })
{-# INLINE lrrssIsTruncated #-}

-- | If the results were truncated, the name of the next record in the list.
-- This element is present only if ListResourceRecordSetsResponse$IsTruncated
-- is true.
lrrssNextRecordName :: Lens' ListResourceRecordSetsResponse (Maybe Text)
lrrssNextRecordName = lens _lrrssNextRecordName (\s a -> s { _lrrssNextRecordName = a })
{-# INLINE lrrssNextRecordName #-}

-- | If the results were truncated, the type of the next record in the list.
-- This element is present only if ListResourceRecordSetsResponse$IsTruncated
-- is true.
lrrssNextRecordType :: Lens' ListResourceRecordSetsResponse (Maybe RecordType)
lrrssNextRecordType = lens _lrrssNextRecordType (\s a -> s { _lrrssNextRecordType = a })
{-# INLINE lrrssNextRecordType #-}

-- | Weighted resource record sets only: If results were truncated for a given
-- DNS name and type, the value of SetIdentifier for the next resource record
-- set that has the current DNS name and type.
lrrssNextRecordIdentifier :: Lens' ListResourceRecordSetsResponse (Maybe Text)
lrrssNextRecordIdentifier = lens _lrrssNextRecordIdentifier (\s a -> s { _lrrssNextRecordIdentifier = a })
{-# INLINE lrrssNextRecordIdentifier #-}

-- | The maximum number of records you requested. The maximum value of MaxItems
-- is 100.
lrrssMaxItems :: Lens' ListResourceRecordSetsResponse (Text)
lrrssMaxItems = lens _lrrssMaxItems (\s a -> s { _lrrssMaxItems = a })
{-# INLINE lrrssMaxItems #-}

instance FromXML ListResourceRecordSetsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListResourceRecordSets where
    type Sv ListResourceRecordSets = Route53
    type Rs ListResourceRecordSets = ListResourceRecordSetsResponse

    request = get
    response _ = xmlResponse

instance AWSPager ListResourceRecordSets where
    next rq rs
        | not (_lrrssIsTruncated rs) = Nothing
        | and [isNothing p1, isNothing p2, isNothing p3] = Nothing
        | otherwise = Just $ rq
            { _lrrsrStartRecordName = p1
            , _lrrsrStartRecordType = p2
            , _lrrsrStartRecordIdentifier = p3
            }
      where
        p1 = _lrrssNextRecordName rs
        p2 = _lrrssNextRecordType rs
        p3 = _lrrssNextRecordIdentifier rs
