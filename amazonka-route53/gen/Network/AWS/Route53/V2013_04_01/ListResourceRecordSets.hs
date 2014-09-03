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
    , listResourceRecordSets
    -- ** Request lenses
    , lrrsrHostedZoneId
    , lrrsrStartRecordName
    , lrrsrMaxItems
    , lrrsrStartRecordType
    , lrrsrStartRecordIdentifier

    -- * Response
    , ListResourceRecordSetsResponse
    -- ** Response lenses
    , lrrssMaxItems
    , lrrssIsTruncated
    , lrrssResourceRecordSets
    , lrrssNextRecordName
    , lrrssNextRecordType
    , lrrssNextRecordIdentifier
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListResourceRecordSets' request.
listResourceRecordSets :: Text -- ^ 'lrrsrHostedZoneId'
                       -> ListResourceRecordSets
listResourceRecordSets p1 = ListResourceRecordSets
    { _lrrsrHostedZoneId = p1
    , _lrrsrStartRecordName = Nothing
    , _lrrsrMaxItems = Nothing
    , _lrrsrStartRecordType = Nothing
    , _lrrsrStartRecordIdentifier = Nothing
    }

data ListResourceRecordSets = ListResourceRecordSets
    { _lrrsrHostedZoneId :: Text
      -- ^ The ID of the hosted zone that contains the resource record sets
      -- that you want to get.
    , _lrrsrStartRecordName :: Maybe Text
      -- ^ The first name in the lexicographic ordering of domain names that
      -- you want the ListResourceRecordSets request to list.
    , _lrrsrMaxItems :: Maybe Text
      -- ^ The maximum number of records you want in the response body.
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
    } deriving (Show, Generic)

-- | The ID of the hosted zone that contains the resource record sets that you
-- want to get.
lrrsrHostedZoneId
    :: Functor f
    => (Text
    -> f (Text))
    -> ListResourceRecordSets
    -> f ListResourceRecordSets
lrrsrHostedZoneId f x =
    (\y -> x { _lrrsrHostedZoneId = y })
       <$> f (_lrrsrHostedZoneId x)
{-# INLINE lrrsrHostedZoneId #-}

-- | The first name in the lexicographic ordering of domain names that you want
-- the ListResourceRecordSets request to list.
lrrsrStartRecordName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListResourceRecordSets
    -> f ListResourceRecordSets
lrrsrStartRecordName f x =
    (\y -> x { _lrrsrStartRecordName = y })
       <$> f (_lrrsrStartRecordName x)
{-# INLINE lrrsrStartRecordName #-}

-- | The maximum number of records you want in the response body.
lrrsrMaxItems
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListResourceRecordSets
    -> f ListResourceRecordSets
lrrsrMaxItems f x =
    (\y -> x { _lrrsrMaxItems = y })
       <$> f (_lrrsrMaxItems x)
{-# INLINE lrrsrMaxItems #-}

-- | The DNS type at which to begin the listing of resource record sets. Valid
-- values: A | AAAA | CNAME | MX | NS | PTR | SOA | SPF | SRV | TXT Values for
-- Weighted Resource Record Sets: A | AAAA | CNAME | TXT Values for Regional
-- Resource Record Sets: A | AAAA | CNAME | TXT Values for Alias Resource
-- Record Sets: A | AAAA Constraint: Specifying type without specifying name
-- returns an InvalidInput error.
lrrsrStartRecordType
    :: Functor f
    => (Maybe RecordType
    -> f (Maybe RecordType))
    -> ListResourceRecordSets
    -> f ListResourceRecordSets
lrrsrStartRecordType f x =
    (\y -> x { _lrrsrStartRecordType = y })
       <$> f (_lrrsrStartRecordType x)
{-# INLINE lrrsrStartRecordType #-}

-- | Weighted resource record sets only: If results were truncated for a given
-- DNS name and type, specify the value of
-- ListResourceRecordSetsResponse$NextRecordIdentifier from the previous
-- response to get the next resource record set that has the current DNS name
-- and type.
lrrsrStartRecordIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListResourceRecordSets
    -> f ListResourceRecordSets
lrrsrStartRecordIdentifier f x =
    (\y -> x { _lrrsrStartRecordIdentifier = y })
       <$> f (_lrrsrStartRecordIdentifier x)
{-# INLINE lrrsrStartRecordIdentifier #-}

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
    { _lrrssMaxItems :: Text
      -- ^ The maximum number of records you requested. The maximum value of
      -- MaxItems is 100.
    , _lrrssIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more resource record sets
      -- to be listed. If your results were truncated, you can make a
      -- follow-up request for the next page of results by using the
      -- ListResourceRecordSetsResponse$NextRecordName element. Valid
      -- Values: true | false.
    , _lrrssResourceRecordSets :: [ResourceRecordSet]
      -- ^ A complex type that contains information about the resource
      -- record sets that are returned by the request.
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
    } deriving (Show, Generic)

-- | The maximum number of records you requested. The maximum value of MaxItems
-- is 100.
lrrssMaxItems
    :: Functor f
    => (Text
    -> f (Text))
    -> ListResourceRecordSetsResponse
    -> f ListResourceRecordSetsResponse
lrrssMaxItems f x =
    (\y -> x { _lrrssMaxItems = y })
       <$> f (_lrrssMaxItems x)
{-# INLINE lrrssMaxItems #-}

-- | A flag that indicates whether there are more resource record sets to be
-- listed. If your results were truncated, you can make a follow-up request
-- for the next page of results by using the
-- ListResourceRecordSetsResponse$NextRecordName element. Valid Values: true |
-- false.
lrrssIsTruncated
    :: Functor f
    => (Bool
    -> f (Bool))
    -> ListResourceRecordSetsResponse
    -> f ListResourceRecordSetsResponse
lrrssIsTruncated f x =
    (\y -> x { _lrrssIsTruncated = y })
       <$> f (_lrrssIsTruncated x)
{-# INLINE lrrssIsTruncated #-}

-- | A complex type that contains information about the resource record sets
-- that are returned by the request.
lrrssResourceRecordSets
    :: Functor f
    => ([ResourceRecordSet]
    -> f ([ResourceRecordSet]))
    -> ListResourceRecordSetsResponse
    -> f ListResourceRecordSetsResponse
lrrssResourceRecordSets f x =
    (\y -> x { _lrrssResourceRecordSets = y })
       <$> f (_lrrssResourceRecordSets x)
{-# INLINE lrrssResourceRecordSets #-}

-- | If the results were truncated, the name of the next record in the list.
-- This element is present only if ListResourceRecordSetsResponse$IsTruncated
-- is true.
lrrssNextRecordName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListResourceRecordSetsResponse
    -> f ListResourceRecordSetsResponse
lrrssNextRecordName f x =
    (\y -> x { _lrrssNextRecordName = y })
       <$> f (_lrrssNextRecordName x)
{-# INLINE lrrssNextRecordName #-}

-- | If the results were truncated, the type of the next record in the list.
-- This element is present only if ListResourceRecordSetsResponse$IsTruncated
-- is true.
lrrssNextRecordType
    :: Functor f
    => (Maybe RecordType
    -> f (Maybe RecordType))
    -> ListResourceRecordSetsResponse
    -> f ListResourceRecordSetsResponse
lrrssNextRecordType f x =
    (\y -> x { _lrrssNextRecordType = y })
       <$> f (_lrrssNextRecordType x)
{-# INLINE lrrssNextRecordType #-}

-- | Weighted resource record sets only: If results were truncated for a given
-- DNS name and type, the value of SetIdentifier for the next resource record
-- set that has the current DNS name and type.
lrrssNextRecordIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListResourceRecordSetsResponse
    -> f ListResourceRecordSetsResponse
lrrssNextRecordIdentifier f x =
    (\y -> x { _lrrssNextRecordIdentifier = y })
       <$> f (_lrrssNextRecordIdentifier x)
{-# INLINE lrrssNextRecordIdentifier #-}

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
