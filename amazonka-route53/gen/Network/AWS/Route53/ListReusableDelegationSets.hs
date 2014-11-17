{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.ListReusableDelegationSets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | To retrieve a list of your reusable delegation sets, send a GET request to
-- the 2013-04-01/delegationset resource. The response to this request
-- includes a DelegationSets element with zero, one, or multiple DelegationSet
-- child elements. By default, the list of delegation sets is displayed on a
-- single page. You can control the length of the page that is displayed by
-- using the MaxItems parameter. You can use the Marker parameter to control
-- the delegation set that the list begins with. Amazon Route 53 returns a
-- maximum of 100 items. If you set MaxItems to a value greater than 100,
-- Amazon Route 53 returns only the first 100.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListReusableDelegationSets.html>
module Network.AWS.Route53.ListReusableDelegationSets
    (
    -- * Request
      ListReusableDelegationSets
    -- ** Request constructor
    , listReusableDelegationSets
    -- ** Request lenses
    , lrdsMarker
    , lrdsMaxItems

    -- * Response
    , ListReusableDelegationSetsResponse
    -- ** Response constructor
    , listReusableDelegationSetsResponse
    -- ** Response lenses
    , lrdsrDelegationSets
    , lrdsrIsTruncated
    , lrdsrMarker
    , lrdsrMaxItems
    , lrdsrNextMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import qualified GHC.Exts

data ListReusableDelegationSets = ListReusableDelegationSets
    { _lrdsMarker   :: Maybe Text
    , _lrdsMaxItems :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListReusableDelegationSets' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrdsMarker' @::@ 'Maybe' 'Text'
--
-- * 'lrdsMaxItems' @::@ 'Maybe' 'Text'
--
listReusableDelegationSets :: ListReusableDelegationSets
listReusableDelegationSets = ListReusableDelegationSets
    { _lrdsMarker   = Nothing
    , _lrdsMaxItems = Nothing
    }

-- | If the request returned more than one page of results, submit another
-- request and specify the value of NextMarker from the last response in the
-- marker parameter to get the next page of results.
lrdsMarker :: Lens' ListReusableDelegationSets (Maybe Text)
lrdsMarker = lens _lrdsMarker (\s a -> s { _lrdsMarker = a })

-- | Specify the maximum number of reusable delegation sets to return per page
-- of results.
lrdsMaxItems :: Lens' ListReusableDelegationSets (Maybe Text)
lrdsMaxItems = lens _lrdsMaxItems (\s a -> s { _lrdsMaxItems = a })

data ListReusableDelegationSetsResponse = ListReusableDelegationSetsResponse
    { _lrdsrDelegationSets :: [DelegationSet]
    , _lrdsrIsTruncated    :: Bool
    , _lrdsrMarker         :: Text
    , _lrdsrMaxItems       :: Text
    , _lrdsrNextMarker     :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ListReusableDelegationSetsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrdsrDelegationSets' @::@ ['DelegationSet']
--
-- * 'lrdsrIsTruncated' @::@ 'Bool'
--
-- * 'lrdsrMarker' @::@ 'Text'
--
-- * 'lrdsrMaxItems' @::@ 'Text'
--
-- * 'lrdsrNextMarker' @::@ 'Maybe' 'Text'
--
listReusableDelegationSetsResponse :: Text -- ^ 'lrdsrMarker'
                                   -> Bool -- ^ 'lrdsrIsTruncated'
                                   -> Text -- ^ 'lrdsrMaxItems'
                                   -> ListReusableDelegationSetsResponse
listReusableDelegationSetsResponse p1 p2 p3 = ListReusableDelegationSetsResponse
    { _lrdsrMarker         = p1
    , _lrdsrIsTruncated    = p2
    , _lrdsrMaxItems       = p3
    , _lrdsrDelegationSets = mempty
    , _lrdsrNextMarker     = Nothing
    }

-- | A complex type that contains information about the reusable delegation
-- sets associated with the current AWS account.
lrdsrDelegationSets :: Lens' ListReusableDelegationSetsResponse [DelegationSet]
lrdsrDelegationSets =
    lens _lrdsrDelegationSets (\s a -> s { _lrdsrDelegationSets = a })

-- | A flag indicating whether there are more reusable delegation sets to be
-- listed. If your results were truncated, you can make a follow-up request
-- for the next page of results by using the Marker element. Valid Values:
-- true | false.
lrdsrIsTruncated :: Lens' ListReusableDelegationSetsResponse Bool
lrdsrIsTruncated = lens _lrdsrIsTruncated (\s a -> s { _lrdsrIsTruncated = a })

-- | If the request returned more than one page of results, submit another
-- request and specify the value of NextMarker from the last response in the
-- marker parameter to get the next page of results.
lrdsrMarker :: Lens' ListReusableDelegationSetsResponse Text
lrdsrMarker = lens _lrdsrMarker (\s a -> s { _lrdsrMarker = a })

-- | The maximum number of reusable delegation sets to be included in the
-- response body. If the number of reusable delegation sets associated with
-- this AWS account exceeds MaxItems, the value of
-- ListReusablDelegationSetsResponse$IsTruncated in the response is true.
-- Call ListReusableDelegationSets again and specify the value of
-- ListReusableDelegationSetsResponse$NextMarker in the
-- ListReusableDelegationSetsRequest$Marker element to get the next page of
-- results.
lrdsrMaxItems :: Lens' ListReusableDelegationSetsResponse Text
lrdsrMaxItems = lens _lrdsrMaxItems (\s a -> s { _lrdsrMaxItems = a })

-- | Indicates where to continue listing reusable delegation sets. If
-- ListReusableDelegationSetsResponse$IsTruncated is true, make another
-- request to ListReusableDelegationSets and include the value of the
-- NextMarker element in the Marker element to get the next page of results.
lrdsrNextMarker :: Lens' ListReusableDelegationSetsResponse (Maybe Text)
lrdsrNextMarker = lens _lrdsrNextMarker (\s a -> s { _lrdsrNextMarker = a })

instance ToPath ListReusableDelegationSets where
    toPath = const "/2013-04-01/delegationset"

instance ToQuery ListReusableDelegationSets

instance ToHeaders ListReusableDelegationSets

instance ToXML ListReusableDelegationSets where
    toXML = const (node "ListReusableDelegationSets" [])

instance AWSRequest ListReusableDelegationSets where
    type Sv ListReusableDelegationSets = Route53
    type Rs ListReusableDelegationSets = ListReusableDelegationSetsResponse

    request  = get
    response = xmlResponse

instance FromXML ListReusableDelegationSetsResponse where
    parseXML c = ListReusableDelegationSetsResponse
        <$> c .: "DelegationSets"
        <*> c .: "IsTruncated"
        <*> c .: "Marker"
        <*> c .: "MaxItems"
        <*> c .:? "NextMarker"
