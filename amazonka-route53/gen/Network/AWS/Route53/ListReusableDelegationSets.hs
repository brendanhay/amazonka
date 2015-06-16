{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Route53.ListReusableDelegationSets
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | To retrieve a list of your reusable delegation sets, send a @GET@
-- request to the @2013-04-01\/delegationset@ resource. The response to
-- this request includes a @DelegationSets@ element with zero, one, or
-- multiple @DelegationSet@ child elements. By default, the list of
-- delegation sets is displayed on a single page. You can control the
-- length of the page that is displayed by using the @MaxItems@ parameter.
-- You can use the @Marker@ parameter to control the delegation set that
-- the list begins with.
--
-- Amazon Route 53 returns a maximum of 100 items. If you set MaxItems to a
-- value greater than 100, Amazon Route 53 returns only the first 100.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListReusableDelegationSets.html>
module Network.AWS.Route53.ListReusableDelegationSets
    (
    -- * Request
      ListReusableDelegationSets
    -- ** Request constructor
    , listReusableDelegationSets
    -- ** Request lenses
    , lrdsMaxItems
    , lrdsMarker

    -- * Response
    , ListReusableDelegationSetsResponse
    -- ** Response constructor
    , listReusableDelegationSetsResponse
    -- ** Response lenses
    , lrdsrNextMarker
    , lrdsrDelegationSets
    , lrdsrMarker
    , lrdsrIsTruncated
    , lrdsrMaxItems
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Route53.Types

-- | /See:/ 'listReusableDelegationSets' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrdsMaxItems'
--
-- * 'lrdsMarker'
data ListReusableDelegationSets = ListReusableDelegationSets'{_lrdsMaxItems :: Maybe Text, _lrdsMarker :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListReusableDelegationSets' smart constructor.
listReusableDelegationSets :: ListReusableDelegationSets
listReusableDelegationSets = ListReusableDelegationSets'{_lrdsMaxItems = Nothing, _lrdsMarker = Nothing};

-- | Specify the maximum number of reusable delegation sets to return per
-- page of results.
lrdsMaxItems :: Lens' ListReusableDelegationSets (Maybe Text)
lrdsMaxItems = lens _lrdsMaxItems (\ s a -> s{_lrdsMaxItems = a});

-- | If the request returned more than one page of results, submit another
-- request and specify the value of @NextMarker@ from the last response in
-- the @marker@ parameter to get the next page of results.
lrdsMarker :: Lens' ListReusableDelegationSets (Maybe Text)
lrdsMarker = lens _lrdsMarker (\ s a -> s{_lrdsMarker = a});

instance AWSRequest ListReusableDelegationSets where
        type Sv ListReusableDelegationSets = Route53
        type Rs ListReusableDelegationSets =
             ListReusableDelegationSetsResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 ListReusableDelegationSetsResponse' <$>
                   (x .@? "NextMarker") <*>
                     (x .@? "DelegationSets" .!@ mempty >>=
                        parseXMLList "DelegationSet")
                     <*> (x .@ "Marker")
                     <*> (x .@ "IsTruncated")
                     <*> (x .@ "MaxItems"))

instance ToHeaders ListReusableDelegationSets where
        toHeaders = const mempty

instance ToPath ListReusableDelegationSets where
        toPath = const "/2013-04-01/delegationset"

instance ToQuery ListReusableDelegationSets where
        toQuery ListReusableDelegationSets'{..}
          = mconcat
              ["maxitems" =: _lrdsMaxItems,
               "marker" =: _lrdsMarker]

-- | /See:/ 'listReusableDelegationSetsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrdsrNextMarker'
--
-- * 'lrdsrDelegationSets'
--
-- * 'lrdsrMarker'
--
-- * 'lrdsrIsTruncated'
--
-- * 'lrdsrMaxItems'
data ListReusableDelegationSetsResponse = ListReusableDelegationSetsResponse'{_lrdsrNextMarker :: Maybe Text, _lrdsrDelegationSets :: [DelegationSet], _lrdsrMarker :: Text, _lrdsrIsTruncated :: Bool, _lrdsrMaxItems :: Text} deriving (Eq, Read, Show)

-- | 'ListReusableDelegationSetsResponse' smart constructor.
listReusableDelegationSetsResponse :: Text -> Bool -> Text -> ListReusableDelegationSetsResponse
listReusableDelegationSetsResponse pMarker pIsTruncated pMaxItems = ListReusableDelegationSetsResponse'{_lrdsrNextMarker = Nothing, _lrdsrDelegationSets = mempty, _lrdsrMarker = pMarker, _lrdsrIsTruncated = pIsTruncated, _lrdsrMaxItems = pMaxItems};

-- | Indicates where to continue listing reusable delegation sets. If
-- ListReusableDelegationSetsResponse$IsTruncated is @true@, make another
-- request to @ListReusableDelegationSets@ and include the value of the
-- @NextMarker@ element in the @Marker@ element to get the next page of
-- results.
lrdsrNextMarker :: Lens' ListReusableDelegationSetsResponse (Maybe Text)
lrdsrNextMarker = lens _lrdsrNextMarker (\ s a -> s{_lrdsrNextMarker = a});

-- | A complex type that contains information about the reusable delegation
-- sets associated with the current AWS account.
lrdsrDelegationSets :: Lens' ListReusableDelegationSetsResponse [DelegationSet]
lrdsrDelegationSets = lens _lrdsrDelegationSets (\ s a -> s{_lrdsrDelegationSets = a});

-- | If the request returned more than one page of results, submit another
-- request and specify the value of @NextMarker@ from the last response in
-- the @marker@ parameter to get the next page of results.
lrdsrMarker :: Lens' ListReusableDelegationSetsResponse Text
lrdsrMarker = lens _lrdsrMarker (\ s a -> s{_lrdsrMarker = a});

-- | A flag indicating whether there are more reusable delegation sets to be
-- listed. If your results were truncated, you can make a follow-up request
-- for the next page of results by using the @Marker@ element.
--
-- Valid Values: @true@ | @false@
lrdsrIsTruncated :: Lens' ListReusableDelegationSetsResponse Bool
lrdsrIsTruncated = lens _lrdsrIsTruncated (\ s a -> s{_lrdsrIsTruncated = a});

-- | The maximum number of reusable delegation sets to be included in the
-- response body. If the number of reusable delegation sets associated with
-- this AWS account exceeds @MaxItems@, the value of
-- ListReusablDelegationSetsResponse$IsTruncated in the response is @true@.
-- Call @ListReusableDelegationSets@ again and specify the value of
-- ListReusableDelegationSetsResponse$NextMarker in the
-- ListReusableDelegationSetsRequest$Marker element to get the next page of
-- results.
lrdsrMaxItems :: Lens' ListReusableDelegationSetsResponse Text
lrdsrMaxItems = lens _lrdsrMaxItems (\ s a -> s{_lrdsrMaxItems = a});
