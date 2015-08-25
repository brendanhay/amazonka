{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListReusableDelegationSets
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- To retrieve a list of your reusable delegation sets, send a 'GET'
-- request to the '2013-04-01\/delegationset' resource. The response to
-- this request includes a 'DelegationSets' element with zero, one, or
-- multiple 'DelegationSet' child elements. By default, the list of
-- delegation sets is displayed on a single page. You can control the
-- length of the page that is displayed by using the 'MaxItems' parameter.
-- You can use the 'Marker' parameter to control the delegation set that
-- the list begins with.
--
-- Amazon Route 53 returns a maximum of 100 items. If you set MaxItems to a
-- value greater than 100, Amazon Route 53 returns only the first 100.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListReusableDelegationSets.html AWS API Reference> for ListReusableDelegationSets.
module Network.AWS.Route53.ListReusableDelegationSets
    (
    -- * Creating a Request
      listReusableDelegationSets
    , ListReusableDelegationSets
    -- * Request Lenses
    , lrdsMaxItems
    , lrdsMarker

    -- * Destructuring the Response
    , listReusableDelegationSetsResponse
    , ListReusableDelegationSetsResponse
    -- * Response Lenses
    , lrdsrsNextMarker
    , lrdsrsStatus
    , lrdsrsDelegationSets
    , lrdsrsMarker
    , lrdsrsIsTruncated
    , lrdsrsMaxItems
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | To retrieve a list of your reusable delegation sets, send a 'GET'
-- request to the '2013-04-01\/delegationset' resource. The response to
-- this request includes a 'DelegationSets' element with zero or more
-- 'DelegationSet' child elements. By default, the list of reusable
-- delegation sets is displayed on a single page. You can control the
-- length of the page that is displayed by using the 'MaxItems' parameter.
-- You can use the 'Marker' parameter to control the delegation set that
-- the list begins with.
--
-- Route 53 returns a maximum of 100 items. If you set 'MaxItems' to a
-- value greater than 100, Route 53 returns only the first 100.
--
-- /See:/ 'listReusableDelegationSets' smart constructor.
data ListReusableDelegationSets = ListReusableDelegationSets'
    { _lrdsMaxItems :: !(Maybe Text)
    , _lrdsMarker   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListReusableDelegationSets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrdsMaxItems'
--
-- * 'lrdsMarker'
listReusableDelegationSets
    :: ListReusableDelegationSets
listReusableDelegationSets =
    ListReusableDelegationSets'
    { _lrdsMaxItems = Nothing
    , _lrdsMarker = Nothing
    }

-- | Specify the maximum number of reusable delegation sets to return per
-- page of results.
lrdsMaxItems :: Lens' ListReusableDelegationSets (Maybe Text)
lrdsMaxItems = lens _lrdsMaxItems (\ s a -> s{_lrdsMaxItems = a});

-- | If the request returned more than one page of results, submit another
-- request and specify the value of 'NextMarker' from the last response in
-- the 'marker' parameter to get the next page of results.
lrdsMarker :: Lens' ListReusableDelegationSets (Maybe Text)
lrdsMarker = lens _lrdsMarker (\ s a -> s{_lrdsMarker = a});

instance AWSRequest ListReusableDelegationSets where
        type Rs ListReusableDelegationSets =
             ListReusableDelegationSetsResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 ListReusableDelegationSetsResponse' <$>
                   (x .@? "NextMarker") <*> (pure (fromEnum s)) <*>
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

-- | A complex type that contains the response for the request.
--
-- /See:/ 'listReusableDelegationSetsResponse' smart constructor.
data ListReusableDelegationSetsResponse = ListReusableDelegationSetsResponse'
    { _lrdsrsNextMarker     :: !(Maybe Text)
    , _lrdsrsStatus         :: !Int
    , _lrdsrsDelegationSets :: ![DelegationSet]
    , _lrdsrsMarker         :: !Text
    , _lrdsrsIsTruncated    :: !Bool
    , _lrdsrsMaxItems       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListReusableDelegationSetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrdsrsNextMarker'
--
-- * 'lrdsrsStatus'
--
-- * 'lrdsrsDelegationSets'
--
-- * 'lrdsrsMarker'
--
-- * 'lrdsrsIsTruncated'
--
-- * 'lrdsrsMaxItems'
listReusableDelegationSetsResponse
    :: Int -- ^ 'lrdsrsStatus'
    -> Text -- ^ 'lrdsrsMarker'
    -> Bool -- ^ 'lrdsrsIsTruncated'
    -> Text -- ^ 'lrdsrsMaxItems'
    -> ListReusableDelegationSetsResponse
listReusableDelegationSetsResponse pStatus_ pMarker_ pIsTruncated_ pMaxItems_ =
    ListReusableDelegationSetsResponse'
    { _lrdsrsNextMarker = Nothing
    , _lrdsrsStatus = pStatus_
    , _lrdsrsDelegationSets = mempty
    , _lrdsrsMarker = pMarker_
    , _lrdsrsIsTruncated = pIsTruncated_
    , _lrdsrsMaxItems = pMaxItems_
    }

-- | Indicates where to continue listing reusable delegation sets. If
-- ListReusableDelegationSetsResponse$IsTruncated is 'true', make another
-- request to 'ListReusableDelegationSets' and include the value of the
-- 'NextMarker' element in the 'Marker' element to get the next page of
-- results.
lrdsrsNextMarker :: Lens' ListReusableDelegationSetsResponse (Maybe Text)
lrdsrsNextMarker = lens _lrdsrsNextMarker (\ s a -> s{_lrdsrsNextMarker = a});

-- | The response status code.
lrdsrsStatus :: Lens' ListReusableDelegationSetsResponse Int
lrdsrsStatus = lens _lrdsrsStatus (\ s a -> s{_lrdsrsStatus = a});

-- | A complex type that contains information about the reusable delegation
-- sets associated with the current AWS account.
lrdsrsDelegationSets :: Lens' ListReusableDelegationSetsResponse [DelegationSet]
lrdsrsDelegationSets = lens _lrdsrsDelegationSets (\ s a -> s{_lrdsrsDelegationSets = a}) . _Coerce;

-- | If the request returned more than one page of results, submit another
-- request and specify the value of 'NextMarker' from the last response in
-- the 'marker' parameter to get the next page of results.
lrdsrsMarker :: Lens' ListReusableDelegationSetsResponse Text
lrdsrsMarker = lens _lrdsrsMarker (\ s a -> s{_lrdsrsMarker = a});

-- | A flag indicating whether there are more reusable delegation sets to be
-- listed. If your results were truncated, you can make a follow-up request
-- for the next page of results by using the 'Marker' element.
--
-- Valid Values: 'true' | 'false'
lrdsrsIsTruncated :: Lens' ListReusableDelegationSetsResponse Bool
lrdsrsIsTruncated = lens _lrdsrsIsTruncated (\ s a -> s{_lrdsrsIsTruncated = a});

-- | The maximum number of reusable delegation sets to be included in the
-- response body. If the number of reusable delegation sets associated with
-- this AWS account exceeds 'MaxItems', the value of
-- ListReusablDelegationSetsResponse$IsTruncated in the response is 'true'.
-- Call 'ListReusableDelegationSets' again and specify the value of
-- ListReusableDelegationSetsResponse$NextMarker in the
-- ListReusableDelegationSetsRequest$Marker element to get the next page of
-- results.
lrdsrsMaxItems :: Lens' ListReusableDelegationSetsResponse Text
lrdsrsMaxItems = lens _lrdsrsMaxItems (\ s a -> s{_lrdsrsMaxItems = a});
