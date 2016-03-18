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
-- Module      : Network.AWS.Route53.ListTrafficPolicies
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the latest version for every traffic policy that
-- is associated with the current AWS account. To get the information, send
-- a 'GET' request to the '\/Route 53 API version\/trafficpolicy' resource.
--
-- Amazon Route 53 returns a maximum of 100 items in each response. If you
-- have a lot of traffic policies, you can use the 'maxitems' parameter to
-- list them in groups of up to 100.
--
-- The response includes three values that help you navigate from one group
-- of 'maxitems' traffic policies to the next:
--
-- -   __IsTruncated__
-- -   __TrafficPolicyIdMarker__
-- -   __MaxItems__
module Network.AWS.Route53.ListTrafficPolicies
    (
    -- * Creating a Request
      listTrafficPolicies
    , ListTrafficPolicies
    -- * Request Lenses
    , ltpTrafficPolicyIdMarker
    , ltpMaxItems

    -- * Destructuring the Response
    , listTrafficPoliciesResponse
    , ListTrafficPoliciesResponse
    -- * Response Lenses
    , ltprsResponseStatus
    , ltprsTrafficPolicySummaries
    , ltprsIsTruncated
    , ltprsTrafficPolicyIdMarker
    , ltprsMaxItems
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | A complex type that contains the information about the request to list
-- the traffic policies that are associated with the current AWS account.
--
-- /See:/ 'listTrafficPolicies' smart constructor.
data ListTrafficPolicies = ListTrafficPolicies'
    { _ltpTrafficPolicyIdMarker :: !(Maybe Text)
    , _ltpMaxItems              :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListTrafficPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltpTrafficPolicyIdMarker'
--
-- * 'ltpMaxItems'
listTrafficPolicies
    :: ListTrafficPolicies
listTrafficPolicies =
    ListTrafficPolicies'
    { _ltpTrafficPolicyIdMarker = Nothing
    , _ltpMaxItems = Nothing
    }

-- | For your first request to 'ListTrafficPolicies', do not include the
-- 'TrafficPolicyIdMarker' parameter.
--
-- If you have more traffic policies than the value of 'MaxItems',
-- 'ListTrafficPolicies' returns only the first 'MaxItems' traffic
-- policies. To get the next group of 'MaxItems' policies, submit another
-- request to 'ListTrafficPolicies'. For the value of
-- 'TrafficPolicyIdMarker', specify the value of the
-- 'TrafficPolicyIdMarker' element that was returned in the previous
-- response.
--
-- Policies are listed in the order in which they were created.
ltpTrafficPolicyIdMarker :: Lens' ListTrafficPolicies (Maybe Text)
ltpTrafficPolicyIdMarker = lens _ltpTrafficPolicyIdMarker (\ s a -> s{_ltpTrafficPolicyIdMarker = a});

-- | The maximum number of traffic policies to be included in the response
-- body for this request. If you have more than 'MaxItems' traffic
-- policies, the value of the 'IsTruncated' element in the response is
-- 'true', and the value of the 'TrafficPolicyIdMarker' element is the ID
-- of the first traffic policy in the next group of 'MaxItems' traffic
-- policies.
ltpMaxItems :: Lens' ListTrafficPolicies (Maybe Text)
ltpMaxItems = lens _ltpMaxItems (\ s a -> s{_ltpMaxItems = a});

instance AWSRequest ListTrafficPolicies where
        type Rs ListTrafficPolicies =
             ListTrafficPoliciesResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 ListTrafficPoliciesResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "TrafficPolicySummaries" .!@ mempty >>=
                        parseXMLList "TrafficPolicySummary")
                     <*> (x .@ "IsTruncated")
                     <*> (x .@ "TrafficPolicyIdMarker")
                     <*> (x .@ "MaxItems"))

instance ToHeaders ListTrafficPolicies where
        toHeaders = const mempty

instance ToPath ListTrafficPolicies where
        toPath = const "/2013-04-01/trafficpolicies"

instance ToQuery ListTrafficPolicies where
        toQuery ListTrafficPolicies'{..}
          = mconcat
              ["trafficpolicyid" =: _ltpTrafficPolicyIdMarker,
               "maxitems" =: _ltpMaxItems]

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'listTrafficPoliciesResponse' smart constructor.
data ListTrafficPoliciesResponse = ListTrafficPoliciesResponse'
    { _ltprsResponseStatus         :: !Int
    , _ltprsTrafficPolicySummaries :: ![TrafficPolicySummary]
    , _ltprsIsTruncated            :: !Bool
    , _ltprsTrafficPolicyIdMarker  :: !Text
    , _ltprsMaxItems               :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListTrafficPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltprsResponseStatus'
--
-- * 'ltprsTrafficPolicySummaries'
--
-- * 'ltprsIsTruncated'
--
-- * 'ltprsTrafficPolicyIdMarker'
--
-- * 'ltprsMaxItems'
listTrafficPoliciesResponse
    :: Int -- ^ 'ltprsResponseStatus'
    -> Bool -- ^ 'ltprsIsTruncated'
    -> Text -- ^ 'ltprsTrafficPolicyIdMarker'
    -> Text -- ^ 'ltprsMaxItems'
    -> ListTrafficPoliciesResponse
listTrafficPoliciesResponse pResponseStatus_ pIsTruncated_ pTrafficPolicyIdMarker_ pMaxItems_ =
    ListTrafficPoliciesResponse'
    { _ltprsResponseStatus = pResponseStatus_
    , _ltprsTrafficPolicySummaries = mempty
    , _ltprsIsTruncated = pIsTruncated_
    , _ltprsTrafficPolicyIdMarker = pTrafficPolicyIdMarker_
    , _ltprsMaxItems = pMaxItems_
    }

-- | The response status code.
ltprsResponseStatus :: Lens' ListTrafficPoliciesResponse Int
ltprsResponseStatus = lens _ltprsResponseStatus (\ s a -> s{_ltprsResponseStatus = a});

-- | A list that contains one 'TrafficPolicySummary' element for each traffic
-- policy that was created by the current AWS account.
ltprsTrafficPolicySummaries :: Lens' ListTrafficPoliciesResponse [TrafficPolicySummary]
ltprsTrafficPolicySummaries = lens _ltprsTrafficPolicySummaries (\ s a -> s{_ltprsTrafficPolicySummaries = a}) . _Coerce;

-- | A flag that indicates whether there are more traffic policies to be
-- listed. If the response was truncated, you can get the next group of
-- 'MaxItems' traffic policies by calling 'ListTrafficPolicies' again and
-- specifying the value of the 'TrafficPolicyIdMarker' element in the
-- 'TrafficPolicyIdMarker' request parameter.
--
-- Valid Values: 'true' | 'false'
ltprsIsTruncated :: Lens' ListTrafficPoliciesResponse Bool
ltprsIsTruncated = lens _ltprsIsTruncated (\ s a -> s{_ltprsIsTruncated = a});

-- | If the value of 'IsTruncated' is 'true', 'TrafficPolicyIdMarker' is the
-- ID of the first traffic policy in the next group of 'MaxItems' traffic
-- policies.
ltprsTrafficPolicyIdMarker :: Lens' ListTrafficPoliciesResponse Text
ltprsTrafficPolicyIdMarker = lens _ltprsTrafficPolicyIdMarker (\ s a -> s{_ltprsTrafficPolicyIdMarker = a});

-- | The value that you specified for the 'MaxItems' parameter in the call to
-- 'ListTrafficPolicies' that produced the current response.
ltprsMaxItems :: Lens' ListTrafficPoliciesResponse Text
ltprsMaxItems = lens _ltprsMaxItems (\ s a -> s{_ltprsMaxItems = a});
