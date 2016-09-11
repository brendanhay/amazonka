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
-- Module      : Network.AWS.Route53.ListTrafficPolicyVersions
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about all of the versions for a specified traffic policy.
--
-- Send a 'GET' request to the '\/Amazon Route 53 API version\/trafficpolicy' resource and specify the ID of the traffic policy for which you want to list versions.
--
-- Amazon Route 53 returns a maximum of 100 items in each response. If you have a lot of traffic policies, you can use the 'maxitems' parameter to list them in groups of up to 100.
--
-- The response includes three values that help you navigate from one group of 'maxitems'maxitems traffic policies to the next:
--
-- -   __IsTruncated__
--
--     If the value of 'IsTruncated' in the response is 'true', there are more traffic policy versions associated with the specified traffic policy.
--
--     If 'IsTruncated' is 'false', this response includes the last traffic policy version that is associated with the specified traffic policy.
--
-- -   __TrafficPolicyVersionMarker__
--
--     The ID of the next traffic policy version that is associated with the current AWS account. If you want to list more traffic policies, make another call to 'ListTrafficPolicyVersions', and specify the value of the 'TrafficPolicyVersionMarker' element in the 'TrafficPolicyVersionMarker' request parameter.
--
--     If 'IsTruncated' is 'false', Amazon Route 53 omits the 'TrafficPolicyVersionMarker' element from the response.
--
-- -   __MaxItems__
--
--     The value that you specified for the 'MaxItems' parameter in the request that produced the current response.
--
module Network.AWS.Route53.ListTrafficPolicyVersions
    (
    -- * Creating a Request
      listTrafficPolicyVersions
    , ListTrafficPolicyVersions
    -- * Request Lenses
    , ltpvMaxItems
    , ltpvTrafficPolicyVersionMarker
    , ltpvId

    -- * Destructuring the Response
    , listTrafficPolicyVersionsResponse
    , ListTrafficPolicyVersionsResponse
    -- * Response Lenses
    , ltpvrsResponseStatus
    , ltpvrsTrafficPolicies
    , ltpvrsIsTruncated
    , ltpvrsTrafficPolicyVersionMarker
    , ltpvrsMaxItems
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | A complex type that contains the information about the request to list your traffic policies.
--
-- /See:/ 'listTrafficPolicyVersions' smart constructor.
data ListTrafficPolicyVersions = ListTrafficPolicyVersions'
    { _ltpvMaxItems                   :: !(Maybe Text)
    , _ltpvTrafficPolicyVersionMarker :: !(Maybe Text)
    , _ltpvId                         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListTrafficPolicyVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltpvMaxItems'
--
-- * 'ltpvTrafficPolicyVersionMarker'
--
-- * 'ltpvId'
listTrafficPolicyVersions
    :: Text -- ^ 'ltpvId'
    -> ListTrafficPolicyVersions
listTrafficPolicyVersions pId_ =
    ListTrafficPolicyVersions'
    { _ltpvMaxItems = Nothing
    , _ltpvTrafficPolicyVersionMarker = Nothing
    , _ltpvId = pId_
    }

-- | The maximum number of traffic policy versions that you want Amazon Route 53 to include in the response body for this request. If the specified traffic policy has more than 'MaxItems' versions, the value of the 'IsTruncated' element in the response is 'true', and the value of the 'TrafficPolicyVersionMarker' element is the ID of the first version in the next group of 'MaxItems' traffic policy versions.
ltpvMaxItems :: Lens' ListTrafficPolicyVersions (Maybe Text)
ltpvMaxItems = lens _ltpvMaxItems (\ s a -> s{_ltpvMaxItems = a});

-- | For your first request to 'ListTrafficPolicyVersions', do not include the 'TrafficPolicyVersionMarker' parameter.
--
-- If you have more traffic policy versions than the value of 'MaxItems', 'ListTrafficPolicyVersions' returns only the first group of 'MaxItems' versions. To get the next group of 'MaxItems' traffic policy versions, submit another request to 'ListTrafficPolicyVersions'. For the value of 'TrafficPolicyVersionMarker', specify the value of the 'TrafficPolicyVersionMarker' element that was returned in the previous response.
--
-- Traffic policy versions are listed in sequential order.
ltpvTrafficPolicyVersionMarker :: Lens' ListTrafficPolicyVersions (Maybe Text)
ltpvTrafficPolicyVersionMarker = lens _ltpvTrafficPolicyVersionMarker (\ s a -> s{_ltpvTrafficPolicyVersionMarker = a});

-- | Specify the value of 'Id' of the traffic policy for which you want to list all versions.
ltpvId :: Lens' ListTrafficPolicyVersions Text
ltpvId = lens _ltpvId (\ s a -> s{_ltpvId = a});

instance AWSRequest ListTrafficPolicyVersions where
        type Rs ListTrafficPolicyVersions =
             ListTrafficPolicyVersionsResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 ListTrafficPolicyVersionsResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "TrafficPolicies" .!@ mempty >>=
                        parseXMLList "TrafficPolicy")
                     <*> (x .@ "IsTruncated")
                     <*> (x .@ "TrafficPolicyVersionMarker")
                     <*> (x .@ "MaxItems"))

instance Hashable ListTrafficPolicyVersions

instance NFData ListTrafficPolicyVersions

instance ToHeaders ListTrafficPolicyVersions where
        toHeaders = const mempty

instance ToPath ListTrafficPolicyVersions where
        toPath ListTrafficPolicyVersions'{..}
          = mconcat
              ["/2013-04-01/trafficpolicies/", toBS _ltpvId,
               "/versions"]

instance ToQuery ListTrafficPolicyVersions where
        toQuery ListTrafficPolicyVersions'{..}
          = mconcat
              ["maxitems" =: _ltpvMaxItems,
               "trafficpolicyversion" =:
                 _ltpvTrafficPolicyVersionMarker]

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'listTrafficPolicyVersionsResponse' smart constructor.
data ListTrafficPolicyVersionsResponse = ListTrafficPolicyVersionsResponse'
    { _ltpvrsResponseStatus             :: !Int
    , _ltpvrsTrafficPolicies            :: ![TrafficPolicy]
    , _ltpvrsIsTruncated                :: !Bool
    , _ltpvrsTrafficPolicyVersionMarker :: !Text
    , _ltpvrsMaxItems                   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListTrafficPolicyVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltpvrsResponseStatus'
--
-- * 'ltpvrsTrafficPolicies'
--
-- * 'ltpvrsIsTruncated'
--
-- * 'ltpvrsTrafficPolicyVersionMarker'
--
-- * 'ltpvrsMaxItems'
listTrafficPolicyVersionsResponse
    :: Int -- ^ 'ltpvrsResponseStatus'
    -> Bool -- ^ 'ltpvrsIsTruncated'
    -> Text -- ^ 'ltpvrsTrafficPolicyVersionMarker'
    -> Text -- ^ 'ltpvrsMaxItems'
    -> ListTrafficPolicyVersionsResponse
listTrafficPolicyVersionsResponse pResponseStatus_ pIsTruncated_ pTrafficPolicyVersionMarker_ pMaxItems_ =
    ListTrafficPolicyVersionsResponse'
    { _ltpvrsResponseStatus = pResponseStatus_
    , _ltpvrsTrafficPolicies = mempty
    , _ltpvrsIsTruncated = pIsTruncated_
    , _ltpvrsTrafficPolicyVersionMarker = pTrafficPolicyVersionMarker_
    , _ltpvrsMaxItems = pMaxItems_
    }

-- | The response status code.
ltpvrsResponseStatus :: Lens' ListTrafficPolicyVersionsResponse Int
ltpvrsResponseStatus = lens _ltpvrsResponseStatus (\ s a -> s{_ltpvrsResponseStatus = a});

-- | A list that contains one 'TrafficPolicy' element for each traffic policy version that is associated with the specified traffic policy.
ltpvrsTrafficPolicies :: Lens' ListTrafficPolicyVersionsResponse [TrafficPolicy]
ltpvrsTrafficPolicies = lens _ltpvrsTrafficPolicies (\ s a -> s{_ltpvrsTrafficPolicies = a}) . _Coerce;

-- | A flag that indicates whether there are more traffic policies to be listed. If the response was truncated, you can get the next group of 'maxitems' traffic policies by calling 'ListTrafficPolicyVersions' again and specifying the value of the 'NextMarker' element in the 'marker' parameter.
ltpvrsIsTruncated :: Lens' ListTrafficPolicyVersionsResponse Bool
ltpvrsIsTruncated = lens _ltpvrsIsTruncated (\ s a -> s{_ltpvrsIsTruncated = a});

-- | If 'IsTruncated' is 'true', the value of 'TrafficPolicyVersionMarker' identifies the first traffic policy in the next group of 'MaxItems' traffic policies. Call 'ListTrafficPolicyVersions' again and specify the value of 'TrafficPolicyVersionMarker' in the 'TrafficPolicyVersionMarker' request parameter.
--
-- This element is present only if 'IsTruncated' is 'true'.
ltpvrsTrafficPolicyVersionMarker :: Lens' ListTrafficPolicyVersionsResponse Text
ltpvrsTrafficPolicyVersionMarker = lens _ltpvrsTrafficPolicyVersionMarker (\ s a -> s{_ltpvrsTrafficPolicyVersionMarker = a});

-- | The value that you specified for the 'maxitems' parameter in the call to 'ListTrafficPolicyVersions' that produced the current response.
ltpvrsMaxItems :: Lens' ListTrafficPolicyVersionsResponse Text
ltpvrsMaxItems = lens _ltpvrsMaxItems (\ s a -> s{_ltpvrsMaxItems = a});

instance NFData ListTrafficPolicyVersionsResponse
