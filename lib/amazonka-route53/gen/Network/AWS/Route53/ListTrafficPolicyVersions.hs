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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about all of the versions for a specified traffic policy.
--
--
-- Traffic policy versions are listed in numerical order by @VersionNumber@ .
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

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A complex type that contains the information about the request to list your traffic policies.
--
--
--
-- /See:/ 'listTrafficPolicyVersions' smart constructor.
data ListTrafficPolicyVersions = ListTrafficPolicyVersions'
  { _ltpvMaxItems                   :: !(Maybe Text)
  , _ltpvTrafficPolicyVersionMarker :: !(Maybe Text)
  , _ltpvId                         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTrafficPolicyVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltpvMaxItems' - The maximum number of traffic policy versions that you want Amazon Route 53 to include in the response body for this request. If the specified traffic policy has more than @MaxItems@ versions, the value of @IsTruncated@ in the response is @true@ , and the value of the @TrafficPolicyVersionMarker@ element is the ID of the first version that Amazon Route 53 will return if you submit another request.
--
-- * 'ltpvTrafficPolicyVersionMarker' - For your first request to @ListTrafficPolicyVersions@ , don't include the @TrafficPolicyVersionMarker@ parameter. If you have more traffic policy versions than the value of @MaxItems@ , @ListTrafficPolicyVersions@ returns only the first group of @MaxItems@ versions. To get more traffic policy versions, submit another @ListTrafficPolicyVersions@ request. For the value of @TrafficPolicyVersionMarker@ , specify the value of @TrafficPolicyVersionMarker@ in the previous response.
--
-- * 'ltpvId' - Specify the value of @Id@ of the traffic policy for which you want to list all versions.
listTrafficPolicyVersions
    :: Text -- ^ 'ltpvId'
    -> ListTrafficPolicyVersions
listTrafficPolicyVersions pId_ =
  ListTrafficPolicyVersions'
    { _ltpvMaxItems = Nothing
    , _ltpvTrafficPolicyVersionMarker = Nothing
    , _ltpvId = pId_
    }


-- | The maximum number of traffic policy versions that you want Amazon Route 53 to include in the response body for this request. If the specified traffic policy has more than @MaxItems@ versions, the value of @IsTruncated@ in the response is @true@ , and the value of the @TrafficPolicyVersionMarker@ element is the ID of the first version that Amazon Route 53 will return if you submit another request.
ltpvMaxItems :: Lens' ListTrafficPolicyVersions (Maybe Text)
ltpvMaxItems = lens _ltpvMaxItems (\ s a -> s{_ltpvMaxItems = a})

-- | For your first request to @ListTrafficPolicyVersions@ , don't include the @TrafficPolicyVersionMarker@ parameter. If you have more traffic policy versions than the value of @MaxItems@ , @ListTrafficPolicyVersions@ returns only the first group of @MaxItems@ versions. To get more traffic policy versions, submit another @ListTrafficPolicyVersions@ request. For the value of @TrafficPolicyVersionMarker@ , specify the value of @TrafficPolicyVersionMarker@ in the previous response.
ltpvTrafficPolicyVersionMarker :: Lens' ListTrafficPolicyVersions (Maybe Text)
ltpvTrafficPolicyVersionMarker = lens _ltpvTrafficPolicyVersionMarker (\ s a -> s{_ltpvTrafficPolicyVersionMarker = a})

-- | Specify the value of @Id@ of the traffic policy for which you want to list all versions.
ltpvId :: Lens' ListTrafficPolicyVersions Text
ltpvId = lens _ltpvId (\ s a -> s{_ltpvId = a})

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

instance Hashable ListTrafficPolicyVersions where

instance NFData ListTrafficPolicyVersions where

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
--
--
-- /See:/ 'listTrafficPolicyVersionsResponse' smart constructor.
data ListTrafficPolicyVersionsResponse = ListTrafficPolicyVersionsResponse'
  { _ltpvrsResponseStatus             :: !Int
  , _ltpvrsTrafficPolicies            :: ![TrafficPolicy]
  , _ltpvrsIsTruncated                :: !Bool
  , _ltpvrsTrafficPolicyVersionMarker :: !Text
  , _ltpvrsMaxItems                   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTrafficPolicyVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltpvrsResponseStatus' - -- | The response status code.
--
-- * 'ltpvrsTrafficPolicies' - A list that contains one @TrafficPolicy@ element for each traffic policy version that is associated with the specified traffic policy.
--
-- * 'ltpvrsIsTruncated' - A flag that indicates whether there are more traffic policies to be listed. If the response was truncated, you can get the next group of traffic policies by submitting another @ListTrafficPolicyVersions@ request and specifying the value of @NextMarker@ in the @marker@ parameter.
--
-- * 'ltpvrsTrafficPolicyVersionMarker' - If @IsTruncated@ is @true@ , the value of @TrafficPolicyVersionMarker@ identifies the first traffic policy that Amazon Route 53 will return if you submit another request. Call @ListTrafficPolicyVersions@ again and specify the value of @TrafficPolicyVersionMarker@ in the @TrafficPolicyVersionMarker@ request parameter. This element is present only if @IsTruncated@ is @true@ .
--
-- * 'ltpvrsMaxItems' - The value that you specified for the @maxitems@ parameter in the @ListTrafficPolicyVersions@ request that produced the current response.
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


-- | -- | The response status code.
ltpvrsResponseStatus :: Lens' ListTrafficPolicyVersionsResponse Int
ltpvrsResponseStatus = lens _ltpvrsResponseStatus (\ s a -> s{_ltpvrsResponseStatus = a})

-- | A list that contains one @TrafficPolicy@ element for each traffic policy version that is associated with the specified traffic policy.
ltpvrsTrafficPolicies :: Lens' ListTrafficPolicyVersionsResponse [TrafficPolicy]
ltpvrsTrafficPolicies = lens _ltpvrsTrafficPolicies (\ s a -> s{_ltpvrsTrafficPolicies = a}) . _Coerce

-- | A flag that indicates whether there are more traffic policies to be listed. If the response was truncated, you can get the next group of traffic policies by submitting another @ListTrafficPolicyVersions@ request and specifying the value of @NextMarker@ in the @marker@ parameter.
ltpvrsIsTruncated :: Lens' ListTrafficPolicyVersionsResponse Bool
ltpvrsIsTruncated = lens _ltpvrsIsTruncated (\ s a -> s{_ltpvrsIsTruncated = a})

-- | If @IsTruncated@ is @true@ , the value of @TrafficPolicyVersionMarker@ identifies the first traffic policy that Amazon Route 53 will return if you submit another request. Call @ListTrafficPolicyVersions@ again and specify the value of @TrafficPolicyVersionMarker@ in the @TrafficPolicyVersionMarker@ request parameter. This element is present only if @IsTruncated@ is @true@ .
ltpvrsTrafficPolicyVersionMarker :: Lens' ListTrafficPolicyVersionsResponse Text
ltpvrsTrafficPolicyVersionMarker = lens _ltpvrsTrafficPolicyVersionMarker (\ s a -> s{_ltpvrsTrafficPolicyVersionMarker = a})

-- | The value that you specified for the @maxitems@ parameter in the @ListTrafficPolicyVersions@ request that produced the current response.
ltpvrsMaxItems :: Lens' ListTrafficPolicyVersionsResponse Text
ltpvrsMaxItems = lens _ltpvrsMaxItems (\ s a -> s{_ltpvrsMaxItems = a})

instance NFData ListTrafficPolicyVersionsResponse
         where
