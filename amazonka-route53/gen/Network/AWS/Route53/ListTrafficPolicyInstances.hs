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
-- Module      : Network.AWS.Route53.ListTrafficPolicyInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the traffic policy instances that you created by using the current AWS account.
--
--
-- Amazon Route 53 returns a maximum of 100 items in each response. If you have a lot of traffic policy instances, you can use the @MaxItems@ parameter to list them in groups of up to 100.
--
module Network.AWS.Route53.ListTrafficPolicyInstances
    (
    -- * Creating a Request
      listTrafficPolicyInstances
    , ListTrafficPolicyInstances
    -- * Request Lenses
    , ltpiTrafficPolicyInstanceTypeMarker
    , ltpiMaxItems
    , ltpiHostedZoneIdMarker
    , ltpiTrafficPolicyInstanceNameMarker

    -- * Destructuring the Response
    , listTrafficPolicyInstancesResponse
    , ListTrafficPolicyInstancesResponse
    -- * Response Lenses
    , ltpirsTrafficPolicyInstanceTypeMarker
    , ltpirsHostedZoneIdMarker
    , ltpirsTrafficPolicyInstanceNameMarker
    , ltpirsResponseStatus
    , ltpirsTrafficPolicyInstances
    , ltpirsIsTruncated
    , ltpirsMaxItems
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A request to get information about the traffic policy instances that you created by using the current AWS account.
--
--
--
-- /See:/ 'listTrafficPolicyInstances' smart constructor.
data ListTrafficPolicyInstances = ListTrafficPolicyInstances'
  { _ltpiTrafficPolicyInstanceTypeMarker :: !(Maybe RecordType)
  , _ltpiMaxItems                        :: !(Maybe Text)
  , _ltpiHostedZoneIdMarker              :: !(Maybe ResourceId)
  , _ltpiTrafficPolicyInstanceNameMarker :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTrafficPolicyInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltpiTrafficPolicyInstanceTypeMarker' - If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @trafficpolicyinstancetype@ , specify the value of @TrafficPolicyInstanceTypeMarker@ from the previous response, which is the type of the first traffic policy instance in the next group of traffic policy instances. If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
--
-- * 'ltpiMaxItems' - The maximum number of traffic policy instances that you want Amazon Route 53 to return in response to a @ListTrafficPolicyInstances@ request. If you have more than @MaxItems@ traffic policy instances, the value of the @IsTruncated@ element in the response is @true@ , and the values of @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ represent the first traffic policy instance in the next group of @MaxItems@ traffic policy instances.
--
-- * 'ltpiHostedZoneIdMarker' - If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @HostedZoneId@ , specify the value of @HostedZoneIdMarker@ from the previous response, which is the hosted zone ID of the first traffic policy instance in the next group of traffic policy instances. If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
--
-- * 'ltpiTrafficPolicyInstanceNameMarker' - If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @trafficpolicyinstancename@ , specify the value of @TrafficPolicyInstanceNameMarker@ from the previous response, which is the name of the first traffic policy instance in the next group of traffic policy instances. If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
listTrafficPolicyInstances
    :: ListTrafficPolicyInstances
listTrafficPolicyInstances =
  ListTrafficPolicyInstances'
    { _ltpiTrafficPolicyInstanceTypeMarker = Nothing
    , _ltpiMaxItems = Nothing
    , _ltpiHostedZoneIdMarker = Nothing
    , _ltpiTrafficPolicyInstanceNameMarker = Nothing
    }


-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @trafficpolicyinstancetype@ , specify the value of @TrafficPolicyInstanceTypeMarker@ from the previous response, which is the type of the first traffic policy instance in the next group of traffic policy instances. If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
ltpiTrafficPolicyInstanceTypeMarker :: Lens' ListTrafficPolicyInstances (Maybe RecordType)
ltpiTrafficPolicyInstanceTypeMarker = lens _ltpiTrafficPolicyInstanceTypeMarker (\ s a -> s{_ltpiTrafficPolicyInstanceTypeMarker = a})

-- | The maximum number of traffic policy instances that you want Amazon Route 53 to return in response to a @ListTrafficPolicyInstances@ request. If you have more than @MaxItems@ traffic policy instances, the value of the @IsTruncated@ element in the response is @true@ , and the values of @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ represent the first traffic policy instance in the next group of @MaxItems@ traffic policy instances.
ltpiMaxItems :: Lens' ListTrafficPolicyInstances (Maybe Text)
ltpiMaxItems = lens _ltpiMaxItems (\ s a -> s{_ltpiMaxItems = a})

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @HostedZoneId@ , specify the value of @HostedZoneIdMarker@ from the previous response, which is the hosted zone ID of the first traffic policy instance in the next group of traffic policy instances. If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
ltpiHostedZoneIdMarker :: Lens' ListTrafficPolicyInstances (Maybe ResourceId)
ltpiHostedZoneIdMarker = lens _ltpiHostedZoneIdMarker (\ s a -> s{_ltpiHostedZoneIdMarker = a})

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @trafficpolicyinstancename@ , specify the value of @TrafficPolicyInstanceNameMarker@ from the previous response, which is the name of the first traffic policy instance in the next group of traffic policy instances. If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
ltpiTrafficPolicyInstanceNameMarker :: Lens' ListTrafficPolicyInstances (Maybe Text)
ltpiTrafficPolicyInstanceNameMarker = lens _ltpiTrafficPolicyInstanceNameMarker (\ s a -> s{_ltpiTrafficPolicyInstanceNameMarker = a})

instance AWSRequest ListTrafficPolicyInstances where
        type Rs ListTrafficPolicyInstances =
             ListTrafficPolicyInstancesResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 ListTrafficPolicyInstancesResponse' <$>
                   (x .@? "TrafficPolicyInstanceTypeMarker") <*>
                     (x .@? "HostedZoneIdMarker")
                     <*> (x .@? "TrafficPolicyInstanceNameMarker")
                     <*> (pure (fromEnum s))
                     <*>
                     (x .@? "TrafficPolicyInstances" .!@ mempty >>=
                        parseXMLList "TrafficPolicyInstance")
                     <*> (x .@ "IsTruncated")
                     <*> (x .@ "MaxItems"))

instance Hashable ListTrafficPolicyInstances where

instance NFData ListTrafficPolicyInstances where

instance ToHeaders ListTrafficPolicyInstances where
        toHeaders = const mempty

instance ToPath ListTrafficPolicyInstances where
        toPath = const "/2013-04-01/trafficpolicyinstances"

instance ToQuery ListTrafficPolicyInstances where
        toQuery ListTrafficPolicyInstances'{..}
          = mconcat
              ["trafficpolicyinstancetype" =:
                 _ltpiTrafficPolicyInstanceTypeMarker,
               "maxitems" =: _ltpiMaxItems,
               "hostedzoneid" =: _ltpiHostedZoneIdMarker,
               "trafficpolicyinstancename" =:
                 _ltpiTrafficPolicyInstanceNameMarker]

-- | A complex type that contains the response information for the request.
--
--
--
-- /See:/ 'listTrafficPolicyInstancesResponse' smart constructor.
data ListTrafficPolicyInstancesResponse = ListTrafficPolicyInstancesResponse'
  { _ltpirsTrafficPolicyInstanceTypeMarker :: !(Maybe RecordType)
  , _ltpirsHostedZoneIdMarker              :: !(Maybe ResourceId)
  , _ltpirsTrafficPolicyInstanceNameMarker :: !(Maybe Text)
  , _ltpirsResponseStatus                  :: !Int
  , _ltpirsTrafficPolicyInstances          :: ![TrafficPolicyInstance]
  , _ltpirsIsTruncated                     :: !Bool
  , _ltpirsMaxItems                        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTrafficPolicyInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltpirsTrafficPolicyInstanceTypeMarker' - If @IsTruncated@ is @true@ , @TrafficPolicyInstanceTypeMarker@ is the DNS type of the resource record sets that are associated with the first traffic policy instance that Amazon Route 53 will return if you submit another @ListTrafficPolicyInstances@ request.
--
-- * 'ltpirsHostedZoneIdMarker' - If @IsTruncated@ is @true@ , @HostedZoneIdMarker@ is the ID of the hosted zone of the first traffic policy instance that Amazon Route 53 will return if you submit another @ListTrafficPolicyInstances@ request.
--
-- * 'ltpirsTrafficPolicyInstanceNameMarker' - If @IsTruncated@ is @true@ , @TrafficPolicyInstanceNameMarker@ is the name of the first traffic policy instance that Amazon Route 53 will return if you submit another @ListTrafficPolicyInstances@ request.
--
-- * 'ltpirsResponseStatus' - -- | The response status code.
--
-- * 'ltpirsTrafficPolicyInstances' - A list that contains one @TrafficPolicyInstance@ element for each traffic policy instance that matches the elements in the request.
--
-- * 'ltpirsIsTruncated' - A flag that indicates whether there are more traffic policy instances to be listed. If the response was truncated, you can get more traffic policy instances by calling @ListTrafficPolicyInstances@ again and specifying the values of the @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ in the corresponding request parameters.
--
-- * 'ltpirsMaxItems' - The value that you specified for the @MaxItems@ parameter in the call to @ListTrafficPolicyInstances@ that produced the current response.
listTrafficPolicyInstancesResponse
    :: Int -- ^ 'ltpirsResponseStatus'
    -> Bool -- ^ 'ltpirsIsTruncated'
    -> Text -- ^ 'ltpirsMaxItems'
    -> ListTrafficPolicyInstancesResponse
listTrafficPolicyInstancesResponse pResponseStatus_ pIsTruncated_ pMaxItems_ =
  ListTrafficPolicyInstancesResponse'
    { _ltpirsTrafficPolicyInstanceTypeMarker = Nothing
    , _ltpirsHostedZoneIdMarker = Nothing
    , _ltpirsTrafficPolicyInstanceNameMarker = Nothing
    , _ltpirsResponseStatus = pResponseStatus_
    , _ltpirsTrafficPolicyInstances = mempty
    , _ltpirsIsTruncated = pIsTruncated_
    , _ltpirsMaxItems = pMaxItems_
    }


-- | If @IsTruncated@ is @true@ , @TrafficPolicyInstanceTypeMarker@ is the DNS type of the resource record sets that are associated with the first traffic policy instance that Amazon Route 53 will return if you submit another @ListTrafficPolicyInstances@ request.
ltpirsTrafficPolicyInstanceTypeMarker :: Lens' ListTrafficPolicyInstancesResponse (Maybe RecordType)
ltpirsTrafficPolicyInstanceTypeMarker = lens _ltpirsTrafficPolicyInstanceTypeMarker (\ s a -> s{_ltpirsTrafficPolicyInstanceTypeMarker = a})

-- | If @IsTruncated@ is @true@ , @HostedZoneIdMarker@ is the ID of the hosted zone of the first traffic policy instance that Amazon Route 53 will return if you submit another @ListTrafficPolicyInstances@ request.
ltpirsHostedZoneIdMarker :: Lens' ListTrafficPolicyInstancesResponse (Maybe ResourceId)
ltpirsHostedZoneIdMarker = lens _ltpirsHostedZoneIdMarker (\ s a -> s{_ltpirsHostedZoneIdMarker = a})

-- | If @IsTruncated@ is @true@ , @TrafficPolicyInstanceNameMarker@ is the name of the first traffic policy instance that Amazon Route 53 will return if you submit another @ListTrafficPolicyInstances@ request.
ltpirsTrafficPolicyInstanceNameMarker :: Lens' ListTrafficPolicyInstancesResponse (Maybe Text)
ltpirsTrafficPolicyInstanceNameMarker = lens _ltpirsTrafficPolicyInstanceNameMarker (\ s a -> s{_ltpirsTrafficPolicyInstanceNameMarker = a})

-- | -- | The response status code.
ltpirsResponseStatus :: Lens' ListTrafficPolicyInstancesResponse Int
ltpirsResponseStatus = lens _ltpirsResponseStatus (\ s a -> s{_ltpirsResponseStatus = a})

-- | A list that contains one @TrafficPolicyInstance@ element for each traffic policy instance that matches the elements in the request.
ltpirsTrafficPolicyInstances :: Lens' ListTrafficPolicyInstancesResponse [TrafficPolicyInstance]
ltpirsTrafficPolicyInstances = lens _ltpirsTrafficPolicyInstances (\ s a -> s{_ltpirsTrafficPolicyInstances = a}) . _Coerce

-- | A flag that indicates whether there are more traffic policy instances to be listed. If the response was truncated, you can get more traffic policy instances by calling @ListTrafficPolicyInstances@ again and specifying the values of the @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ in the corresponding request parameters.
ltpirsIsTruncated :: Lens' ListTrafficPolicyInstancesResponse Bool
ltpirsIsTruncated = lens _ltpirsIsTruncated (\ s a -> s{_ltpirsIsTruncated = a})

-- | The value that you specified for the @MaxItems@ parameter in the call to @ListTrafficPolicyInstances@ that produced the current response.
ltpirsMaxItems :: Lens' ListTrafficPolicyInstancesResponse Text
ltpirsMaxItems = lens _ltpirsMaxItems (\ s a -> s{_ltpirsMaxItems = a})

instance NFData ListTrafficPolicyInstancesResponse
         where
