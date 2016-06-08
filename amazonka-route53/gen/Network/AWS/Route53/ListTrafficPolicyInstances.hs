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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the traffic policy instances that you created by using the current AWS account.
--
-- After you submit an 'UpdateTrafficPolicyInstance' request, there\'s a brief delay while Amazon Route 53 creates the resource record sets that are specified in the traffic policy definition. For more information, see the < State> response element.
--
-- To get information about the traffic policy instances that are associated with the current AWS account, send a 'GET' request to the '\/Route 53 API version\/trafficpolicyinstance' resource.
--
-- Amazon Route 53 returns a maximum of 100 items in each response. If you have a lot of traffic policy instances, you can use the 'MaxItems' parameter to list them in groups of up to 100.
--
-- The response includes five values that help you navigate from one group of 'MaxItems' traffic policy instances to the next:
--
-- -   __IsTruncated__
-- -   __MaxItems__
-- -   __HostedZoneIdMarker__, __TrafficPolicyInstanceNameMarker__, and __TrafficPolicyInstanceTypeMarker__
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

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | A complex type that contains the information about the request to list your traffic policy instances.
--
-- /See:/ 'listTrafficPolicyInstances' smart constructor.
data ListTrafficPolicyInstances = ListTrafficPolicyInstances'
    { _ltpiTrafficPolicyInstanceTypeMarker :: !(Maybe RecordType)
    , _ltpiMaxItems                        :: !(Maybe Text)
    , _ltpiHostedZoneIdMarker              :: !(Maybe Text)
    , _ltpiTrafficPolicyInstanceNameMarker :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListTrafficPolicyInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltpiTrafficPolicyInstanceTypeMarker'
--
-- * 'ltpiMaxItems'
--
-- * 'ltpiHostedZoneIdMarker'
--
-- * 'ltpiTrafficPolicyInstanceNameMarker'
listTrafficPolicyInstances
    :: ListTrafficPolicyInstances
listTrafficPolicyInstances =
    ListTrafficPolicyInstances'
    { _ltpiTrafficPolicyInstanceTypeMarker = Nothing
    , _ltpiMaxItems = Nothing
    , _ltpiHostedZoneIdMarker = Nothing
    , _ltpiTrafficPolicyInstanceNameMarker = Nothing
    }

-- | For the first request to 'ListTrafficPolicyInstances', omit this value.
--
-- If the value of 'IsTruncated' in the previous response was 'true', 'TrafficPolicyInstanceTypeMarker' is the DNS type of the first traffic policy instance in the next group of 'MaxItems' traffic policy instances.
--
-- If the value of 'IsTruncated' in the previous response was 'false', there are no more traffic policy instances to get.
ltpiTrafficPolicyInstanceTypeMarker :: Lens' ListTrafficPolicyInstances (Maybe RecordType)
ltpiTrafficPolicyInstanceTypeMarker = lens _ltpiTrafficPolicyInstanceTypeMarker (\ s a -> s{_ltpiTrafficPolicyInstanceTypeMarker = a});

-- | The maximum number of traffic policy instances to be included in the response body for this request. If you have more than 'MaxItems' traffic policy instances, the value of the 'IsTruncated' element in the response is 'true', and the values of 'HostedZoneIdMarker', 'TrafficPolicyInstanceNameMarker', and 'TrafficPolicyInstanceTypeMarker' represent the first traffic policy instance in the next group of 'MaxItems' traffic policy instances.
ltpiMaxItems :: Lens' ListTrafficPolicyInstances (Maybe Text)
ltpiMaxItems = lens _ltpiMaxItems (\ s a -> s{_ltpiMaxItems = a});

-- | For the first request to 'ListTrafficPolicyInstances', omit this value.
--
-- If the value of 'IsTruncated' in the previous response was 'true', you have more traffic policy instances. To get the next group of 'MaxItems' traffic policy instances, submit another 'ListTrafficPolicyInstances' request. For the value of 'HostedZoneIdMarker', specify the value of 'HostedZoneIdMarker' from the previous response, which is the hosted zone ID of the first traffic policy instance in the next group of 'MaxItems' traffic policy instances.
--
-- If the value of 'IsTruncated' in the previous response was 'false', there are no more traffic policy instances to get.
ltpiHostedZoneIdMarker :: Lens' ListTrafficPolicyInstances (Maybe Text)
ltpiHostedZoneIdMarker = lens _ltpiHostedZoneIdMarker (\ s a -> s{_ltpiHostedZoneIdMarker = a});

-- | For the first request to 'ListTrafficPolicyInstances', omit this value.
--
-- If the value of 'IsTruncated' in the previous response was 'true', 'TrafficPolicyInstanceNameMarker' is the name of the first traffic policy instance in the next group of 'MaxItems' traffic policy instances.
--
-- If the value of 'IsTruncated' in the previous response was 'false', there are no more traffic policy instances to get.
ltpiTrafficPolicyInstanceNameMarker :: Lens' ListTrafficPolicyInstances (Maybe Text)
ltpiTrafficPolicyInstanceNameMarker = lens _ltpiTrafficPolicyInstanceNameMarker (\ s a -> s{_ltpiTrafficPolicyInstanceNameMarker = a});

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

instance Hashable ListTrafficPolicyInstances

instance NFData ListTrafficPolicyInstances

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
-- /See:/ 'listTrafficPolicyInstancesResponse' smart constructor.
data ListTrafficPolicyInstancesResponse = ListTrafficPolicyInstancesResponse'
    { _ltpirsTrafficPolicyInstanceTypeMarker :: !(Maybe RecordType)
    , _ltpirsHostedZoneIdMarker              :: !(Maybe Text)
    , _ltpirsTrafficPolicyInstanceNameMarker :: !(Maybe Text)
    , _ltpirsResponseStatus                  :: !Int
    , _ltpirsTrafficPolicyInstances          :: ![TrafficPolicyInstance]
    , _ltpirsIsTruncated                     :: !Bool
    , _ltpirsMaxItems                        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListTrafficPolicyInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltpirsTrafficPolicyInstanceTypeMarker'
--
-- * 'ltpirsHostedZoneIdMarker'
--
-- * 'ltpirsTrafficPolicyInstanceNameMarker'
--
-- * 'ltpirsResponseStatus'
--
-- * 'ltpirsTrafficPolicyInstances'
--
-- * 'ltpirsIsTruncated'
--
-- * 'ltpirsMaxItems'
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

-- | If 'IsTruncated' is 'true', 'TrafficPolicyInstanceTypeMarker' is the DNS type of the resource record sets that are associated with the first traffic policy instance in the next group of 'MaxItems' traffic policy instances.
ltpirsTrafficPolicyInstanceTypeMarker :: Lens' ListTrafficPolicyInstancesResponse (Maybe RecordType)
ltpirsTrafficPolicyInstanceTypeMarker = lens _ltpirsTrafficPolicyInstanceTypeMarker (\ s a -> s{_ltpirsTrafficPolicyInstanceTypeMarker = a});

-- | If 'IsTruncated' is 'true', 'HostedZoneIdMarker' is the ID of the hosted zone of the first traffic policy instance in the next group of 'MaxItems' traffic policy instances.
ltpirsHostedZoneIdMarker :: Lens' ListTrafficPolicyInstancesResponse (Maybe Text)
ltpirsHostedZoneIdMarker = lens _ltpirsHostedZoneIdMarker (\ s a -> s{_ltpirsHostedZoneIdMarker = a});

-- | If 'IsTruncated' is 'true', 'TrafficPolicyInstanceNameMarker' is the name of the first traffic policy instance in the next group of 'MaxItems' traffic policy instances.
ltpirsTrafficPolicyInstanceNameMarker :: Lens' ListTrafficPolicyInstancesResponse (Maybe Text)
ltpirsTrafficPolicyInstanceNameMarker = lens _ltpirsTrafficPolicyInstanceNameMarker (\ s a -> s{_ltpirsTrafficPolicyInstanceNameMarker = a});

-- | The response status code.
ltpirsResponseStatus :: Lens' ListTrafficPolicyInstancesResponse Int
ltpirsResponseStatus = lens _ltpirsResponseStatus (\ s a -> s{_ltpirsResponseStatus = a});

-- | A list that contains one 'TrafficPolicyInstance' element for each traffic policy instance that matches the elements in the request.
ltpirsTrafficPolicyInstances :: Lens' ListTrafficPolicyInstancesResponse [TrafficPolicyInstance]
ltpirsTrafficPolicyInstances = lens _ltpirsTrafficPolicyInstances (\ s a -> s{_ltpirsTrafficPolicyInstances = a}) . _Coerce;

-- | A flag that indicates whether there are more traffic policy instances to be listed. If the response was truncated, you can get the next group of 'MaxItems' traffic policy instances by calling 'ListTrafficPolicyInstances' again and specifying the values of the 'HostedZoneIdMarker', 'TrafficPolicyInstanceNameMarker', and 'TrafficPolicyInstanceTypeMarker' elements in the corresponding request parameters.
--
-- Valid Values: 'true' | 'false'
ltpirsIsTruncated :: Lens' ListTrafficPolicyInstancesResponse Bool
ltpirsIsTruncated = lens _ltpirsIsTruncated (\ s a -> s{_ltpirsIsTruncated = a});

-- | The value that you specified for the 'MaxItems' parameter in the call to 'ListTrafficPolicyInstances' that produced the current response.
ltpirsMaxItems :: Lens' ListTrafficPolicyInstancesResponse Text
ltpirsMaxItems = lens _ltpirsMaxItems (\ s a -> s{_ltpirsMaxItems = a});

instance NFData ListTrafficPolicyInstancesResponse
