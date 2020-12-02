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
-- Module      : Network.AWS.Route53.ListTrafficPolicyInstancesByPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the traffic policy instances that you created by using a specify traffic policy version.
--
--
-- Amazon Route 53 returns a maximum of 100 items in each response. If you have a lot of traffic policy instances, you can use the @MaxItems@ parameter to list them in groups of up to 100.
--
module Network.AWS.Route53.ListTrafficPolicyInstancesByPolicy
    (
    -- * Creating a Request
      listTrafficPolicyInstancesByPolicy
    , ListTrafficPolicyInstancesByPolicy
    -- * Request Lenses
    , ltpibpTrafficPolicyInstanceTypeMarker
    , ltpibpMaxItems
    , ltpibpHostedZoneIdMarker
    , ltpibpTrafficPolicyInstanceNameMarker
    , ltpibpTrafficPolicyId
    , ltpibpTrafficPolicyVersion

    -- * Destructuring the Response
    , listTrafficPolicyInstancesByPolicyResponse
    , ListTrafficPolicyInstancesByPolicyResponse
    -- * Response Lenses
    , ltpibprsTrafficPolicyInstanceTypeMarker
    , ltpibprsHostedZoneIdMarker
    , ltpibprsTrafficPolicyInstanceNameMarker
    , ltpibprsResponseStatus
    , ltpibprsTrafficPolicyInstances
    , ltpibprsIsTruncated
    , ltpibprsMaxItems
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A complex type that contains the information about the request to list your traffic policy instances.
--
--
--
-- /See:/ 'listTrafficPolicyInstancesByPolicy' smart constructor.
data ListTrafficPolicyInstancesByPolicy = ListTrafficPolicyInstancesByPolicy'
  { _ltpibpTrafficPolicyInstanceTypeMarker :: !(Maybe RecordType)
  , _ltpibpMaxItems                        :: !(Maybe Text)
  , _ltpibpHostedZoneIdMarker              :: !(Maybe ResourceId)
  , _ltpibpTrafficPolicyInstanceNameMarker :: !(Maybe Text)
  , _ltpibpTrafficPolicyId                 :: !Text
  , _ltpibpTrafficPolicyVersion            :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTrafficPolicyInstancesByPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltpibpTrafficPolicyInstanceTypeMarker' - If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstancesByPolicy@ request. For the value of @trafficpolicyinstancetype@ , specify the value of @TrafficPolicyInstanceTypeMarker@ from the previous response, which is the name of the first traffic policy instance that Amazon Route 53 will return if you submit another request. If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
--
-- * 'ltpibpMaxItems' - The maximum number of traffic policy instances to be included in the response body for this request. If you have more than @MaxItems@ traffic policy instances, the value of the @IsTruncated@ element in the response is @true@ , and the values of @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ represent the first traffic policy instance that Amazon Route 53 will return if you submit another request.
--
-- * 'ltpibpHostedZoneIdMarker' - If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.  For the value of @hostedzoneid@ , specify the value of @HostedZoneIdMarker@ from the previous response, which is the hosted zone ID of the first traffic policy instance that Amazon Route 53 will return if you submit another request. If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
--
-- * 'ltpibpTrafficPolicyInstanceNameMarker' - If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstancesByPolicy@ request. For the value of @trafficpolicyinstancename@ , specify the value of @TrafficPolicyInstanceNameMarker@ from the previous response, which is the name of the first traffic policy instance that Amazon Route 53 will return if you submit another request. If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
--
-- * 'ltpibpTrafficPolicyId' - The ID of the traffic policy for which you want to list traffic policy instances.
--
-- * 'ltpibpTrafficPolicyVersion' - The version of the traffic policy for which you want to list traffic policy instances. The version must be associated with the traffic policy that is specified by @TrafficPolicyId@ .
listTrafficPolicyInstancesByPolicy
    :: Text -- ^ 'ltpibpTrafficPolicyId'
    -> Natural -- ^ 'ltpibpTrafficPolicyVersion'
    -> ListTrafficPolicyInstancesByPolicy
listTrafficPolicyInstancesByPolicy pTrafficPolicyId_ pTrafficPolicyVersion_ =
  ListTrafficPolicyInstancesByPolicy'
    { _ltpibpTrafficPolicyInstanceTypeMarker = Nothing
    , _ltpibpMaxItems = Nothing
    , _ltpibpHostedZoneIdMarker = Nothing
    , _ltpibpTrafficPolicyInstanceNameMarker = Nothing
    , _ltpibpTrafficPolicyId = pTrafficPolicyId_
    , _ltpibpTrafficPolicyVersion = _Nat # pTrafficPolicyVersion_
    }


-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstancesByPolicy@ request. For the value of @trafficpolicyinstancetype@ , specify the value of @TrafficPolicyInstanceTypeMarker@ from the previous response, which is the name of the first traffic policy instance that Amazon Route 53 will return if you submit another request. If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
ltpibpTrafficPolicyInstanceTypeMarker :: Lens' ListTrafficPolicyInstancesByPolicy (Maybe RecordType)
ltpibpTrafficPolicyInstanceTypeMarker = lens _ltpibpTrafficPolicyInstanceTypeMarker (\ s a -> s{_ltpibpTrafficPolicyInstanceTypeMarker = a})

-- | The maximum number of traffic policy instances to be included in the response body for this request. If you have more than @MaxItems@ traffic policy instances, the value of the @IsTruncated@ element in the response is @true@ , and the values of @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ represent the first traffic policy instance that Amazon Route 53 will return if you submit another request.
ltpibpMaxItems :: Lens' ListTrafficPolicyInstancesByPolicy (Maybe Text)
ltpibpMaxItems = lens _ltpibpMaxItems (\ s a -> s{_ltpibpMaxItems = a})

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.  For the value of @hostedzoneid@ , specify the value of @HostedZoneIdMarker@ from the previous response, which is the hosted zone ID of the first traffic policy instance that Amazon Route 53 will return if you submit another request. If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
ltpibpHostedZoneIdMarker :: Lens' ListTrafficPolicyInstancesByPolicy (Maybe ResourceId)
ltpibpHostedZoneIdMarker = lens _ltpibpHostedZoneIdMarker (\ s a -> s{_ltpibpHostedZoneIdMarker = a})

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstancesByPolicy@ request. For the value of @trafficpolicyinstancename@ , specify the value of @TrafficPolicyInstanceNameMarker@ from the previous response, which is the name of the first traffic policy instance that Amazon Route 53 will return if you submit another request. If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
ltpibpTrafficPolicyInstanceNameMarker :: Lens' ListTrafficPolicyInstancesByPolicy (Maybe Text)
ltpibpTrafficPolicyInstanceNameMarker = lens _ltpibpTrafficPolicyInstanceNameMarker (\ s a -> s{_ltpibpTrafficPolicyInstanceNameMarker = a})

-- | The ID of the traffic policy for which you want to list traffic policy instances.
ltpibpTrafficPolicyId :: Lens' ListTrafficPolicyInstancesByPolicy Text
ltpibpTrafficPolicyId = lens _ltpibpTrafficPolicyId (\ s a -> s{_ltpibpTrafficPolicyId = a})

-- | The version of the traffic policy for which you want to list traffic policy instances. The version must be associated with the traffic policy that is specified by @TrafficPolicyId@ .
ltpibpTrafficPolicyVersion :: Lens' ListTrafficPolicyInstancesByPolicy Natural
ltpibpTrafficPolicyVersion = lens _ltpibpTrafficPolicyVersion (\ s a -> s{_ltpibpTrafficPolicyVersion = a}) . _Nat

instance AWSRequest
           ListTrafficPolicyInstancesByPolicy
         where
        type Rs ListTrafficPolicyInstancesByPolicy =
             ListTrafficPolicyInstancesByPolicyResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 ListTrafficPolicyInstancesByPolicyResponse' <$>
                   (x .@? "TrafficPolicyInstanceTypeMarker") <*>
                     (x .@? "HostedZoneIdMarker")
                     <*> (x .@? "TrafficPolicyInstanceNameMarker")
                     <*> (pure (fromEnum s))
                     <*>
                     (x .@? "TrafficPolicyInstances" .!@ mempty >>=
                        parseXMLList "TrafficPolicyInstance")
                     <*> (x .@ "IsTruncated")
                     <*> (x .@ "MaxItems"))

instance Hashable ListTrafficPolicyInstancesByPolicy
         where

instance NFData ListTrafficPolicyInstancesByPolicy
         where

instance ToHeaders ListTrafficPolicyInstancesByPolicy
         where
        toHeaders = const mempty

instance ToPath ListTrafficPolicyInstancesByPolicy
         where
        toPath
          = const
              "/2013-04-01/trafficpolicyinstances/trafficpolicy"

instance ToQuery ListTrafficPolicyInstancesByPolicy
         where
        toQuery ListTrafficPolicyInstancesByPolicy'{..}
          = mconcat
              ["trafficpolicyinstancetype" =:
                 _ltpibpTrafficPolicyInstanceTypeMarker,
               "maxitems" =: _ltpibpMaxItems,
               "hostedzoneid" =: _ltpibpHostedZoneIdMarker,
               "trafficpolicyinstancename" =:
                 _ltpibpTrafficPolicyInstanceNameMarker,
               "id" =: _ltpibpTrafficPolicyId,
               "version" =: _ltpibpTrafficPolicyVersion]

-- | A complex type that contains the response information for the request.
--
--
--
-- /See:/ 'listTrafficPolicyInstancesByPolicyResponse' smart constructor.
data ListTrafficPolicyInstancesByPolicyResponse = ListTrafficPolicyInstancesByPolicyResponse'
  { _ltpibprsTrafficPolicyInstanceTypeMarker :: !(Maybe RecordType)
  , _ltpibprsHostedZoneIdMarker              :: !(Maybe ResourceId)
  , _ltpibprsTrafficPolicyInstanceNameMarker :: !(Maybe Text)
  , _ltpibprsResponseStatus                  :: !Int
  , _ltpibprsTrafficPolicyInstances          :: ![TrafficPolicyInstance]
  , _ltpibprsIsTruncated                     :: !Bool
  , _ltpibprsMaxItems                        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTrafficPolicyInstancesByPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltpibprsTrafficPolicyInstanceTypeMarker' - If @IsTruncated@ is @true@ , @TrafficPolicyInstanceTypeMarker@ is the DNS type of the resource record sets that are associated with the first traffic policy instance in the next group of @MaxItems@ traffic policy instances.
--
-- * 'ltpibprsHostedZoneIdMarker' - If @IsTruncated@ is @true@ , @HostedZoneIdMarker@ is the ID of the hosted zone of the first traffic policy instance in the next group of traffic policy instances.
--
-- * 'ltpibprsTrafficPolicyInstanceNameMarker' - If @IsTruncated@ is @true@ , @TrafficPolicyInstanceNameMarker@ is the name of the first traffic policy instance in the next group of @MaxItems@ traffic policy instances.
--
-- * 'ltpibprsResponseStatus' - -- | The response status code.
--
-- * 'ltpibprsTrafficPolicyInstances' - A list that contains one @TrafficPolicyInstance@ element for each traffic policy instance that matches the elements in the request.
--
-- * 'ltpibprsIsTruncated' - A flag that indicates whether there are more traffic policy instances to be listed. If the response was truncated, you can get the next group of traffic policy instances by calling @ListTrafficPolicyInstancesByPolicy@ again and specifying the values of the @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ elements in the corresponding request parameters.
--
-- * 'ltpibprsMaxItems' - The value that you specified for the @MaxItems@ parameter in the call to @ListTrafficPolicyInstancesByPolicy@ that produced the current response.
listTrafficPolicyInstancesByPolicyResponse
    :: Int -- ^ 'ltpibprsResponseStatus'
    -> Bool -- ^ 'ltpibprsIsTruncated'
    -> Text -- ^ 'ltpibprsMaxItems'
    -> ListTrafficPolicyInstancesByPolicyResponse
listTrafficPolicyInstancesByPolicyResponse pResponseStatus_ pIsTruncated_ pMaxItems_ =
  ListTrafficPolicyInstancesByPolicyResponse'
    { _ltpibprsTrafficPolicyInstanceTypeMarker = Nothing
    , _ltpibprsHostedZoneIdMarker = Nothing
    , _ltpibprsTrafficPolicyInstanceNameMarker = Nothing
    , _ltpibprsResponseStatus = pResponseStatus_
    , _ltpibprsTrafficPolicyInstances = mempty
    , _ltpibprsIsTruncated = pIsTruncated_
    , _ltpibprsMaxItems = pMaxItems_
    }


-- | If @IsTruncated@ is @true@ , @TrafficPolicyInstanceTypeMarker@ is the DNS type of the resource record sets that are associated with the first traffic policy instance in the next group of @MaxItems@ traffic policy instances.
ltpibprsTrafficPolicyInstanceTypeMarker :: Lens' ListTrafficPolicyInstancesByPolicyResponse (Maybe RecordType)
ltpibprsTrafficPolicyInstanceTypeMarker = lens _ltpibprsTrafficPolicyInstanceTypeMarker (\ s a -> s{_ltpibprsTrafficPolicyInstanceTypeMarker = a})

-- | If @IsTruncated@ is @true@ , @HostedZoneIdMarker@ is the ID of the hosted zone of the first traffic policy instance in the next group of traffic policy instances.
ltpibprsHostedZoneIdMarker :: Lens' ListTrafficPolicyInstancesByPolicyResponse (Maybe ResourceId)
ltpibprsHostedZoneIdMarker = lens _ltpibprsHostedZoneIdMarker (\ s a -> s{_ltpibprsHostedZoneIdMarker = a})

-- | If @IsTruncated@ is @true@ , @TrafficPolicyInstanceNameMarker@ is the name of the first traffic policy instance in the next group of @MaxItems@ traffic policy instances.
ltpibprsTrafficPolicyInstanceNameMarker :: Lens' ListTrafficPolicyInstancesByPolicyResponse (Maybe Text)
ltpibprsTrafficPolicyInstanceNameMarker = lens _ltpibprsTrafficPolicyInstanceNameMarker (\ s a -> s{_ltpibprsTrafficPolicyInstanceNameMarker = a})

-- | -- | The response status code.
ltpibprsResponseStatus :: Lens' ListTrafficPolicyInstancesByPolicyResponse Int
ltpibprsResponseStatus = lens _ltpibprsResponseStatus (\ s a -> s{_ltpibprsResponseStatus = a})

-- | A list that contains one @TrafficPolicyInstance@ element for each traffic policy instance that matches the elements in the request.
ltpibprsTrafficPolicyInstances :: Lens' ListTrafficPolicyInstancesByPolicyResponse [TrafficPolicyInstance]
ltpibprsTrafficPolicyInstances = lens _ltpibprsTrafficPolicyInstances (\ s a -> s{_ltpibprsTrafficPolicyInstances = a}) . _Coerce

-- | A flag that indicates whether there are more traffic policy instances to be listed. If the response was truncated, you can get the next group of traffic policy instances by calling @ListTrafficPolicyInstancesByPolicy@ again and specifying the values of the @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ elements in the corresponding request parameters.
ltpibprsIsTruncated :: Lens' ListTrafficPolicyInstancesByPolicyResponse Bool
ltpibprsIsTruncated = lens _ltpibprsIsTruncated (\ s a -> s{_ltpibprsIsTruncated = a})

-- | The value that you specified for the @MaxItems@ parameter in the call to @ListTrafficPolicyInstancesByPolicy@ that produced the current response.
ltpibprsMaxItems :: Lens' ListTrafficPolicyInstancesByPolicyResponse Text
ltpibprsMaxItems = lens _ltpibprsMaxItems (\ s a -> s{_ltpibprsMaxItems = a})

instance NFData
           ListTrafficPolicyInstancesByPolicyResponse
         where
