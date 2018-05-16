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
-- Module      : Network.AWS.Route53AutoNaming.GetInstancesHealthStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the current health status (@Healthy@ , @Unhealthy@ , or @Unknown@ ) of one or more instances that are associated with a specified service.
--
--
module Network.AWS.Route53AutoNaming.GetInstancesHealthStatus
    (
    -- * Creating a Request
      getInstancesHealthStatus
    , GetInstancesHealthStatus
    -- * Request Lenses
    , gihsNextToken
    , gihsInstances
    , gihsMaxResults
    , gihsServiceId

    -- * Destructuring the Response
    , getInstancesHealthStatusResponse
    , GetInstancesHealthStatusResponse
    -- * Response Lenses
    , gihsrsStatus
    , gihsrsNextToken
    , gihsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53AutoNaming.Types
import Network.AWS.Route53AutoNaming.Types.Product

-- | /See:/ 'getInstancesHealthStatus' smart constructor.
data GetInstancesHealthStatus = GetInstancesHealthStatus'
  { _gihsNextToken  :: !(Maybe Text)
  , _gihsInstances  :: !(Maybe (List1 Text))
  , _gihsMaxResults :: !(Maybe Nat)
  , _gihsServiceId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInstancesHealthStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gihsNextToken' - For the first @GetInstancesHealthStatus@ request, omit this value. If more than @MaxResults@ instances match the specified criteria, you can submit another @GetInstancesHealthStatus@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- * 'gihsInstances' - An array that contains the IDs of all the instances that you want to get the health status for. If you omit @Instances@ , Amazon Route 53 returns the health status for all the instances that are associated with the specified service.
--
-- * 'gihsMaxResults' - The maximum number of instances that you want Route 53 to return in the response to a @GetInstancesHealthStatus@ request. If you don't specify a value for @MaxResults@ , Route 53 returns up to 100 instances.
--
-- * 'gihsServiceId' - The ID of the service that the instance is associated with.
getInstancesHealthStatus
    :: Text -- ^ 'gihsServiceId'
    -> GetInstancesHealthStatus
getInstancesHealthStatus pServiceId_ =
  GetInstancesHealthStatus'
    { _gihsNextToken = Nothing
    , _gihsInstances = Nothing
    , _gihsMaxResults = Nothing
    , _gihsServiceId = pServiceId_
    }


-- | For the first @GetInstancesHealthStatus@ request, omit this value. If more than @MaxResults@ instances match the specified criteria, you can submit another @GetInstancesHealthStatus@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
gihsNextToken :: Lens' GetInstancesHealthStatus (Maybe Text)
gihsNextToken = lens _gihsNextToken (\ s a -> s{_gihsNextToken = a})

-- | An array that contains the IDs of all the instances that you want to get the health status for. If you omit @Instances@ , Amazon Route 53 returns the health status for all the instances that are associated with the specified service.
gihsInstances :: Lens' GetInstancesHealthStatus (Maybe (NonEmpty Text))
gihsInstances = lens _gihsInstances (\ s a -> s{_gihsInstances = a}) . mapping _List1

-- | The maximum number of instances that you want Route 53 to return in the response to a @GetInstancesHealthStatus@ request. If you don't specify a value for @MaxResults@ , Route 53 returns up to 100 instances.
gihsMaxResults :: Lens' GetInstancesHealthStatus (Maybe Natural)
gihsMaxResults = lens _gihsMaxResults (\ s a -> s{_gihsMaxResults = a}) . mapping _Nat

-- | The ID of the service that the instance is associated with.
gihsServiceId :: Lens' GetInstancesHealthStatus Text
gihsServiceId = lens _gihsServiceId (\ s a -> s{_gihsServiceId = a})

instance AWSRequest GetInstancesHealthStatus where
        type Rs GetInstancesHealthStatus =
             GetInstancesHealthStatusResponse
        request = postJSON route53AutoNaming
        response
          = receiveJSON
              (\ s h x ->
                 GetInstancesHealthStatusResponse' <$>
                   (x .?> "Status" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetInstancesHealthStatus where

instance NFData GetInstancesHealthStatus where

instance ToHeaders GetInstancesHealthStatus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53AutoNaming_v20170314.GetInstancesHealthStatus"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetInstancesHealthStatus where
        toJSON GetInstancesHealthStatus'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gihsNextToken,
                  ("Instances" .=) <$> _gihsInstances,
                  ("MaxResults" .=) <$> _gihsMaxResults,
                  Just ("ServiceId" .= _gihsServiceId)])

instance ToPath GetInstancesHealthStatus where
        toPath = const "/"

instance ToQuery GetInstancesHealthStatus where
        toQuery = const mempty

-- | /See:/ 'getInstancesHealthStatusResponse' smart constructor.
data GetInstancesHealthStatusResponse = GetInstancesHealthStatusResponse'
  { _gihsrsStatus         :: !(Maybe (Map Text HealthStatus))
  , _gihsrsNextToken      :: !(Maybe Text)
  , _gihsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInstancesHealthStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gihsrsStatus' - A complex type that contains the IDs and the health status of the instances that you specified in the @GetInstancesHealthStatus@ request.
--
-- * 'gihsrsNextToken' - If more than @MaxResults@ instances match the specified criteria, you can submit another @GetInstancesHealthStatus@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- * 'gihsrsResponseStatus' - -- | The response status code.
getInstancesHealthStatusResponse
    :: Int -- ^ 'gihsrsResponseStatus'
    -> GetInstancesHealthStatusResponse
getInstancesHealthStatusResponse pResponseStatus_ =
  GetInstancesHealthStatusResponse'
    { _gihsrsStatus = Nothing
    , _gihsrsNextToken = Nothing
    , _gihsrsResponseStatus = pResponseStatus_
    }


-- | A complex type that contains the IDs and the health status of the instances that you specified in the @GetInstancesHealthStatus@ request.
gihsrsStatus :: Lens' GetInstancesHealthStatusResponse (HashMap Text HealthStatus)
gihsrsStatus = lens _gihsrsStatus (\ s a -> s{_gihsrsStatus = a}) . _Default . _Map

-- | If more than @MaxResults@ instances match the specified criteria, you can submit another @GetInstancesHealthStatus@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
gihsrsNextToken :: Lens' GetInstancesHealthStatusResponse (Maybe Text)
gihsrsNextToken = lens _gihsrsNextToken (\ s a -> s{_gihsrsNextToken = a})

-- | -- | The response status code.
gihsrsResponseStatus :: Lens' GetInstancesHealthStatusResponse Int
gihsrsResponseStatus = lens _gihsrsResponseStatus (\ s a -> s{_gihsrsResponseStatus = a})

instance NFData GetInstancesHealthStatusResponse
         where
