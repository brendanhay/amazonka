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
-- Module      : Network.AWS.Route53AutoNaming.DiscoverInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Discovers registered instances for a specified namespace and service.
--
--
module Network.AWS.Route53AutoNaming.DiscoverInstances
    (
    -- * Creating a Request
      discoverInstances
    , DiscoverInstances
    -- * Request Lenses
    , diQueryParameters
    , diHealthStatus
    , diMaxResults
    , diNamespaceName
    , diServiceName

    -- * Destructuring the Response
    , discoverInstancesResponse
    , DiscoverInstancesResponse
    -- * Response Lenses
    , dirsInstances
    , dirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53AutoNaming.Types
import Network.AWS.Route53AutoNaming.Types.Product

-- | /See:/ 'discoverInstances' smart constructor.
data DiscoverInstances = DiscoverInstances'
  { _diQueryParameters :: !(Maybe (Map Text Text))
  , _diHealthStatus    :: !(Maybe HealthStatusFilter)
  , _diMaxResults      :: !(Maybe Nat)
  , _diNamespaceName   :: !Text
  , _diServiceName     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DiscoverInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diQueryParameters' - A string map that contains attributes with values that you can use to filter instances by any custom attribute that you specified when you registered the instance. Only instances that match all the specified key/value pairs will be returned.
--
-- * 'diHealthStatus' - The health status of the instances that you want to discover.
--
-- * 'diMaxResults' - The maximum number of instances that you want Cloud Map to return in the response to a @DiscoverInstances@ request. If you don't specify a value for @MaxResults@ , Cloud Map returns up to 100 instances.
--
-- * 'diNamespaceName' - The name of the namespace that you specified when you registered the instance.
--
-- * 'diServiceName' - The name of the service that you specified when you registered the instance.
discoverInstances
    :: Text -- ^ 'diNamespaceName'
    -> Text -- ^ 'diServiceName'
    -> DiscoverInstances
discoverInstances pNamespaceName_ pServiceName_ =
  DiscoverInstances'
    { _diQueryParameters = Nothing
    , _diHealthStatus = Nothing
    , _diMaxResults = Nothing
    , _diNamespaceName = pNamespaceName_
    , _diServiceName = pServiceName_
    }


-- | A string map that contains attributes with values that you can use to filter instances by any custom attribute that you specified when you registered the instance. Only instances that match all the specified key/value pairs will be returned.
diQueryParameters :: Lens' DiscoverInstances (HashMap Text Text)
diQueryParameters = lens _diQueryParameters (\ s a -> s{_diQueryParameters = a}) . _Default . _Map

-- | The health status of the instances that you want to discover.
diHealthStatus :: Lens' DiscoverInstances (Maybe HealthStatusFilter)
diHealthStatus = lens _diHealthStatus (\ s a -> s{_diHealthStatus = a})

-- | The maximum number of instances that you want Cloud Map to return in the response to a @DiscoverInstances@ request. If you don't specify a value for @MaxResults@ , Cloud Map returns up to 100 instances.
diMaxResults :: Lens' DiscoverInstances (Maybe Natural)
diMaxResults = lens _diMaxResults (\ s a -> s{_diMaxResults = a}) . mapping _Nat

-- | The name of the namespace that you specified when you registered the instance.
diNamespaceName :: Lens' DiscoverInstances Text
diNamespaceName = lens _diNamespaceName (\ s a -> s{_diNamespaceName = a})

-- | The name of the service that you specified when you registered the instance.
diServiceName :: Lens' DiscoverInstances Text
diServiceName = lens _diServiceName (\ s a -> s{_diServiceName = a})

instance AWSRequest DiscoverInstances where
        type Rs DiscoverInstances = DiscoverInstancesResponse
        request = postJSON route53AutoNaming
        response
          = receiveJSON
              (\ s h x ->
                 DiscoverInstancesResponse' <$>
                   (x .?> "Instances" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DiscoverInstances where

instance NFData DiscoverInstances where

instance ToHeaders DiscoverInstances where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53AutoNaming_v20170314.DiscoverInstances" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DiscoverInstances where
        toJSON DiscoverInstances'{..}
          = object
              (catMaybes
                 [("QueryParameters" .=) <$> _diQueryParameters,
                  ("HealthStatus" .=) <$> _diHealthStatus,
                  ("MaxResults" .=) <$> _diMaxResults,
                  Just ("NamespaceName" .= _diNamespaceName),
                  Just ("ServiceName" .= _diServiceName)])

instance ToPath DiscoverInstances where
        toPath = const "/"

instance ToQuery DiscoverInstances where
        toQuery = const mempty

-- | /See:/ 'discoverInstancesResponse' smart constructor.
data DiscoverInstancesResponse = DiscoverInstancesResponse'
  { _dirsInstances      :: !(Maybe [HTTPInstanceSummary])
  , _dirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DiscoverInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsInstances' - A complex type that contains one @HttpInstanceSummary@ for each registered instance.
--
-- * 'dirsResponseStatus' - -- | The response status code.
discoverInstancesResponse
    :: Int -- ^ 'dirsResponseStatus'
    -> DiscoverInstancesResponse
discoverInstancesResponse pResponseStatus_ =
  DiscoverInstancesResponse'
    {_dirsInstances = Nothing, _dirsResponseStatus = pResponseStatus_}


-- | A complex type that contains one @HttpInstanceSummary@ for each registered instance.
dirsInstances :: Lens' DiscoverInstancesResponse [HTTPInstanceSummary]
dirsInstances = lens _dirsInstances (\ s a -> s{_dirsInstances = a}) . _Default . _Coerce

-- | -- | The response status code.
dirsResponseStatus :: Lens' DiscoverInstancesResponse Int
dirsResponseStatus = lens _dirsResponseStatus (\ s a -> s{_dirsResponseStatus = a})

instance NFData DiscoverInstancesResponse where
