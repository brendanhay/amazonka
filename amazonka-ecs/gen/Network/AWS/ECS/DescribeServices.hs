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
-- Module      : Network.AWS.ECS.DescribeServices
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified services running in your cluster.
--
--
module Network.AWS.ECS.DescribeServices
    (
    -- * Creating a Request
      describeServices
    , DescribeServices
    -- * Request Lenses
    , dCluster
    , dServices

    -- * Destructuring the Response
    , describeServicesResponse
    , DescribeServicesResponse
    -- * Response Lenses
    , dssrsFailures
    , dssrsServices
    , dssrsResponseStatus
    ) where

import Network.AWS.ECS.Types
import Network.AWS.ECS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeServices' smart constructor.
data DescribeServices = DescribeServices'
  { _dCluster  :: !(Maybe Text)
  , _dServices :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeServices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCluster' - The short name or full Amazon Resource Name (ARN)the cluster that hosts the service to describe. If you do not specify a cluster, the default cluster is assumed.
--
-- * 'dServices' - A list of services to describe. You may specify up to 10 services to describe in a single operation.
describeServices
    :: DescribeServices
describeServices = DescribeServices' {_dCluster = Nothing, _dServices = mempty}


-- | The short name or full Amazon Resource Name (ARN)the cluster that hosts the service to describe. If you do not specify a cluster, the default cluster is assumed.
dCluster :: Lens' DescribeServices (Maybe Text)
dCluster = lens _dCluster (\ s a -> s{_dCluster = a})

-- | A list of services to describe. You may specify up to 10 services to describe in a single operation.
dServices :: Lens' DescribeServices [Text]
dServices = lens _dServices (\ s a -> s{_dServices = a}) . _Coerce

instance AWSRequest DescribeServices where
        type Rs DescribeServices = DescribeServicesResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeServicesResponse' <$>
                   (x .?> "failures" .!@ mempty) <*>
                     (x .?> "services" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeServices where

instance NFData DescribeServices where

instance ToHeaders DescribeServices where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.DescribeServices"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeServices where
        toJSON DescribeServices'{..}
          = object
              (catMaybes
                 [("cluster" .=) <$> _dCluster,
                  Just ("services" .= _dServices)])

instance ToPath DescribeServices where
        toPath = const "/"

instance ToQuery DescribeServices where
        toQuery = const mempty

-- | /See:/ 'describeServicesResponse' smart constructor.
data DescribeServicesResponse = DescribeServicesResponse'
  { _dssrsFailures       :: !(Maybe [Failure])
  , _dssrsServices       :: !(Maybe [ContainerService])
  , _dssrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeServicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssrsFailures' - Any failures associated with the call.
--
-- * 'dssrsServices' - The list of services described.
--
-- * 'dssrsResponseStatus' - -- | The response status code.
describeServicesResponse
    :: Int -- ^ 'dssrsResponseStatus'
    -> DescribeServicesResponse
describeServicesResponse pResponseStatus_ =
  DescribeServicesResponse'
    { _dssrsFailures = Nothing
    , _dssrsServices = Nothing
    , _dssrsResponseStatus = pResponseStatus_
    }


-- | Any failures associated with the call.
dssrsFailures :: Lens' DescribeServicesResponse [Failure]
dssrsFailures = lens _dssrsFailures (\ s a -> s{_dssrsFailures = a}) . _Default . _Coerce

-- | The list of services described.
dssrsServices :: Lens' DescribeServicesResponse [ContainerService]
dssrsServices = lens _dssrsServices (\ s a -> s{_dssrsServices = a}) . _Default . _Coerce

-- | -- | The response status code.
dssrsResponseStatus :: Lens' DescribeServicesResponse Int
dssrsResponseStatus = lens _dssrsResponseStatus (\ s a -> s{_dssrsResponseStatus = a})

instance NFData DescribeServicesResponse where
