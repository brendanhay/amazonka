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
-- Module      : Network.AWS.EMR.DescribeCluster
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides cluster-level details including status, hardware and software
-- configuration, VPC settings, and so on. For information about the
-- cluster steps, see ListSteps.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_DescribeCluster.html AWS API Reference> for DescribeCluster.
module Network.AWS.EMR.DescribeCluster
    (
    -- * Creating a Request
      describeCluster
    , DescribeCluster
    -- * Request Lenses
    , dcClusterId

    -- * Destructuring the Response
    , describeClusterResponse
    , DescribeClusterResponse
    -- * Response Lenses
    , dcrsStatus
    , dcrsCluster
    ) where

import           Network.AWS.EMR.Types
import           Network.AWS.EMR.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This input determines which cluster to describe.
--
-- /See:/ 'describeCluster' smart constructor.
newtype DescribeCluster = DescribeCluster'
    { _dcClusterId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcClusterId'
describeCluster
    :: Text -- ^ 'dcClusterId'
    -> DescribeCluster
describeCluster pClusterId_ =
    DescribeCluster'
    { _dcClusterId = pClusterId_
    }

-- | The identifier of the cluster to describe.
dcClusterId :: Lens' DescribeCluster Text
dcClusterId = lens _dcClusterId (\ s a -> s{_dcClusterId = a});

instance AWSRequest DescribeCluster where
        type Rs DescribeCluster = DescribeClusterResponse
        request = postJSON eMR
        response
          = receiveJSON
              (\ s h x ->
                 DescribeClusterResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "Cluster"))

instance ToHeaders DescribeCluster where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.DescribeCluster" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeCluster where
        toJSON DescribeCluster'{..}
          = object
              (catMaybes [Just ("ClusterId" .= _dcClusterId)])

instance ToPath DescribeCluster where
        toPath = const "/"

instance ToQuery DescribeCluster where
        toQuery = const mempty

-- | This output contains the description of the cluster.
--
-- /See:/ 'describeClusterResponse' smart constructor.
data DescribeClusterResponse = DescribeClusterResponse'
    { _dcrsStatus  :: !Int
    , _dcrsCluster :: !Cluster
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsStatus'
--
-- * 'dcrsCluster'
describeClusterResponse
    :: Int -- ^ 'dcrsStatus'
    -> Cluster -- ^ 'dcrsCluster'
    -> DescribeClusterResponse
describeClusterResponse pStatus_ pCluster_ =
    DescribeClusterResponse'
    { _dcrsStatus = pStatus_
    , _dcrsCluster = pCluster_
    }

-- | The response status code.
dcrsStatus :: Lens' DescribeClusterResponse Int
dcrsStatus = lens _dcrsStatus (\ s a -> s{_dcrsStatus = a});

-- | This output contains the details for the requested cluster.
dcrsCluster :: Lens' DescribeClusterResponse Cluster
dcrsCluster = lens _dcrsCluster (\ s a -> s{_dcrsCluster = a});
