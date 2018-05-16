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
-- Module      : Network.AWS.Snowball.DescribeCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific cluster including shipping information, cluster status, and other important metadata.
--
--
module Network.AWS.Snowball.DescribeCluster
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
    , dcrsClusterMetadata
    , dcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Snowball.Types
import Network.AWS.Snowball.Types.Product

-- | /See:/ 'describeCluster' smart constructor.
newtype DescribeCluster = DescribeCluster'
  { _dcClusterId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcClusterId' - The automatically generated ID for a cluster.
describeCluster
    :: Text -- ^ 'dcClusterId'
    -> DescribeCluster
describeCluster pClusterId_ = DescribeCluster' {_dcClusterId = pClusterId_}


-- | The automatically generated ID for a cluster.
dcClusterId :: Lens' DescribeCluster Text
dcClusterId = lens _dcClusterId (\ s a -> s{_dcClusterId = a})

instance AWSRequest DescribeCluster where
        type Rs DescribeCluster = DescribeClusterResponse
        request = postJSON snowball
        response
          = receiveJSON
              (\ s h x ->
                 DescribeClusterResponse' <$>
                   (x .?> "ClusterMetadata") <*> (pure (fromEnum s)))

instance Hashable DescribeCluster where

instance NFData DescribeCluster where

instance ToHeaders DescribeCluster where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSIESnowballJobManagementService.DescribeCluster"
                       :: ByteString),
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

-- | /See:/ 'describeClusterResponse' smart constructor.
data DescribeClusterResponse = DescribeClusterResponse'
  { _dcrsClusterMetadata :: !(Maybe ClusterMetadata)
  , _dcrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsClusterMetadata' - Information about a specific cluster, including shipping information, cluster status, and other important metadata.
--
-- * 'dcrsResponseStatus' - -- | The response status code.
describeClusterResponse
    :: Int -- ^ 'dcrsResponseStatus'
    -> DescribeClusterResponse
describeClusterResponse pResponseStatus_ =
  DescribeClusterResponse'
    {_dcrsClusterMetadata = Nothing, _dcrsResponseStatus = pResponseStatus_}


-- | Information about a specific cluster, including shipping information, cluster status, and other important metadata.
dcrsClusterMetadata :: Lens' DescribeClusterResponse (Maybe ClusterMetadata)
dcrsClusterMetadata = lens _dcrsClusterMetadata (\ s a -> s{_dcrsClusterMetadata = a})

-- | -- | The response status code.
dcrsResponseStatus :: Lens' DescribeClusterResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\ s a -> s{_dcrsResponseStatus = a})

instance NFData DescribeClusterResponse where
