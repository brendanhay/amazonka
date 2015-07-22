{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.DescribeCluster
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Provides cluster-level details including status, hardware and software
-- configuration, VPC settings, and so on. For information about the
-- cluster steps, see ListSteps.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_DescribeCluster.html>
module Network.AWS.EMR.DescribeCluster
    (
    -- * Request
      DescribeCluster
    -- ** Request constructor
    , describeCluster
    -- ** Request lenses
    , dcrqClusterId

    -- * Response
    , DescribeClusterResponse
    -- ** Response constructor
    , describeClusterResponse
    -- ** Response lenses
    , dcrsStatus
    , dcrsCluster
    ) where

import           Network.AWS.EMR.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This input determines which cluster to describe.
--
-- /See:/ 'describeCluster' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrqClusterId'
newtype DescribeCluster = DescribeCluster'
    { _dcrqClusterId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCluster' smart constructor.
describeCluster :: Text -> DescribeCluster
describeCluster pClusterId =
    DescribeCluster'
    { _dcrqClusterId = pClusterId
    }

-- | The identifier of the cluster to describe.
dcrqClusterId :: Lens' DescribeCluster Text
dcrqClusterId = lens _dcrqClusterId (\ s a -> s{_dcrqClusterId = a});

instance AWSRequest DescribeCluster where
        type Sv DescribeCluster = EMR
        type Rs DescribeCluster = DescribeClusterResponse
        request = postJSON
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
          = object ["ClusterId" .= _dcrqClusterId]

instance ToPath DescribeCluster where
        toPath = const "/"

instance ToQuery DescribeCluster where
        toQuery = const mempty

-- | This output contains the description of the cluster.
--
-- /See:/ 'describeClusterResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcrsStatus'
--
-- * 'dcrsCluster'
data DescribeClusterResponse = DescribeClusterResponse'
    { _dcrsStatus  :: !Int
    , _dcrsCluster :: !Cluster
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeClusterResponse' smart constructor.
describeClusterResponse :: Int -> Cluster -> DescribeClusterResponse
describeClusterResponse pStatus pCluster =
    DescribeClusterResponse'
    { _dcrsStatus = pStatus
    , _dcrsCluster = pCluster
    }

-- | FIXME: Undocumented member.
dcrsStatus :: Lens' DescribeClusterResponse Int
dcrsStatus = lens _dcrsStatus (\ s a -> s{_dcrsStatus = a});

-- | This output contains the details for the requested cluster.
dcrsCluster :: Lens' DescribeClusterResponse Cluster
dcrsCluster = lens _dcrsCluster (\ s a -> s{_dcrsCluster = a});
