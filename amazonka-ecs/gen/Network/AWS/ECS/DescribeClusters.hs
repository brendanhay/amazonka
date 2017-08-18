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
-- Module      : Network.AWS.ECS.DescribeClusters
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your clusters.
--
--
module Network.AWS.ECS.DescribeClusters
    (
    -- * Creating a Request
      describeClusters
    , DescribeClusters
    -- * Request Lenses
    , dcClusters

    -- * Destructuring the Response
    , describeClustersResponse
    , DescribeClustersResponse
    -- * Response Lenses
    , dcrsFailures
    , dcrsClusters
    , dcrsResponseStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.ECS.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeClusters' smart constructor.
newtype DescribeClusters = DescribeClusters'
    { _dcClusters :: Maybe [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeClusters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcClusters' - A list of up to 100 cluster names or full cluster Amazon Resource Name (ARN) entries. If you do not specify a cluster, the default cluster is assumed.
describeClusters
    :: DescribeClusters
describeClusters =
    DescribeClusters'
    { _dcClusters = Nothing
    }

-- | A list of up to 100 cluster names or full cluster Amazon Resource Name (ARN) entries. If you do not specify a cluster, the default cluster is assumed.
dcClusters :: Lens' DescribeClusters [Text]
dcClusters = lens _dcClusters (\ s a -> s{_dcClusters = a}) . _Default . _Coerce;

instance AWSRequest DescribeClusters where
        type Rs DescribeClusters = DescribeClustersResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeClustersResponse' <$>
                   (x .?> "failures" .!@ mempty) <*>
                     (x .?> "clusters" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeClusters

instance NFData DescribeClusters

instance ToHeaders DescribeClusters where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.DescribeClusters"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeClusters where
        toJSON DescribeClusters'{..}
          = object
              (catMaybes [("clusters" .=) <$> _dcClusters])

instance ToPath DescribeClusters where
        toPath = const "/"

instance ToQuery DescribeClusters where
        toQuery = const mempty

-- | /See:/ 'describeClustersResponse' smart constructor.
data DescribeClustersResponse = DescribeClustersResponse'
    { _dcrsFailures       :: !(Maybe [Failure])
    , _dcrsClusters       :: !(Maybe [Cluster])
    , _dcrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeClustersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsFailures' - Any failures associated with the call.
--
-- * 'dcrsClusters' - The list of clusters.
--
-- * 'dcrsResponseStatus' - -- | The response status code.
describeClustersResponse
    :: Int -- ^ 'dcrsResponseStatus'
    -> DescribeClustersResponse
describeClustersResponse pResponseStatus_ =
    DescribeClustersResponse'
    { _dcrsFailures = Nothing
    , _dcrsClusters = Nothing
    , _dcrsResponseStatus = pResponseStatus_
    }

-- | Any failures associated with the call.
dcrsFailures :: Lens' DescribeClustersResponse [Failure]
dcrsFailures = lens _dcrsFailures (\ s a -> s{_dcrsFailures = a}) . _Default . _Coerce;

-- | The list of clusters.
dcrsClusters :: Lens' DescribeClustersResponse [Cluster]
dcrsClusters = lens _dcrsClusters (\ s a -> s{_dcrsClusters = a}) . _Default . _Coerce;

-- | -- | The response status code.
dcrsResponseStatus :: Lens' DescribeClustersResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\ s a -> s{_dcrsResponseStatus = a});

instance NFData DescribeClustersResponse
