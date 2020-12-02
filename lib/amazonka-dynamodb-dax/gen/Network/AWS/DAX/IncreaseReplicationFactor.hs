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
-- Module      : Network.AWS.DAX.IncreaseReplicationFactor
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more nodes to a DAX cluster.
--
--
module Network.AWS.DAX.IncreaseReplicationFactor
    (
    -- * Creating a Request
      increaseReplicationFactor
    , IncreaseReplicationFactor
    -- * Request Lenses
    , irfAvailabilityZones
    , irfClusterName
    , irfNewReplicationFactor

    -- * Destructuring the Response
    , increaseReplicationFactorResponse
    , IncreaseReplicationFactorResponse
    -- * Response Lenses
    , irfrsCluster
    , irfrsResponseStatus
    ) where

import Network.AWS.DAX.Types
import Network.AWS.DAX.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'increaseReplicationFactor' smart constructor.
data IncreaseReplicationFactor = IncreaseReplicationFactor'
  { _irfAvailabilityZones    :: !(Maybe [Text])
  , _irfClusterName          :: !Text
  , _irfNewReplicationFactor :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IncreaseReplicationFactor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irfAvailabilityZones' - The Availability Zones (AZs) in which the cluster nodes will be created. All nodes belonging to the cluster are placed in these Availability Zones. Use this parameter if you want to distribute the nodes across multiple AZs.
--
-- * 'irfClusterName' - The name of the DAX cluster that will receive additional nodes.
--
-- * 'irfNewReplicationFactor' - The new number of nodes for the DAX cluster.
increaseReplicationFactor
    :: Text -- ^ 'irfClusterName'
    -> Int -- ^ 'irfNewReplicationFactor'
    -> IncreaseReplicationFactor
increaseReplicationFactor pClusterName_ pNewReplicationFactor_ =
  IncreaseReplicationFactor'
    { _irfAvailabilityZones = Nothing
    , _irfClusterName = pClusterName_
    , _irfNewReplicationFactor = pNewReplicationFactor_
    }


-- | The Availability Zones (AZs) in which the cluster nodes will be created. All nodes belonging to the cluster are placed in these Availability Zones. Use this parameter if you want to distribute the nodes across multiple AZs.
irfAvailabilityZones :: Lens' IncreaseReplicationFactor [Text]
irfAvailabilityZones = lens _irfAvailabilityZones (\ s a -> s{_irfAvailabilityZones = a}) . _Default . _Coerce

-- | The name of the DAX cluster that will receive additional nodes.
irfClusterName :: Lens' IncreaseReplicationFactor Text
irfClusterName = lens _irfClusterName (\ s a -> s{_irfClusterName = a})

-- | The new number of nodes for the DAX cluster.
irfNewReplicationFactor :: Lens' IncreaseReplicationFactor Int
irfNewReplicationFactor = lens _irfNewReplicationFactor (\ s a -> s{_irfNewReplicationFactor = a})

instance AWSRequest IncreaseReplicationFactor where
        type Rs IncreaseReplicationFactor =
             IncreaseReplicationFactorResponse
        request = postJSON dax
        response
          = receiveJSON
              (\ s h x ->
                 IncreaseReplicationFactorResponse' <$>
                   (x .?> "Cluster") <*> (pure (fromEnum s)))

instance Hashable IncreaseReplicationFactor where

instance NFData IncreaseReplicationFactor where

instance ToHeaders IncreaseReplicationFactor where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDAXV3.IncreaseReplicationFactor" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON IncreaseReplicationFactor where
        toJSON IncreaseReplicationFactor'{..}
          = object
              (catMaybes
                 [("AvailabilityZones" .=) <$> _irfAvailabilityZones,
                  Just ("ClusterName" .= _irfClusterName),
                  Just
                    ("NewReplicationFactor" .=
                       _irfNewReplicationFactor)])

instance ToPath IncreaseReplicationFactor where
        toPath = const "/"

instance ToQuery IncreaseReplicationFactor where
        toQuery = const mempty

-- | /See:/ 'increaseReplicationFactorResponse' smart constructor.
data IncreaseReplicationFactorResponse = IncreaseReplicationFactorResponse'
  { _irfrsCluster        :: !(Maybe Cluster)
  , _irfrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IncreaseReplicationFactorResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irfrsCluster' - A description of the DAX cluster. with its new replication factor.
--
-- * 'irfrsResponseStatus' - -- | The response status code.
increaseReplicationFactorResponse
    :: Int -- ^ 'irfrsResponseStatus'
    -> IncreaseReplicationFactorResponse
increaseReplicationFactorResponse pResponseStatus_ =
  IncreaseReplicationFactorResponse'
    {_irfrsCluster = Nothing, _irfrsResponseStatus = pResponseStatus_}


-- | A description of the DAX cluster. with its new replication factor.
irfrsCluster :: Lens' IncreaseReplicationFactorResponse (Maybe Cluster)
irfrsCluster = lens _irfrsCluster (\ s a -> s{_irfrsCluster = a})

-- | -- | The response status code.
irfrsResponseStatus :: Lens' IncreaseReplicationFactorResponse Int
irfrsResponseStatus = lens _irfrsResponseStatus (\ s a -> s{_irfrsResponseStatus = a})

instance NFData IncreaseReplicationFactorResponse
         where
