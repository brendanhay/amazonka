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
-- Module      : Network.AWS.DAX.DecreaseReplicationFactor
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more nodes from a DAX cluster.
--
--
module Network.AWS.DAX.DecreaseReplicationFactor
    (
    -- * Creating a Request
      decreaseReplicationFactor
    , DecreaseReplicationFactor
    -- * Request Lenses
    , drfNodeIdsToRemove
    , drfAvailabilityZones
    , drfClusterName
    , drfNewReplicationFactor

    -- * Destructuring the Response
    , decreaseReplicationFactorResponse
    , DecreaseReplicationFactorResponse
    -- * Response Lenses
    , drfrsCluster
    , drfrsResponseStatus
    ) where

import Network.AWS.DAX.Types
import Network.AWS.DAX.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'decreaseReplicationFactor' smart constructor.
data DecreaseReplicationFactor = DecreaseReplicationFactor'
  { _drfNodeIdsToRemove      :: !(Maybe [Text])
  , _drfAvailabilityZones    :: !(Maybe [Text])
  , _drfClusterName          :: !Text
  , _drfNewReplicationFactor :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DecreaseReplicationFactor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drfNodeIdsToRemove' - The unique identifiers of the nodes to be removed from the cluster.
--
-- * 'drfAvailabilityZones' - The Availability Zone(s) from which to remove nodes.
--
-- * 'drfClusterName' - The name of the DAX cluster from which you want to remove nodes.
--
-- * 'drfNewReplicationFactor' - The new number of nodes for the DAX cluster.
decreaseReplicationFactor
    :: Text -- ^ 'drfClusterName'
    -> Int -- ^ 'drfNewReplicationFactor'
    -> DecreaseReplicationFactor
decreaseReplicationFactor pClusterName_ pNewReplicationFactor_ =
  DecreaseReplicationFactor'
    { _drfNodeIdsToRemove = Nothing
    , _drfAvailabilityZones = Nothing
    , _drfClusterName = pClusterName_
    , _drfNewReplicationFactor = pNewReplicationFactor_
    }


-- | The unique identifiers of the nodes to be removed from the cluster.
drfNodeIdsToRemove :: Lens' DecreaseReplicationFactor [Text]
drfNodeIdsToRemove = lens _drfNodeIdsToRemove (\ s a -> s{_drfNodeIdsToRemove = a}) . _Default . _Coerce

-- | The Availability Zone(s) from which to remove nodes.
drfAvailabilityZones :: Lens' DecreaseReplicationFactor [Text]
drfAvailabilityZones = lens _drfAvailabilityZones (\ s a -> s{_drfAvailabilityZones = a}) . _Default . _Coerce

-- | The name of the DAX cluster from which you want to remove nodes.
drfClusterName :: Lens' DecreaseReplicationFactor Text
drfClusterName = lens _drfClusterName (\ s a -> s{_drfClusterName = a})

-- | The new number of nodes for the DAX cluster.
drfNewReplicationFactor :: Lens' DecreaseReplicationFactor Int
drfNewReplicationFactor = lens _drfNewReplicationFactor (\ s a -> s{_drfNewReplicationFactor = a})

instance AWSRequest DecreaseReplicationFactor where
        type Rs DecreaseReplicationFactor =
             DecreaseReplicationFactorResponse
        request = postJSON dax
        response
          = receiveJSON
              (\ s h x ->
                 DecreaseReplicationFactorResponse' <$>
                   (x .?> "Cluster") <*> (pure (fromEnum s)))

instance Hashable DecreaseReplicationFactor where

instance NFData DecreaseReplicationFactor where

instance ToHeaders DecreaseReplicationFactor where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDAXV3.DecreaseReplicationFactor" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DecreaseReplicationFactor where
        toJSON DecreaseReplicationFactor'{..}
          = object
              (catMaybes
                 [("NodeIdsToRemove" .=) <$> _drfNodeIdsToRemove,
                  ("AvailabilityZones" .=) <$> _drfAvailabilityZones,
                  Just ("ClusterName" .= _drfClusterName),
                  Just
                    ("NewReplicationFactor" .=
                       _drfNewReplicationFactor)])

instance ToPath DecreaseReplicationFactor where
        toPath = const "/"

instance ToQuery DecreaseReplicationFactor where
        toQuery = const mempty

-- | /See:/ 'decreaseReplicationFactorResponse' smart constructor.
data DecreaseReplicationFactorResponse = DecreaseReplicationFactorResponse'
  { _drfrsCluster        :: !(Maybe Cluster)
  , _drfrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DecreaseReplicationFactorResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drfrsCluster' - A description of the DAX cluster, after you have decreased its replication factor.
--
-- * 'drfrsResponseStatus' - -- | The response status code.
decreaseReplicationFactorResponse
    :: Int -- ^ 'drfrsResponseStatus'
    -> DecreaseReplicationFactorResponse
decreaseReplicationFactorResponse pResponseStatus_ =
  DecreaseReplicationFactorResponse'
    {_drfrsCluster = Nothing, _drfrsResponseStatus = pResponseStatus_}


-- | A description of the DAX cluster, after you have decreased its replication factor.
drfrsCluster :: Lens' DecreaseReplicationFactorResponse (Maybe Cluster)
drfrsCluster = lens _drfrsCluster (\ s a -> s{_drfrsCluster = a})

-- | -- | The response status code.
drfrsResponseStatus :: Lens' DecreaseReplicationFactorResponse Int
drfrsResponseStatus = lens _drfrsResponseStatus (\ s a -> s{_drfrsResponseStatus = a})

instance NFData DecreaseReplicationFactorResponse
         where
