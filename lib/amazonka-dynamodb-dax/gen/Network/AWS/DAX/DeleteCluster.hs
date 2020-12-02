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
-- Module      : Network.AWS.DAX.DeleteCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously provisioned DAX cluster. /DeleteCluster/ deletes all associated nodes, node endpoints and the DAX cluster itself. When you receive a successful response from this action, DAX immediately begins deleting the cluster; you cannot cancel or revert this action.
--
--
module Network.AWS.DAX.DeleteCluster
    (
    -- * Creating a Request
      deleteCluster
    , DeleteCluster
    -- * Request Lenses
    , dcClusterName

    -- * Destructuring the Response
    , deleteClusterResponse
    , DeleteClusterResponse
    -- * Response Lenses
    , drsCluster
    , drsResponseStatus
    ) where

import Network.AWS.DAX.Types
import Network.AWS.DAX.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteCluster' smart constructor.
newtype DeleteCluster = DeleteCluster'
  { _dcClusterName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcClusterName' - The name of the cluster to be deleted.
deleteCluster
    :: Text -- ^ 'dcClusterName'
    -> DeleteCluster
deleteCluster pClusterName_ = DeleteCluster' {_dcClusterName = pClusterName_}


-- | The name of the cluster to be deleted.
dcClusterName :: Lens' DeleteCluster Text
dcClusterName = lens _dcClusterName (\ s a -> s{_dcClusterName = a})

instance AWSRequest DeleteCluster where
        type Rs DeleteCluster = DeleteClusterResponse
        request = postJSON dax
        response
          = receiveJSON
              (\ s h x ->
                 DeleteClusterResponse' <$>
                   (x .?> "Cluster") <*> (pure (fromEnum s)))

instance Hashable DeleteCluster where

instance NFData DeleteCluster where

instance ToHeaders DeleteCluster where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDAXV3.DeleteCluster" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteCluster where
        toJSON DeleteCluster'{..}
          = object
              (catMaybes [Just ("ClusterName" .= _dcClusterName)])

instance ToPath DeleteCluster where
        toPath = const "/"

instance ToQuery DeleteCluster where
        toQuery = const mempty

-- | /See:/ 'deleteClusterResponse' smart constructor.
data DeleteClusterResponse = DeleteClusterResponse'
  { _drsCluster        :: !(Maybe Cluster)
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsCluster' - A description of the DAX cluster that is being deleted.
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteClusterResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteClusterResponse
deleteClusterResponse pResponseStatus_ =
  DeleteClusterResponse'
    {_drsCluster = Nothing, _drsResponseStatus = pResponseStatus_}


-- | A description of the DAX cluster that is being deleted.
drsCluster :: Lens' DeleteClusterResponse (Maybe Cluster)
drsCluster = lens _drsCluster (\ s a -> s{_drsCluster = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteClusterResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteClusterResponse where
