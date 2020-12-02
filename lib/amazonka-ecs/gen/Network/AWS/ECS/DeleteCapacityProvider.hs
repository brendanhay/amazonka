{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeleteCapacityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified capacity provider.
--
--
-- Prior to a capacity provider being deleted, the capacity provider must be removed from the capacity provider strategy from all services. The 'UpdateService' API can be used to remove a capacity provider from a service's capacity provider strategy. When updating a service, the @forceNewDeployment@ option can be used to ensure that any tasks using the Amazon EC2 instance capacity provided by the capacity provider are transitioned to use the capacity from the remaining capacity providers. Only capacity providers that are not associated with a cluster can be deleted. To remove a capacity provider from a cluster, you can either use 'PutClusterCapacityProviders' or delete the cluster.
module Network.AWS.ECS.DeleteCapacityProvider
  ( -- * Creating a Request
    deleteCapacityProvider,
    DeleteCapacityProvider,

    -- * Request Lenses
    dcpCapacityProvider,

    -- * Destructuring the Response
    deleteCapacityProviderResponse,
    DeleteCapacityProviderResponse,

    -- * Response Lenses
    delrsCapacityProvider,
    delrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteCapacityProvider' smart constructor.
newtype DeleteCapacityProvider = DeleteCapacityProvider'
  { _dcpCapacityProvider ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteCapacityProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpCapacityProvider' - The short name or full Amazon Resource Name (ARN) of the capacity provider to delete.
deleteCapacityProvider ::
  -- | 'dcpCapacityProvider'
  Text ->
  DeleteCapacityProvider
deleteCapacityProvider pCapacityProvider_ =
  DeleteCapacityProvider'
    { _dcpCapacityProvider =
        pCapacityProvider_
    }

-- | The short name or full Amazon Resource Name (ARN) of the capacity provider to delete.
dcpCapacityProvider :: Lens' DeleteCapacityProvider Text
dcpCapacityProvider = lens _dcpCapacityProvider (\s a -> s {_dcpCapacityProvider = a})

instance AWSRequest DeleteCapacityProvider where
  type Rs DeleteCapacityProvider = DeleteCapacityProviderResponse
  request = postJSON ecs
  response =
    receiveJSON
      ( \s h x ->
          DeleteCapacityProviderResponse'
            <$> (x .?> "capacityProvider") <*> (pure (fromEnum s))
      )

instance Hashable DeleteCapacityProvider

instance NFData DeleteCapacityProvider

instance ToHeaders DeleteCapacityProvider where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AmazonEC2ContainerServiceV20141113.DeleteCapacityProvider" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteCapacityProvider where
  toJSON DeleteCapacityProvider' {..} =
    object
      (catMaybes [Just ("capacityProvider" .= _dcpCapacityProvider)])

instance ToPath DeleteCapacityProvider where
  toPath = const "/"

instance ToQuery DeleteCapacityProvider where
  toQuery = const mempty

-- | /See:/ 'deleteCapacityProviderResponse' smart constructor.
data DeleteCapacityProviderResponse = DeleteCapacityProviderResponse'
  { _delrsCapacityProvider ::
      !(Maybe CapacityProvider),
    _delrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteCapacityProviderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsCapacityProvider' - Undocumented member.
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteCapacityProviderResponse ::
  -- | 'delrsResponseStatus'
  Int ->
  DeleteCapacityProviderResponse
deleteCapacityProviderResponse pResponseStatus_ =
  DeleteCapacityProviderResponse'
    { _delrsCapacityProvider = Nothing,
      _delrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
delrsCapacityProvider :: Lens' DeleteCapacityProviderResponse (Maybe CapacityProvider)
delrsCapacityProvider = lens _delrsCapacityProvider (\s a -> s {_delrsCapacityProvider = a})

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteCapacityProviderResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\s a -> s {_delrsResponseStatus = a})

instance NFData DeleteCapacityProviderResponse
