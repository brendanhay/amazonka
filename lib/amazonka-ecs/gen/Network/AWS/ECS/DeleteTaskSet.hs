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
-- Module      : Network.AWS.ECS.DeleteTaskSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified task set within a service. This is used when a service uses the @EXTERNAL@ deployment controller type. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.DeleteTaskSet
  ( -- * Creating a Request
    deleteTaskSet,
    DeleteTaskSet,

    -- * Request Lenses
    dtsForce,
    dtsCluster,
    dtsService,
    dtsTaskSet,

    -- * Destructuring the Response
    deleteTaskSetResponse,
    DeleteTaskSetResponse,

    -- * Response Lenses
    dtsrsTaskSet,
    dtsrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTaskSet' smart constructor.
data DeleteTaskSet = DeleteTaskSet'
  { _dtsForce :: !(Maybe Bool),
    _dtsCluster :: !Text,
    _dtsService :: !Text,
    _dtsTaskSet :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteTaskSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtsForce' - If @true@ , this allows you to delete a task set even if it hasn't been scaled down to zero.
--
-- * 'dtsCluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in to delete.
--
-- * 'dtsService' - The short name or full Amazon Resource Name (ARN) of the service that hosts the task set to delete.
--
-- * 'dtsTaskSet' - The task set ID or full Amazon Resource Name (ARN) of the task set to delete.
deleteTaskSet ::
  -- | 'dtsCluster'
  Text ->
  -- | 'dtsService'
  Text ->
  -- | 'dtsTaskSet'
  Text ->
  DeleteTaskSet
deleteTaskSet pCluster_ pService_ pTaskSet_ =
  DeleteTaskSet'
    { _dtsForce = Nothing,
      _dtsCluster = pCluster_,
      _dtsService = pService_,
      _dtsTaskSet = pTaskSet_
    }

-- | If @true@ , this allows you to delete a task set even if it hasn't been scaled down to zero.
dtsForce :: Lens' DeleteTaskSet (Maybe Bool)
dtsForce = lens _dtsForce (\s a -> s {_dtsForce = a})

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in to delete.
dtsCluster :: Lens' DeleteTaskSet Text
dtsCluster = lens _dtsCluster (\s a -> s {_dtsCluster = a})

-- | The short name or full Amazon Resource Name (ARN) of the service that hosts the task set to delete.
dtsService :: Lens' DeleteTaskSet Text
dtsService = lens _dtsService (\s a -> s {_dtsService = a})

-- | The task set ID or full Amazon Resource Name (ARN) of the task set to delete.
dtsTaskSet :: Lens' DeleteTaskSet Text
dtsTaskSet = lens _dtsTaskSet (\s a -> s {_dtsTaskSet = a})

instance AWSRequest DeleteTaskSet where
  type Rs DeleteTaskSet = DeleteTaskSetResponse
  request = postJSON ecs
  response =
    receiveJSON
      ( \s h x ->
          DeleteTaskSetResponse'
            <$> (x .?> "taskSet") <*> (pure (fromEnum s))
      )

instance Hashable DeleteTaskSet

instance NFData DeleteTaskSet

instance ToHeaders DeleteTaskSet where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonEC2ContainerServiceV20141113.DeleteTaskSet" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteTaskSet where
  toJSON DeleteTaskSet' {..} =
    object
      ( catMaybes
          [ ("force" .=) <$> _dtsForce,
            Just ("cluster" .= _dtsCluster),
            Just ("service" .= _dtsService),
            Just ("taskSet" .= _dtsTaskSet)
          ]
      )

instance ToPath DeleteTaskSet where
  toPath = const "/"

instance ToQuery DeleteTaskSet where
  toQuery = const mempty

-- | /See:/ 'deleteTaskSetResponse' smart constructor.
data DeleteTaskSetResponse = DeleteTaskSetResponse'
  { _dtsrsTaskSet ::
      !(Maybe TaskSet),
    _dtsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteTaskSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtsrsTaskSet' - Undocumented member.
--
-- * 'dtsrsResponseStatus' - -- | The response status code.
deleteTaskSetResponse ::
  -- | 'dtsrsResponseStatus'
  Int ->
  DeleteTaskSetResponse
deleteTaskSetResponse pResponseStatus_ =
  DeleteTaskSetResponse'
    { _dtsrsTaskSet = Nothing,
      _dtsrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dtsrsTaskSet :: Lens' DeleteTaskSetResponse (Maybe TaskSet)
dtsrsTaskSet = lens _dtsrsTaskSet (\s a -> s {_dtsrsTaskSet = a})

-- | -- | The response status code.
dtsrsResponseStatus :: Lens' DeleteTaskSetResponse Int
dtsrsResponseStatus = lens _dtsrsResponseStatus (\s a -> s {_dtsrsResponseStatus = a})

instance NFData DeleteTaskSetResponse
