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
-- Module      : Network.AWS.ECS.UpdateTaskSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a task set. This is used when a service uses the @EXTERNAL@ deployment controller type. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.UpdateTaskSet
  ( -- * Creating a Request
    updateTaskSet,
    UpdateTaskSet,

    -- * Request Lenses
    utsCluster,
    utsService,
    utsTaskSet,
    utsScale,

    -- * Destructuring the Response
    updateTaskSetResponse,
    UpdateTaskSetResponse,

    -- * Response Lenses
    utsrsTaskSet,
    utsrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateTaskSet' smart constructor.
data UpdateTaskSet = UpdateTaskSet'
  { _utsCluster :: !Text,
    _utsService :: !Text,
    _utsTaskSet :: !Text,
    _utsScale :: !Scale
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateTaskSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utsCluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in.
--
-- * 'utsService' - The short name or full Amazon Resource Name (ARN) of the service that the task set exists in.
--
-- * 'utsTaskSet' - The short name or full Amazon Resource Name (ARN) of the task set to update.
--
-- * 'utsScale' - Undocumented member.
updateTaskSet ::
  -- | 'utsCluster'
  Text ->
  -- | 'utsService'
  Text ->
  -- | 'utsTaskSet'
  Text ->
  -- | 'utsScale'
  Scale ->
  UpdateTaskSet
updateTaskSet pCluster_ pService_ pTaskSet_ pScale_ =
  UpdateTaskSet'
    { _utsCluster = pCluster_,
      _utsService = pService_,
      _utsTaskSet = pTaskSet_,
      _utsScale = pScale_
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in.
utsCluster :: Lens' UpdateTaskSet Text
utsCluster = lens _utsCluster (\s a -> s {_utsCluster = a})

-- | The short name or full Amazon Resource Name (ARN) of the service that the task set exists in.
utsService :: Lens' UpdateTaskSet Text
utsService = lens _utsService (\s a -> s {_utsService = a})

-- | The short name or full Amazon Resource Name (ARN) of the task set to update.
utsTaskSet :: Lens' UpdateTaskSet Text
utsTaskSet = lens _utsTaskSet (\s a -> s {_utsTaskSet = a})

-- | Undocumented member.
utsScale :: Lens' UpdateTaskSet Scale
utsScale = lens _utsScale (\s a -> s {_utsScale = a})

instance AWSRequest UpdateTaskSet where
  type Rs UpdateTaskSet = UpdateTaskSetResponse
  request = postJSON ecs
  response =
    receiveJSON
      ( \s h x ->
          UpdateTaskSetResponse'
            <$> (x .?> "taskSet") <*> (pure (fromEnum s))
      )

instance Hashable UpdateTaskSet

instance NFData UpdateTaskSet

instance ToHeaders UpdateTaskSet where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonEC2ContainerServiceV20141113.UpdateTaskSet" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateTaskSet where
  toJSON UpdateTaskSet' {..} =
    object
      ( catMaybes
          [ Just ("cluster" .= _utsCluster),
            Just ("service" .= _utsService),
            Just ("taskSet" .= _utsTaskSet),
            Just ("scale" .= _utsScale)
          ]
      )

instance ToPath UpdateTaskSet where
  toPath = const "/"

instance ToQuery UpdateTaskSet where
  toQuery = const mempty

-- | /See:/ 'updateTaskSetResponse' smart constructor.
data UpdateTaskSetResponse = UpdateTaskSetResponse'
  { _utsrsTaskSet ::
      !(Maybe TaskSet),
    _utsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateTaskSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utsrsTaskSet' - Undocumented member.
--
-- * 'utsrsResponseStatus' - -- | The response status code.
updateTaskSetResponse ::
  -- | 'utsrsResponseStatus'
  Int ->
  UpdateTaskSetResponse
updateTaskSetResponse pResponseStatus_ =
  UpdateTaskSetResponse'
    { _utsrsTaskSet = Nothing,
      _utsrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
utsrsTaskSet :: Lens' UpdateTaskSetResponse (Maybe TaskSet)
utsrsTaskSet = lens _utsrsTaskSet (\s a -> s {_utsrsTaskSet = a})

-- | -- | The response status code.
utsrsResponseStatus :: Lens' UpdateTaskSetResponse Int
utsrsResponseStatus = lens _utsrsResponseStatus (\s a -> s {_utsrsResponseStatus = a})

instance NFData UpdateTaskSetResponse
