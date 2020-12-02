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
-- Module      : Network.AWS.ECS.UpdateServicePrimaryTaskSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies which task set in a service is the primary task set. Any parameters that are updated on the primary task set in a service will transition to the service. This is used when a service uses the @EXTERNAL@ deployment controller type. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.UpdateServicePrimaryTaskSet
  ( -- * Creating a Request
    updateServicePrimaryTaskSet,
    UpdateServicePrimaryTaskSet,

    -- * Request Lenses
    usptsCluster,
    usptsService,
    usptsPrimaryTaskSet,

    -- * Destructuring the Response
    updateServicePrimaryTaskSetResponse,
    UpdateServicePrimaryTaskSetResponse,

    -- * Response Lenses
    usptsrsTaskSet,
    usptsrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateServicePrimaryTaskSet' smart constructor.
data UpdateServicePrimaryTaskSet = UpdateServicePrimaryTaskSet'
  { _usptsCluster ::
      !Text,
    _usptsService :: !Text,
    _usptsPrimaryTaskSet :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateServicePrimaryTaskSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usptsCluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in.
--
-- * 'usptsService' - The short name or full Amazon Resource Name (ARN) of the service that the task set exists in.
--
-- * 'usptsPrimaryTaskSet' - The short name or full Amazon Resource Name (ARN) of the task set to set as the primary task set in the deployment.
updateServicePrimaryTaskSet ::
  -- | 'usptsCluster'
  Text ->
  -- | 'usptsService'
  Text ->
  -- | 'usptsPrimaryTaskSet'
  Text ->
  UpdateServicePrimaryTaskSet
updateServicePrimaryTaskSet pCluster_ pService_ pPrimaryTaskSet_ =
  UpdateServicePrimaryTaskSet'
    { _usptsCluster = pCluster_,
      _usptsService = pService_,
      _usptsPrimaryTaskSet = pPrimaryTaskSet_
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task set exists in.
usptsCluster :: Lens' UpdateServicePrimaryTaskSet Text
usptsCluster = lens _usptsCluster (\s a -> s {_usptsCluster = a})

-- | The short name or full Amazon Resource Name (ARN) of the service that the task set exists in.
usptsService :: Lens' UpdateServicePrimaryTaskSet Text
usptsService = lens _usptsService (\s a -> s {_usptsService = a})

-- | The short name or full Amazon Resource Name (ARN) of the task set to set as the primary task set in the deployment.
usptsPrimaryTaskSet :: Lens' UpdateServicePrimaryTaskSet Text
usptsPrimaryTaskSet = lens _usptsPrimaryTaskSet (\s a -> s {_usptsPrimaryTaskSet = a})

instance AWSRequest UpdateServicePrimaryTaskSet where
  type
    Rs UpdateServicePrimaryTaskSet =
      UpdateServicePrimaryTaskSetResponse
  request = postJSON ecs
  response =
    receiveJSON
      ( \s h x ->
          UpdateServicePrimaryTaskSetResponse'
            <$> (x .?> "taskSet") <*> (pure (fromEnum s))
      )

instance Hashable UpdateServicePrimaryTaskSet

instance NFData UpdateServicePrimaryTaskSet

instance ToHeaders UpdateServicePrimaryTaskSet where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AmazonEC2ContainerServiceV20141113.UpdateServicePrimaryTaskSet" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateServicePrimaryTaskSet where
  toJSON UpdateServicePrimaryTaskSet' {..} =
    object
      ( catMaybes
          [ Just ("cluster" .= _usptsCluster),
            Just ("service" .= _usptsService),
            Just ("primaryTaskSet" .= _usptsPrimaryTaskSet)
          ]
      )

instance ToPath UpdateServicePrimaryTaskSet where
  toPath = const "/"

instance ToQuery UpdateServicePrimaryTaskSet where
  toQuery = const mempty

-- | /See:/ 'updateServicePrimaryTaskSetResponse' smart constructor.
data UpdateServicePrimaryTaskSetResponse = UpdateServicePrimaryTaskSetResponse'
  { _usptsrsTaskSet ::
      !(Maybe TaskSet),
    _usptsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateServicePrimaryTaskSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usptsrsTaskSet' - Undocumented member.
--
-- * 'usptsrsResponseStatus' - -- | The response status code.
updateServicePrimaryTaskSetResponse ::
  -- | 'usptsrsResponseStatus'
  Int ->
  UpdateServicePrimaryTaskSetResponse
updateServicePrimaryTaskSetResponse pResponseStatus_ =
  UpdateServicePrimaryTaskSetResponse'
    { _usptsrsTaskSet = Nothing,
      _usptsrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
usptsrsTaskSet :: Lens' UpdateServicePrimaryTaskSetResponse (Maybe TaskSet)
usptsrsTaskSet = lens _usptsrsTaskSet (\s a -> s {_usptsrsTaskSet = a})

-- | -- | The response status code.
usptsrsResponseStatus :: Lens' UpdateServicePrimaryTaskSetResponse Int
usptsrsResponseStatus = lens _usptsrsResponseStatus (\s a -> s {_usptsrsResponseStatus = a})

instance NFData UpdateServicePrimaryTaskSetResponse
