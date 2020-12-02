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
-- Module      : Network.AWS.ECS.UpdateContainerAgent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Amazon ECS container agent on a specified container instance. Updating the Amazon ECS container agent does not interrupt running tasks or services on the container instance. The process for updating the agent differs depending on whether your container instance was launched with the Amazon ECS-optimized AMI or another operating system.
--
--
-- @UpdateContainerAgent@ requires the Amazon ECS-optimized AMI or Amazon Linux with the @ecs-init@ service installed and running. For help updating the Amazon ECS container agent on other operating systems, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html#manually_update_agent Manually Updating the Amazon ECS Container Agent> in the /Amazon Elastic Container Service Developer Guide/ .
--
module Network.AWS.ECS.UpdateContainerAgent
    (
    -- * Creating a Request
      updateContainerAgent
    , UpdateContainerAgent
    -- * Request Lenses
    , ucaCluster
    , ucaContainerInstance

    -- * Destructuring the Response
    , updateContainerAgentResponse
    , UpdateContainerAgentResponse
    -- * Response Lenses
    , ucarsContainerInstance
    , ucarsResponseStatus
    ) where

import Network.AWS.ECS.Types
import Network.AWS.ECS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateContainerAgent' smart constructor.
data UpdateContainerAgent = UpdateContainerAgent'
  { _ucaCluster           :: !(Maybe Text)
  , _ucaContainerInstance :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateContainerAgent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucaCluster' - The short name or full Amazon Resource Name (ARN) of the cluster that your container instance is running on. If you do not specify a cluster, the default cluster is assumed.
--
-- * 'ucaContainerInstance' - The container instance ID or full ARN entries for the container instance on which you would like to update the Amazon ECS container agent.
updateContainerAgent
    :: Text -- ^ 'ucaContainerInstance'
    -> UpdateContainerAgent
updateContainerAgent pContainerInstance_ =
  UpdateContainerAgent'
    {_ucaCluster = Nothing, _ucaContainerInstance = pContainerInstance_}


-- | The short name or full Amazon Resource Name (ARN) of the cluster that your container instance is running on. If you do not specify a cluster, the default cluster is assumed.
ucaCluster :: Lens' UpdateContainerAgent (Maybe Text)
ucaCluster = lens _ucaCluster (\ s a -> s{_ucaCluster = a})

-- | The container instance ID or full ARN entries for the container instance on which you would like to update the Amazon ECS container agent.
ucaContainerInstance :: Lens' UpdateContainerAgent Text
ucaContainerInstance = lens _ucaContainerInstance (\ s a -> s{_ucaContainerInstance = a})

instance AWSRequest UpdateContainerAgent where
        type Rs UpdateContainerAgent =
             UpdateContainerAgentResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 UpdateContainerAgentResponse' <$>
                   (x .?> "containerInstance") <*> (pure (fromEnum s)))

instance Hashable UpdateContainerAgent where

instance NFData UpdateContainerAgent where

instance ToHeaders UpdateContainerAgent where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.UpdateContainerAgent"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateContainerAgent where
        toJSON UpdateContainerAgent'{..}
          = object
              (catMaybes
                 [("cluster" .=) <$> _ucaCluster,
                  Just ("containerInstance" .= _ucaContainerInstance)])

instance ToPath UpdateContainerAgent where
        toPath = const "/"

instance ToQuery UpdateContainerAgent where
        toQuery = const mempty

-- | /See:/ 'updateContainerAgentResponse' smart constructor.
data UpdateContainerAgentResponse = UpdateContainerAgentResponse'
  { _ucarsContainerInstance :: !(Maybe ContainerInstance)
  , _ucarsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateContainerAgentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucarsContainerInstance' - The container instance for which the container agent was updated.
--
-- * 'ucarsResponseStatus' - -- | The response status code.
updateContainerAgentResponse
    :: Int -- ^ 'ucarsResponseStatus'
    -> UpdateContainerAgentResponse
updateContainerAgentResponse pResponseStatus_ =
  UpdateContainerAgentResponse'
    {_ucarsContainerInstance = Nothing, _ucarsResponseStatus = pResponseStatus_}


-- | The container instance for which the container agent was updated.
ucarsContainerInstance :: Lens' UpdateContainerAgentResponse (Maybe ContainerInstance)
ucarsContainerInstance = lens _ucarsContainerInstance (\ s a -> s{_ucarsContainerInstance = a})

-- | -- | The response status code.
ucarsResponseStatus :: Lens' UpdateContainerAgentResponse Int
ucarsResponseStatus = lens _ucarsResponseStatus (\ s a -> s{_ucarsResponseStatus = a})

instance NFData UpdateContainerAgentResponse where
