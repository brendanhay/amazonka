{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.UpdateContainerAgent
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates the Amazon ECS container agent on a specified container
-- instance. Updating the Amazon ECS container agent does not interrupt
-- running tasks or services on the container instance. The process for
-- updating the agent differs depending on whether your container instance
-- was launched with the Amazon ECS-optimized AMI or another operating
-- system.
--
-- @UpdateContainerAgent@ requires the Amazon ECS-optimized AMI or Amazon
-- Linux with the @ecs-init@ service installed and running. For help
-- updating the Amazon ECS container agent on other operating systems, see
-- <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html#manually_update_agent Manually Updating the Amazon ECS Container Agent>
-- in the /Amazon EC2 Container Service Developer Guide/.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_UpdateContainerAgent.html>
module Network.AWS.ECS.UpdateContainerAgent
    (
    -- * Request
      UpdateContainerAgent
    -- ** Request constructor
    , updateContainerAgent
    -- ** Request lenses
    , ucarqCluster
    , ucarqContainerInstance

    -- * Response
    , UpdateContainerAgentResponse
    -- ** Response constructor
    , updateContainerAgentResponse
    -- ** Response lenses
    , ucarsContainerInstance
    , ucarsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateContainerAgent' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ucarqCluster'
--
-- * 'ucarqContainerInstance'
data UpdateContainerAgent = UpdateContainerAgent'
    { _ucarqCluster           :: !(Maybe Text)
    , _ucarqContainerInstance :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateContainerAgent' smart constructor.
updateContainerAgent :: Text -> UpdateContainerAgent
updateContainerAgent pContainerInstance_ =
    UpdateContainerAgent'
    { _ucarqCluster = Nothing
    , _ucarqContainerInstance = pContainerInstance_
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- your container instance is running on. If you do not specify a cluster,
-- the default cluster is assumed.
ucarqCluster :: Lens' UpdateContainerAgent (Maybe Text)
ucarqCluster = lens _ucarqCluster (\ s a -> s{_ucarqCluster = a});

-- | The container instance UUID or full Amazon Resource Name (ARN) entries
-- for the container instance on which you would like to update the Amazon
-- ECS container agent.
ucarqContainerInstance :: Lens' UpdateContainerAgent Text
ucarqContainerInstance = lens _ucarqContainerInstance (\ s a -> s{_ucarqContainerInstance = a});

instance AWSRequest UpdateContainerAgent where
        type Sv UpdateContainerAgent = ECS
        type Rs UpdateContainerAgent =
             UpdateContainerAgentResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateContainerAgentResponse' <$>
                   (x .?> "containerInstance") <*> (pure (fromEnum s)))

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
              ["cluster" .= _ucarqCluster,
               "containerInstance" .= _ucarqContainerInstance]

instance ToPath UpdateContainerAgent where
        toPath = const "/"

instance ToQuery UpdateContainerAgent where
        toQuery = const mempty

-- | /See:/ 'updateContainerAgentResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ucarsContainerInstance'
--
-- * 'ucarsStatus'
data UpdateContainerAgentResponse = UpdateContainerAgentResponse'
    { _ucarsContainerInstance :: !(Maybe ContainerInstance)
    , _ucarsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateContainerAgentResponse' smart constructor.
updateContainerAgentResponse :: Int -> UpdateContainerAgentResponse
updateContainerAgentResponse pStatus_ =
    UpdateContainerAgentResponse'
    { _ucarsContainerInstance = Nothing
    , _ucarsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
ucarsContainerInstance :: Lens' UpdateContainerAgentResponse (Maybe ContainerInstance)
ucarsContainerInstance = lens _ucarsContainerInstance (\ s a -> s{_ucarsContainerInstance = a});

-- | FIXME: Undocumented member.
ucarsStatus :: Lens' UpdateContainerAgentResponse Int
ucarsStatus = lens _ucarsStatus (\ s a -> s{_ucarsStatus = a});
