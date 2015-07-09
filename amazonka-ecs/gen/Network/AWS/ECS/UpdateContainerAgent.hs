{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.UpdateContainerAgent
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Updates the Amazon ECS container agent on a specified container
-- instance.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_UpdateContainerAgent.html>
module Network.AWS.ECS.UpdateContainerAgent
    (
    -- * Request
      UpdateContainerAgent
    -- ** Request constructor
    , updateContainerAgent
    -- ** Request lenses
    , ucaCluster
    , ucaContainerInstance

    -- * Response
    , UpdateContainerAgentResponse
    -- ** Response constructor
    , updateContainerAgentResponse
    -- ** Response lenses
    , ucarContainerInstance
    , ucarStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateContainerAgent' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ucaCluster'
--
-- * 'ucaContainerInstance'
data UpdateContainerAgent = UpdateContainerAgent'
    { _ucaCluster           :: !(Maybe Text)
    , _ucaContainerInstance :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateContainerAgent' smart constructor.
updateContainerAgent :: Text -> UpdateContainerAgent
updateContainerAgent pContainerInstance =
    UpdateContainerAgent'
    { _ucaCluster = Nothing
    , _ucaContainerInstance = pContainerInstance
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- your container instance is running on. If you do not specify a cluster,
-- the default cluster is assumed.
ucaCluster :: Lens' UpdateContainerAgent (Maybe Text)
ucaCluster = lens _ucaCluster (\ s a -> s{_ucaCluster = a});

-- | The container instance UUID or full Amazon Resource Name (ARN) entries
-- for the container instance on which you would like to update the Amazon
-- ECS container agent.
ucaContainerInstance :: Lens' UpdateContainerAgent Text
ucaContainerInstance = lens _ucaContainerInstance (\ s a -> s{_ucaContainerInstance = a});

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
              ["cluster" .= _ucaCluster,
               "containerInstance" .= _ucaContainerInstance]

instance ToPath UpdateContainerAgent where
        toPath = const "/"

instance ToQuery UpdateContainerAgent where
        toQuery = const mempty

-- | /See:/ 'updateContainerAgentResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ucarContainerInstance'
--
-- * 'ucarStatus'
data UpdateContainerAgentResponse = UpdateContainerAgentResponse'
    { _ucarContainerInstance :: !(Maybe ContainerInstance)
    , _ucarStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateContainerAgentResponse' smart constructor.
updateContainerAgentResponse :: Int -> UpdateContainerAgentResponse
updateContainerAgentResponse pStatus =
    UpdateContainerAgentResponse'
    { _ucarContainerInstance = Nothing
    , _ucarStatus = pStatus
    }

-- | FIXME: Undocumented member.
ucarContainerInstance :: Lens' UpdateContainerAgentResponse (Maybe ContainerInstance)
ucarContainerInstance = lens _ucarContainerInstance (\ s a -> s{_ucarContainerInstance = a});

-- | FIXME: Undocumented member.
ucarStatus :: Lens' UpdateContainerAgentResponse Int
ucarStatus = lens _ucarStatus (\ s a -> s{_ucarStatus = a});
