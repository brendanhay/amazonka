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
-- Module      : Network.AWS.ECS.DeregisterTaskDefinition
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified task definition by family and revision. Upon
-- deregistration, the task definition is marked as 'INACTIVE'. Existing
-- tasks and services that reference an 'INACTIVE' task definition continue
-- to run without disruption. Existing services that reference an
-- 'INACTIVE' task definition can still scale up or down by modifying the
-- service\'s desired count.
--
-- You cannot use an 'INACTIVE' task definition to run new tasks or create
-- new services, and you cannot update an existing service to reference an
-- 'INACTIVE' task definition (although there may be up to a 10 minute
-- window following deregistration where these restrictions have not yet
-- taken effect).
--
-- /See:/ <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeregisterTaskDefinition.html AWS API Reference> for DeregisterTaskDefinition.
module Network.AWS.ECS.DeregisterTaskDefinition
    (
    -- * Creating a Request
      deregisterTaskDefinition
    , DeregisterTaskDefinition
    -- * Request Lenses
    , derTaskDefinition

    -- * Destructuring the Response
    , deregisterTaskDefinitionResponse
    , DeregisterTaskDefinitionResponse
    -- * Response Lenses
    , dtdrsTaskDefinition
    , dtdrsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.ECS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deregisterTaskDefinition' smart constructor.
newtype DeregisterTaskDefinition = DeregisterTaskDefinition'
    { _derTaskDefinition :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeregisterTaskDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'derTaskDefinition'
deregisterTaskDefinition
    :: Text -- ^ 'derTaskDefinition'
    -> DeregisterTaskDefinition
deregisterTaskDefinition pTaskDefinition_ =
    DeregisterTaskDefinition'
    { _derTaskDefinition = pTaskDefinition_
    }

-- | The 'family' and 'revision' ('family:revision') or full Amazon Resource
-- Name (ARN) of the task definition that you want to deregister. You must
-- specify a 'revision'.
derTaskDefinition :: Lens' DeregisterTaskDefinition Text
derTaskDefinition = lens _derTaskDefinition (\ s a -> s{_derTaskDefinition = a});

instance AWSRequest DeregisterTaskDefinition where
        type Rs DeregisterTaskDefinition =
             DeregisterTaskDefinitionResponse
        request = postJSON eCS
        response
          = receiveJSON
              (\ s h x ->
                 DeregisterTaskDefinitionResponse' <$>
                   (x .?> "taskDefinition") <*> (pure (fromEnum s)))

instance ToHeaders DeregisterTaskDefinition where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.DeregisterTaskDefinition"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeregisterTaskDefinition where
        toJSON DeregisterTaskDefinition'{..}
          = object
              (catMaybes
                 [Just ("taskDefinition" .= _derTaskDefinition)])

instance ToPath DeregisterTaskDefinition where
        toPath = const "/"

instance ToQuery DeregisterTaskDefinition where
        toQuery = const mempty

-- | /See:/ 'deregisterTaskDefinitionResponse' smart constructor.
data DeregisterTaskDefinitionResponse = DeregisterTaskDefinitionResponse'
    { _dtdrsTaskDefinition :: !(Maybe TaskDefinition)
    , _dtdrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeregisterTaskDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtdrsTaskDefinition'
--
-- * 'dtdrsStatus'
deregisterTaskDefinitionResponse
    :: Int -- ^ 'dtdrsStatus'
    -> DeregisterTaskDefinitionResponse
deregisterTaskDefinitionResponse pStatus_ =
    DeregisterTaskDefinitionResponse'
    { _dtdrsTaskDefinition = Nothing
    , _dtdrsStatus = pStatus_
    }

-- | The full description of the deregistered task.
dtdrsTaskDefinition :: Lens' DeregisterTaskDefinitionResponse (Maybe TaskDefinition)
dtdrsTaskDefinition = lens _dtdrsTaskDefinition (\ s a -> s{_dtdrsTaskDefinition = a});

-- | The response status code.
dtdrsStatus :: Lens' DeregisterTaskDefinitionResponse Int
dtdrsStatus = lens _dtdrsStatus (\ s a -> s{_dtdrsStatus = a});
