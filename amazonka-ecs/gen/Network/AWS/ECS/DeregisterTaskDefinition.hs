{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeregisterTaskDefinition
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Deregisters the specified task definition by family and revision. Upon
-- deregistration, the task definition is marked as @INACTIVE@. Existing
-- tasks and services that reference an @INACTIVE@ task definition continue
-- to run without disruption. Existing services that reference an
-- @INACTIVE@ task definition can still scale up or down by modifying the
-- service\'s desired count.
--
-- You cannot use an @INACTIVE@ task definition to run new tasks or create
-- new services, and you cannot update an existing service to reference an
-- @INACTIVE@ task definition (although there may be up to a 10 minute
-- window following deregistration where these restrictions have not yet
-- taken effect).
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeregisterTaskDefinition.html>
module Network.AWS.ECS.DeregisterTaskDefinition
    (
    -- * Request
      DeregisterTaskDefinition
    -- ** Request constructor
    , deregisterTaskDefinition
    -- ** Request lenses
    , derTaskDefinition

    -- * Response
    , DeregisterTaskDefinitionResponse
    -- ** Response constructor
    , deregisterTaskDefinitionResponse
    -- ** Response lenses
    , dtdrTaskDefinition
    , dtdrStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deregisterTaskDefinition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derTaskDefinition'
newtype DeregisterTaskDefinition = DeregisterTaskDefinition'
    { _derTaskDefinition :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeregisterTaskDefinition' smart constructor.
deregisterTaskDefinition :: Text -> DeregisterTaskDefinition
deregisterTaskDefinition pTaskDefinition =
    DeregisterTaskDefinition'
    { _derTaskDefinition = pTaskDefinition
    }

-- | The @family@ and @revision@ (@family:revision@) or full Amazon Resource
-- Name (ARN) of the task definition that you want to deregister. You must
-- specify a @revision@.
derTaskDefinition :: Lens' DeregisterTaskDefinition Text
derTaskDefinition = lens _derTaskDefinition (\ s a -> s{_derTaskDefinition = a});

instance AWSRequest DeregisterTaskDefinition where
        type Sv DeregisterTaskDefinition = ECS
        type Rs DeregisterTaskDefinition =
             DeregisterTaskDefinitionResponse
        request = postJSON
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
          = object ["taskDefinition" .= _derTaskDefinition]

instance ToPath DeregisterTaskDefinition where
        toPath = const "/"

instance ToQuery DeregisterTaskDefinition where
        toQuery = const mempty

-- | /See:/ 'deregisterTaskDefinitionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtdrTaskDefinition'
--
-- * 'dtdrStatus'
data DeregisterTaskDefinitionResponse = DeregisterTaskDefinitionResponse'
    { _dtdrTaskDefinition :: !(Maybe TaskDefinition)
    , _dtdrStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeregisterTaskDefinitionResponse' smart constructor.
deregisterTaskDefinitionResponse :: Int -> DeregisterTaskDefinitionResponse
deregisterTaskDefinitionResponse pStatus =
    DeregisterTaskDefinitionResponse'
    { _dtdrTaskDefinition = Nothing
    , _dtdrStatus = pStatus
    }

-- | The full description of the deregistered task.
dtdrTaskDefinition :: Lens' DeregisterTaskDefinitionResponse (Maybe TaskDefinition)
dtdrTaskDefinition = lens _dtdrTaskDefinition (\ s a -> s{_dtdrTaskDefinition = a});

-- | FIXME: Undocumented member.
dtdrStatus :: Lens' DeregisterTaskDefinitionResponse Int
dtdrStatus = lens _dtdrStatus (\ s a -> s{_dtdrStatus = a});
