{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.RegisterTaskDefinition
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Registers a new task definition from the supplied @family@ and
-- @containerDefinitions@. Optionally, you can add data volumes to your
-- containers with the @volumes@ parameter. For more information on task
-- definition parameters and defaults, see
-- <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html Amazon ECS Task Definitions>
-- in the /Amazon EC2 Container Service Developer Guide/.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_RegisterTaskDefinition.html>
module Network.AWS.ECS.RegisterTaskDefinition
    (
    -- * Request
      RegisterTaskDefinition
    -- ** Request constructor
    , registerTaskDefinition
    -- ** Request lenses
    , rtdVolumes
    , rtdFamily
    , rtdContainerDefinitions

    -- * Response
    , RegisterTaskDefinitionResponse
    -- ** Response constructor
    , registerTaskDefinitionResponse
    -- ** Response lenses
    , rtdrsTaskDefinition
    , rtdrsStatus
    ) where

import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'registerTaskDefinition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtdVolumes'
--
-- * 'rtdFamily'
--
-- * 'rtdContainerDefinitions'
data RegisterTaskDefinition = RegisterTaskDefinition'
    { _rtdVolumes              :: !(Maybe [Volume])
    , _rtdFamily               :: !Text
    , _rtdContainerDefinitions :: ![ContainerDefinition]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterTaskDefinition' smart constructor.
registerTaskDefinition :: Text -> RegisterTaskDefinition
registerTaskDefinition pFamily_ =
    RegisterTaskDefinition'
    { _rtdVolumes = Nothing
    , _rtdFamily = pFamily_
    , _rtdContainerDefinitions = mempty
    }

-- | A list of volume definitions in JSON format that containers in your task
-- may use.
rtdVolumes :: Lens' RegisterTaskDefinition [Volume]
rtdVolumes = lens _rtdVolumes (\ s a -> s{_rtdVolumes = a}) . _Default;

-- | You must specify a @family@ for a task definition, which allows you to
-- track multiple versions of the same task definition. You can think of
-- the @family@ as a name for your task definition. Up to 255 letters
-- (uppercase and lowercase), numbers, hyphens, and underscores are
-- allowed.
rtdFamily :: Lens' RegisterTaskDefinition Text
rtdFamily = lens _rtdFamily (\ s a -> s{_rtdFamily = a});

-- | A list of container definitions in JSON format that describe the
-- different containers that make up your task.
rtdContainerDefinitions :: Lens' RegisterTaskDefinition [ContainerDefinition]
rtdContainerDefinitions = lens _rtdContainerDefinitions (\ s a -> s{_rtdContainerDefinitions = a});

instance AWSRequest RegisterTaskDefinition where
        type Sv RegisterTaskDefinition = ECS
        type Rs RegisterTaskDefinition =
             RegisterTaskDefinitionResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 RegisterTaskDefinitionResponse' <$>
                   (x .?> "taskDefinition") <*> (pure (fromEnum s)))

instance ToHeaders RegisterTaskDefinition where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.RegisterTaskDefinition"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterTaskDefinition where
        toJSON RegisterTaskDefinition'{..}
          = object
              ["volumes" .= _rtdVolumes, "family" .= _rtdFamily,
               "containerDefinitions" .= _rtdContainerDefinitions]

instance ToPath RegisterTaskDefinition where
        toPath = const "/"

instance ToQuery RegisterTaskDefinition where
        toQuery = const mempty

-- | /See:/ 'registerTaskDefinitionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtdrsTaskDefinition'
--
-- * 'rtdrsStatus'
data RegisterTaskDefinitionResponse = RegisterTaskDefinitionResponse'
    { _rtdrsTaskDefinition :: !(Maybe TaskDefinition)
    , _rtdrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterTaskDefinitionResponse' smart constructor.
registerTaskDefinitionResponse :: Int -> RegisterTaskDefinitionResponse
registerTaskDefinitionResponse pStatus_ =
    RegisterTaskDefinitionResponse'
    { _rtdrsTaskDefinition = Nothing
    , _rtdrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
rtdrsTaskDefinition :: Lens' RegisterTaskDefinitionResponse (Maybe TaskDefinition)
rtdrsTaskDefinition = lens _rtdrsTaskDefinition (\ s a -> s{_rtdrsTaskDefinition = a});

-- | FIXME: Undocumented member.
rtdrsStatus :: Lens' RegisterTaskDefinitionResponse Int
rtdrsStatus = lens _rtdrsStatus (\ s a -> s{_rtdrsStatus = a});
