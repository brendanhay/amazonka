{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ECS.DeregisterTaskDefinition
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | NOT YET IMPLEMENTED.
--
-- Deregisters the specified task definition. You will no longer be able to
-- run tasks from this definition after deregistration.
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
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ECS.Types

-- | /See:/ 'deregisterTaskDefinition' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'derTaskDefinition'
newtype DeregisterTaskDefinition = DeregisterTaskDefinition'{_derTaskDefinition :: Text} deriving (Eq, Read, Show)

-- | 'DeregisterTaskDefinition' smart constructor.
deregisterTaskDefinition :: Text -> DeregisterTaskDefinition
deregisterTaskDefinition pTaskDefinition = DeregisterTaskDefinition'{_derTaskDefinition = pTaskDefinition};

-- | The @family@ and @revision@ (@family:revision@) or full Amazon Resource
-- Name (ARN) of the task definition that you want to deregister.
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
                   x .?> "taskDefinition")

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
newtype DeregisterTaskDefinitionResponse = DeregisterTaskDefinitionResponse'{_dtdrTaskDefinition :: Maybe TaskDefinition} deriving (Eq, Read, Show)

-- | 'DeregisterTaskDefinitionResponse' smart constructor.
deregisterTaskDefinitionResponse :: DeregisterTaskDefinitionResponse
deregisterTaskDefinitionResponse = DeregisterTaskDefinitionResponse'{_dtdrTaskDefinition = Nothing};

-- | The full description of the deregistered task.
dtdrTaskDefinition :: Lens' DeregisterTaskDefinitionResponse (Maybe TaskDefinition)
dtdrTaskDefinition = lens _dtdrTaskDefinition (\ s a -> s{_dtdrTaskDefinition = a});
