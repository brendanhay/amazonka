{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ECS.DeregisterContainerInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deregisters an Amazon ECS container instance from the specified cluster. This
-- instance will no longer be available to run tasks.
--
-- <http://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_DeregisterContainerInstance.html>
module Network.AWS.ECS.DeregisterContainerInstance
    (
    -- * Request
      DeregisterContainerInstance
    -- ** Request constructor
    , deregisterContainerInstance
    -- ** Request lenses
    , dci1Cluster
    , dci1ContainerInstance
    , dci1Force

    -- * Response
    , DeregisterContainerInstanceResponse
    -- ** Response constructor
    , deregisterContainerInstanceResponse
    -- ** Response lenses
    , dcirContainerInstance
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.ECS.Types
import qualified GHC.Exts

data DeregisterContainerInstance = DeregisterContainerInstance
    { _dci1Cluster           :: Maybe Text
    , _dci1ContainerInstance :: Text
    , _dci1Force             :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'DeregisterContainerInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dci1Cluster' @::@ 'Maybe' 'Text'
--
-- * 'dci1ContainerInstance' @::@ 'Text'
--
-- * 'dci1Force' @::@ 'Maybe' 'Bool'
--
deregisterContainerInstance :: Text -- ^ 'dci1ContainerInstance'
                            -> DeregisterContainerInstance
deregisterContainerInstance p1 = DeregisterContainerInstance
    { _dci1ContainerInstance = p1
    , _dci1Cluster           = Nothing
    , _dci1Force             = Nothing
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts
-- the container instance you want to deregister. If you do not specify a
-- cluster, the default cluster is assumed.
dci1Cluster :: Lens' DeregisterContainerInstance (Maybe Text)
dci1Cluster = lens _dci1Cluster (\s a -> s { _dci1Cluster = a })

-- | The container instance UUID or full Amazon Resource Name (ARN) of the
-- container instance you want to deregister. The ARN contains the 'arn:aws:ecs'
-- namespace, followed by the region of the container instance, the AWS account
-- ID of the container instance owner, the 'container-instance' namespace, and
-- then the container instance UUID. For example, arn:aws:ecs:/region/:/aws_account_id/:container-instance//container_instance_UUID/.
dci1ContainerInstance :: Lens' DeregisterContainerInstance Text
dci1ContainerInstance =
    lens _dci1ContainerInstance (\s a -> s { _dci1ContainerInstance = a })

-- | Force the deregistration of the container instance. You can use the 'force'
-- parameter if you have several tasks running on a container instance and you
-- don't want to run 'StopTask' for each task before deregistering the container
-- instance.
dci1Force :: Lens' DeregisterContainerInstance (Maybe Bool)
dci1Force = lens _dci1Force (\s a -> s { _dci1Force = a })

newtype DeregisterContainerInstanceResponse = DeregisterContainerInstanceResponse
    { _dcirContainerInstance :: Maybe ContainerInstance
    } deriving (Eq, Read, Show)

-- | 'DeregisterContainerInstanceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcirContainerInstance' @::@ 'Maybe' 'ContainerInstance'
--
deregisterContainerInstanceResponse :: DeregisterContainerInstanceResponse
deregisterContainerInstanceResponse = DeregisterContainerInstanceResponse
    { _dcirContainerInstance = Nothing
    }

dcirContainerInstance :: Lens' DeregisterContainerInstanceResponse (Maybe ContainerInstance)
dcirContainerInstance =
    lens _dcirContainerInstance (\s a -> s { _dcirContainerInstance = a })

instance ToPath DeregisterContainerInstance where
    toPath = const "/"

instance ToQuery DeregisterContainerInstance where
    toQuery = const mempty

instance ToHeaders DeregisterContainerInstance

instance ToJSON DeregisterContainerInstance where
    toJSON DeregisterContainerInstance{..} = object
        [ "cluster"           .= _dci1Cluster
        , "containerInstance" .= _dci1ContainerInstance
        , "force"             .= _dci1Force
        ]

instance AWSRequest DeregisterContainerInstance where
    type Sv DeregisterContainerInstance = ECS
    type Rs DeregisterContainerInstance = DeregisterContainerInstanceResponse

    request  = post "DeregisterContainerInstance"
    response = jsonResponse

instance FromJSON DeregisterContainerInstanceResponse where
    parseJSON = withObject "DeregisterContainerInstanceResponse" $ \o -> DeregisterContainerInstanceResponse
        <$> o .:? "containerInstance"
