{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ECS.Types
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

module Network.AWS.ECS.Types
    (
    -- * Service
      ECS
    -- ** Error
    , RESTError
    -- ** XML
    , ns

    -- * NetworkBinding
    , NetworkBinding
    , networkBinding
    , nbBindIP
    , nbContainerPort
    , nbHostPort

    -- * Cluster
    , Cluster
    , cluster
    , cClusterArn
    , cClusterName
    , cStatus

    -- * ContainerOverride
    , ContainerOverride
    , containerOverride
    , coCommand
    , coName

    -- * KeyValuePair
    , KeyValuePair
    , keyValuePair
    , kvpName
    , kvpValue

    -- * TaskOverride
    , TaskOverride
    , taskOverride
    , toContainerOverrides

    -- * Container
    , Container
    , container
    , cContainerArn
    , cExitCode
    , cLastStatus
    , cName
    , cNetworkBindings
    , cReason
    , cTaskArn

    -- * ContainerDefinition
    , ContainerDefinition
    , containerDefinition
    , cdCommand
    , cdCpu
    , cdEntryPoint
    , cdEnvironment
    , cdEssential
    , cdImage
    , cdLinks
    , cdMemory
    , cdName
    , cdPortMappings

    -- * Resource
    , Resource
    , resource
    , rDoubleValue
    , rIntegerValue
    , rLongValue
    , rName
    , rStringSetValue
    , rType

    -- * Task
    , Task
    , task
    , tClusterArn
    , tContainerInstanceArn
    , tContainers
    , tDesiredStatus
    , tLastStatus
    , tOverrides
    , tTaskArn
    , tTaskDefinitionArn

    -- * PortMapping
    , PortMapping
    , portMapping
    , pmContainerPort
    , pmHostPort

    -- * TaskDefinition
    , TaskDefinition
    , taskDefinition
    , tdContainerDefinitions
    , tdFamily
    , tdRevision
    , tdTaskDefinitionArn

    -- * Failure
    , Failure
    , failure
    , fArn
    , fReason

    -- * ContainerInstance
    , ContainerInstance
    , containerInstance
    , ciAgentConnected
    , ciContainerInstanceArn
    , ciEc2InstanceId
    , ciRegisteredResources
    , ciRemainingResources
    , ciStatus
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2014-11-13@ of the Amazon EC2 Container Service service.
data ECS

instance AWSService ECS where
    type Sg ECS = V4
    type Er ECS = RESTError

    service = service'
      where
        service' :: Service ECS
        service' = Service
            { _svcAbbrev       = "ECS"
            , _svcPrefix       = "ecs"
            , _svcVersion      = "2014-11-13"
            , _svcTargetPrefix = Nothing
            , _svcJSONVersion  = Nothing
            , _svcHandle       = handle
            , _svcRetry        = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError RESTError)
        handle = restError statusSuccess service'

        retry :: Retry ECS
        retry = Exponential
            { _retryBase     = 0.05
            , _retryGrowth   = 2
            , _retryAttempts = 5
            , _retryCheck    = check
            }

        check :: Status
              -> RESTError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e)
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

ns :: Text
ns = "http://ecs.amazonaws.com/doc/2014-11-13/"
{-# INLINE ns #-}

data NetworkBinding = NetworkBinding
    { _nbBindIP        :: Maybe Text
    , _nbContainerPort :: Maybe Int
    , _nbHostPort      :: Maybe Int
    } deriving (Eq, Ord, Read, Show)

-- | 'NetworkBinding' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'nbBindIP' @::@ 'Maybe' 'Text'
--
-- * 'nbContainerPort' @::@ 'Maybe' 'Int'
--
-- * 'nbHostPort' @::@ 'Maybe' 'Int'
--
networkBinding :: NetworkBinding
networkBinding = NetworkBinding
    { _nbBindIP        = Nothing
    , _nbContainerPort = Nothing
    , _nbHostPort      = Nothing
    }

-- | The IP address that the container is bound to on the container instance.
nbBindIP :: Lens' NetworkBinding (Maybe Text)
nbBindIP = lens _nbBindIP (\s a -> s { _nbBindIP = a })

-- | The port number on the container that should be used with the network binding.
nbContainerPort :: Lens' NetworkBinding (Maybe Int)
nbContainerPort = lens _nbContainerPort (\s a -> s { _nbContainerPort = a })

-- | The port number on the host that should be used with the network binding.
nbHostPort :: Lens' NetworkBinding (Maybe Int)
nbHostPort = lens _nbHostPort (\s a -> s { _nbHostPort = a })

instance FromXML NetworkBinding where
    parseXML x = NetworkBinding
        <$> x .@? "bindIP"
        <*> x .@? "containerPort"
        <*> x .@? "hostPort"

instance ToQuery NetworkBinding where
    toQuery NetworkBinding{..} = mconcat
        [ "bindIP"        =? _nbBindIP
        , "containerPort" =? _nbContainerPort
        , "hostPort"      =? _nbHostPort
        ]

data Cluster = Cluster
    { _cClusterArn  :: Maybe Text
    , _cClusterName :: Maybe Text
    , _cStatus      :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'Cluster' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cClusterArn' @::@ 'Maybe' 'Text'
--
-- * 'cClusterName' @::@ 'Maybe' 'Text'
--
-- * 'cStatus' @::@ 'Maybe' 'Text'
--
cluster :: Cluster
cluster = Cluster
    { _cClusterArn  = Nothing
    , _cClusterName = Nothing
    , _cStatus      = Nothing
    }

-- | The Amazon Resource Name (ARN) that identifies the cluster. The ARN contains
-- the 'arn:aws:ecs' namespace, followed by the region of the cluster, the AWS
-- account ID of the cluster owner, the 'cluster' namespace, and then the cluster
-- name. For example, arn:aws:ecs:/region/:/012345678910/:cluster//test/.
cClusterArn :: Lens' Cluster (Maybe Text)
cClusterArn = lens _cClusterArn (\s a -> s { _cClusterArn = a })

-- | A user-generated string that you can use to identify your cluster.
cClusterName :: Lens' Cluster (Maybe Text)
cClusterName = lens _cClusterName (\s a -> s { _cClusterName = a })

-- | The status of the cluster. The valid values are 'ACTIVE' or 'INACTIVE'. 'ACTIVE'
-- indicates that you can register container instances with the cluster and the
-- associated instances can accept tasks.
cStatus :: Lens' Cluster (Maybe Text)
cStatus = lens _cStatus (\s a -> s { _cStatus = a })

instance FromXML Cluster where
    parseXML x = Cluster
        <$> x .@? "clusterArn"
        <*> x .@? "clusterName"
        <*> x .@? "status"

instance ToQuery Cluster where
    toQuery Cluster{..} = mconcat
        [ "clusterArn"  =? _cClusterArn
        , "clusterName" =? _cClusterName
        , "status"      =? _cStatus
        ]

data ContainerOverride = ContainerOverride
    { _coCommand :: List "member" Text
    , _coName    :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ContainerOverride' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'coCommand' @::@ ['Text']
--
-- * 'coName' @::@ 'Maybe' 'Text'
--
containerOverride :: ContainerOverride
containerOverride = ContainerOverride
    { _coName    = Nothing
    , _coCommand = mempty
    }

-- | The command to send to the container that receives the override.
coCommand :: Lens' ContainerOverride [Text]
coCommand = lens _coCommand (\s a -> s { _coCommand = a }) . _List

-- | The name of the container that receives the override.
coName :: Lens' ContainerOverride (Maybe Text)
coName = lens _coName (\s a -> s { _coName = a })

instance FromXML ContainerOverride where
    parseXML x = ContainerOverride
        <$> x .@? "command" .!@ mempty
        <*> x .@? "name"

instance ToQuery ContainerOverride where
    toQuery ContainerOverride{..} = mconcat
        [ "command" =? _coCommand
        , "name"    =? _coName
        ]

data KeyValuePair = KeyValuePair
    { _kvpName  :: Maybe Text
    , _kvpValue :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'KeyValuePair' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'kvpName' @::@ 'Maybe' 'Text'
--
-- * 'kvpValue' @::@ 'Maybe' 'Text'
--
keyValuePair :: KeyValuePair
keyValuePair = KeyValuePair
    { _kvpName  = Nothing
    , _kvpValue = Nothing
    }

-- | The name of the key value pair.
kvpName :: Lens' KeyValuePair (Maybe Text)
kvpName = lens _kvpName (\s a -> s { _kvpName = a })

-- | The value of the key value pair.
kvpValue :: Lens' KeyValuePair (Maybe Text)
kvpValue = lens _kvpValue (\s a -> s { _kvpValue = a })

instance FromXML KeyValuePair where
    parseXML x = KeyValuePair
        <$> x .@? "name"
        <*> x .@? "value"

instance ToQuery KeyValuePair where
    toQuery KeyValuePair{..} = mconcat
        [ "name"  =? _kvpName
        , "value" =? _kvpValue
        ]

newtype TaskOverride = TaskOverride
    { _toContainerOverrides :: List "member" ContainerOverride
    } deriving (Eq, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList TaskOverride where
    type Item TaskOverride = ContainerOverride

    fromList = TaskOverride . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _toContainerOverrides

-- | 'TaskOverride' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'toContainerOverrides' @::@ ['ContainerOverride']
--
taskOverride :: TaskOverride
taskOverride = TaskOverride
    { _toContainerOverrides = mempty
    }

-- | One or more container overrides to send when running a task.
toContainerOverrides :: Lens' TaskOverride [ContainerOverride]
toContainerOverrides =
    lens _toContainerOverrides (\s a -> s { _toContainerOverrides = a })
        . _List

instance FromXML TaskOverride where
    parseXML x = TaskOverride
        <$> x .@? "containerOverrides" .!@ mempty

instance ToQuery TaskOverride where
    toQuery TaskOverride{..} = mconcat
        [ "containerOverrides" =? _toContainerOverrides
        ]

data Container = Container
    { _cContainerArn    :: Maybe Text
    , _cExitCode        :: Maybe Int
    , _cLastStatus      :: Maybe Text
    , _cName            :: Maybe Text
    , _cNetworkBindings :: List "member" NetworkBinding
    , _cReason          :: Maybe Text
    , _cTaskArn         :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'Container' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cContainerArn' @::@ 'Maybe' 'Text'
--
-- * 'cExitCode' @::@ 'Maybe' 'Int'
--
-- * 'cLastStatus' @::@ 'Maybe' 'Text'
--
-- * 'cName' @::@ 'Maybe' 'Text'
--
-- * 'cNetworkBindings' @::@ ['NetworkBinding']
--
-- * 'cReason' @::@ 'Maybe' 'Text'
--
-- * 'cTaskArn' @::@ 'Maybe' 'Text'
--
container :: Container
container = Container
    { _cContainerArn    = Nothing
    , _cTaskArn         = Nothing
    , _cName            = Nothing
    , _cLastStatus      = Nothing
    , _cExitCode        = Nothing
    , _cReason          = Nothing
    , _cNetworkBindings = mempty
    }

-- | The Amazon Resource Name (ARN) of the container.
cContainerArn :: Lens' Container (Maybe Text)
cContainerArn = lens _cContainerArn (\s a -> s { _cContainerArn = a })

-- | The exit code returned from the container.
cExitCode :: Lens' Container (Maybe Int)
cExitCode = lens _cExitCode (\s a -> s { _cExitCode = a })

-- | The last known status of the container.
cLastStatus :: Lens' Container (Maybe Text)
cLastStatus = lens _cLastStatus (\s a -> s { _cLastStatus = a })

-- | The name of the container.
cName :: Lens' Container (Maybe Text)
cName = lens _cName (\s a -> s { _cName = a })

cNetworkBindings :: Lens' Container [NetworkBinding]
cNetworkBindings = lens _cNetworkBindings (\s a -> s { _cNetworkBindings = a }) . _List

-- | A short (255 max characters) human-readable string to provide additional
-- detail about a running or stopped container.
cReason :: Lens' Container (Maybe Text)
cReason = lens _cReason (\s a -> s { _cReason = a })

-- | The Amazon Resource Name (ARN) of the task.
cTaskArn :: Lens' Container (Maybe Text)
cTaskArn = lens _cTaskArn (\s a -> s { _cTaskArn = a })

instance FromXML Container where
    parseXML x = Container
        <$> x .@? "containerArn"
        <*> x .@? "exitCode"
        <*> x .@? "lastStatus"
        <*> x .@? "name"
        <*> x .@? "networkBindings" .!@ mempty
        <*> x .@? "reason"
        <*> x .@? "taskArn"

instance ToQuery Container where
    toQuery Container{..} = mconcat
        [ "containerArn"    =? _cContainerArn
        , "exitCode"        =? _cExitCode
        , "lastStatus"      =? _cLastStatus
        , "name"            =? _cName
        , "networkBindings" =? _cNetworkBindings
        , "reason"          =? _cReason
        , "taskArn"         =? _cTaskArn
        ]

data ContainerDefinition = ContainerDefinition
    { _cdCommand      :: List "member" Text
    , _cdCpu          :: Maybe Int
    , _cdEntryPoint   :: List "member" Text
    , _cdEnvironment  :: List "member" KeyValuePair
    , _cdEssential    :: Maybe Bool
    , _cdImage        :: Maybe Text
    , _cdLinks        :: List "member" Text
    , _cdMemory       :: Maybe Int
    , _cdName         :: Maybe Text
    , _cdPortMappings :: List "member" PortMapping
    } deriving (Eq, Read, Show)

-- | 'ContainerDefinition' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdCommand' @::@ ['Text']
--
-- * 'cdCpu' @::@ 'Maybe' 'Int'
--
-- * 'cdEntryPoint' @::@ ['Text']
--
-- * 'cdEnvironment' @::@ ['KeyValuePair']
--
-- * 'cdEssential' @::@ 'Maybe' 'Bool'
--
-- * 'cdImage' @::@ 'Maybe' 'Text'
--
-- * 'cdLinks' @::@ ['Text']
--
-- * 'cdMemory' @::@ 'Maybe' 'Int'
--
-- * 'cdName' @::@ 'Maybe' 'Text'
--
-- * 'cdPortMappings' @::@ ['PortMapping']
--
containerDefinition :: ContainerDefinition
containerDefinition = ContainerDefinition
    { _cdName         = Nothing
    , _cdImage        = Nothing
    , _cdCpu          = Nothing
    , _cdMemory       = Nothing
    , _cdLinks        = mempty
    , _cdPortMappings = mempty
    , _cdEssential    = Nothing
    , _cdEntryPoint   = mempty
    , _cdCommand      = mempty
    , _cdEnvironment  = mempty
    }

-- | The 'CMD' that is passed to the container. For more information on the Docker 'CMD' parameter, see <https://docs.docker.com/reference/builder/#cmd https://docs.docker.com/reference/builder/#cmd>.
cdCommand :: Lens' ContainerDefinition [Text]
cdCommand = lens _cdCommand (\s a -> s { _cdCommand = a }) . _List

-- | The number of 'cpu' units reserved for the container. A container instance has
-- 1,024 'cpu' units for every CPU core.
cdCpu :: Lens' ContainerDefinition (Maybe Int)
cdCpu = lens _cdCpu (\s a -> s { _cdCpu = a })

-- | The 'ENTRYPOINT' that is passed to the container. For more information on the
-- Docker 'ENTRYPOINT' parameter, see <https://docs.docker.com/reference/builder/#entrypoint https://docs.docker.com/reference/builder/#entrypoint>.
cdEntryPoint :: Lens' ContainerDefinition [Text]
cdEntryPoint = lens _cdEntryPoint (\s a -> s { _cdEntryPoint = a }) . _List

-- | The environment variables to pass to a container.
cdEnvironment :: Lens' ContainerDefinition [KeyValuePair]
cdEnvironment = lens _cdEnvironment (\s a -> s { _cdEnvironment = a }) . _List

-- | If the 'essential' parameter of a container is marked as 'true', the failure of
-- that container will stop the task. If the 'essential' parameter of a container
-- is marked as 'false', then its failure will not affect the rest of the
-- containers in a task.
cdEssential :: Lens' ContainerDefinition (Maybe Bool)
cdEssential = lens _cdEssential (\s a -> s { _cdEssential = a })

-- | The image used to start a container. This string is passed directly to the
-- Docker daemon. Images in the Docker Hub registry are available by default.
-- Other repositories are specified with '/repository-url///image/:/tag/.
cdImage :: Lens' ContainerDefinition (Maybe Text)
cdImage = lens _cdImage (\s a -> s { _cdImage = a })

-- | The 'link' parameter allows containers to communicate with each other without
-- the need for port mappings, using the 'name' parameter. For more information on
-- linking Docker containers, see <https://docs.docker.com/userguide/dockerlinks/ https://docs.docker.com/userguide/dockerlinks/>.
cdLinks :: Lens' ContainerDefinition [Text]
cdLinks = lens _cdLinks (\s a -> s { _cdLinks = a }) . _List

-- | The number of MiB of memory reserved for the container. Docker will allocate
-- a minimum of 4 MiB of memory to a container.
cdMemory :: Lens' ContainerDefinition (Maybe Int)
cdMemory = lens _cdMemory (\s a -> s { _cdMemory = a })

-- | The name of a container. If you are linking multiple containers together in a
-- task definition, the 'name' of one container can be entered in the 'links' of
-- another container to connect the containers.
cdName :: Lens' ContainerDefinition (Maybe Text)
cdName = lens _cdName (\s a -> s { _cdName = a })

-- | The list of port mappings for the container.
cdPortMappings :: Lens' ContainerDefinition [PortMapping]
cdPortMappings = lens _cdPortMappings (\s a -> s { _cdPortMappings = a }) . _List

instance FromXML ContainerDefinition where
    parseXML x = ContainerDefinition
        <$> x .@? "command" .!@ mempty
        <*> x .@? "cpu"
        <*> x .@? "entryPoint" .!@ mempty
        <*> x .@? "environment" .!@ mempty
        <*> x .@? "essential"
        <*> x .@? "image"
        <*> x .@? "links" .!@ mempty
        <*> x .@? "memory"
        <*> x .@? "name"
        <*> x .@? "portMappings" .!@ mempty

instance ToQuery ContainerDefinition where
    toQuery ContainerDefinition{..} = mconcat
        [ "command"      =? _cdCommand
        , "cpu"          =? _cdCpu
        , "entryPoint"   =? _cdEntryPoint
        , "environment"  =? _cdEnvironment
        , "essential"    =? _cdEssential
        , "image"        =? _cdImage
        , "links"        =? _cdLinks
        , "memory"       =? _cdMemory
        , "name"         =? _cdName
        , "portMappings" =? _cdPortMappings
        ]

data Resource = Resource
    { _rDoubleValue    :: Maybe Double
    , _rIntegerValue   :: Maybe Int
    , _rLongValue      :: Maybe Integer
    , _rName           :: Maybe Text
    , _rStringSetValue :: List "member" Text
    , _rType           :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'Resource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rDoubleValue' @::@ 'Maybe' 'Double'
--
-- * 'rIntegerValue' @::@ 'Maybe' 'Int'
--
-- * 'rLongValue' @::@ 'Maybe' 'Integer'
--
-- * 'rName' @::@ 'Maybe' 'Text'
--
-- * 'rStringSetValue' @::@ ['Text']
--
-- * 'rType' @::@ 'Maybe' 'Text'
--
resource :: Resource
resource = Resource
    { _rName           = Nothing
    , _rType           = Nothing
    , _rDoubleValue    = Nothing
    , _rLongValue      = Nothing
    , _rIntegerValue   = Nothing
    , _rStringSetValue = mempty
    }

-- | When the 'doubleValue' type is set, the value of the resource must be a double
-- precision floating-point type.
rDoubleValue :: Lens' Resource (Maybe Double)
rDoubleValue = lens _rDoubleValue (\s a -> s { _rDoubleValue = a })

-- | When the 'integerValue' type is set, the value of the resource must be an
-- integer.
rIntegerValue :: Lens' Resource (Maybe Int)
rIntegerValue = lens _rIntegerValue (\s a -> s { _rIntegerValue = a })

-- | When the 'longValue' type is set, the value of the resource must be an extended
-- precision floating-point type.
rLongValue :: Lens' Resource (Maybe Integer)
rLongValue = lens _rLongValue (\s a -> s { _rLongValue = a })

-- | The name of the resource, such as 'CPU', 'MEMORY', 'PORTS', or a user-defined
-- resource.
rName :: Lens' Resource (Maybe Text)
rName = lens _rName (\s a -> s { _rName = a })

-- | When the 'stringSetValue' type is set, the value of the resource must be a
-- string type.
rStringSetValue :: Lens' Resource [Text]
rStringSetValue = lens _rStringSetValue (\s a -> s { _rStringSetValue = a }) . _List

-- | The type of the resource, such as 'INTEGER', 'DOUBLE', 'LONG', or 'STRINGSET'.
rType :: Lens' Resource (Maybe Text)
rType = lens _rType (\s a -> s { _rType = a })

instance FromXML Resource where
    parseXML x = Resource
        <$> x .@? "doubleValue"
        <*> x .@? "integerValue"
        <*> x .@? "longValue"
        <*> x .@? "name"
        <*> x .@? "stringSetValue" .!@ mempty
        <*> x .@? "type"

instance ToQuery Resource where
    toQuery Resource{..} = mconcat
        [ "doubleValue"    =? _rDoubleValue
        , "integerValue"   =? _rIntegerValue
        , "longValue"      =? _rLongValue
        , "name"           =? _rName
        , "stringSetValue" =? _rStringSetValue
        , "type"           =? _rType
        ]

data Task = Task
    { _tClusterArn           :: Maybe Text
    , _tContainerInstanceArn :: Maybe Text
    , _tContainers           :: List "member" Container
    , _tDesiredStatus        :: Maybe Text
    , _tLastStatus           :: Maybe Text
    , _tOverrides            :: Maybe TaskOverride
    , _tTaskArn              :: Maybe Text
    , _tTaskDefinitionArn    :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'Task' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tClusterArn' @::@ 'Maybe' 'Text'
--
-- * 'tContainerInstanceArn' @::@ 'Maybe' 'Text'
--
-- * 'tContainers' @::@ ['Container']
--
-- * 'tDesiredStatus' @::@ 'Maybe' 'Text'
--
-- * 'tLastStatus' @::@ 'Maybe' 'Text'
--
-- * 'tOverrides' @::@ 'Maybe' 'TaskOverride'
--
-- * 'tTaskArn' @::@ 'Maybe' 'Text'
--
-- * 'tTaskDefinitionArn' @::@ 'Maybe' 'Text'
--
task :: Task
task = Task
    { _tTaskArn              = Nothing
    , _tClusterArn           = Nothing
    , _tTaskDefinitionArn    = Nothing
    , _tContainerInstanceArn = Nothing
    , _tOverrides            = Nothing
    , _tLastStatus           = Nothing
    , _tDesiredStatus        = Nothing
    , _tContainers           = mempty
    }

-- | The Amazon Resource Name (ARN) of the of the cluster that hosts the task.
tClusterArn :: Lens' Task (Maybe Text)
tClusterArn = lens _tClusterArn (\s a -> s { _tClusterArn = a })

-- | The Amazon Resource Name (ARN) of the container instances that host the task.
tContainerInstanceArn :: Lens' Task (Maybe Text)
tContainerInstanceArn =
    lens _tContainerInstanceArn (\s a -> s { _tContainerInstanceArn = a })

-- | The containers associated with the task.
tContainers :: Lens' Task [Container]
tContainers = lens _tContainers (\s a -> s { _tContainers = a }) . _List

-- | The desired status of the task.
tDesiredStatus :: Lens' Task (Maybe Text)
tDesiredStatus = lens _tDesiredStatus (\s a -> s { _tDesiredStatus = a })

-- | The last known status of the task.
tLastStatus :: Lens' Task (Maybe Text)
tLastStatus = lens _tLastStatus (\s a -> s { _tLastStatus = a })

-- | One or more container overrides.
tOverrides :: Lens' Task (Maybe TaskOverride)
tOverrides = lens _tOverrides (\s a -> s { _tOverrides = a })

-- | The Amazon Resource Name (ARN) of the task.
tTaskArn :: Lens' Task (Maybe Text)
tTaskArn = lens _tTaskArn (\s a -> s { _tTaskArn = a })

-- | The Amazon Resource Name (ARN) of the of the task definition that creates the
-- task.
tTaskDefinitionArn :: Lens' Task (Maybe Text)
tTaskDefinitionArn =
    lens _tTaskDefinitionArn (\s a -> s { _tTaskDefinitionArn = a })

instance FromXML Task where
    parseXML x = Task
        <$> x .@? "clusterArn"
        <*> x .@? "containerInstanceArn"
        <*> x .@? "containers" .!@ mempty
        <*> x .@? "desiredStatus"
        <*> x .@? "lastStatus"
        <*> x .@? "overrides"
        <*> x .@? "taskArn"
        <*> x .@? "taskDefinitionArn"

instance ToQuery Task where
    toQuery Task{..} = mconcat
        [ "clusterArn"           =? _tClusterArn
        , "containerInstanceArn" =? _tContainerInstanceArn
        , "containers"           =? _tContainers
        , "desiredStatus"        =? _tDesiredStatus
        , "lastStatus"           =? _tLastStatus
        , "overrides"            =? _tOverrides
        , "taskArn"              =? _tTaskArn
        , "taskDefinitionArn"    =? _tTaskDefinitionArn
        ]

data PortMapping = PortMapping
    { _pmContainerPort :: Maybe Int
    , _pmHostPort      :: Maybe Int
    } deriving (Eq, Ord, Read, Show)

-- | 'PortMapping' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pmContainerPort' @::@ 'Maybe' 'Int'
--
-- * 'pmHostPort' @::@ 'Maybe' 'Int'
--
portMapping :: PortMapping
portMapping = PortMapping
    { _pmContainerPort = Nothing
    , _pmHostPort      = Nothing
    }

-- | The port number on the container that should be used with the port mapping.
pmContainerPort :: Lens' PortMapping (Maybe Int)
pmContainerPort = lens _pmContainerPort (\s a -> s { _pmContainerPort = a })

-- | The port number on the host that should be used with the port mapping.
pmHostPort :: Lens' PortMapping (Maybe Int)
pmHostPort = lens _pmHostPort (\s a -> s { _pmHostPort = a })

instance FromXML PortMapping where
    parseXML x = PortMapping
        <$> x .@? "containerPort"
        <*> x .@? "hostPort"

instance ToQuery PortMapping where
    toQuery PortMapping{..} = mconcat
        [ "containerPort" =? _pmContainerPort
        , "hostPort"      =? _pmHostPort
        ]

data TaskDefinition = TaskDefinition
    { _tdContainerDefinitions :: List "member" ContainerDefinition
    , _tdFamily               :: Maybe Text
    , _tdRevision             :: Maybe Int
    , _tdTaskDefinitionArn    :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'TaskDefinition' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tdContainerDefinitions' @::@ ['ContainerDefinition']
--
-- * 'tdFamily' @::@ 'Maybe' 'Text'
--
-- * 'tdRevision' @::@ 'Maybe' 'Int'
--
-- * 'tdTaskDefinitionArn' @::@ 'Maybe' 'Text'
--
taskDefinition :: TaskDefinition
taskDefinition = TaskDefinition
    { _tdTaskDefinitionArn    = Nothing
    , _tdContainerDefinitions = mempty
    , _tdFamily               = Nothing
    , _tdRevision             = Nothing
    }

-- | A list of container definitions in JSON format that describe the different
-- containers that make up your task.
tdContainerDefinitions :: Lens' TaskDefinition [ContainerDefinition]
tdContainerDefinitions =
    lens _tdContainerDefinitions (\s a -> s { _tdContainerDefinitions = a })
        . _List

-- | The family of your task definition. You can think of the 'family' as the name
-- of your task definition.
tdFamily :: Lens' TaskDefinition (Maybe Text)
tdFamily = lens _tdFamily (\s a -> s { _tdFamily = a })

-- | The revision of the task in a particular family. You can think of the
-- revision as a version number of a task definition in a family. When you
-- register a task definition for the first time, the revision is '1', and each
-- time you register a task definition in the same family, the revision value
-- increases by one.
tdRevision :: Lens' TaskDefinition (Maybe Int)
tdRevision = lens _tdRevision (\s a -> s { _tdRevision = a })

-- | The full Amazon Resource Name (ARN) of the of the task definition.
tdTaskDefinitionArn :: Lens' TaskDefinition (Maybe Text)
tdTaskDefinitionArn =
    lens _tdTaskDefinitionArn (\s a -> s { _tdTaskDefinitionArn = a })

instance FromXML TaskDefinition where
    parseXML x = TaskDefinition
        <$> x .@? "containerDefinitions" .!@ mempty
        <*> x .@? "family"
        <*> x .@? "revision"
        <*> x .@? "taskDefinitionArn"

instance ToQuery TaskDefinition where
    toQuery TaskDefinition{..} = mconcat
        [ "containerDefinitions" =? _tdContainerDefinitions
        , "family"               =? _tdFamily
        , "revision"             =? _tdRevision
        , "taskDefinitionArn"    =? _tdTaskDefinitionArn
        ]

data Failure = Failure
    { _fArn    :: Maybe Text
    , _fReason :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'Failure' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fArn' @::@ 'Maybe' 'Text'
--
-- * 'fReason' @::@ 'Maybe' 'Text'
--
failure :: Failure
failure = Failure
    { _fArn    = Nothing
    , _fReason = Nothing
    }

-- | The Amazon Resource Name (ARN) of the failed resource.
fArn :: Lens' Failure (Maybe Text)
fArn = lens _fArn (\s a -> s { _fArn = a })

-- | The reason for the failure.
fReason :: Lens' Failure (Maybe Text)
fReason = lens _fReason (\s a -> s { _fReason = a })

instance FromXML Failure where
    parseXML x = Failure
        <$> x .@? "arn"
        <*> x .@? "reason"

instance ToQuery Failure where
    toQuery Failure{..} = mconcat
        [ "arn"    =? _fArn
        , "reason" =? _fReason
        ]

data ContainerInstance = ContainerInstance
    { _ciAgentConnected       :: Maybe Bool
    , _ciContainerInstanceArn :: Maybe Text
    , _ciEc2InstanceId        :: Maybe Text
    , _ciRegisteredResources  :: List "member" Resource
    , _ciRemainingResources   :: List "member" Resource
    , _ciStatus               :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ContainerInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ciAgentConnected' @::@ 'Maybe' 'Bool'
--
-- * 'ciContainerInstanceArn' @::@ 'Maybe' 'Text'
--
-- * 'ciEc2InstanceId' @::@ 'Maybe' 'Text'
--
-- * 'ciRegisteredResources' @::@ ['Resource']
--
-- * 'ciRemainingResources' @::@ ['Resource']
--
-- * 'ciStatus' @::@ 'Maybe' 'Text'
--
containerInstance :: ContainerInstance
containerInstance = ContainerInstance
    { _ciContainerInstanceArn = Nothing
    , _ciEc2InstanceId        = Nothing
    , _ciRemainingResources   = mempty
    , _ciRegisteredResources  = mempty
    , _ciStatus               = Nothing
    , _ciAgentConnected       = Nothing
    }

-- | This parameter returns 'true' if the agent is actually connected to Amazon ECS.
-- Registered instances with an agent that may be unhealthy or stopped will
-- return 'false', and instances without a connected agent cannot accept placement
-- request.
ciAgentConnected :: Lens' ContainerInstance (Maybe Bool)
ciAgentConnected = lens _ciAgentConnected (\s a -> s { _ciAgentConnected = a })

-- | The Amazon Resource Name (ARN) of the container instance. The ARN contains
-- the 'arn:aws:ecs' namespace, followed by the region of the container instance,
-- the AWS account ID of the container instance owner, the 'container-instance'
-- namespace, and then the container instance UUID. For example, arn:aws:ecs:/region/:/aws_account_id/:container-instance//container_instance_UUID/.
ciContainerInstanceArn :: Lens' ContainerInstance (Maybe Text)
ciContainerInstanceArn =
    lens _ciContainerInstanceArn (\s a -> s { _ciContainerInstanceArn = a })

-- | The Amazon EC2 instance ID of the container instance.
ciEc2InstanceId :: Lens' ContainerInstance (Maybe Text)
ciEc2InstanceId = lens _ciEc2InstanceId (\s a -> s { _ciEc2InstanceId = a })

-- | The registered resources on the container instance that are in use by current
-- tasks.
ciRegisteredResources :: Lens' ContainerInstance [Resource]
ciRegisteredResources =
    lens _ciRegisteredResources (\s a -> s { _ciRegisteredResources = a })
        . _List

-- | The remaining resources of the container instance that are available for new
-- tasks.
ciRemainingResources :: Lens' ContainerInstance [Resource]
ciRemainingResources =
    lens _ciRemainingResources (\s a -> s { _ciRemainingResources = a })
        . _List

-- | The status of the container instance. The valid values are 'ACTIVE' or 'INACTIVE'.
-- 'ACTIVE' indicates that the container instance can accept tasks.
ciStatus :: Lens' ContainerInstance (Maybe Text)
ciStatus = lens _ciStatus (\s a -> s { _ciStatus = a })

instance FromXML ContainerInstance where
    parseXML x = ContainerInstance
        <$> x .@? "agentConnected"
        <*> x .@? "containerInstanceArn"
        <*> x .@? "ec2InstanceId"
        <*> x .@? "registeredResources" .!@ mempty
        <*> x .@? "remainingResources" .!@ mempty
        <*> x .@? "status"

instance ToQuery ContainerInstance where
    toQuery ContainerInstance{..} = mconcat
        [ "agentConnected"       =? _ciAgentConnected
        , "containerInstanceArn" =? _ciContainerInstanceArn
        , "ec2InstanceId"        =? _ciEc2InstanceId
        , "registeredResources"  =? _ciRegisteredResources
        , "remainingResources"   =? _ciRemainingResources
        , "status"               =? _ciStatus
        ]
