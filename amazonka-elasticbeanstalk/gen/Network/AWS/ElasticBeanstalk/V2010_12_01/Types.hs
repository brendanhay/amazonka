{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.V2010_12_01.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS Elastic Beanstalk is an easy-to-use service for deploying and scaling
-- web applications and services developed with Java, .NET, PHP, Node.js,
-- Python, Ruby, and Docker on familiar servers such as Apache HTTP Server,
-- Apache Tomcat, Nginx, Passenger, and IIS 7.5/8. You can simply upload your
-- code and Elastic Beanstalk automatically handles the deployment, from
-- capacity provisioning, load balancing, auto-scaling to application health
-- monitoring. At the same time, you retain full control over the AWS
-- resources powering your application and can access the underlying resources
-- at any time. There is no additional charge for Elastic Beanstalk - you pay
-- only for the AWS resources needed to store and run your applications.
module Network.AWS.ElasticBeanstalk.V2010_12_01.Types
    (
    -- * Service
      ElasticBeanstalk
    -- ** Errors
    , Er (..)
    -- ** XML
    , xmlOptions

    -- * ConfigurationDeploymentStatus
    , ConfigurationDeploymentStatus (..)

    -- * ConfigurationOptionValueType
    , ConfigurationOptionValueType (..)

    -- * EnvironmentHealth
    , EnvironmentHealth (..)

    -- * EnvironmentInfoType
    , EnvironmentInfoType (..)

    -- * EnvironmentStatus
    , EnvironmentStatus (..)

    -- * EventSeverity
    , EventSeverity (..)

    -- * ValidationSeverity
    , ValidationSeverity (..)

    -- * AutoScalingGroup
    , AutoScalingGroup (..)
    , asgName

    -- * EnvironmentResourcesDescription
    , EnvironmentResourcesDescription (..)
    , erdLoadBalancer

    -- * Instance
    , Instance (..)
    , rId

    -- * LaunchConfiguration
    , LaunchConfiguration (..)
    , lcName

    -- * LoadBalancer
    , LoadBalancer (..)
    , lbName

    -- * Trigger
    , Trigger (..)
    , trName

    -- * ApplicationDescription
    , ApplicationDescription (..)
    , adApplicationName
    , adDescription
    , adDateCreated
    , adDateUpdated
    , adVersions
    , adConfigurationTemplates

    -- * ApplicationVersionDescription
    , ApplicationVersionDescription (..)
    , avdApplicationName
    , avdDescription
    , avdVersionLabel
    , avdSourceBundle
    , avdDateCreated
    , avdDateUpdated

    -- * ConfigurationOptionDescription
    , ConfigurationOptionDescription (..)
    , coeNamespace
    , coeName
    , coeDefaultValue
    , coeChangeSeverity
    , coeUserDefined
    , coeValueType
    , coeValueOptions
    , coeMinValue
    , coeMaxValue
    , coeMaxLength
    , coeRegex

    -- * ConfigurationOptionSetting
    , ConfigurationOptionSetting (..)
    , cosNamespace
    , cosOptionName
    , cosValue

    -- * ConfigurationSettingsDescription
    , ConfigurationSettingsDescription (..)
    , csfSolutionStackName
    , csfApplicationName
    , csfTemplateName
    , csfDescription
    , csfEnvironmentName
    , csfDeploymentStatus
    , csfDateCreated
    , csfDateUpdated
    , csfOptionSettings

    -- * EnvironmentDescription
    , EnvironmentDescription (..)
    , eeEnvironmentName
    , eeEnvironmentId
    , eeApplicationName
    , eeVersionLabel
    , eeSolutionStackName
    , eeTemplateName
    , eeDescription
    , eeEndpointURL
    , eeCNAME
    , eeDateCreated
    , eeDateUpdated
    , eeStatus
    , eeHealth
    , eeResources
    , eeTier

    -- * EnvironmentInfoDescription
    , EnvironmentInfoDescription (..)
    , eidInfoType
    , eidEc2InstanceId
    , eidSampleTimestamp
    , eidMessage

    -- * EnvironmentResourceDescription
    , EnvironmentResourceDescription (..)
    , ereEnvironmentName
    , ereAutoScalingGroups
    , ereInstances
    , ereLaunchConfigurations
    , ereLoadBalancers
    , ereTriggers
    , ereQueues

    -- * EnvironmentTier
    , EnvironmentTier (..)
    , etName
    , etType
    , etVersion

    -- * EventDescription
    , EventDescription (..)
    , efEventDate
    , efMessage
    , efApplicationName
    , efVersionLabel
    , efTemplateName
    , efEnvironmentName
    , efRequestId
    , efSeverity

    -- * Listener
    , Listener (..)
    , lProtocol
    , lPort

    -- * LoadBalancerDescription
    , LoadBalancerDescription (..)
    , lbdLoadBalancerName
    , lbdDomain
    , lbdListeners

    -- * OptionRestrictionRegex
    , OptionRestrictionRegex (..)
    , orrPattern
    , orrLabel

    -- * OptionSpecification
    , OptionSpecification (..)
    , osNamespace
    , osOptionName

    -- * Queue
    , Queue (..)
    , qeName
    , qeURL

    -- * S3Location
    , S3Location (..)
    , slS3Bucket
    , slS3Key

    -- * SolutionStackDescription
    , SolutionStackDescription (..)
    , ssdSolutionStackName
    , ssdPermittedFileTypes

    -- * SourceConfiguration
    , SourceConfiguration (..)
    , sdApplicationName
    , sdTemplateName

    -- * Tag
    , Tag (..)
    , wKey
    , wValue

    -- * ValidationMessage
    , ValidationMessage (..)
    , vveMessage
    , vveSeverity
    , vveNamespace
    , vveOptionName

    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2010-12-01@) of the
-- @AWS Elastic Beanstalk@ service.
data ElasticBeanstalk deriving (Typeable)

instance AWSService ElasticBeanstalk where
    type Sg ElasticBeanstalk = V4
    data Er ElasticBeanstalk
        = ElasticBeanstalkClient HttpException
        | ElasticBeanstalkSerializer String
        | ElasticBeanstalkService String
        | InsufficientPrivilegesException
        | OperationInProgressException
        | S3LocationNotInServiceRegionException
        | S3SubscriptionRequiredException
        | SourceBundleDeletionException
        | TooManyApplicationVersionsException
        | TooManyApplicationsException
        | TooManyBucketsException
        | TooManyConfigurationTemplatesException
        | TooManyEnvironmentsException

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "elasticbeanstalk"
        , _svcVersion  = "2010-12-01"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er ElasticBeanstalk)
deriving instance Generic (Er ElasticBeanstalk)

instance AWSError (Er ElasticBeanstalk) where
    awsError = const "ElasticBeanstalkError"

instance AWSServiceError (Er ElasticBeanstalk) where
    serviceError    = ElasticBeanstalkService
    clientError     = ElasticBeanstalkClient
    serializerError = ElasticBeanstalkSerializer

instance Exception (Er ElasticBeanstalk)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "http://elasticbeanstalk.amazonaws.com/docs/2010-12-01/"
    }

-- | If this configuration set is associated with an environment, the
-- DeploymentStatus parameter indicates the deployment status of this
-- configuration set: null: This configuration is not associated with a
-- running environment. pending: This is a draft configuration that is not
-- deployed to the associated environment but is in the process of deploying.
-- deployed: This is the configuration that is currently deployed to the
-- associated running environment. failed: This is a draft configuration, that
-- failed to successfully deploy. null: This configuration is not associated
-- with a running environment. pending: This is a draft configuration that is
-- not deployed to the associated environment but is in the process of
-- deploying. deployed: This is the configuration that is currently deployed
-- to the associated running environment. failed: This is a draft
-- configuration that failed to successfully deploy.
data ConfigurationDeploymentStatus
    = ConfigurationDeploymentStatusDeployed -- ^ deployed
    | ConfigurationDeploymentStatusFailed -- ^ failed
    | ConfigurationDeploymentStatusPending -- ^ pending
      deriving (Eq, Show, Generic)

instance Hashable ConfigurationDeploymentStatus

instance FromText ConfigurationDeploymentStatus where
    parser = match "deployed" ConfigurationDeploymentStatusDeployed
         <|> match "failed" ConfigurationDeploymentStatusFailed
         <|> match "pending" ConfigurationDeploymentStatusPending

instance ToText ConfigurationDeploymentStatus where
    toText ConfigurationDeploymentStatusDeployed = "deployed"
    toText ConfigurationDeploymentStatusFailed = "failed"
    toText ConfigurationDeploymentStatusPending = "pending"

instance ToByteString ConfigurationDeploymentStatus

instance FromXML ConfigurationDeploymentStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ConfigurationDeploymentStatus"

instance ToQuery ConfigurationDeploymentStatus where
    toQuery = genericQuery def

-- | An indication of which type of values this option has and whether it is
-- allowable to select one or more than one of the possible values: Scalar :
-- Values for this option are a single selection from the possible values, or
-- a unformatted string or numeric value governed by the MIN/MAX/Regex
-- constraints: List : Values for this option are multiple selections of the
-- possible values. Boolean : Values for this option are either true or false
-- . Scalar : Values for this option are a single selection from the possible
-- values, or an unformatted string, or numeric value governed by the
-- MIN/MAX/Regex constraints. List : Values for this option are multiple
-- selections from the possible values. Boolean : Values for this option are
-- either true or false .
data ConfigurationOptionValueType
    = ConfigurationOptionValueTypeList -- ^ List
    | ConfigurationOptionValueTypeScalar -- ^ Scalar
      deriving (Eq, Show, Generic)

instance Hashable ConfigurationOptionValueType

instance FromText ConfigurationOptionValueType where
    parser = match "List" ConfigurationOptionValueTypeList
         <|> match "Scalar" ConfigurationOptionValueTypeScalar

instance ToText ConfigurationOptionValueType where
    toText ConfigurationOptionValueTypeList = "List"
    toText ConfigurationOptionValueTypeScalar = "Scalar"

instance ToByteString ConfigurationOptionValueType

instance FromXML ConfigurationOptionValueType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ConfigurationOptionValueType"

instance ToQuery ConfigurationOptionValueType where
    toQuery = genericQuery def

-- | Describes the health status of the environment. AWS Elastic Beanstalk
-- indicates the failure levels for a running environment: Red : Indicates the
-- environment is not working. Yellow: Indicates that something is wrong, the
-- application might not be available, but the instances appear running.
-- Green: Indicates the environment is healthy and fully functional. Red:
-- Indicates the environment is not responsive. Occurs when three or more
-- consecutive failures occur for an environment. Yellow: Indicates that
-- something is wrong. Occurs when two consecutive failures occur for an
-- environment. Green: Indicates the environment is healthy and fully
-- functional. Grey: Default health for a new environment. The environment is
-- not fully launched and health checks have not started or health checks are
-- suspended during an UpdateEnvironment or RestartEnvironement request.
-- Default: Grey.
data EnvironmentHealth
    = EnvironmentHealthGreen -- ^ Green
    | EnvironmentHealthGrey -- ^ Grey
    | EnvironmentHealthRed -- ^ Red
    | EnvironmentHealthYellow -- ^ Yellow
      deriving (Eq, Show, Generic)

instance Hashable EnvironmentHealth

instance FromText EnvironmentHealth where
    parser = match "Green" EnvironmentHealthGreen
         <|> match "Grey" EnvironmentHealthGrey
         <|> match "Red" EnvironmentHealthRed
         <|> match "Yellow" EnvironmentHealthYellow

instance ToText EnvironmentHealth where
    toText EnvironmentHealthGreen = "Green"
    toText EnvironmentHealthGrey = "Grey"
    toText EnvironmentHealthRed = "Red"
    toText EnvironmentHealthYellow = "Yellow"

instance ToByteString EnvironmentHealth

instance FromXML EnvironmentHealth where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentHealth"

instance ToQuery EnvironmentHealth where
    toQuery = genericQuery def

-- | The type of information to request.
data EnvironmentInfoType
    = EnvironmentInfoTypeTail -- ^ tail
      deriving (Eq, Show, Generic)

instance Hashable EnvironmentInfoType

instance FromText EnvironmentInfoType where
    parser = match "tail" EnvironmentInfoTypeTail

instance ToText EnvironmentInfoType where
    toText EnvironmentInfoTypeTail = "tail"

instance ToByteString EnvironmentInfoType

instance FromXML EnvironmentInfoType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentInfoType"

instance ToQuery EnvironmentInfoType where
    toQuery = genericQuery def

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such as
-- update or terminate. Terminating: Environment is in the shut-down process.
-- Terminated: Environment is not running.
data EnvironmentStatus
    = EnvironmentStatusLaunching -- ^ Launching
    | EnvironmentStatusReady -- ^ Ready
    | EnvironmentStatusTerminated -- ^ Terminated
    | EnvironmentStatusTerminating -- ^ Terminating
    | EnvironmentStatusUpdating -- ^ Updating
      deriving (Eq, Show, Generic)

instance Hashable EnvironmentStatus

instance FromText EnvironmentStatus where
    parser = match "Launching" EnvironmentStatusLaunching
         <|> match "Ready" EnvironmentStatusReady
         <|> match "Terminated" EnvironmentStatusTerminated
         <|> match "Terminating" EnvironmentStatusTerminating
         <|> match "Updating" EnvironmentStatusUpdating

instance ToText EnvironmentStatus where
    toText EnvironmentStatusLaunching = "Launching"
    toText EnvironmentStatusReady = "Ready"
    toText EnvironmentStatusTerminated = "Terminated"
    toText EnvironmentStatusTerminating = "Terminating"
    toText EnvironmentStatusUpdating = "Updating"

instance ToByteString EnvironmentStatus

instance FromXML EnvironmentStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentStatus"

instance ToQuery EnvironmentStatus where
    toQuery = genericQuery def

-- | If specified, limits the events returned from this call to include only
-- those with the specified severity or higher.
data EventSeverity
    = EventSeverityDebug -- ^ DEBUG
    | EventSeverityError -- ^ ERROR
    | EventSeverityFatal -- ^ FATAL
    | EventSeverityInfo -- ^ INFO
    | EventSeverityTrace -- ^ TRACE
    | EventSeverityWarn -- ^ WARN
      deriving (Eq, Show, Generic)

instance Hashable EventSeverity

instance FromText EventSeverity where
    parser = match "DEBUG" EventSeverityDebug
         <|> match "ERROR" EventSeverityError
         <|> match "FATAL" EventSeverityFatal
         <|> match "INFO" EventSeverityInfo
         <|> match "TRACE" EventSeverityTrace
         <|> match "WARN" EventSeverityWarn

instance ToText EventSeverity where
    toText EventSeverityDebug = "DEBUG"
    toText EventSeverityError = "ERROR"
    toText EventSeverityFatal = "FATAL"
    toText EventSeverityInfo = "INFO"
    toText EventSeverityTrace = "TRACE"
    toText EventSeverityWarn = "WARN"

instance ToByteString EventSeverity

instance FromXML EventSeverity where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventSeverity"

instance ToQuery EventSeverity where
    toQuery = genericQuery def

-- | An indication of the severity of this message: error: This message
-- indicates that this is not a valid setting for an option. warning: This
-- message is providing information you should take into account. error: This
-- message indicates that this is not a valid setting for an option. warning:
-- This message is providing information you should take into account.
data ValidationSeverity
    = ValidationSeverityError -- ^ error
    | ValidationSeverityWarning -- ^ warning
      deriving (Eq, Show, Generic)

instance Hashable ValidationSeverity

instance FromText ValidationSeverity where
    parser = match "error" ValidationSeverityError
         <|> match "warning" ValidationSeverityWarning

instance ToText ValidationSeverity where
    toText ValidationSeverityError = "error"
    toText ValidationSeverityWarning = "warning"

instance ToByteString ValidationSeverity

instance FromXML ValidationSeverity where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ValidationSeverity"

instance ToQuery ValidationSeverity where
    toQuery = genericQuery def

-- | Describes an Auto Scaling launch configuration.
newtype AutoScalingGroup = AutoScalingGroup
    { _asgName :: Maybe Text
      -- ^ The name of the AutoScalingGroup .
    } deriving (Show, Generic)

-- | The name of the AutoScalingGroup .
asgName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AutoScalingGroup
    -> f AutoScalingGroup
asgName f x =
    (\y -> x { _asgName = y })
       <$> f (_asgName x)
{-# INLINE asgName #-}

instance FromXML AutoScalingGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AutoScalingGroup"

-- | The description of the AWS resources used by this environment.
newtype EnvironmentResourcesDescription = EnvironmentResourcesDescription
    { _erdLoadBalancer :: Maybe LoadBalancerDescription
      -- ^ Describes the LoadBalancer.
    } deriving (Show, Generic)

-- | Describes the LoadBalancer.
erdLoadBalancer
    :: Functor f
    => (Maybe LoadBalancerDescription
    -> f (Maybe LoadBalancerDescription))
    -> EnvironmentResourcesDescription
    -> f EnvironmentResourcesDescription
erdLoadBalancer f x =
    (\y -> x { _erdLoadBalancer = y })
       <$> f (_erdLoadBalancer x)
{-# INLINE erdLoadBalancer #-}

instance FromXML EnvironmentResourcesDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentResourcesDescription"

instance ToQuery EnvironmentResourcesDescription where
    toQuery = genericQuery def

-- | The description of an Amazon EC2 instance.
newtype Instance = Instance
    { _rId :: Maybe Text
      -- ^ The ID of the Amazon EC2 instance.
    } deriving (Show, Generic)

-- | The ID of the Amazon EC2 instance.
rId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Instance
    -> f Instance
rId f x =
    (\y -> x { _rId = y })
       <$> f (_rId x)
{-# INLINE rId #-}

instance FromXML Instance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Instance"

-- | Describes an Auto Scaling launch configuration.
newtype LaunchConfiguration = LaunchConfiguration
    { _lcName :: Maybe Text
      -- ^ The name of the launch configuration.
    } deriving (Show, Generic)

-- | The name of the launch configuration.
lcName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LaunchConfiguration
    -> f LaunchConfiguration
lcName f x =
    (\y -> x { _lcName = y })
       <$> f (_lcName x)
{-# INLINE lcName #-}

instance FromXML LaunchConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LaunchConfiguration"

-- | Describes a LoadBalancer.
newtype LoadBalancer = LoadBalancer
    { _lbName :: Maybe Text
      -- ^ The name of the LoadBalancer.
    } deriving (Show, Generic)

-- | The name of the LoadBalancer.
lbName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LoadBalancer
    -> f LoadBalancer
lbName f x =
    (\y -> x { _lbName = y })
       <$> f (_lbName x)
{-# INLINE lbName #-}

instance FromXML LoadBalancer where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LoadBalancer"

-- | Describes a trigger.
newtype Trigger = Trigger
    { _trName :: Maybe Text
      -- ^ The name of the trigger.
    } deriving (Show, Generic)

-- | The name of the trigger.
trName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Trigger
    -> f Trigger
trName f x =
    (\y -> x { _trName = y })
       <$> f (_trName x)
{-# INLINE trName #-}

instance FromXML Trigger where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Trigger"

-- | The ApplicationDescription of the application.
data ApplicationDescription = ApplicationDescription
    { _adApplicationName :: Maybe Text
      -- ^ The name of the application.
    , _adDescription :: Maybe Text
      -- ^ User-defined description of the application.
    , _adDateCreated :: Maybe ISO8601
      -- ^ The date when the application was created.
    , _adDateUpdated :: Maybe ISO8601
      -- ^ The date when the application was last modified.
    , _adVersions :: [Text]
      -- ^ The names of the versions for this application.
    , _adConfigurationTemplates :: [Text]
      -- ^ The names of the configuration templates associated with this
      -- application.
    } deriving (Show, Generic)

-- | The name of the application.
adApplicationName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ApplicationDescription
    -> f ApplicationDescription
adApplicationName f x =
    (\y -> x { _adApplicationName = y })
       <$> f (_adApplicationName x)
{-# INLINE adApplicationName #-}

-- | User-defined description of the application.
adDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ApplicationDescription
    -> f ApplicationDescription
adDescription f x =
    (\y -> x { _adDescription = y })
       <$> f (_adDescription x)
{-# INLINE adDescription #-}

-- | The date when the application was created.
adDateCreated
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> ApplicationDescription
    -> f ApplicationDescription
adDateCreated f x =
    (\y -> x { _adDateCreated = y })
       <$> f (_adDateCreated x)
{-# INLINE adDateCreated #-}

-- | The date when the application was last modified.
adDateUpdated
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> ApplicationDescription
    -> f ApplicationDescription
adDateUpdated f x =
    (\y -> x { _adDateUpdated = y })
       <$> f (_adDateUpdated x)
{-# INLINE adDateUpdated #-}

-- | The names of the versions for this application.
adVersions
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ApplicationDescription
    -> f ApplicationDescription
adVersions f x =
    (\y -> x { _adVersions = y })
       <$> f (_adVersions x)
{-# INLINE adVersions #-}

-- | The names of the configuration templates associated with this application.
adConfigurationTemplates
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ApplicationDescription
    -> f ApplicationDescription
adConfigurationTemplates f x =
    (\y -> x { _adConfigurationTemplates = y })
       <$> f (_adConfigurationTemplates x)
{-# INLINE adConfigurationTemplates #-}

instance FromXML ApplicationDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ApplicationDescription"

-- | The ApplicationVersionDescription of the application version.
data ApplicationVersionDescription = ApplicationVersionDescription
    { _avdApplicationName :: Maybe Text
      -- ^ The name of the application associated with this release.
    , _avdDescription :: Maybe Text
      -- ^ The description of this application version.
    , _avdVersionLabel :: Maybe Text
      -- ^ A label uniquely identifying the version for the associated
      -- application.
    , _avdSourceBundle :: Maybe S3Location
      -- ^ The location where the source bundle is located for this version.
    , _avdDateCreated :: Maybe ISO8601
      -- ^ The creation date of the application version.
    , _avdDateUpdated :: Maybe ISO8601
      -- ^ The last modified date of the application version.
    } deriving (Show, Generic)

-- | The name of the application associated with this release.
avdApplicationName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ApplicationVersionDescription
    -> f ApplicationVersionDescription
avdApplicationName f x =
    (\y -> x { _avdApplicationName = y })
       <$> f (_avdApplicationName x)
{-# INLINE avdApplicationName #-}

-- | The description of this application version.
avdDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ApplicationVersionDescription
    -> f ApplicationVersionDescription
avdDescription f x =
    (\y -> x { _avdDescription = y })
       <$> f (_avdDescription x)
{-# INLINE avdDescription #-}

-- | A label uniquely identifying the version for the associated application.
avdVersionLabel
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ApplicationVersionDescription
    -> f ApplicationVersionDescription
avdVersionLabel f x =
    (\y -> x { _avdVersionLabel = y })
       <$> f (_avdVersionLabel x)
{-# INLINE avdVersionLabel #-}

-- | The location where the source bundle is located for this version.
avdSourceBundle
    :: Functor f
    => (Maybe S3Location
    -> f (Maybe S3Location))
    -> ApplicationVersionDescription
    -> f ApplicationVersionDescription
avdSourceBundle f x =
    (\y -> x { _avdSourceBundle = y })
       <$> f (_avdSourceBundle x)
{-# INLINE avdSourceBundle #-}

-- | The creation date of the application version.
avdDateCreated
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> ApplicationVersionDescription
    -> f ApplicationVersionDescription
avdDateCreated f x =
    (\y -> x { _avdDateCreated = y })
       <$> f (_avdDateCreated x)
{-# INLINE avdDateCreated #-}

-- | The last modified date of the application version.
avdDateUpdated
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> ApplicationVersionDescription
    -> f ApplicationVersionDescription
avdDateUpdated f x =
    (\y -> x { _avdDateUpdated = y })
       <$> f (_avdDateUpdated x)
{-# INLINE avdDateUpdated #-}

instance FromXML ApplicationVersionDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ApplicationVersionDescription"

-- | Describes the possible values for a configuration option.
data ConfigurationOptionDescription = ConfigurationOptionDescription
    { _coeNamespace :: Maybe Text
      -- ^ A unique namespace identifying the option's associated AWS
      -- resource.
    , _coeName :: Maybe Text
      -- ^ The name of the configuration option.
    , _coeDefaultValue :: Maybe Text
      -- ^ The default value for this configuration option.
    , _coeChangeSeverity :: Maybe Text
      -- ^ An indication of which action is required if the value for this
      -- configuration option changes: NoInterruption - There is no
      -- interruption to the environment or application availability.
      -- RestartEnvironment - The environment is restarted, all AWS
      -- resources are deleted and recreated, and the environment is
      -- unavailable during the process. RestartApplicationServer - The
      -- environment is available the entire time. However, a short
      -- application outage occurs when the application servers on the
      -- running Amazon EC2 instances are restarted. NoInterruption :
      -- There is no interruption to the environment or application
      -- availability. RestartEnvironment : The environment is entirely
      -- restarted, all AWS resources are deleted and recreated, and the
      -- environment is unavailable during the process.
      -- RestartApplicationServer : The environment is available the
      -- entire time. However, a short application outage occurs when the
      -- application servers on the running Amazon EC2 instances are
      -- restarted.
    , _coeUserDefined :: Maybe Bool
      -- ^ An indication of whether the user defined this configuration
      -- option: true : This configuration option was defined by the user.
      -- It is a valid choice for specifying this as an Option to Remove
      -- when updating configuration settings. false : This configuration
      -- was not defined by the user. true : This configuration option was
      -- defined by the user. It is a valid choice for specifying if this
      -- as an Option to Remove when updating configuration settings.
      -- false : This configuration was not defined by the user.
      -- Constraint: You can remove only UserDefined options from a
      -- configuration. Valid Values: true | false.
    , _coeValueType :: Maybe ConfigurationOptionValueType
      -- ^ An indication of which type of values this option has and whether
      -- it is allowable to select one or more than one of the possible
      -- values: Scalar : Values for this option are a single selection
      -- from the possible values, or a unformatted string or numeric
      -- value governed by the MIN/MAX/Regex constraints: List : Values
      -- for this option are multiple selections of the possible values.
      -- Boolean : Values for this option are either true or false .
      -- Scalar : Values for this option are a single selection from the
      -- possible values, or an unformatted string, or numeric value
      -- governed by the MIN/MAX/Regex constraints. List : Values for this
      -- option are multiple selections from the possible values. Boolean
      -- : Values for this option are either true or false .
    , _coeValueOptions :: [Text]
      -- ^ If specified, values for the configuration option are selected
      -- from this list.
    , _coeMinValue :: Maybe Integer
      -- ^ If specified, the configuration option must be a numeric value
      -- greater than this value.
    , _coeMaxValue :: Maybe Integer
      -- ^ If specified, the configuration option must be a numeric value
      -- less than this value.
    , _coeMaxLength :: Maybe Integer
      -- ^ If specified, the configuration option must be a string value no
      -- longer than this value.
    , _coeRegex :: Maybe OptionRestrictionRegex
      -- ^ If specified, the configuration option must be a string value
      -- that satisfies this regular expression.
    } deriving (Show, Generic)

-- | A unique namespace identifying the option's associated AWS resource.
coeNamespace
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ConfigurationOptionDescription
    -> f ConfigurationOptionDescription
coeNamespace f x =
    (\y -> x { _coeNamespace = y })
       <$> f (_coeNamespace x)
{-# INLINE coeNamespace #-}

-- | The name of the configuration option.
coeName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ConfigurationOptionDescription
    -> f ConfigurationOptionDescription
coeName f x =
    (\y -> x { _coeName = y })
       <$> f (_coeName x)
{-# INLINE coeName #-}

-- | The default value for this configuration option.
coeDefaultValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ConfigurationOptionDescription
    -> f ConfigurationOptionDescription
coeDefaultValue f x =
    (\y -> x { _coeDefaultValue = y })
       <$> f (_coeDefaultValue x)
{-# INLINE coeDefaultValue #-}

-- | An indication of which action is required if the value for this
-- configuration option changes: NoInterruption - There is no interruption to
-- the environment or application availability. RestartEnvironment - The
-- environment is restarted, all AWS resources are deleted and recreated, and
-- the environment is unavailable during the process. RestartApplicationServer
-- - The environment is available the entire time. However, a short
-- application outage occurs when the application servers on the running
-- Amazon EC2 instances are restarted. NoInterruption : There is no
-- interruption to the environment or application availability.
-- RestartEnvironment : The environment is entirely restarted, all AWS
-- resources are deleted and recreated, and the environment is unavailable
-- during the process. RestartApplicationServer : The environment is available
-- the entire time. However, a short application outage occurs when the
-- application servers on the running Amazon EC2 instances are restarted.
coeChangeSeverity
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ConfigurationOptionDescription
    -> f ConfigurationOptionDescription
coeChangeSeverity f x =
    (\y -> x { _coeChangeSeverity = y })
       <$> f (_coeChangeSeverity x)
{-# INLINE coeChangeSeverity #-}

-- | An indication of whether the user defined this configuration option: true :
-- This configuration option was defined by the user. It is a valid choice for
-- specifying this as an Option to Remove when updating configuration
-- settings. false : This configuration was not defined by the user. true :
-- This configuration option was defined by the user. It is a valid choice for
-- specifying if this as an Option to Remove when updating configuration
-- settings. false : This configuration was not defined by the user.
-- Constraint: You can remove only UserDefined options from a configuration.
-- Valid Values: true | false.
coeUserDefined
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> ConfigurationOptionDescription
    -> f ConfigurationOptionDescription
coeUserDefined f x =
    (\y -> x { _coeUserDefined = y })
       <$> f (_coeUserDefined x)
{-# INLINE coeUserDefined #-}

-- | An indication of which type of values this option has and whether it is
-- allowable to select one or more than one of the possible values: Scalar :
-- Values for this option are a single selection from the possible values, or
-- a unformatted string or numeric value governed by the MIN/MAX/Regex
-- constraints: List : Values for this option are multiple selections of the
-- possible values. Boolean : Values for this option are either true or false
-- . Scalar : Values for this option are a single selection from the possible
-- values, or an unformatted string, or numeric value governed by the
-- MIN/MAX/Regex constraints. List : Values for this option are multiple
-- selections from the possible values. Boolean : Values for this option are
-- either true or false .
coeValueType
    :: Functor f
    => (Maybe ConfigurationOptionValueType
    -> f (Maybe ConfigurationOptionValueType))
    -> ConfigurationOptionDescription
    -> f ConfigurationOptionDescription
coeValueType f x =
    (\y -> x { _coeValueType = y })
       <$> f (_coeValueType x)
{-# INLINE coeValueType #-}

-- | If specified, values for the configuration option are selected from this
-- list.
coeValueOptions
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ConfigurationOptionDescription
    -> f ConfigurationOptionDescription
coeValueOptions f x =
    (\y -> x { _coeValueOptions = y })
       <$> f (_coeValueOptions x)
{-# INLINE coeValueOptions #-}

-- | If specified, the configuration option must be a numeric value greater than
-- this value.
coeMinValue
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ConfigurationOptionDescription
    -> f ConfigurationOptionDescription
coeMinValue f x =
    (\y -> x { _coeMinValue = y })
       <$> f (_coeMinValue x)
{-# INLINE coeMinValue #-}

-- | If specified, the configuration option must be a numeric value less than
-- this value.
coeMaxValue
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ConfigurationOptionDescription
    -> f ConfigurationOptionDescription
coeMaxValue f x =
    (\y -> x { _coeMaxValue = y })
       <$> f (_coeMaxValue x)
{-# INLINE coeMaxValue #-}

-- | If specified, the configuration option must be a string value no longer
-- than this value.
coeMaxLength
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ConfigurationOptionDescription
    -> f ConfigurationOptionDescription
coeMaxLength f x =
    (\y -> x { _coeMaxLength = y })
       <$> f (_coeMaxLength x)
{-# INLINE coeMaxLength #-}

-- | If specified, the configuration option must be a string value that
-- satisfies this regular expression.
coeRegex
    :: Functor f
    => (Maybe OptionRestrictionRegex
    -> f (Maybe OptionRestrictionRegex))
    -> ConfigurationOptionDescription
    -> f ConfigurationOptionDescription
coeRegex f x =
    (\y -> x { _coeRegex = y })
       <$> f (_coeRegex x)
{-# INLINE coeRegex #-}

instance FromXML ConfigurationOptionDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ConfigurationOptionDescription"

-- | A specification identifying an individual configuration option along with
-- its current value. For a list of possible option values, go to Option
-- Values in the AWS Elastic Beanstalk Developer Guide.
data ConfigurationOptionSetting = ConfigurationOptionSetting
    { _cosNamespace :: Maybe Text
      -- ^ A unique namespace identifying the option's associated AWS
      -- resource.
    , _cosOptionName :: Maybe Text
      -- ^ The name of the configuration option.
    , _cosValue :: Maybe Text
      -- ^ The current value for the configuration option.
    } deriving (Show, Generic)

-- | A unique namespace identifying the option's associated AWS resource.
cosNamespace
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ConfigurationOptionSetting
    -> f ConfigurationOptionSetting
cosNamespace f x =
    (\y -> x { _cosNamespace = y })
       <$> f (_cosNamespace x)
{-# INLINE cosNamespace #-}

-- | The name of the configuration option.
cosOptionName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ConfigurationOptionSetting
    -> f ConfigurationOptionSetting
cosOptionName f x =
    (\y -> x { _cosOptionName = y })
       <$> f (_cosOptionName x)
{-# INLINE cosOptionName #-}

-- | The current value for the configuration option.
cosValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ConfigurationOptionSetting
    -> f ConfigurationOptionSetting
cosValue f x =
    (\y -> x { _cosValue = y })
       <$> f (_cosValue x)
{-# INLINE cosValue #-}

instance FromXML ConfigurationOptionSetting where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ConfigurationOptionSetting"

instance ToQuery ConfigurationOptionSetting where
    toQuery = genericQuery def

-- | Describes the settings for a configuration set.
data ConfigurationSettingsDescription = ConfigurationSettingsDescription
    { _csfSolutionStackName :: Maybe Text
      -- ^ The name of the solution stack this configuration set uses.
    , _csfApplicationName :: Maybe Text
      -- ^ The name of the application associated with this configuration
      -- set.
    , _csfTemplateName :: Maybe Text
      -- ^ If not null, the name of the configuration template for this
      -- configuration set.
    , _csfDescription :: Maybe Text
      -- ^ Describes this configuration set.
    , _csfEnvironmentName :: Maybe Text
      -- ^ If not null, the name of the environment for this configuration
      -- set.
    , _csfDeploymentStatus :: Maybe ConfigurationDeploymentStatus
      -- ^ If this configuration set is associated with an environment, the
      -- DeploymentStatus parameter indicates the deployment status of
      -- this configuration set: null: This configuration is not
      -- associated with a running environment. pending: This is a draft
      -- configuration that is not deployed to the associated environment
      -- but is in the process of deploying. deployed: This is the
      -- configuration that is currently deployed to the associated
      -- running environment. failed: This is a draft configuration, that
      -- failed to successfully deploy. null: This configuration is not
      -- associated with a running environment. pending: This is a draft
      -- configuration that is not deployed to the associated environment
      -- but is in the process of deploying. deployed: This is the
      -- configuration that is currently deployed to the associated
      -- running environment. failed: This is a draft configuration that
      -- failed to successfully deploy.
    , _csfDateCreated :: Maybe ISO8601
      -- ^ The date (in UTC time) when this configuration set was created.
    , _csfDateUpdated :: Maybe ISO8601
      -- ^ The date (in UTC time) when this configuration set was last
      -- modified.
    , _csfOptionSettings :: [ConfigurationOptionSetting]
      -- ^ A list of the configuration options and their values in this
      -- configuration set.
    } deriving (Show, Generic)

-- | The name of the solution stack this configuration set uses.
csfSolutionStackName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ConfigurationSettingsDescription
    -> f ConfigurationSettingsDescription
csfSolutionStackName f x =
    (\y -> x { _csfSolutionStackName = y })
       <$> f (_csfSolutionStackName x)
{-# INLINE csfSolutionStackName #-}

-- | The name of the application associated with this configuration set.
csfApplicationName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ConfigurationSettingsDescription
    -> f ConfigurationSettingsDescription
csfApplicationName f x =
    (\y -> x { _csfApplicationName = y })
       <$> f (_csfApplicationName x)
{-# INLINE csfApplicationName #-}

-- | If not null, the name of the configuration template for this configuration
-- set.
csfTemplateName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ConfigurationSettingsDescription
    -> f ConfigurationSettingsDescription
csfTemplateName f x =
    (\y -> x { _csfTemplateName = y })
       <$> f (_csfTemplateName x)
{-# INLINE csfTemplateName #-}

-- | Describes this configuration set.
csfDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ConfigurationSettingsDescription
    -> f ConfigurationSettingsDescription
csfDescription f x =
    (\y -> x { _csfDescription = y })
       <$> f (_csfDescription x)
{-# INLINE csfDescription #-}

-- | If not null, the name of the environment for this configuration set.
csfEnvironmentName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ConfigurationSettingsDescription
    -> f ConfigurationSettingsDescription
csfEnvironmentName f x =
    (\y -> x { _csfEnvironmentName = y })
       <$> f (_csfEnvironmentName x)
{-# INLINE csfEnvironmentName #-}

-- | If this configuration set is associated with an environment, the
-- DeploymentStatus parameter indicates the deployment status of this
-- configuration set: null: This configuration is not associated with a
-- running environment. pending: This is a draft configuration that is not
-- deployed to the associated environment but is in the process of deploying.
-- deployed: This is the configuration that is currently deployed to the
-- associated running environment. failed: This is a draft configuration, that
-- failed to successfully deploy. null: This configuration is not associated
-- with a running environment. pending: This is a draft configuration that is
-- not deployed to the associated environment but is in the process of
-- deploying. deployed: This is the configuration that is currently deployed
-- to the associated running environment. failed: This is a draft
-- configuration that failed to successfully deploy.
csfDeploymentStatus
    :: Functor f
    => (Maybe ConfigurationDeploymentStatus
    -> f (Maybe ConfigurationDeploymentStatus))
    -> ConfigurationSettingsDescription
    -> f ConfigurationSettingsDescription
csfDeploymentStatus f x =
    (\y -> x { _csfDeploymentStatus = y })
       <$> f (_csfDeploymentStatus x)
{-# INLINE csfDeploymentStatus #-}

-- | The date (in UTC time) when this configuration set was created.
csfDateCreated
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> ConfigurationSettingsDescription
    -> f ConfigurationSettingsDescription
csfDateCreated f x =
    (\y -> x { _csfDateCreated = y })
       <$> f (_csfDateCreated x)
{-# INLINE csfDateCreated #-}

-- | The date (in UTC time) when this configuration set was last modified.
csfDateUpdated
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> ConfigurationSettingsDescription
    -> f ConfigurationSettingsDescription
csfDateUpdated f x =
    (\y -> x { _csfDateUpdated = y })
       <$> f (_csfDateUpdated x)
{-# INLINE csfDateUpdated #-}

-- | A list of the configuration options and their values in this configuration
-- set.
csfOptionSettings
    :: Functor f
    => ([ConfigurationOptionSetting]
    -> f ([ConfigurationOptionSetting]))
    -> ConfigurationSettingsDescription
    -> f ConfigurationSettingsDescription
csfOptionSettings f x =
    (\y -> x { _csfOptionSettings = y })
       <$> f (_csfOptionSettings x)
{-# INLINE csfOptionSettings #-}

instance FromXML ConfigurationSettingsDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ConfigurationSettingsDescription"

-- | Describes the properties of an environment.
data EnvironmentDescription = EnvironmentDescription
    { _eeEnvironmentName :: Maybe Text
      -- ^ The name of this environment.
    , _eeEnvironmentId :: Maybe Text
      -- ^ The ID of this environment.
    , _eeApplicationName :: Maybe Text
      -- ^ The name of the application associated with this environment.
    , _eeVersionLabel :: Maybe Text
      -- ^ The application version deployed in this environment.
    , _eeSolutionStackName :: Maybe Text
      -- ^ The name of the SolutionStack deployed with this environment.
    , _eeTemplateName :: Maybe Text
      -- ^ The name of the configuration template used to originally launch
      -- this environment.
    , _eeDescription :: Maybe Text
      -- ^ Describes this environment.
    , _eeEndpointURL :: Maybe Text
      -- ^ For load-balanced, autoscaling environments, the URL to the
      -- LoadBalancer. For single-instance environments, the IP address of
      -- the instance.
    , _eeCNAME :: Maybe Text
      -- ^ The URL to the CNAME for this environment.
    , _eeDateCreated :: Maybe ISO8601
      -- ^ The creation date for this environment.
    , _eeDateUpdated :: Maybe ISO8601
      -- ^ The last modified date for this environment.
    , _eeStatus :: Maybe EnvironmentStatus
      -- ^ The current operational status of the environment: Launching:
      -- Environment is in the process of initial deployment. Updating:
      -- Environment is in the process of updating its configuration
      -- settings or application version. Ready: Environment is available
      -- to have an action performed on it, such as update or terminate.
      -- Terminating: Environment is in the shut-down process. Terminated:
      -- Environment is not running.
    , _eeHealth :: Maybe EnvironmentHealth
      -- ^ Describes the health status of the environment. AWS Elastic
      -- Beanstalk indicates the failure levels for a running environment:
      -- Red : Indicates the environment is not working. Yellow: Indicates
      -- that something is wrong, the application might not be available,
      -- but the instances appear running. Green: Indicates the
      -- environment is healthy and fully functional. Red: Indicates the
      -- environment is not responsive. Occurs when three or more
      -- consecutive failures occur for an environment. Yellow: Indicates
      -- that something is wrong. Occurs when two consecutive failures
      -- occur for an environment. Green: Indicates the environment is
      -- healthy and fully functional. Grey: Default health for a new
      -- environment. The environment is not fully launched and health
      -- checks have not started or health checks are suspended during an
      -- UpdateEnvironment or RestartEnvironement request. Default: Grey.
    , _eeResources :: Maybe EnvironmentResourcesDescription
      -- ^ The description of the AWS resources used by this environment.
    , _eeTier :: Maybe EnvironmentTier
      -- ^ Describes the current tier of this environment.
    } deriving (Show, Generic)

-- | The name of this environment.
eeEnvironmentName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EnvironmentDescription
    -> f EnvironmentDescription
eeEnvironmentName f x =
    (\y -> x { _eeEnvironmentName = y })
       <$> f (_eeEnvironmentName x)
{-# INLINE eeEnvironmentName #-}

-- | The ID of this environment.
eeEnvironmentId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EnvironmentDescription
    -> f EnvironmentDescription
eeEnvironmentId f x =
    (\y -> x { _eeEnvironmentId = y })
       <$> f (_eeEnvironmentId x)
{-# INLINE eeEnvironmentId #-}

-- | The name of the application associated with this environment.
eeApplicationName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EnvironmentDescription
    -> f EnvironmentDescription
eeApplicationName f x =
    (\y -> x { _eeApplicationName = y })
       <$> f (_eeApplicationName x)
{-# INLINE eeApplicationName #-}

-- | The application version deployed in this environment.
eeVersionLabel
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EnvironmentDescription
    -> f EnvironmentDescription
eeVersionLabel f x =
    (\y -> x { _eeVersionLabel = y })
       <$> f (_eeVersionLabel x)
{-# INLINE eeVersionLabel #-}

-- | The name of the SolutionStack deployed with this environment.
eeSolutionStackName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EnvironmentDescription
    -> f EnvironmentDescription
eeSolutionStackName f x =
    (\y -> x { _eeSolutionStackName = y })
       <$> f (_eeSolutionStackName x)
{-# INLINE eeSolutionStackName #-}

-- | The name of the configuration template used to originally launch this
-- environment.
eeTemplateName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EnvironmentDescription
    -> f EnvironmentDescription
eeTemplateName f x =
    (\y -> x { _eeTemplateName = y })
       <$> f (_eeTemplateName x)
{-# INLINE eeTemplateName #-}

-- | Describes this environment.
eeDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EnvironmentDescription
    -> f EnvironmentDescription
eeDescription f x =
    (\y -> x { _eeDescription = y })
       <$> f (_eeDescription x)
{-# INLINE eeDescription #-}

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer.
-- For single-instance environments, the IP address of the instance.
eeEndpointURL
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EnvironmentDescription
    -> f EnvironmentDescription
eeEndpointURL f x =
    (\y -> x { _eeEndpointURL = y })
       <$> f (_eeEndpointURL x)
{-# INLINE eeEndpointURL #-}

-- | The URL to the CNAME for this environment.
eeCNAME
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EnvironmentDescription
    -> f EnvironmentDescription
eeCNAME f x =
    (\y -> x { _eeCNAME = y })
       <$> f (_eeCNAME x)
{-# INLINE eeCNAME #-}

-- | The creation date for this environment.
eeDateCreated
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> EnvironmentDescription
    -> f EnvironmentDescription
eeDateCreated f x =
    (\y -> x { _eeDateCreated = y })
       <$> f (_eeDateCreated x)
{-# INLINE eeDateCreated #-}

-- | The last modified date for this environment.
eeDateUpdated
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> EnvironmentDescription
    -> f EnvironmentDescription
eeDateUpdated f x =
    (\y -> x { _eeDateUpdated = y })
       <$> f (_eeDateUpdated x)
{-# INLINE eeDateUpdated #-}

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such as
-- update or terminate. Terminating: Environment is in the shut-down process.
-- Terminated: Environment is not running.
eeStatus
    :: Functor f
    => (Maybe EnvironmentStatus
    -> f (Maybe EnvironmentStatus))
    -> EnvironmentDescription
    -> f EnvironmentDescription
eeStatus f x =
    (\y -> x { _eeStatus = y })
       <$> f (_eeStatus x)
{-# INLINE eeStatus #-}

-- | Describes the health status of the environment. AWS Elastic Beanstalk
-- indicates the failure levels for a running environment: Red : Indicates the
-- environment is not working. Yellow: Indicates that something is wrong, the
-- application might not be available, but the instances appear running.
-- Green: Indicates the environment is healthy and fully functional. Red:
-- Indicates the environment is not responsive. Occurs when three or more
-- consecutive failures occur for an environment. Yellow: Indicates that
-- something is wrong. Occurs when two consecutive failures occur for an
-- environment. Green: Indicates the environment is healthy and fully
-- functional. Grey: Default health for a new environment. The environment is
-- not fully launched and health checks have not started or health checks are
-- suspended during an UpdateEnvironment or RestartEnvironement request.
-- Default: Grey.
eeHealth
    :: Functor f
    => (Maybe EnvironmentHealth
    -> f (Maybe EnvironmentHealth))
    -> EnvironmentDescription
    -> f EnvironmentDescription
eeHealth f x =
    (\y -> x { _eeHealth = y })
       <$> f (_eeHealth x)
{-# INLINE eeHealth #-}

-- | The description of the AWS resources used by this environment.
eeResources
    :: Functor f
    => (Maybe EnvironmentResourcesDescription
    -> f (Maybe EnvironmentResourcesDescription))
    -> EnvironmentDescription
    -> f EnvironmentDescription
eeResources f x =
    (\y -> x { _eeResources = y })
       <$> f (_eeResources x)
{-# INLINE eeResources #-}

-- | Describes the current tier of this environment.
eeTier
    :: Functor f
    => (Maybe EnvironmentTier
    -> f (Maybe EnvironmentTier))
    -> EnvironmentDescription
    -> f EnvironmentDescription
eeTier f x =
    (\y -> x { _eeTier = y })
       <$> f (_eeTier x)
{-# INLINE eeTier #-}

instance FromXML EnvironmentDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentDescription"

-- | The information retrieved from the Amazon EC2 instances.
data EnvironmentInfoDescription = EnvironmentInfoDescription
    { _eidInfoType :: Maybe EnvironmentInfoType
      -- ^ The type of information retrieved.
    , _eidEc2InstanceId :: Maybe Text
      -- ^ The Amazon EC2 Instance ID for this information.
    , _eidSampleTimestamp :: Maybe ISO8601
      -- ^ The time stamp when this information was retrieved.
    , _eidMessage :: Maybe Text
      -- ^ The retrieved information.
    } deriving (Show, Generic)

-- | The type of information retrieved.
eidInfoType
    :: Functor f
    => (Maybe EnvironmentInfoType
    -> f (Maybe EnvironmentInfoType))
    -> EnvironmentInfoDescription
    -> f EnvironmentInfoDescription
eidInfoType f x =
    (\y -> x { _eidInfoType = y })
       <$> f (_eidInfoType x)
{-# INLINE eidInfoType #-}

-- | The Amazon EC2 Instance ID for this information.
eidEc2InstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EnvironmentInfoDescription
    -> f EnvironmentInfoDescription
eidEc2InstanceId f x =
    (\y -> x { _eidEc2InstanceId = y })
       <$> f (_eidEc2InstanceId x)
{-# INLINE eidEc2InstanceId #-}

-- | The time stamp when this information was retrieved.
eidSampleTimestamp
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> EnvironmentInfoDescription
    -> f EnvironmentInfoDescription
eidSampleTimestamp f x =
    (\y -> x { _eidSampleTimestamp = y })
       <$> f (_eidSampleTimestamp x)
{-# INLINE eidSampleTimestamp #-}

-- | The retrieved information.
eidMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EnvironmentInfoDescription
    -> f EnvironmentInfoDescription
eidMessage f x =
    (\y -> x { _eidMessage = y })
       <$> f (_eidMessage x)
{-# INLINE eidMessage #-}

instance FromXML EnvironmentInfoDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentInfoDescription"

-- | A list of EnvironmentResourceDescription.
data EnvironmentResourceDescription = EnvironmentResourceDescription
    { _ereEnvironmentName :: Maybe Text
      -- ^ The name of the environment.
    , _ereAutoScalingGroups :: [AutoScalingGroup]
      -- ^ The AutoScalingGroups used by this environment.
    , _ereInstances :: [Instance]
      -- ^ The Amazon EC2 instances used by this environment.
    , _ereLaunchConfigurations :: [LaunchConfiguration]
      -- ^ The Auto Scaling launch configurations in use by this
      -- environment.
    , _ereLoadBalancers :: [LoadBalancer]
      -- ^ The LoadBalancers in use by this environment.
    , _ereTriggers :: [Trigger]
      -- ^ The AutoScaling triggers in use by this environment.
    , _ereQueues :: [Queue]
      -- ^ The queues used by this environment.
    } deriving (Show, Generic)

-- | The name of the environment.
ereEnvironmentName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EnvironmentResourceDescription
    -> f EnvironmentResourceDescription
ereEnvironmentName f x =
    (\y -> x { _ereEnvironmentName = y })
       <$> f (_ereEnvironmentName x)
{-# INLINE ereEnvironmentName #-}

-- | The AutoScalingGroups used by this environment.
ereAutoScalingGroups
    :: Functor f
    => ([AutoScalingGroup]
    -> f ([AutoScalingGroup]))
    -> EnvironmentResourceDescription
    -> f EnvironmentResourceDescription
ereAutoScalingGroups f x =
    (\y -> x { _ereAutoScalingGroups = y })
       <$> f (_ereAutoScalingGroups x)
{-# INLINE ereAutoScalingGroups #-}

-- | The Amazon EC2 instances used by this environment.
ereInstances
    :: Functor f
    => ([Instance]
    -> f ([Instance]))
    -> EnvironmentResourceDescription
    -> f EnvironmentResourceDescription
ereInstances f x =
    (\y -> x { _ereInstances = y })
       <$> f (_ereInstances x)
{-# INLINE ereInstances #-}

-- | The Auto Scaling launch configurations in use by this environment.
ereLaunchConfigurations
    :: Functor f
    => ([LaunchConfiguration]
    -> f ([LaunchConfiguration]))
    -> EnvironmentResourceDescription
    -> f EnvironmentResourceDescription
ereLaunchConfigurations f x =
    (\y -> x { _ereLaunchConfigurations = y })
       <$> f (_ereLaunchConfigurations x)
{-# INLINE ereLaunchConfigurations #-}

-- | The LoadBalancers in use by this environment.
ereLoadBalancers
    :: Functor f
    => ([LoadBalancer]
    -> f ([LoadBalancer]))
    -> EnvironmentResourceDescription
    -> f EnvironmentResourceDescription
ereLoadBalancers f x =
    (\y -> x { _ereLoadBalancers = y })
       <$> f (_ereLoadBalancers x)
{-# INLINE ereLoadBalancers #-}

-- | The AutoScaling triggers in use by this environment.
ereTriggers
    :: Functor f
    => ([Trigger]
    -> f ([Trigger]))
    -> EnvironmentResourceDescription
    -> f EnvironmentResourceDescription
ereTriggers f x =
    (\y -> x { _ereTriggers = y })
       <$> f (_ereTriggers x)
{-# INLINE ereTriggers #-}

-- | The queues used by this environment.
ereQueues
    :: Functor f
    => ([Queue]
    -> f ([Queue]))
    -> EnvironmentResourceDescription
    -> f EnvironmentResourceDescription
ereQueues f x =
    (\y -> x { _ereQueues = y })
       <$> f (_ereQueues x)
{-# INLINE ereQueues #-}

instance FromXML EnvironmentResourceDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentResourceDescription"

-- | This specifies the tier to use for creating this environment.
data EnvironmentTier = EnvironmentTier
    { _etName :: Maybe Text
      -- ^ The name of this environment tier.
    , _etType :: Maybe Text
      -- ^ The type of this environment tier.
    , _etVersion :: Maybe Text
      -- ^ The version of this environment tier.
    } deriving (Show, Generic)

-- | The name of this environment tier.
etName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EnvironmentTier
    -> f EnvironmentTier
etName f x =
    (\y -> x { _etName = y })
       <$> f (_etName x)
{-# INLINE etName #-}

-- | The type of this environment tier.
etType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EnvironmentTier
    -> f EnvironmentTier
etType f x =
    (\y -> x { _etType = y })
       <$> f (_etType x)
{-# INLINE etType #-}

-- | The version of this environment tier.
etVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EnvironmentTier
    -> f EnvironmentTier
etVersion f x =
    (\y -> x { _etVersion = y })
       <$> f (_etVersion x)
{-# INLINE etVersion #-}

instance FromXML EnvironmentTier where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentTier"

instance ToQuery EnvironmentTier where
    toQuery = genericQuery def

-- | Describes an event.
data EventDescription = EventDescription
    { _efEventDate :: Maybe ISO8601
      -- ^ The date when the event occurred.
    , _efMessage :: Maybe Text
      -- ^ The event message.
    , _efApplicationName :: Maybe Text
      -- ^ The application associated with the event.
    , _efVersionLabel :: Maybe Text
      -- ^ The release label for the application version associated with
      -- this event.
    , _efTemplateName :: Maybe Text
      -- ^ The name of the configuration associated with this event.
    , _efEnvironmentName :: Maybe Text
      -- ^ The name of the environment associated with this event.
    , _efRequestId :: Maybe Text
      -- ^ The web service request ID for the activity of this event.
    , _efSeverity :: Maybe EventSeverity
      -- ^ The severity level of this event.
    } deriving (Show, Generic)

-- | The date when the event occurred.
efEventDate
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> EventDescription
    -> f EventDescription
efEventDate f x =
    (\y -> x { _efEventDate = y })
       <$> f (_efEventDate x)
{-# INLINE efEventDate #-}

-- | The event message.
efMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventDescription
    -> f EventDescription
efMessage f x =
    (\y -> x { _efMessage = y })
       <$> f (_efMessage x)
{-# INLINE efMessage #-}

-- | The application associated with the event.
efApplicationName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventDescription
    -> f EventDescription
efApplicationName f x =
    (\y -> x { _efApplicationName = y })
       <$> f (_efApplicationName x)
{-# INLINE efApplicationName #-}

-- | The release label for the application version associated with this event.
efVersionLabel
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventDescription
    -> f EventDescription
efVersionLabel f x =
    (\y -> x { _efVersionLabel = y })
       <$> f (_efVersionLabel x)
{-# INLINE efVersionLabel #-}

-- | The name of the configuration associated with this event.
efTemplateName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventDescription
    -> f EventDescription
efTemplateName f x =
    (\y -> x { _efTemplateName = y })
       <$> f (_efTemplateName x)
{-# INLINE efTemplateName #-}

-- | The name of the environment associated with this event.
efEnvironmentName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventDescription
    -> f EventDescription
efEnvironmentName f x =
    (\y -> x { _efEnvironmentName = y })
       <$> f (_efEnvironmentName x)
{-# INLINE efEnvironmentName #-}

-- | The web service request ID for the activity of this event.
efRequestId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EventDescription
    -> f EventDescription
efRequestId f x =
    (\y -> x { _efRequestId = y })
       <$> f (_efRequestId x)
{-# INLINE efRequestId #-}

-- | The severity level of this event.
efSeverity
    :: Functor f
    => (Maybe EventSeverity
    -> f (Maybe EventSeverity))
    -> EventDescription
    -> f EventDescription
efSeverity f x =
    (\y -> x { _efSeverity = y })
       <$> f (_efSeverity x)
{-# INLINE efSeverity #-}

instance FromXML EventDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventDescription"

-- | Describes the properties of a Listener for the LoadBalancer.
data Listener = Listener
    { _lProtocol :: Maybe Text
      -- ^ The protocol that is used by the Listener.
    , _lPort :: Maybe Integer
      -- ^ The port that is used by the Listener.
    } deriving (Show, Generic)

-- | The protocol that is used by the Listener.
lProtocol
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Listener
    -> f Listener
lProtocol f x =
    (\y -> x { _lProtocol = y })
       <$> f (_lProtocol x)
{-# INLINE lProtocol #-}

-- | The port that is used by the Listener.
lPort
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Listener
    -> f Listener
lPort f x =
    (\y -> x { _lPort = y })
       <$> f (_lPort x)
{-# INLINE lPort #-}

instance FromXML Listener where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Listener"

instance ToQuery Listener where
    toQuery = genericQuery def

-- | Describes the LoadBalancer.
data LoadBalancerDescription = LoadBalancerDescription
    { _lbdLoadBalancerName :: Maybe Text
      -- ^ The name of the LoadBalancer.
    , _lbdDomain :: Maybe Text
      -- ^ The domain name of the LoadBalancer.
    , _lbdListeners :: [Listener]
      -- ^ A list of Listeners used by the LoadBalancer.
    } deriving (Show, Generic)

-- | The name of the LoadBalancer.
lbdLoadBalancerName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LoadBalancerDescription
    -> f LoadBalancerDescription
lbdLoadBalancerName f x =
    (\y -> x { _lbdLoadBalancerName = y })
       <$> f (_lbdLoadBalancerName x)
{-# INLINE lbdLoadBalancerName #-}

-- | The domain name of the LoadBalancer.
lbdDomain
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> LoadBalancerDescription
    -> f LoadBalancerDescription
lbdDomain f x =
    (\y -> x { _lbdDomain = y })
       <$> f (_lbdDomain x)
{-# INLINE lbdDomain #-}

-- | A list of Listeners used by the LoadBalancer.
lbdListeners
    :: Functor f
    => ([Listener]
    -> f ([Listener]))
    -> LoadBalancerDescription
    -> f LoadBalancerDescription
lbdListeners f x =
    (\y -> x { _lbdListeners = y })
       <$> f (_lbdListeners x)
{-# INLINE lbdListeners #-}

instance FromXML LoadBalancerDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LoadBalancerDescription"

instance ToQuery LoadBalancerDescription where
    toQuery = genericQuery def

-- | If specified, the configuration option must be a string value that
-- satisfies this regular expression.
data OptionRestrictionRegex = OptionRestrictionRegex
    { _orrPattern :: Maybe Text
      -- ^ The regular expression pattern that a string configuration option
      -- value with this restriction must match.
    , _orrLabel :: Maybe Text
      -- ^ A unique name representing this regular expression.
    } deriving (Show, Generic)

-- | The regular expression pattern that a string configuration option value
-- with this restriction must match.
orrPattern
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionRestrictionRegex
    -> f OptionRestrictionRegex
orrPattern f x =
    (\y -> x { _orrPattern = y })
       <$> f (_orrPattern x)
{-# INLINE orrPattern #-}

-- | A unique name representing this regular expression.
orrLabel
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionRestrictionRegex
    -> f OptionRestrictionRegex
orrLabel f x =
    (\y -> x { _orrLabel = y })
       <$> f (_orrLabel x)
{-# INLINE orrLabel #-}

instance FromXML OptionRestrictionRegex where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionRestrictionRegex"

instance ToQuery OptionRestrictionRegex where
    toQuery = genericQuery def

-- | A specification identifying an individual configuration option.
data OptionSpecification = OptionSpecification
    { _osNamespace :: Maybe Text
      -- ^ A unique namespace identifying the option's associated AWS
      -- resource.
    , _osOptionName :: Maybe Text
      -- ^ The name of the configuration option.
    } deriving (Show, Generic)

-- | A unique namespace identifying the option's associated AWS resource.
osNamespace
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionSpecification
    -> f OptionSpecification
osNamespace f x =
    (\y -> x { _osNamespace = y })
       <$> f (_osNamespace x)
{-# INLINE osNamespace #-}

-- | The name of the configuration option.
osOptionName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> OptionSpecification
    -> f OptionSpecification
osOptionName f x =
    (\y -> x { _osOptionName = y })
       <$> f (_osOptionName x)
{-# INLINE osOptionName #-}

instance ToQuery OptionSpecification where
    toQuery = genericQuery def

-- | Describes a queue.
data Queue = Queue
    { _qeName :: Maybe Text
      -- ^ The name of the queue.
    , _qeURL :: Maybe Text
      -- ^ The URL of the queue.
    } deriving (Show, Generic)

-- | The name of the queue.
qeName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Queue
    -> f Queue
qeName f x =
    (\y -> x { _qeName = y })
       <$> f (_qeName x)
{-# INLINE qeName #-}

-- | The URL of the queue.
qeURL
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Queue
    -> f Queue
qeURL f x =
    (\y -> x { _qeURL = y })
       <$> f (_qeURL x)
{-# INLINE qeURL #-}

instance FromXML Queue where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Queue"

-- | The Amazon S3 bucket and key that identify the location of the source
-- bundle for this version. If data found at the Amazon S3 location exceeds
-- the maximum allowed source bundle size, AWS Elastic Beanstalk returns an
-- InvalidParameterValue error. The maximum size allowed is 512 MB. Default:
-- If not specified, AWS Elastic Beanstalk uses a sample application. If only
-- partially specified (for example, a bucket is provided but not the key) or
-- if no data is found at the Amazon S3 location, AWS Elastic Beanstalk
-- returns an InvalidParameterCombination error.
data S3Location = S3Location
    { _slS3Bucket :: Maybe Text
      -- ^ The Amazon S3 bucket where the data is located.
    , _slS3Key :: Maybe Text
      -- ^ The Amazon S3 key where the data is located.
    } deriving (Show, Generic)

-- | The Amazon S3 bucket where the data is located.
slS3Bucket
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> S3Location
    -> f S3Location
slS3Bucket f x =
    (\y -> x { _slS3Bucket = y })
       <$> f (_slS3Bucket x)
{-# INLINE slS3Bucket #-}

-- | The Amazon S3 key where the data is located.
slS3Key
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> S3Location
    -> f S3Location
slS3Key f x =
    (\y -> x { _slS3Key = y })
       <$> f (_slS3Key x)
{-# INLINE slS3Key #-}

instance FromXML S3Location where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "S3Location"

instance ToQuery S3Location where
    toQuery = genericQuery def

-- | Describes the solution stack.
data SolutionStackDescription = SolutionStackDescription
    { _ssdSolutionStackName :: Maybe Text
      -- ^ The name of the solution stack.
    , _ssdPermittedFileTypes :: [Text]
      -- ^ The permitted file types allowed for a solution stack.
    } deriving (Show, Generic)

-- | The name of the solution stack.
ssdSolutionStackName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SolutionStackDescription
    -> f SolutionStackDescription
ssdSolutionStackName f x =
    (\y -> x { _ssdSolutionStackName = y })
       <$> f (_ssdSolutionStackName x)
{-# INLINE ssdSolutionStackName #-}

-- | The permitted file types allowed for a solution stack.
ssdPermittedFileTypes
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> SolutionStackDescription
    -> f SolutionStackDescription
ssdPermittedFileTypes f x =
    (\y -> x { _ssdPermittedFileTypes = y })
       <$> f (_ssdPermittedFileTypes x)
{-# INLINE ssdPermittedFileTypes #-}

instance FromXML SolutionStackDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SolutionStackDescription"

-- | If specified, AWS Elastic Beanstalk uses the configuration values from the
-- specified configuration template to create a new configuration. Values
-- specified in the OptionSettings parameter of this call overrides any values
-- obtained from the SourceConfiguration. If no configuration template is
-- found, returns an InvalidParameterValue error. Constraint: If both the
-- solution stack name parameter and the source configuration parameters are
-- specified, the solution stack of the source configuration template must
-- match the specified solution stack name or else AWS Elastic Beanstalk
-- returns an InvalidParameterCombination error.
data SourceConfiguration = SourceConfiguration
    { _sdApplicationName :: Maybe Text
      -- ^ The name of the application associated with the configuration.
    , _sdTemplateName :: Maybe Text
      -- ^ The name of the configuration template.
    } deriving (Show, Generic)

-- | The name of the application associated with the configuration.
sdApplicationName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SourceConfiguration
    -> f SourceConfiguration
sdApplicationName f x =
    (\y -> x { _sdApplicationName = y })
       <$> f (_sdApplicationName x)
{-# INLINE sdApplicationName #-}

-- | The name of the configuration template.
sdTemplateName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SourceConfiguration
    -> f SourceConfiguration
sdTemplateName f x =
    (\y -> x { _sdTemplateName = y })
       <$> f (_sdTemplateName x)
{-# INLINE sdTemplateName #-}

instance ToQuery SourceConfiguration where
    toQuery = genericQuery def

-- | Describes a tag applied to a resource in an environment.
data Tag = Tag
    { _wKey :: Maybe Text
      -- ^ The key of the tag.
    , _wValue :: Maybe Text
      -- ^ The value of the tag.
    } deriving (Show, Generic)

-- | The key of the tag.
wKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Tag
    -> f Tag
wKey f x =
    (\y -> x { _wKey = y })
       <$> f (_wKey x)
{-# INLINE wKey #-}

-- | The value of the tag.
wValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Tag
    -> f Tag
wValue f x =
    (\y -> x { _wValue = y })
       <$> f (_wValue x)
{-# INLINE wValue #-}

instance ToQuery Tag where
    toQuery = genericQuery def

-- | An error or warning for a desired configuration option value.
data ValidationMessage = ValidationMessage
    { _vveMessage :: Maybe Text
      -- ^ A message describing the error or warning.
    , _vveSeverity :: Maybe ValidationSeverity
      -- ^ An indication of the severity of this message: error: This
      -- message indicates that this is not a valid setting for an option.
      -- warning: This message is providing information you should take
      -- into account. error: This message indicates that this is not a
      -- valid setting for an option. warning: This message is providing
      -- information you should take into account.
    , _vveNamespace :: Maybe Text
      -- ^ 
    , _vveOptionName :: Maybe Text
      -- ^ 
    } deriving (Show, Generic)

-- | A message describing the error or warning.
vveMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ValidationMessage
    -> f ValidationMessage
vveMessage f x =
    (\y -> x { _vveMessage = y })
       <$> f (_vveMessage x)
{-# INLINE vveMessage #-}

-- | An indication of the severity of this message: error: This message
-- indicates that this is not a valid setting for an option. warning: This
-- message is providing information you should take into account. error: This
-- message indicates that this is not a valid setting for an option. warning:
-- This message is providing information you should take into account.
vveSeverity
    :: Functor f
    => (Maybe ValidationSeverity
    -> f (Maybe ValidationSeverity))
    -> ValidationMessage
    -> f ValidationMessage
vveSeverity f x =
    (\y -> x { _vveSeverity = y })
       <$> f (_vveSeverity x)
{-# INLINE vveSeverity #-}

-- | 
vveNamespace
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ValidationMessage
    -> f ValidationMessage
vveNamespace f x =
    (\y -> x { _vveNamespace = y })
       <$> f (_vveNamespace x)
{-# INLINE vveNamespace #-}

-- | 
vveOptionName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ValidationMessage
    -> f ValidationMessage
vveOptionName f x =
    (\y -> x { _vveOptionName = y })
       <$> f (_vveOptionName x)
{-# INLINE vveOptionName #-}

instance FromXML ValidationMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ValidationMessage"
