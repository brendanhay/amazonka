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
asgName :: Lens' AutoScalingGroup (Maybe Text)
asgName f x =
    f (_asgName x)
        <&> \y -> x { _asgName = y }
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
erdLoadBalancer :: Lens' EnvironmentResourcesDescription (Maybe LoadBalancerDescription)
erdLoadBalancer f x =
    f (_erdLoadBalancer x)
        <&> \y -> x { _erdLoadBalancer = y }
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
rId :: Lens' Instance (Maybe Text)
rId f x =
    f (_rId x)
        <&> \y -> x { _rId = y }
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
lcName :: Lens' LaunchConfiguration (Maybe Text)
lcName f x =
    f (_lcName x)
        <&> \y -> x { _lcName = y }
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
lbName :: Lens' LoadBalancer (Maybe Text)
lbName f x =
    f (_lbName x)
        <&> \y -> x { _lbName = y }
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
trName :: Lens' Trigger (Maybe Text)
trName f x =
    f (_trName x)
        <&> \y -> x { _trName = y }
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
adApplicationName :: Lens' ApplicationDescription (Maybe Text)
adApplicationName f x =
    f (_adApplicationName x)
        <&> \y -> x { _adApplicationName = y }
{-# INLINE adApplicationName #-}

-- | User-defined description of the application.
adDescription :: Lens' ApplicationDescription (Maybe Text)
adDescription f x =
    f (_adDescription x)
        <&> \y -> x { _adDescription = y }
{-# INLINE adDescription #-}

-- | The date when the application was created.
adDateCreated :: Lens' ApplicationDescription (Maybe ISO8601)
adDateCreated f x =
    f (_adDateCreated x)
        <&> \y -> x { _adDateCreated = y }
{-# INLINE adDateCreated #-}

-- | The date when the application was last modified.
adDateUpdated :: Lens' ApplicationDescription (Maybe ISO8601)
adDateUpdated f x =
    f (_adDateUpdated x)
        <&> \y -> x { _adDateUpdated = y }
{-# INLINE adDateUpdated #-}

-- | The names of the versions for this application.
adVersions :: Lens' ApplicationDescription ([Text])
adVersions f x =
    f (_adVersions x)
        <&> \y -> x { _adVersions = y }
{-# INLINE adVersions #-}

-- | The names of the configuration templates associated with this application.
adConfigurationTemplates :: Lens' ApplicationDescription ([Text])
adConfigurationTemplates f x =
    f (_adConfigurationTemplates x)
        <&> \y -> x { _adConfigurationTemplates = y }
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
avdApplicationName :: Lens' ApplicationVersionDescription (Maybe Text)
avdApplicationName f x =
    f (_avdApplicationName x)
        <&> \y -> x { _avdApplicationName = y }
{-# INLINE avdApplicationName #-}

-- | The description of this application version.
avdDescription :: Lens' ApplicationVersionDescription (Maybe Text)
avdDescription f x =
    f (_avdDescription x)
        <&> \y -> x { _avdDescription = y }
{-# INLINE avdDescription #-}

-- | A label uniquely identifying the version for the associated application.
avdVersionLabel :: Lens' ApplicationVersionDescription (Maybe Text)
avdVersionLabel f x =
    f (_avdVersionLabel x)
        <&> \y -> x { _avdVersionLabel = y }
{-# INLINE avdVersionLabel #-}

-- | The location where the source bundle is located for this version.
avdSourceBundle :: Lens' ApplicationVersionDescription (Maybe S3Location)
avdSourceBundle f x =
    f (_avdSourceBundle x)
        <&> \y -> x { _avdSourceBundle = y }
{-# INLINE avdSourceBundle #-}

-- | The creation date of the application version.
avdDateCreated :: Lens' ApplicationVersionDescription (Maybe ISO8601)
avdDateCreated f x =
    f (_avdDateCreated x)
        <&> \y -> x { _avdDateCreated = y }
{-# INLINE avdDateCreated #-}

-- | The last modified date of the application version.
avdDateUpdated :: Lens' ApplicationVersionDescription (Maybe ISO8601)
avdDateUpdated f x =
    f (_avdDateUpdated x)
        <&> \y -> x { _avdDateUpdated = y }
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
coeNamespace :: Lens' ConfigurationOptionDescription (Maybe Text)
coeNamespace f x =
    f (_coeNamespace x)
        <&> \y -> x { _coeNamespace = y }
{-# INLINE coeNamespace #-}

-- | The name of the configuration option.
coeName :: Lens' ConfigurationOptionDescription (Maybe Text)
coeName f x =
    f (_coeName x)
        <&> \y -> x { _coeName = y }
{-# INLINE coeName #-}

-- | The default value for this configuration option.
coeDefaultValue :: Lens' ConfigurationOptionDescription (Maybe Text)
coeDefaultValue f x =
    f (_coeDefaultValue x)
        <&> \y -> x { _coeDefaultValue = y }
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
coeChangeSeverity :: Lens' ConfigurationOptionDescription (Maybe Text)
coeChangeSeverity f x =
    f (_coeChangeSeverity x)
        <&> \y -> x { _coeChangeSeverity = y }
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
coeUserDefined :: Lens' ConfigurationOptionDescription (Maybe Bool)
coeUserDefined f x =
    f (_coeUserDefined x)
        <&> \y -> x { _coeUserDefined = y }
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
coeValueType :: Lens' ConfigurationOptionDescription (Maybe ConfigurationOptionValueType)
coeValueType f x =
    f (_coeValueType x)
        <&> \y -> x { _coeValueType = y }
{-# INLINE coeValueType #-}

-- | If specified, values for the configuration option are selected from this
-- list.
coeValueOptions :: Lens' ConfigurationOptionDescription ([Text])
coeValueOptions f x =
    f (_coeValueOptions x)
        <&> \y -> x { _coeValueOptions = y }
{-# INLINE coeValueOptions #-}

-- | If specified, the configuration option must be a numeric value greater than
-- this value.
coeMinValue :: Lens' ConfigurationOptionDescription (Maybe Integer)
coeMinValue f x =
    f (_coeMinValue x)
        <&> \y -> x { _coeMinValue = y }
{-# INLINE coeMinValue #-}

-- | If specified, the configuration option must be a numeric value less than
-- this value.
coeMaxValue :: Lens' ConfigurationOptionDescription (Maybe Integer)
coeMaxValue f x =
    f (_coeMaxValue x)
        <&> \y -> x { _coeMaxValue = y }
{-# INLINE coeMaxValue #-}

-- | If specified, the configuration option must be a string value no longer
-- than this value.
coeMaxLength :: Lens' ConfigurationOptionDescription (Maybe Integer)
coeMaxLength f x =
    f (_coeMaxLength x)
        <&> \y -> x { _coeMaxLength = y }
{-# INLINE coeMaxLength #-}

-- | If specified, the configuration option must be a string value that
-- satisfies this regular expression.
coeRegex :: Lens' ConfigurationOptionDescription (Maybe OptionRestrictionRegex)
coeRegex f x =
    f (_coeRegex x)
        <&> \y -> x { _coeRegex = y }
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
cosNamespace :: Lens' ConfigurationOptionSetting (Maybe Text)
cosNamespace f x =
    f (_cosNamespace x)
        <&> \y -> x { _cosNamespace = y }
{-# INLINE cosNamespace #-}

-- | The name of the configuration option.
cosOptionName :: Lens' ConfigurationOptionSetting (Maybe Text)
cosOptionName f x =
    f (_cosOptionName x)
        <&> \y -> x { _cosOptionName = y }
{-# INLINE cosOptionName #-}

-- | The current value for the configuration option.
cosValue :: Lens' ConfigurationOptionSetting (Maybe Text)
cosValue f x =
    f (_cosValue x)
        <&> \y -> x { _cosValue = y }
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
csfSolutionStackName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csfSolutionStackName f x =
    f (_csfSolutionStackName x)
        <&> \y -> x { _csfSolutionStackName = y }
{-# INLINE csfSolutionStackName #-}

-- | The name of the application associated with this configuration set.
csfApplicationName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csfApplicationName f x =
    f (_csfApplicationName x)
        <&> \y -> x { _csfApplicationName = y }
{-# INLINE csfApplicationName #-}

-- | If not null, the name of the configuration template for this configuration
-- set.
csfTemplateName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csfTemplateName f x =
    f (_csfTemplateName x)
        <&> \y -> x { _csfTemplateName = y }
{-# INLINE csfTemplateName #-}

-- | Describes this configuration set.
csfDescription :: Lens' ConfigurationSettingsDescription (Maybe Text)
csfDescription f x =
    f (_csfDescription x)
        <&> \y -> x { _csfDescription = y }
{-# INLINE csfDescription #-}

-- | If not null, the name of the environment for this configuration set.
csfEnvironmentName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csfEnvironmentName f x =
    f (_csfEnvironmentName x)
        <&> \y -> x { _csfEnvironmentName = y }
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
csfDeploymentStatus :: Lens' ConfigurationSettingsDescription (Maybe ConfigurationDeploymentStatus)
csfDeploymentStatus f x =
    f (_csfDeploymentStatus x)
        <&> \y -> x { _csfDeploymentStatus = y }
{-# INLINE csfDeploymentStatus #-}

-- | The date (in UTC time) when this configuration set was created.
csfDateCreated :: Lens' ConfigurationSettingsDescription (Maybe ISO8601)
csfDateCreated f x =
    f (_csfDateCreated x)
        <&> \y -> x { _csfDateCreated = y }
{-# INLINE csfDateCreated #-}

-- | The date (in UTC time) when this configuration set was last modified.
csfDateUpdated :: Lens' ConfigurationSettingsDescription (Maybe ISO8601)
csfDateUpdated f x =
    f (_csfDateUpdated x)
        <&> \y -> x { _csfDateUpdated = y }
{-# INLINE csfDateUpdated #-}

-- | A list of the configuration options and their values in this configuration
-- set.
csfOptionSettings :: Lens' ConfigurationSettingsDescription ([ConfigurationOptionSetting])
csfOptionSettings f x =
    f (_csfOptionSettings x)
        <&> \y -> x { _csfOptionSettings = y }
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
eeEnvironmentName :: Lens' EnvironmentDescription (Maybe Text)
eeEnvironmentName f x =
    f (_eeEnvironmentName x)
        <&> \y -> x { _eeEnvironmentName = y }
{-# INLINE eeEnvironmentName #-}

-- | The ID of this environment.
eeEnvironmentId :: Lens' EnvironmentDescription (Maybe Text)
eeEnvironmentId f x =
    f (_eeEnvironmentId x)
        <&> \y -> x { _eeEnvironmentId = y }
{-# INLINE eeEnvironmentId #-}

-- | The name of the application associated with this environment.
eeApplicationName :: Lens' EnvironmentDescription (Maybe Text)
eeApplicationName f x =
    f (_eeApplicationName x)
        <&> \y -> x { _eeApplicationName = y }
{-# INLINE eeApplicationName #-}

-- | The application version deployed in this environment.
eeVersionLabel :: Lens' EnvironmentDescription (Maybe Text)
eeVersionLabel f x =
    f (_eeVersionLabel x)
        <&> \y -> x { _eeVersionLabel = y }
{-# INLINE eeVersionLabel #-}

-- | The name of the SolutionStack deployed with this environment.
eeSolutionStackName :: Lens' EnvironmentDescription (Maybe Text)
eeSolutionStackName f x =
    f (_eeSolutionStackName x)
        <&> \y -> x { _eeSolutionStackName = y }
{-# INLINE eeSolutionStackName #-}

-- | The name of the configuration template used to originally launch this
-- environment.
eeTemplateName :: Lens' EnvironmentDescription (Maybe Text)
eeTemplateName f x =
    f (_eeTemplateName x)
        <&> \y -> x { _eeTemplateName = y }
{-# INLINE eeTemplateName #-}

-- | Describes this environment.
eeDescription :: Lens' EnvironmentDescription (Maybe Text)
eeDescription f x =
    f (_eeDescription x)
        <&> \y -> x { _eeDescription = y }
{-# INLINE eeDescription #-}

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer.
-- For single-instance environments, the IP address of the instance.
eeEndpointURL :: Lens' EnvironmentDescription (Maybe Text)
eeEndpointURL f x =
    f (_eeEndpointURL x)
        <&> \y -> x { _eeEndpointURL = y }
{-# INLINE eeEndpointURL #-}

-- | The URL to the CNAME for this environment.
eeCNAME :: Lens' EnvironmentDescription (Maybe Text)
eeCNAME f x =
    f (_eeCNAME x)
        <&> \y -> x { _eeCNAME = y }
{-# INLINE eeCNAME #-}

-- | The creation date for this environment.
eeDateCreated :: Lens' EnvironmentDescription (Maybe ISO8601)
eeDateCreated f x =
    f (_eeDateCreated x)
        <&> \y -> x { _eeDateCreated = y }
{-# INLINE eeDateCreated #-}

-- | The last modified date for this environment.
eeDateUpdated :: Lens' EnvironmentDescription (Maybe ISO8601)
eeDateUpdated f x =
    f (_eeDateUpdated x)
        <&> \y -> x { _eeDateUpdated = y }
{-# INLINE eeDateUpdated #-}

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such as
-- update or terminate. Terminating: Environment is in the shut-down process.
-- Terminated: Environment is not running.
eeStatus :: Lens' EnvironmentDescription (Maybe EnvironmentStatus)
eeStatus f x =
    f (_eeStatus x)
        <&> \y -> x { _eeStatus = y }
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
eeHealth :: Lens' EnvironmentDescription (Maybe EnvironmentHealth)
eeHealth f x =
    f (_eeHealth x)
        <&> \y -> x { _eeHealth = y }
{-# INLINE eeHealth #-}

-- | The description of the AWS resources used by this environment.
eeResources :: Lens' EnvironmentDescription (Maybe EnvironmentResourcesDescription)
eeResources f x =
    f (_eeResources x)
        <&> \y -> x { _eeResources = y }
{-# INLINE eeResources #-}

-- | Describes the current tier of this environment.
eeTier :: Lens' EnvironmentDescription (Maybe EnvironmentTier)
eeTier f x =
    f (_eeTier x)
        <&> \y -> x { _eeTier = y }
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
eidInfoType :: Lens' EnvironmentInfoDescription (Maybe EnvironmentInfoType)
eidInfoType f x =
    f (_eidInfoType x)
        <&> \y -> x { _eidInfoType = y }
{-# INLINE eidInfoType #-}

-- | The Amazon EC2 Instance ID for this information.
eidEc2InstanceId :: Lens' EnvironmentInfoDescription (Maybe Text)
eidEc2InstanceId f x =
    f (_eidEc2InstanceId x)
        <&> \y -> x { _eidEc2InstanceId = y }
{-# INLINE eidEc2InstanceId #-}

-- | The time stamp when this information was retrieved.
eidSampleTimestamp :: Lens' EnvironmentInfoDescription (Maybe ISO8601)
eidSampleTimestamp f x =
    f (_eidSampleTimestamp x)
        <&> \y -> x { _eidSampleTimestamp = y }
{-# INLINE eidSampleTimestamp #-}

-- | The retrieved information.
eidMessage :: Lens' EnvironmentInfoDescription (Maybe Text)
eidMessage f x =
    f (_eidMessage x)
        <&> \y -> x { _eidMessage = y }
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
ereEnvironmentName :: Lens' EnvironmentResourceDescription (Maybe Text)
ereEnvironmentName f x =
    f (_ereEnvironmentName x)
        <&> \y -> x { _ereEnvironmentName = y }
{-# INLINE ereEnvironmentName #-}

-- | The AutoScalingGroups used by this environment.
ereAutoScalingGroups :: Lens' EnvironmentResourceDescription ([AutoScalingGroup])
ereAutoScalingGroups f x =
    f (_ereAutoScalingGroups x)
        <&> \y -> x { _ereAutoScalingGroups = y }
{-# INLINE ereAutoScalingGroups #-}

-- | The Amazon EC2 instances used by this environment.
ereInstances :: Lens' EnvironmentResourceDescription ([Instance])
ereInstances f x =
    f (_ereInstances x)
        <&> \y -> x { _ereInstances = y }
{-# INLINE ereInstances #-}

-- | The Auto Scaling launch configurations in use by this environment.
ereLaunchConfigurations :: Lens' EnvironmentResourceDescription ([LaunchConfiguration])
ereLaunchConfigurations f x =
    f (_ereLaunchConfigurations x)
        <&> \y -> x { _ereLaunchConfigurations = y }
{-# INLINE ereLaunchConfigurations #-}

-- | The LoadBalancers in use by this environment.
ereLoadBalancers :: Lens' EnvironmentResourceDescription ([LoadBalancer])
ereLoadBalancers f x =
    f (_ereLoadBalancers x)
        <&> \y -> x { _ereLoadBalancers = y }
{-# INLINE ereLoadBalancers #-}

-- | The AutoScaling triggers in use by this environment.
ereTriggers :: Lens' EnvironmentResourceDescription ([Trigger])
ereTriggers f x =
    f (_ereTriggers x)
        <&> \y -> x { _ereTriggers = y }
{-# INLINE ereTriggers #-}

-- | The queues used by this environment.
ereQueues :: Lens' EnvironmentResourceDescription ([Queue])
ereQueues f x =
    f (_ereQueues x)
        <&> \y -> x { _ereQueues = y }
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
etName :: Lens' EnvironmentTier (Maybe Text)
etName f x =
    f (_etName x)
        <&> \y -> x { _etName = y }
{-# INLINE etName #-}

-- | The type of this environment tier.
etType :: Lens' EnvironmentTier (Maybe Text)
etType f x =
    f (_etType x)
        <&> \y -> x { _etType = y }
{-# INLINE etType #-}

-- | The version of this environment tier.
etVersion :: Lens' EnvironmentTier (Maybe Text)
etVersion f x =
    f (_etVersion x)
        <&> \y -> x { _etVersion = y }
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
efEventDate :: Lens' EventDescription (Maybe ISO8601)
efEventDate f x =
    f (_efEventDate x)
        <&> \y -> x { _efEventDate = y }
{-# INLINE efEventDate #-}

-- | The event message.
efMessage :: Lens' EventDescription (Maybe Text)
efMessage f x =
    f (_efMessage x)
        <&> \y -> x { _efMessage = y }
{-# INLINE efMessage #-}

-- | The application associated with the event.
efApplicationName :: Lens' EventDescription (Maybe Text)
efApplicationName f x =
    f (_efApplicationName x)
        <&> \y -> x { _efApplicationName = y }
{-# INLINE efApplicationName #-}

-- | The release label for the application version associated with this event.
efVersionLabel :: Lens' EventDescription (Maybe Text)
efVersionLabel f x =
    f (_efVersionLabel x)
        <&> \y -> x { _efVersionLabel = y }
{-# INLINE efVersionLabel #-}

-- | The name of the configuration associated with this event.
efTemplateName :: Lens' EventDescription (Maybe Text)
efTemplateName f x =
    f (_efTemplateName x)
        <&> \y -> x { _efTemplateName = y }
{-# INLINE efTemplateName #-}

-- | The name of the environment associated with this event.
efEnvironmentName :: Lens' EventDescription (Maybe Text)
efEnvironmentName f x =
    f (_efEnvironmentName x)
        <&> \y -> x { _efEnvironmentName = y }
{-# INLINE efEnvironmentName #-}

-- | The web service request ID for the activity of this event.
efRequestId :: Lens' EventDescription (Maybe Text)
efRequestId f x =
    f (_efRequestId x)
        <&> \y -> x { _efRequestId = y }
{-# INLINE efRequestId #-}

-- | The severity level of this event.
efSeverity :: Lens' EventDescription (Maybe EventSeverity)
efSeverity f x =
    f (_efSeverity x)
        <&> \y -> x { _efSeverity = y }
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
lProtocol :: Lens' Listener (Maybe Text)
lProtocol f x =
    f (_lProtocol x)
        <&> \y -> x { _lProtocol = y }
{-# INLINE lProtocol #-}

-- | The port that is used by the Listener.
lPort :: Lens' Listener (Maybe Integer)
lPort f x =
    f (_lPort x)
        <&> \y -> x { _lPort = y }
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
lbdLoadBalancerName :: Lens' LoadBalancerDescription (Maybe Text)
lbdLoadBalancerName f x =
    f (_lbdLoadBalancerName x)
        <&> \y -> x { _lbdLoadBalancerName = y }
{-# INLINE lbdLoadBalancerName #-}

-- | The domain name of the LoadBalancer.
lbdDomain :: Lens' LoadBalancerDescription (Maybe Text)
lbdDomain f x =
    f (_lbdDomain x)
        <&> \y -> x { _lbdDomain = y }
{-# INLINE lbdDomain #-}

-- | A list of Listeners used by the LoadBalancer.
lbdListeners :: Lens' LoadBalancerDescription ([Listener])
lbdListeners f x =
    f (_lbdListeners x)
        <&> \y -> x { _lbdListeners = y }
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
orrPattern :: Lens' OptionRestrictionRegex (Maybe Text)
orrPattern f x =
    f (_orrPattern x)
        <&> \y -> x { _orrPattern = y }
{-# INLINE orrPattern #-}

-- | A unique name representing this regular expression.
orrLabel :: Lens' OptionRestrictionRegex (Maybe Text)
orrLabel f x =
    f (_orrLabel x)
        <&> \y -> x { _orrLabel = y }
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
osNamespace :: Lens' OptionSpecification (Maybe Text)
osNamespace f x =
    f (_osNamespace x)
        <&> \y -> x { _osNamespace = y }
{-# INLINE osNamespace #-}

-- | The name of the configuration option.
osOptionName :: Lens' OptionSpecification (Maybe Text)
osOptionName f x =
    f (_osOptionName x)
        <&> \y -> x { _osOptionName = y }
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
qeName :: Lens' Queue (Maybe Text)
qeName f x =
    f (_qeName x)
        <&> \y -> x { _qeName = y }
{-# INLINE qeName #-}

-- | The URL of the queue.
qeURL :: Lens' Queue (Maybe Text)
qeURL f x =
    f (_qeURL x)
        <&> \y -> x { _qeURL = y }
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
slS3Bucket :: Lens' S3Location (Maybe Text)
slS3Bucket f x =
    f (_slS3Bucket x)
        <&> \y -> x { _slS3Bucket = y }
{-# INLINE slS3Bucket #-}

-- | The Amazon S3 key where the data is located.
slS3Key :: Lens' S3Location (Maybe Text)
slS3Key f x =
    f (_slS3Key x)
        <&> \y -> x { _slS3Key = y }
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
ssdSolutionStackName :: Lens' SolutionStackDescription (Maybe Text)
ssdSolutionStackName f x =
    f (_ssdSolutionStackName x)
        <&> \y -> x { _ssdSolutionStackName = y }
{-# INLINE ssdSolutionStackName #-}

-- | The permitted file types allowed for a solution stack.
ssdPermittedFileTypes :: Lens' SolutionStackDescription ([Text])
ssdPermittedFileTypes f x =
    f (_ssdPermittedFileTypes x)
        <&> \y -> x { _ssdPermittedFileTypes = y }
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
sdApplicationName :: Lens' SourceConfiguration (Maybe Text)
sdApplicationName f x =
    f (_sdApplicationName x)
        <&> \y -> x { _sdApplicationName = y }
{-# INLINE sdApplicationName #-}

-- | The name of the configuration template.
sdTemplateName :: Lens' SourceConfiguration (Maybe Text)
sdTemplateName f x =
    f (_sdTemplateName x)
        <&> \y -> x { _sdTemplateName = y }
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
wKey :: Lens' Tag (Maybe Text)
wKey f x =
    f (_wKey x)
        <&> \y -> x { _wKey = y }
{-# INLINE wKey #-}

-- | The value of the tag.
wValue :: Lens' Tag (Maybe Text)
wValue f x =
    f (_wValue x)
        <&> \y -> x { _wValue = y }
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
vveMessage :: Lens' ValidationMessage (Maybe Text)
vveMessage f x =
    f (_vveMessage x)
        <&> \y -> x { _vveMessage = y }
{-# INLINE vveMessage #-}

-- | An indication of the severity of this message: error: This message
-- indicates that this is not a valid setting for an option. warning: This
-- message is providing information you should take into account. error: This
-- message indicates that this is not a valid setting for an option. warning:
-- This message is providing information you should take into account.
vveSeverity :: Lens' ValidationMessage (Maybe ValidationSeverity)
vveSeverity f x =
    f (_vveSeverity x)
        <&> \y -> x { _vveSeverity = y }
{-# INLINE vveSeverity #-}

-- | 
vveNamespace :: Lens' ValidationMessage (Maybe Text)
vveNamespace f x =
    f (_vveNamespace x)
        <&> \y -> x { _vveNamespace = y }
{-# INLINE vveNamespace #-}

-- | 
vveOptionName :: Lens' ValidationMessage (Maybe Text)
vveOptionName f x =
    f (_vveOptionName x)
        <&> \y -> x { _vveOptionName = y }
{-# INLINE vveOptionName #-}

instance FromXML ValidationMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ValidationMessage"
