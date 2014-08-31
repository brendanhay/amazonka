{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.ElasticBeanstalk.V2010_12_01.Types where

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

instance FromXML AutoScalingGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AutoScalingGroup"

-- | The description of the AWS resources used by this environment.
newtype EnvironmentResourcesDescription = EnvironmentResourcesDescription
    { _erdLoadBalancer :: Maybe LoadBalancerDescription
      -- ^ Describes the LoadBalancer.
    } deriving (Show, Generic)

instance FromXML EnvironmentResourcesDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentResourcesDescription"

instance ToQuery EnvironmentResourcesDescription where
    toQuery = genericQuery def

-- | The description of an Amazon EC2 instance.
newtype Instance = Instance
    { _ieId :: Maybe Text
      -- ^ The ID of the Amazon EC2 instance.
    } deriving (Show, Generic)

instance FromXML Instance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Instance"

-- | Describes an Auto Scaling launch configuration.
newtype LaunchConfiguration = LaunchConfiguration
    { _lcName :: Maybe Text
      -- ^ The name of the launch configuration.
    } deriving (Show, Generic)

instance FromXML LaunchConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LaunchConfiguration"

-- | Describes a LoadBalancer.
newtype LoadBalancer = LoadBalancer
    { _lbName :: Maybe Text
      -- ^ The name of the LoadBalancer.
    } deriving (Show, Generic)

instance FromXML LoadBalancer where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LoadBalancer"

-- | Describes a trigger.
newtype Trigger = Trigger
    { _trName :: Maybe Text
      -- ^ The name of the trigger.
    } deriving (Show, Generic)

instance FromXML Trigger where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Trigger"

-- | Describes the properties of an application.
data ApplicationDescription = ApplicationDescription
    { _adVersions :: [Text]
      -- ^ The names of the versions for this application.
    , _adDateUpdated :: Maybe ISO8601
      -- ^ The date when the application was last modified.
    , _adDateCreated :: Maybe ISO8601
      -- ^ The date when the application was created.
    , _adApplicationName :: Maybe Text
      -- ^ The name of the application.
    , _adConfigurationTemplates :: [Text]
      -- ^ The names of the configuration templates associated with this
      -- application.
    , _adDescription :: Maybe Text
      -- ^ User-defined description of the application.
    } deriving (Show, Generic)

instance FromXML ApplicationDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ApplicationDescription"

-- | The ApplicationVersionDescription of the application version.
data ApplicationVersionDescription = ApplicationVersionDescription
    { _avdSourceBundle :: Maybe S3Location
      -- ^ The location where the source bundle is located for this version.
    , _avdDateUpdated :: Maybe ISO8601
      -- ^ The last modified date of the application version.
    , _avdDateCreated :: Maybe ISO8601
      -- ^ The creation date of the application version.
    , _avdVersionLabel :: Maybe Text
      -- ^ A label uniquely identifying the version for the associated
      -- application.
    , _avdApplicationName :: Maybe Text
      -- ^ The name of the application associated with this release.
    , _avdDescription :: Maybe Text
      -- ^ The description of this application version.
    } deriving (Show, Generic)

instance FromXML ApplicationVersionDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ApplicationVersionDescription"

-- | Describes the possible values for a configuration option.
data ConfigurationOptionDescription = ConfigurationOptionDescription
    { _coeMaxValue :: Maybe Integer
      -- ^ If specified, the configuration option must be a numeric value
      -- less than this value.
    , _coeRegex :: Maybe OptionRestrictionRegex
      -- ^ If specified, the configuration option must be a string value
      -- that satisfies this regular expression.
    , _coeMaxLength :: Maybe Integer
      -- ^ If specified, the configuration option must be a string value no
      -- longer than this value.
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
    , _coeNamespace :: Maybe Text
      -- ^ A unique namespace identifying the option's associated AWS
      -- resource.
    , _coeValueOptions :: [Text]
      -- ^ If specified, values for the configuration option are selected
      -- from this list.
    , _coeName :: Maybe Text
      -- ^ The name of the configuration option.
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
    , _coeDefaultValue :: Maybe Text
      -- ^ The default value for this configuration option.
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
    , _coeMinValue :: Maybe Integer
      -- ^ If specified, the configuration option must be a numeric value
      -- greater than this value.
    } deriving (Show, Generic)

instance FromXML ConfigurationOptionDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ConfigurationOptionDescription"

-- | A specification identifying an individual configuration option along with
-- its current value. For a list of possible option values, go to Option
-- Values in the AWS Elastic Beanstalk Developer Guide.
data ConfigurationOptionSetting = ConfigurationOptionSetting
    { _cosOptionName :: Maybe Text
      -- ^ The name of the configuration option.
    , _cosNamespace :: Maybe Text
      -- ^ A unique namespace identifying the option's associated AWS
      -- resource.
    , _cosValue :: Maybe Text
      -- ^ The current value for the configuration option.
    } deriving (Show, Generic)

instance FromXML ConfigurationOptionSetting where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ConfigurationOptionSetting"

instance ToQuery ConfigurationOptionSetting where
    toQuery = genericQuery def

-- | Describes the settings for a configuration set.
data ConfigurationSettingsDescription = ConfigurationSettingsDescription
    { _csgTemplateName :: Maybe Text
      -- ^ If not null, the name of the configuration template for this
      -- configuration set.
    , _csgOptionSettings :: [ConfigurationOptionSetting]
      -- ^ A list of the configuration options and their values in this
      -- configuration set.
    , _csgDateUpdated :: Maybe ISO8601
      -- ^ The date (in UTC time) when this configuration set was last
      -- modified.
    , _csgDateCreated :: Maybe ISO8601
      -- ^ The date (in UTC time) when this configuration set was created.
    , _csgEnvironmentName :: Maybe Text
      -- ^ If not null, the name of the environment for this configuration
      -- set.
    , _csgApplicationName :: Maybe Text
      -- ^ The name of the application associated with this configuration
      -- set.
    , _csgDeploymentStatus :: Maybe ConfigurationDeploymentStatus
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
    , _csgSolutionStackName :: Maybe Text
      -- ^ The name of the solution stack this configuration set uses.
    , _csgDescription :: Maybe Text
      -- ^ Describes this configuration set.
    } deriving (Show, Generic)

instance FromXML ConfigurationSettingsDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ConfigurationSettingsDescription"

-- | Describes the properties of an environment.
data EnvironmentDescription = EnvironmentDescription
    { _eeenStatus :: Maybe EnvironmentStatus
      -- ^ The current operational status of the environment: Launching:
      -- Environment is in the process of initial deployment. Updating:
      -- Environment is in the process of updating its configuration
      -- settings or application version. Ready: Environment is available
      -- to have an action performed on it, such as update or terminate.
      -- Terminating: Environment is in the shut-down process. Terminated:
      -- Environment is not running.
    , _eeenCNAME :: Maybe Text
      -- ^ The URL to the CNAME for this environment.
    , _eeenTemplateName :: Maybe Text
      -- ^ The name of the configuration template used to originally launch
      -- this environment.
    , _eeenEndpointURL :: Maybe Text
      -- ^ For load-balanced, autoscaling environments, the URL to the
      -- LoadBalancer. For single-instance environments, the IP address of
      -- the instance.
    , _eeenResources :: Maybe EnvironmentResourcesDescription
      -- ^ The description of the AWS resources used by this environment.
    , _eeenDateUpdated :: Maybe ISO8601
      -- ^ The last modified date for this environment.
    , _eeenDateCreated :: Maybe ISO8601
      -- ^ The creation date for this environment.
    , _eeenHealth :: Maybe EnvironmentHealth
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
    , _eeenVersionLabel :: Maybe Text
      -- ^ The application version deployed in this environment.
    , _eeenTier :: Maybe EnvironmentTier
      -- ^ Describes the current tier of this environment.
    , _eeenEnvironmentName :: Maybe Text
      -- ^ The name of this environment.
    , _eeenApplicationName :: Maybe Text
      -- ^ The name of the application associated with this environment.
    , _eeenSolutionStackName :: Maybe Text
      -- ^ The name of the SolutionStack deployed with this environment.
    , _eeenEnvironmentId :: Maybe Text
      -- ^ The ID of this environment.
    , _eeenDescription :: Maybe Text
      -- ^ Describes this environment.
    } deriving (Show, Generic)

instance FromXML EnvironmentDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentDescription"

-- | The information retrieved from the Amazon EC2 instances.
data EnvironmentInfoDescription = EnvironmentInfoDescription
    { _eidSampleTimestamp :: Maybe ISO8601
      -- ^ The time stamp when this information was retrieved.
    , _eidEc2InstanceId :: Maybe Text
      -- ^ The Amazon EC2 Instance ID for this information.
    , _eidInfoType :: Maybe EnvironmentInfoType
      -- ^ The type of information retrieved.
    , _eidMessage :: Maybe Text
      -- ^ The retrieved information.
    } deriving (Show, Generic)

instance FromXML EnvironmentInfoDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentInfoDescription"

-- | A list of EnvironmentResourceDescription.
data EnvironmentResourceDescription = EnvironmentResourceDescription
    { _erfQueues :: [Queue]
      -- ^ The queues used by this environment.
    , _erfTriggers :: [Trigger]
      -- ^ The AutoScaling triggers in use by this environment.
    , _erfLoadBalancers :: [LoadBalancer]
      -- ^ The LoadBalancers in use by this environment.
    , _erfEnvironmentName :: Maybe Text
      -- ^ The name of the environment.
    , _erfInstances :: [Instance]
      -- ^ The Amazon EC2 instances used by this environment.
    , _erfLaunchConfigurations :: [LaunchConfiguration]
      -- ^ The Auto Scaling launch configurations in use by this
      -- environment.
    , _erfAutoScalingGroups :: [AutoScalingGroup]
      -- ^ The AutoScalingGroups used by this environment.
    } deriving (Show, Generic)

instance FromXML EnvironmentResourceDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentResourceDescription"

-- | This specifies the tier to use to update the environment. Condition: You
-- can only update the tier version for an environment. If you change the name
-- of the type, AWS Elastic Beanstalk returns InvalidParameterValue error.
data EnvironmentTier = EnvironmentTier
    { _etName :: Maybe Text
      -- ^ The name of this environment tier.
    , _etVersion :: Maybe Text
      -- ^ The version of this environment tier.
    , _etType :: Maybe Text
      -- ^ The type of this environment tier.
    } deriving (Show, Generic)

instance FromXML EnvironmentTier where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentTier"

instance ToQuery EnvironmentTier where
    toQuery = genericQuery def

-- | Describes an event.
data EventDescription = EventDescription
    { _efRequestId :: Maybe Text
      -- ^ The web service request ID for the activity of this event.
    , _efTemplateName :: Maybe Text
      -- ^ The name of the configuration associated with this event.
    , _efSeverity :: Maybe EventSeverity
      -- ^ The severity level of this event.
    , _efVersionLabel :: Maybe Text
      -- ^ The release label for the application version associated with
      -- this event.
    , _efEnvironmentName :: Maybe Text
      -- ^ The name of the environment associated with this event.
    , _efApplicationName :: Maybe Text
      -- ^ The application associated with the event.
    , _efEventDate :: Maybe ISO8601
      -- ^ The date when the event occurred.
    , _efMessage :: Maybe Text
      -- ^ The event message.
    } deriving (Show, Generic)

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

instance FromXML OptionRestrictionRegex where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionRestrictionRegex"

instance ToQuery OptionRestrictionRegex where
    toQuery = genericQuery def

-- | A specification identifying an individual configuration option.
data OptionSpecification = OptionSpecification
    { _osOptionName :: Maybe Text
      -- ^ The name of the configuration option.
    , _osNamespace :: Maybe Text
      -- ^ A unique namespace identifying the option's associated AWS
      -- resource.
    } deriving (Show, Generic)

instance ToQuery OptionSpecification where
    toQuery = genericQuery def

-- | Describes a queue.
data Queue = Queue
    { _qeURL :: Maybe Text
      -- ^ The URL of the queue.
    , _qeName :: Maybe Text
      -- ^ The name of the queue.
    } deriving (Show, Generic)

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
    { _snS3Key :: Maybe Text
      -- ^ The Amazon S3 key where the data is located.
    , _snS3Bucket :: Maybe Text
      -- ^ The Amazon S3 bucket where the data is located.
    } deriving (Show, Generic)

instance FromXML S3Location where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "S3Location"

instance ToQuery S3Location where
    toQuery = genericQuery def

-- | Describes the solution stack.
data SolutionStackDescription = SolutionStackDescription
    { _ssdPermittedFileTypes :: [Text]
      -- ^ The permitted file types allowed for a solution stack.
    , _ssdSolutionStackName :: Maybe Text
      -- ^ The name of the solution stack.
    } deriving (Show, Generic)

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
    { _seTemplateName :: Maybe Text
      -- ^ The name of the configuration template.
    , _seApplicationName :: Maybe Text
      -- ^ The name of the application associated with the configuration.
    } deriving (Show, Generic)

instance ToQuery SourceConfiguration where
    toQuery = genericQuery def

-- | Describes a tag applied to a resource in an environment.
data Tag = Tag
    { _tgValue :: Maybe Text
      -- ^ The value of the tag.
    , _tgKey :: Maybe Text
      -- ^ The key of the tag.
    } deriving (Show, Generic)

instance ToQuery Tag where
    toQuery = genericQuery def

-- | An error or warning for a desired configuration option value.
data ValidationMessage = ValidationMessage
    { _vyOptionName :: Maybe Text
      -- ^ 
    , _vySeverity :: Maybe ValidationSeverity
      -- ^ An indication of the severity of this message: error: This
      -- message indicates that this is not a valid setting for an option.
      -- warning: This message is providing information you should take
      -- into account. error: This message indicates that this is not a
      -- valid setting for an option. warning: This message is providing
      -- information you should take into account.
    , _vyNamespace :: Maybe Text
      -- ^ 
    , _vyMessage :: Maybe Text
      -- ^ A message describing the error or warning.
    } deriving (Show, Generic)

instance FromXML ValidationMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ValidationMessage"

makeLenses ''AutoScalingGroup
makeLenses ''EnvironmentResourcesDescription
makeLenses ''Instance
makeLenses ''LaunchConfiguration
makeLenses ''LoadBalancer
makeLenses ''Trigger
makeLenses ''ApplicationDescription
makeLenses ''ApplicationVersionDescription
makeLenses ''ConfigurationOptionDescription
makeLenses ''ConfigurationOptionSetting
makeLenses ''ConfigurationSettingsDescription
makeLenses ''EnvironmentDescription
makeLenses ''EnvironmentInfoDescription
makeLenses ''EnvironmentResourceDescription
makeLenses ''EnvironmentTier
makeLenses ''EventDescription
makeLenses ''Listener
makeLenses ''LoadBalancerDescription
makeLenses ''OptionRestrictionRegex
makeLenses ''OptionSpecification
makeLenses ''Queue
makeLenses ''S3Location
makeLenses ''SolutionStackDescription
makeLenses ''SourceConfiguration
makeLenses ''Tag
makeLenses ''ValidationMessage
