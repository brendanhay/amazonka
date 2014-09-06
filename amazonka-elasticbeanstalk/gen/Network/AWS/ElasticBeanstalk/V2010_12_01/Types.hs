{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
    , AutoScalingGroup
    , mkAutoScalingGroup
    , asgName

    -- * EnvironmentResourcesDescription
    , EnvironmentResourcesDescription
    , mkEnvironmentResourcesDescription
    , erdLoadBalancer

    -- * Instance
    , Instance
    , mkInstance
    , iId

    -- * LaunchConfiguration
    , LaunchConfiguration
    , mkLaunchConfiguration
    , lcName

    -- * LoadBalancer
    , LoadBalancer
    , mkLoadBalancer
    , lbName

    -- * Trigger
    , Trigger
    , mkTrigger
    , trsName

    -- * ApplicationDescription
    , ApplicationDescription
    , mkApplicationDescription
    , adApplicationName
    , adDescription
    , adDateCreated
    , adDateUpdated
    , adVersions
    , adConfigurationTemplates

    -- * ApplicationVersionDescription
    , ApplicationVersionDescription
    , mkApplicationVersionDescription
    , avdApplicationName
    , avdDescription
    , avdVersionLabel
    , avdSourceBundle
    , avdDateCreated
    , avdDateUpdated

    -- * ConfigurationOptionDescription
    , ConfigurationOptionDescription
    , mkConfigurationOptionDescription
    , codNamespace
    , codName
    , codDefaultValue
    , codChangeSeverity
    , codUserDefined
    , codValueType
    , codValueOptions
    , codMinValue
    , codMaxValue
    , codMaxLength
    , codRegex

    -- * ConfigurationOptionSetting
    , ConfigurationOptionSetting
    , mkConfigurationOptionSetting
    , cosNamespace
    , cosOptionName
    , cosValue

    -- * ConfigurationSettingsDescription
    , ConfigurationSettingsDescription
    , mkConfigurationSettingsDescription
    , csdSolutionStackName
    , csdApplicationName
    , csdTemplateName
    , csdDescription
    , csdEnvironmentName
    , csdDeploymentStatus
    , csdDateCreated
    , csdDateUpdated
    , csdOptionSettings

    -- * EnvironmentDescription
    , EnvironmentDescription
    , mkEnvironmentDescription
    , edEnvironmentName
    , edEnvironmentId
    , edApplicationName
    , edVersionLabel
    , edSolutionStackName
    , edTemplateName
    , edDescription
    , edEndpointURL
    , edCNAME
    , edDateCreated
    , edDateUpdated
    , edStatus
    , edHealth
    , edResources
    , edTier

    -- * EnvironmentInfoDescription
    , EnvironmentInfoDescription
    , mkEnvironmentInfoDescription
    , eidInfoType
    , eidEc2InstanceId
    , eidSampleTimestamp
    , eidMessage

    -- * EnvironmentResourceDescription
    , EnvironmentResourceDescription
    , mkEnvironmentResourceDescription
    , erdrsEnvironmentName
    , erdrsAutoScalingGroups
    , erdrsInstances
    , erdrsLaunchConfigurations
    , erdrsLoadBalancers
    , erdrsTriggers
    , erdrsQueues

    -- * EnvironmentTier
    , EnvironmentTier
    , mkEnvironmentTier
    , etName
    , etType
    , etVersion

    -- * EventDescription
    , EventDescription
    , mkEventDescription
    , edrsEventDate
    , edrsMessage
    , edrsApplicationName
    , edrsVersionLabel
    , edrsTemplateName
    , edrsEnvironmentName
    , edrsRequestId
    , edrsSeverity

    -- * Listener
    , Listener
    , mkListener
    , lProtocol
    , lPort

    -- * LoadBalancerDescription
    , LoadBalancerDescription
    , mkLoadBalancerDescription
    , lbdLoadBalancerName
    , lbdDomain
    , lbdListeners

    -- * OptionRestrictionRegex
    , OptionRestrictionRegex
    , mkOptionRestrictionRegex
    , orrPattern
    , orrLabel

    -- * OptionSpecification
    , OptionSpecification
    , mkOptionSpecification
    , osNamespace
    , osOptionName

    -- * Queue
    , Queue
    , mkQueue
    , qName
    , qURL

    -- * S3Location
    , S3Location
    , mkS3Location
    , slS3Bucket
    , slS3Key

    -- * SolutionStackDescription
    , SolutionStackDescription
    , mkSolutionStackDescription
    , ssdSolutionStackName
    , ssdPermittedFileTypes

    -- * SourceConfiguration
    , SourceConfiguration
    , mkSourceConfiguration
    , scApplicationName
    , scTemplateName

    -- * Tag
    , Tag
    , mkTag
    , tKey
    , tValue

    -- * ValidationMessage
    , ValidationMessage
    , mkValidationMessage
    , vmMessage
    , vmSeverity
    , vmNamespace
    , vmOptionName
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
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AutoScalingGroup' data type to populate a request.
mkAutoScalingGroup :: AutoScalingGroup
mkAutoScalingGroup = AutoScalingGroup
    { _asgName = Nothing
    }
{-# INLINE mkAutoScalingGroup #-}

-- | The name of the AutoScalingGroup .
asgName :: Lens' AutoScalingGroup (Maybe Text)
asgName = lens _asgName (\s a -> s { _asgName = a })
{-# INLINE asgName #-}

instance FromXML AutoScalingGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AutoScalingGroup"

-- | The description of the AWS resources used by this environment.
newtype EnvironmentResourcesDescription = EnvironmentResourcesDescription
    { _erdLoadBalancer :: Maybe LoadBalancerDescription
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EnvironmentResourcesDescription' data type to populate a request.
mkEnvironmentResourcesDescription :: EnvironmentResourcesDescription
mkEnvironmentResourcesDescription = EnvironmentResourcesDescription
    { _erdLoadBalancer = Nothing
    }
{-# INLINE mkEnvironmentResourcesDescription #-}

-- | Describes the LoadBalancer.
erdLoadBalancer :: Lens' EnvironmentResourcesDescription (Maybe LoadBalancerDescription)
erdLoadBalancer = lens _erdLoadBalancer (\s a -> s { _erdLoadBalancer = a })
{-# INLINE erdLoadBalancer #-}

instance FromXML EnvironmentResourcesDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentResourcesDescription"

instance ToQuery EnvironmentResourcesDescription where
    toQuery = genericQuery def

-- | The description of an Amazon EC2 instance.
newtype Instance = Instance
    { _iId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Instance' data type to populate a request.
mkInstance :: Instance
mkInstance = Instance
    { _iId = Nothing
    }
{-# INLINE mkInstance #-}

-- | The ID of the Amazon EC2 instance.
iId :: Lens' Instance (Maybe Text)
iId = lens _iId (\s a -> s { _iId = a })
{-# INLINE iId #-}

instance FromXML Instance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Instance"

-- | Describes an Auto Scaling launch configuration.
newtype LaunchConfiguration = LaunchConfiguration
    { _lcName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LaunchConfiguration' data type to populate a request.
mkLaunchConfiguration :: LaunchConfiguration
mkLaunchConfiguration = LaunchConfiguration
    { _lcName = Nothing
    }
{-# INLINE mkLaunchConfiguration #-}

-- | The name of the launch configuration.
lcName :: Lens' LaunchConfiguration (Maybe Text)
lcName = lens _lcName (\s a -> s { _lcName = a })
{-# INLINE lcName #-}

instance FromXML LaunchConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LaunchConfiguration"

-- | Describes a LoadBalancer.
newtype LoadBalancer = LoadBalancer
    { _lbName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LoadBalancer' data type to populate a request.
mkLoadBalancer :: LoadBalancer
mkLoadBalancer = LoadBalancer
    { _lbName = Nothing
    }
{-# INLINE mkLoadBalancer #-}

-- | The name of the LoadBalancer.
lbName :: Lens' LoadBalancer (Maybe Text)
lbName = lens _lbName (\s a -> s { _lbName = a })
{-# INLINE lbName #-}

instance FromXML LoadBalancer where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LoadBalancer"

-- | Describes a trigger.
newtype Trigger = Trigger
    { _trsName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Trigger' data type to populate a request.
mkTrigger :: Trigger
mkTrigger = Trigger
    { _trsName = Nothing
    }
{-# INLINE mkTrigger #-}

-- | The name of the trigger.
trsName :: Lens' Trigger (Maybe Text)
trsName = lens _trsName (\s a -> s { _trsName = a })
{-# INLINE trsName #-}

instance FromXML Trigger where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Trigger"

-- | The ApplicationDescription of the application.
data ApplicationDescription = ApplicationDescription
    { _adApplicationName :: Maybe Text
    , _adDescription :: Maybe Text
    , _adDateCreated :: Maybe ISO8601
    , _adDateUpdated :: Maybe ISO8601
    , _adVersions :: [Text]
    , _adConfigurationTemplates :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ApplicationDescription' data type to populate a request.
mkApplicationDescription :: ApplicationDescription
mkApplicationDescription = ApplicationDescription
    { _adApplicationName = Nothing
    , _adDescription = Nothing
    , _adDateCreated = Nothing
    , _adDateUpdated = Nothing
    , _adVersions = mempty
    , _adConfigurationTemplates = mempty
    }
{-# INLINE mkApplicationDescription #-}

-- | The name of the application.
adApplicationName :: Lens' ApplicationDescription (Maybe Text)
adApplicationName =
    lens _adApplicationName (\s a -> s { _adApplicationName = a })
{-# INLINE adApplicationName #-}

-- | User-defined description of the application.
adDescription :: Lens' ApplicationDescription (Maybe Text)
adDescription = lens _adDescription (\s a -> s { _adDescription = a })
{-# INLINE adDescription #-}

-- | The date when the application was created.
adDateCreated :: Lens' ApplicationDescription (Maybe ISO8601)
adDateCreated = lens _adDateCreated (\s a -> s { _adDateCreated = a })
{-# INLINE adDateCreated #-}

-- | The date when the application was last modified.
adDateUpdated :: Lens' ApplicationDescription (Maybe ISO8601)
adDateUpdated = lens _adDateUpdated (\s a -> s { _adDateUpdated = a })
{-# INLINE adDateUpdated #-}

-- | The names of the versions for this application.
adVersions :: Lens' ApplicationDescription [Text]
adVersions = lens _adVersions (\s a -> s { _adVersions = a })
{-# INLINE adVersions #-}

-- | The names of the configuration templates associated with this application.
adConfigurationTemplates :: Lens' ApplicationDescription [Text]
adConfigurationTemplates =
    lens _adConfigurationTemplates
         (\s a -> s { _adConfigurationTemplates = a })
{-# INLINE adConfigurationTemplates #-}

instance FromXML ApplicationDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ApplicationDescription"

-- | The ApplicationVersionDescription of the application version.
data ApplicationVersionDescription = ApplicationVersionDescription
    { _avdApplicationName :: Maybe Text
    , _avdDescription :: Maybe Text
    , _avdVersionLabel :: Maybe Text
    , _avdSourceBundle :: Maybe S3Location
    , _avdDateCreated :: Maybe ISO8601
    , _avdDateUpdated :: Maybe ISO8601
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ApplicationVersionDescription' data type to populate a request.
mkApplicationVersionDescription :: ApplicationVersionDescription
mkApplicationVersionDescription = ApplicationVersionDescription
    { _avdApplicationName = Nothing
    , _avdDescription = Nothing
    , _avdVersionLabel = Nothing
    , _avdSourceBundle = Nothing
    , _avdDateCreated = Nothing
    , _avdDateUpdated = Nothing
    }
{-# INLINE mkApplicationVersionDescription #-}

-- | The name of the application associated with this release.
avdApplicationName :: Lens' ApplicationVersionDescription (Maybe Text)
avdApplicationName =
    lens _avdApplicationName (\s a -> s { _avdApplicationName = a })
{-# INLINE avdApplicationName #-}

-- | The description of this application version.
avdDescription :: Lens' ApplicationVersionDescription (Maybe Text)
avdDescription = lens _avdDescription (\s a -> s { _avdDescription = a })
{-# INLINE avdDescription #-}

-- | A label uniquely identifying the version for the associated application.
avdVersionLabel :: Lens' ApplicationVersionDescription (Maybe Text)
avdVersionLabel = lens _avdVersionLabel (\s a -> s { _avdVersionLabel = a })
{-# INLINE avdVersionLabel #-}

-- | The location where the source bundle is located for this version.
avdSourceBundle :: Lens' ApplicationVersionDescription (Maybe S3Location)
avdSourceBundle = lens _avdSourceBundle (\s a -> s { _avdSourceBundle = a })
{-# INLINE avdSourceBundle #-}

-- | The creation date of the application version.
avdDateCreated :: Lens' ApplicationVersionDescription (Maybe ISO8601)
avdDateCreated = lens _avdDateCreated (\s a -> s { _avdDateCreated = a })
{-# INLINE avdDateCreated #-}

-- | The last modified date of the application version.
avdDateUpdated :: Lens' ApplicationVersionDescription (Maybe ISO8601)
avdDateUpdated = lens _avdDateUpdated (\s a -> s { _avdDateUpdated = a })
{-# INLINE avdDateUpdated #-}

instance FromXML ApplicationVersionDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ApplicationVersionDescription"

-- | Describes the possible values for a configuration option.
data ConfigurationOptionDescription = ConfigurationOptionDescription
    { _codNamespace :: Maybe Text
    , _codName :: Maybe Text
    , _codDefaultValue :: Maybe Text
    , _codChangeSeverity :: Maybe Text
    , _codUserDefined :: Maybe Bool
    , _codValueType :: Maybe ConfigurationOptionValueType
    , _codValueOptions :: [Text]
    , _codMinValue :: Maybe Integer
    , _codMaxValue :: Maybe Integer
    , _codMaxLength :: Maybe Integer
    , _codRegex :: Maybe OptionRestrictionRegex
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ConfigurationOptionDescription' data type to populate a request.
mkConfigurationOptionDescription :: ConfigurationOptionDescription
mkConfigurationOptionDescription = ConfigurationOptionDescription
    { _codNamespace = Nothing
    , _codName = Nothing
    , _codDefaultValue = Nothing
    , _codChangeSeverity = Nothing
    , _codUserDefined = Nothing
    , _codValueType = Nothing
    , _codValueOptions = mempty
    , _codMinValue = Nothing
    , _codMaxValue = Nothing
    , _codMaxLength = Nothing
    , _codRegex = Nothing
    }
{-# INLINE mkConfigurationOptionDescription #-}

-- | A unique namespace identifying the option's associated AWS resource.
codNamespace :: Lens' ConfigurationOptionDescription (Maybe Text)
codNamespace = lens _codNamespace (\s a -> s { _codNamespace = a })
{-# INLINE codNamespace #-}

-- | The name of the configuration option.
codName :: Lens' ConfigurationOptionDescription (Maybe Text)
codName = lens _codName (\s a -> s { _codName = a })
{-# INLINE codName #-}

-- | The default value for this configuration option.
codDefaultValue :: Lens' ConfigurationOptionDescription (Maybe Text)
codDefaultValue = lens _codDefaultValue (\s a -> s { _codDefaultValue = a })
{-# INLINE codDefaultValue #-}

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
codChangeSeverity :: Lens' ConfigurationOptionDescription (Maybe Text)
codChangeSeverity =
    lens _codChangeSeverity (\s a -> s { _codChangeSeverity = a })
{-# INLINE codChangeSeverity #-}

-- | An indication of whether the user defined this configuration option: true :
-- This configuration option was defined by the user. It is a valid choice for
-- specifying this as an Option to Remove when updating configuration
-- settings. false : This configuration was not defined by the user. true :
-- This configuration option was defined by the user. It is a valid choice for
-- specifying if this as an Option to Remove when updating configuration
-- settings. false : This configuration was not defined by the user.
-- Constraint: You can remove only UserDefined options from a configuration.
-- Valid Values: true | false.
codUserDefined :: Lens' ConfigurationOptionDescription (Maybe Bool)
codUserDefined = lens _codUserDefined (\s a -> s { _codUserDefined = a })
{-# INLINE codUserDefined #-}

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
codValueType :: Lens' ConfigurationOptionDescription (Maybe ConfigurationOptionValueType)
codValueType = lens _codValueType (\s a -> s { _codValueType = a })
{-# INLINE codValueType #-}

-- | If specified, values for the configuration option are selected from this
-- list.
codValueOptions :: Lens' ConfigurationOptionDescription [Text]
codValueOptions = lens _codValueOptions (\s a -> s { _codValueOptions = a })
{-# INLINE codValueOptions #-}

-- | If specified, the configuration option must be a numeric value greater than
-- this value.
codMinValue :: Lens' ConfigurationOptionDescription (Maybe Integer)
codMinValue = lens _codMinValue (\s a -> s { _codMinValue = a })
{-# INLINE codMinValue #-}

-- | If specified, the configuration option must be a numeric value less than
-- this value.
codMaxValue :: Lens' ConfigurationOptionDescription (Maybe Integer)
codMaxValue = lens _codMaxValue (\s a -> s { _codMaxValue = a })
{-# INLINE codMaxValue #-}

-- | If specified, the configuration option must be a string value no longer
-- than this value.
codMaxLength :: Lens' ConfigurationOptionDescription (Maybe Integer)
codMaxLength = lens _codMaxLength (\s a -> s { _codMaxLength = a })
{-# INLINE codMaxLength #-}

-- | If specified, the configuration option must be a string value that
-- satisfies this regular expression.
codRegex :: Lens' ConfigurationOptionDescription (Maybe OptionRestrictionRegex)
codRegex = lens _codRegex (\s a -> s { _codRegex = a })
{-# INLINE codRegex #-}

instance FromXML ConfigurationOptionDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ConfigurationOptionDescription"

-- | A specification identifying an individual configuration option along with
-- its current value. For a list of possible option values, go to Option
-- Values in the AWS Elastic Beanstalk Developer Guide.
data ConfigurationOptionSetting = ConfigurationOptionSetting
    { _cosNamespace :: Maybe Text
    , _cosOptionName :: Maybe Text
    , _cosValue :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ConfigurationOptionSetting' data type to populate a request.
mkConfigurationOptionSetting :: ConfigurationOptionSetting
mkConfigurationOptionSetting = ConfigurationOptionSetting
    { _cosNamespace = Nothing
    , _cosOptionName = Nothing
    , _cosValue = Nothing
    }
{-# INLINE mkConfigurationOptionSetting #-}

-- | A unique namespace identifying the option's associated AWS resource.
cosNamespace :: Lens' ConfigurationOptionSetting (Maybe Text)
cosNamespace = lens _cosNamespace (\s a -> s { _cosNamespace = a })
{-# INLINE cosNamespace #-}

-- | The name of the configuration option.
cosOptionName :: Lens' ConfigurationOptionSetting (Maybe Text)
cosOptionName = lens _cosOptionName (\s a -> s { _cosOptionName = a })
{-# INLINE cosOptionName #-}

-- | The current value for the configuration option.
cosValue :: Lens' ConfigurationOptionSetting (Maybe Text)
cosValue = lens _cosValue (\s a -> s { _cosValue = a })
{-# INLINE cosValue #-}

instance FromXML ConfigurationOptionSetting where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ConfigurationOptionSetting"

instance ToQuery ConfigurationOptionSetting where
    toQuery = genericQuery def

-- | Describes the settings for a configuration set.
data ConfigurationSettingsDescription = ConfigurationSettingsDescription
    { _csdSolutionStackName :: Maybe Text
    , _csdApplicationName :: Maybe Text
    , _csdTemplateName :: Maybe Text
    , _csdDescription :: Maybe Text
    , _csdEnvironmentName :: Maybe Text
    , _csdDeploymentStatus :: Maybe ConfigurationDeploymentStatus
    , _csdDateCreated :: Maybe ISO8601
    , _csdDateUpdated :: Maybe ISO8601
    , _csdOptionSettings :: [ConfigurationOptionSetting]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ConfigurationSettingsDescription' data type to populate a request.
mkConfigurationSettingsDescription :: ConfigurationSettingsDescription
mkConfigurationSettingsDescription = ConfigurationSettingsDescription
    { _csdSolutionStackName = Nothing
    , _csdApplicationName = Nothing
    , _csdTemplateName = Nothing
    , _csdDescription = Nothing
    , _csdEnvironmentName = Nothing
    , _csdDeploymentStatus = Nothing
    , _csdDateCreated = Nothing
    , _csdDateUpdated = Nothing
    , _csdOptionSettings = mempty
    }
{-# INLINE mkConfigurationSettingsDescription #-}

-- | The name of the solution stack this configuration set uses.
csdSolutionStackName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdSolutionStackName =
    lens _csdSolutionStackName (\s a -> s { _csdSolutionStackName = a })
{-# INLINE csdSolutionStackName #-}

-- | The name of the application associated with this configuration set.
csdApplicationName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdApplicationName =
    lens _csdApplicationName (\s a -> s { _csdApplicationName = a })
{-# INLINE csdApplicationName #-}

-- | If not null, the name of the configuration template for this configuration
-- set.
csdTemplateName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdTemplateName = lens _csdTemplateName (\s a -> s { _csdTemplateName = a })
{-# INLINE csdTemplateName #-}

-- | Describes this configuration set.
csdDescription :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdDescription = lens _csdDescription (\s a -> s { _csdDescription = a })
{-# INLINE csdDescription #-}

-- | If not null, the name of the environment for this configuration set.
csdEnvironmentName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdEnvironmentName =
    lens _csdEnvironmentName (\s a -> s { _csdEnvironmentName = a })
{-# INLINE csdEnvironmentName #-}

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
csdDeploymentStatus :: Lens' ConfigurationSettingsDescription (Maybe ConfigurationDeploymentStatus)
csdDeploymentStatus =
    lens _csdDeploymentStatus (\s a -> s { _csdDeploymentStatus = a })
{-# INLINE csdDeploymentStatus #-}

-- | The date (in UTC time) when this configuration set was created.
csdDateCreated :: Lens' ConfigurationSettingsDescription (Maybe ISO8601)
csdDateCreated = lens _csdDateCreated (\s a -> s { _csdDateCreated = a })
{-# INLINE csdDateCreated #-}

-- | The date (in UTC time) when this configuration set was last modified.
csdDateUpdated :: Lens' ConfigurationSettingsDescription (Maybe ISO8601)
csdDateUpdated = lens _csdDateUpdated (\s a -> s { _csdDateUpdated = a })
{-# INLINE csdDateUpdated #-}

-- | A list of the configuration options and their values in this configuration
-- set.
csdOptionSettings :: Lens' ConfigurationSettingsDescription [ConfigurationOptionSetting]
csdOptionSettings =
    lens _csdOptionSettings (\s a -> s { _csdOptionSettings = a })
{-# INLINE csdOptionSettings #-}

instance FromXML ConfigurationSettingsDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ConfigurationSettingsDescription"

-- | Describes the properties of an environment.
data EnvironmentDescription = EnvironmentDescription
    { _edEnvironmentName :: Maybe Text
    , _edEnvironmentId :: Maybe Text
    , _edApplicationName :: Maybe Text
    , _edVersionLabel :: Maybe Text
    , _edSolutionStackName :: Maybe Text
    , _edTemplateName :: Maybe Text
    , _edDescription :: Maybe Text
    , _edEndpointURL :: Maybe Text
    , _edCNAME :: Maybe Text
    , _edDateCreated :: Maybe ISO8601
    , _edDateUpdated :: Maybe ISO8601
    , _edStatus :: Maybe EnvironmentStatus
    , _edHealth :: Maybe EnvironmentHealth
    , _edResources :: Maybe EnvironmentResourcesDescription
    , _edTier :: Maybe EnvironmentTier
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EnvironmentDescription' data type to populate a request.
mkEnvironmentDescription :: EnvironmentDescription
mkEnvironmentDescription = EnvironmentDescription
    { _edEnvironmentName = Nothing
    , _edEnvironmentId = Nothing
    , _edApplicationName = Nothing
    , _edVersionLabel = Nothing
    , _edSolutionStackName = Nothing
    , _edTemplateName = Nothing
    , _edDescription = Nothing
    , _edEndpointURL = Nothing
    , _edCNAME = Nothing
    , _edDateCreated = Nothing
    , _edDateUpdated = Nothing
    , _edStatus = Nothing
    , _edHealth = Nothing
    , _edResources = Nothing
    , _edTier = Nothing
    }
{-# INLINE mkEnvironmentDescription #-}

-- | The name of this environment.
edEnvironmentName :: Lens' EnvironmentDescription (Maybe Text)
edEnvironmentName =
    lens _edEnvironmentName (\s a -> s { _edEnvironmentName = a })
{-# INLINE edEnvironmentName #-}

-- | The ID of this environment.
edEnvironmentId :: Lens' EnvironmentDescription (Maybe Text)
edEnvironmentId = lens _edEnvironmentId (\s a -> s { _edEnvironmentId = a })
{-# INLINE edEnvironmentId #-}

-- | The name of the application associated with this environment.
edApplicationName :: Lens' EnvironmentDescription (Maybe Text)
edApplicationName =
    lens _edApplicationName (\s a -> s { _edApplicationName = a })
{-# INLINE edApplicationName #-}

-- | The application version deployed in this environment.
edVersionLabel :: Lens' EnvironmentDescription (Maybe Text)
edVersionLabel = lens _edVersionLabel (\s a -> s { _edVersionLabel = a })
{-# INLINE edVersionLabel #-}

-- | The name of the SolutionStack deployed with this environment.
edSolutionStackName :: Lens' EnvironmentDescription (Maybe Text)
edSolutionStackName =
    lens _edSolutionStackName (\s a -> s { _edSolutionStackName = a })
{-# INLINE edSolutionStackName #-}

-- | The name of the configuration template used to originally launch this
-- environment.
edTemplateName :: Lens' EnvironmentDescription (Maybe Text)
edTemplateName = lens _edTemplateName (\s a -> s { _edTemplateName = a })
{-# INLINE edTemplateName #-}

-- | Describes this environment.
edDescription :: Lens' EnvironmentDescription (Maybe Text)
edDescription = lens _edDescription (\s a -> s { _edDescription = a })
{-# INLINE edDescription #-}

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer.
-- For single-instance environments, the IP address of the instance.
edEndpointURL :: Lens' EnvironmentDescription (Maybe Text)
edEndpointURL = lens _edEndpointURL (\s a -> s { _edEndpointURL = a })
{-# INLINE edEndpointURL #-}

-- | The URL to the CNAME for this environment.
edCNAME :: Lens' EnvironmentDescription (Maybe Text)
edCNAME = lens _edCNAME (\s a -> s { _edCNAME = a })
{-# INLINE edCNAME #-}

-- | The creation date for this environment.
edDateCreated :: Lens' EnvironmentDescription (Maybe ISO8601)
edDateCreated = lens _edDateCreated (\s a -> s { _edDateCreated = a })
{-# INLINE edDateCreated #-}

-- | The last modified date for this environment.
edDateUpdated :: Lens' EnvironmentDescription (Maybe ISO8601)
edDateUpdated = lens _edDateUpdated (\s a -> s { _edDateUpdated = a })
{-# INLINE edDateUpdated #-}

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such as
-- update or terminate. Terminating: Environment is in the shut-down process.
-- Terminated: Environment is not running.
edStatus :: Lens' EnvironmentDescription (Maybe EnvironmentStatus)
edStatus = lens _edStatus (\s a -> s { _edStatus = a })
{-# INLINE edStatus #-}

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
edHealth :: Lens' EnvironmentDescription (Maybe EnvironmentHealth)
edHealth = lens _edHealth (\s a -> s { _edHealth = a })
{-# INLINE edHealth #-}

-- | The description of the AWS resources used by this environment.
edResources :: Lens' EnvironmentDescription (Maybe EnvironmentResourcesDescription)
edResources = lens _edResources (\s a -> s { _edResources = a })
{-# INLINE edResources #-}

-- | Describes the current tier of this environment.
edTier :: Lens' EnvironmentDescription (Maybe EnvironmentTier)
edTier = lens _edTier (\s a -> s { _edTier = a })
{-# INLINE edTier #-}

instance FromXML EnvironmentDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentDescription"

-- | The information retrieved from the Amazon EC2 instances.
data EnvironmentInfoDescription = EnvironmentInfoDescription
    { _eidInfoType :: Maybe EnvironmentInfoType
    , _eidEc2InstanceId :: Maybe Text
    , _eidSampleTimestamp :: Maybe ISO8601
    , _eidMessage :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EnvironmentInfoDescription' data type to populate a request.
mkEnvironmentInfoDescription :: EnvironmentInfoDescription
mkEnvironmentInfoDescription = EnvironmentInfoDescription
    { _eidInfoType = Nothing
    , _eidEc2InstanceId = Nothing
    , _eidSampleTimestamp = Nothing
    , _eidMessage = Nothing
    }
{-# INLINE mkEnvironmentInfoDescription #-}

-- | The type of information retrieved.
eidInfoType :: Lens' EnvironmentInfoDescription (Maybe EnvironmentInfoType)
eidInfoType = lens _eidInfoType (\s a -> s { _eidInfoType = a })
{-# INLINE eidInfoType #-}

-- | The Amazon EC2 Instance ID for this information.
eidEc2InstanceId :: Lens' EnvironmentInfoDescription (Maybe Text)
eidEc2InstanceId =
    lens _eidEc2InstanceId (\s a -> s { _eidEc2InstanceId = a })
{-# INLINE eidEc2InstanceId #-}

-- | The time stamp when this information was retrieved.
eidSampleTimestamp :: Lens' EnvironmentInfoDescription (Maybe ISO8601)
eidSampleTimestamp =
    lens _eidSampleTimestamp (\s a -> s { _eidSampleTimestamp = a })
{-# INLINE eidSampleTimestamp #-}

-- | The retrieved information.
eidMessage :: Lens' EnvironmentInfoDescription (Maybe Text)
eidMessage = lens _eidMessage (\s a -> s { _eidMessage = a })
{-# INLINE eidMessage #-}

instance FromXML EnvironmentInfoDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentInfoDescription"

-- | A list of EnvironmentResourceDescription.
data EnvironmentResourceDescription = EnvironmentResourceDescription
    { _erdrsEnvironmentName :: Maybe Text
    , _erdrsAutoScalingGroups :: [AutoScalingGroup]
    , _erdrsInstances :: [Instance]
    , _erdrsLaunchConfigurations :: [LaunchConfiguration]
    , _erdrsLoadBalancers :: [LoadBalancer]
    , _erdrsTriggers :: [Trigger]
    , _erdrsQueues :: [Queue]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EnvironmentResourceDescription' data type to populate a request.
mkEnvironmentResourceDescription :: EnvironmentResourceDescription
mkEnvironmentResourceDescription = EnvironmentResourceDescription
    { _erdrsEnvironmentName = Nothing
    , _erdrsAutoScalingGroups = mempty
    , _erdrsInstances = mempty
    , _erdrsLaunchConfigurations = mempty
    , _erdrsLoadBalancers = mempty
    , _erdrsTriggers = mempty
    , _erdrsQueues = mempty
    }
{-# INLINE mkEnvironmentResourceDescription #-}

-- | The name of the environment.
erdrsEnvironmentName :: Lens' EnvironmentResourceDescription (Maybe Text)
erdrsEnvironmentName =
    lens _erdrsEnvironmentName (\s a -> s { _erdrsEnvironmentName = a })
{-# INLINE erdrsEnvironmentName #-}

-- | The AutoScalingGroups used by this environment.
erdrsAutoScalingGroups :: Lens' EnvironmentResourceDescription [AutoScalingGroup]
erdrsAutoScalingGroups =
    lens _erdrsAutoScalingGroups (\s a -> s { _erdrsAutoScalingGroups = a })
{-# INLINE erdrsAutoScalingGroups #-}

-- | The Amazon EC2 instances used by this environment.
erdrsInstances :: Lens' EnvironmentResourceDescription [Instance]
erdrsInstances = lens _erdrsInstances (\s a -> s { _erdrsInstances = a })
{-# INLINE erdrsInstances #-}

-- | The Auto Scaling launch configurations in use by this environment.
erdrsLaunchConfigurations :: Lens' EnvironmentResourceDescription [LaunchConfiguration]
erdrsLaunchConfigurations =
    lens _erdrsLaunchConfigurations
         (\s a -> s { _erdrsLaunchConfigurations = a })
{-# INLINE erdrsLaunchConfigurations #-}

-- | The LoadBalancers in use by this environment.
erdrsLoadBalancers :: Lens' EnvironmentResourceDescription [LoadBalancer]
erdrsLoadBalancers =
    lens _erdrsLoadBalancers (\s a -> s { _erdrsLoadBalancers = a })
{-# INLINE erdrsLoadBalancers #-}

-- | The AutoScaling triggers in use by this environment.
erdrsTriggers :: Lens' EnvironmentResourceDescription [Trigger]
erdrsTriggers = lens _erdrsTriggers (\s a -> s { _erdrsTriggers = a })
{-# INLINE erdrsTriggers #-}

-- | The queues used by this environment.
erdrsQueues :: Lens' EnvironmentResourceDescription [Queue]
erdrsQueues = lens _erdrsQueues (\s a -> s { _erdrsQueues = a })
{-# INLINE erdrsQueues #-}

instance FromXML EnvironmentResourceDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentResourceDescription"

-- | This specifies the tier to use for creating this environment.
data EnvironmentTier = EnvironmentTier
    { _etName :: Maybe Text
    , _etType :: Maybe Text
    , _etVersion :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EnvironmentTier' data type to populate a request.
mkEnvironmentTier :: EnvironmentTier
mkEnvironmentTier = EnvironmentTier
    { _etName = Nothing
    , _etType = Nothing
    , _etVersion = Nothing
    }
{-# INLINE mkEnvironmentTier #-}

-- | The name of this environment tier.
etName :: Lens' EnvironmentTier (Maybe Text)
etName = lens _etName (\s a -> s { _etName = a })
{-# INLINE etName #-}

-- | The type of this environment tier.
etType :: Lens' EnvironmentTier (Maybe Text)
etType = lens _etType (\s a -> s { _etType = a })
{-# INLINE etType #-}

-- | The version of this environment tier.
etVersion :: Lens' EnvironmentTier (Maybe Text)
etVersion = lens _etVersion (\s a -> s { _etVersion = a })
{-# INLINE etVersion #-}

instance FromXML EnvironmentTier where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentTier"

instance ToQuery EnvironmentTier where
    toQuery = genericQuery def

-- | Describes an event.
data EventDescription = EventDescription
    { _edrsEventDate :: Maybe ISO8601
    , _edrsMessage :: Maybe Text
    , _edrsApplicationName :: Maybe Text
    , _edrsVersionLabel :: Maybe Text
    , _edrsTemplateName :: Maybe Text
    , _edrsEnvironmentName :: Maybe Text
    , _edrsRequestId :: Maybe Text
    , _edrsSeverity :: Maybe EventSeverity
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EventDescription' data type to populate a request.
mkEventDescription :: EventDescription
mkEventDescription = EventDescription
    { _edrsEventDate = Nothing
    , _edrsMessage = Nothing
    , _edrsApplicationName = Nothing
    , _edrsVersionLabel = Nothing
    , _edrsTemplateName = Nothing
    , _edrsEnvironmentName = Nothing
    , _edrsRequestId = Nothing
    , _edrsSeverity = Nothing
    }
{-# INLINE mkEventDescription #-}

-- | The date when the event occurred.
edrsEventDate :: Lens' EventDescription (Maybe ISO8601)
edrsEventDate = lens _edrsEventDate (\s a -> s { _edrsEventDate = a })
{-# INLINE edrsEventDate #-}

-- | The event message.
edrsMessage :: Lens' EventDescription (Maybe Text)
edrsMessage = lens _edrsMessage (\s a -> s { _edrsMessage = a })
{-# INLINE edrsMessage #-}

-- | The application associated with the event.
edrsApplicationName :: Lens' EventDescription (Maybe Text)
edrsApplicationName =
    lens _edrsApplicationName (\s a -> s { _edrsApplicationName = a })
{-# INLINE edrsApplicationName #-}

-- | The release label for the application version associated with this event.
edrsVersionLabel :: Lens' EventDescription (Maybe Text)
edrsVersionLabel =
    lens _edrsVersionLabel (\s a -> s { _edrsVersionLabel = a })
{-# INLINE edrsVersionLabel #-}

-- | The name of the configuration associated with this event.
edrsTemplateName :: Lens' EventDescription (Maybe Text)
edrsTemplateName =
    lens _edrsTemplateName (\s a -> s { _edrsTemplateName = a })
{-# INLINE edrsTemplateName #-}

-- | The name of the environment associated with this event.
edrsEnvironmentName :: Lens' EventDescription (Maybe Text)
edrsEnvironmentName =
    lens _edrsEnvironmentName (\s a -> s { _edrsEnvironmentName = a })
{-# INLINE edrsEnvironmentName #-}

-- | The web service request ID for the activity of this event.
edrsRequestId :: Lens' EventDescription (Maybe Text)
edrsRequestId = lens _edrsRequestId (\s a -> s { _edrsRequestId = a })
{-# INLINE edrsRequestId #-}

-- | The severity level of this event.
edrsSeverity :: Lens' EventDescription (Maybe EventSeverity)
edrsSeverity = lens _edrsSeverity (\s a -> s { _edrsSeverity = a })
{-# INLINE edrsSeverity #-}

instance FromXML EventDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventDescription"

-- | Describes the properties of a Listener for the LoadBalancer.
data Listener = Listener
    { _lProtocol :: Maybe Text
    , _lPort :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Listener' data type to populate a request.
mkListener :: Listener
mkListener = Listener
    { _lProtocol = Nothing
    , _lPort = Nothing
    }
{-# INLINE mkListener #-}

-- | The protocol that is used by the Listener.
lProtocol :: Lens' Listener (Maybe Text)
lProtocol = lens _lProtocol (\s a -> s { _lProtocol = a })
{-# INLINE lProtocol #-}

-- | The port that is used by the Listener.
lPort :: Lens' Listener (Maybe Integer)
lPort = lens _lPort (\s a -> s { _lPort = a })
{-# INLINE lPort #-}

instance FromXML Listener where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Listener"

instance ToQuery Listener where
    toQuery = genericQuery def

-- | Describes the LoadBalancer.
data LoadBalancerDescription = LoadBalancerDescription
    { _lbdLoadBalancerName :: Maybe Text
    , _lbdDomain :: Maybe Text
    , _lbdListeners :: [Listener]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LoadBalancerDescription' data type to populate a request.
mkLoadBalancerDescription :: LoadBalancerDescription
mkLoadBalancerDescription = LoadBalancerDescription
    { _lbdLoadBalancerName = Nothing
    , _lbdDomain = Nothing
    , _lbdListeners = mempty
    }
{-# INLINE mkLoadBalancerDescription #-}

-- | The name of the LoadBalancer.
lbdLoadBalancerName :: Lens' LoadBalancerDescription (Maybe Text)
lbdLoadBalancerName =
    lens _lbdLoadBalancerName (\s a -> s { _lbdLoadBalancerName = a })
{-# INLINE lbdLoadBalancerName #-}

-- | The domain name of the LoadBalancer.
lbdDomain :: Lens' LoadBalancerDescription (Maybe Text)
lbdDomain = lens _lbdDomain (\s a -> s { _lbdDomain = a })
{-# INLINE lbdDomain #-}

-- | A list of Listeners used by the LoadBalancer.
lbdListeners :: Lens' LoadBalancerDescription [Listener]
lbdListeners = lens _lbdListeners (\s a -> s { _lbdListeners = a })
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
    , _orrLabel :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OptionRestrictionRegex' data type to populate a request.
mkOptionRestrictionRegex :: OptionRestrictionRegex
mkOptionRestrictionRegex = OptionRestrictionRegex
    { _orrPattern = Nothing
    , _orrLabel = Nothing
    }
{-# INLINE mkOptionRestrictionRegex #-}

-- | The regular expression pattern that a string configuration option value
-- with this restriction must match.
orrPattern :: Lens' OptionRestrictionRegex (Maybe Text)
orrPattern = lens _orrPattern (\s a -> s { _orrPattern = a })
{-# INLINE orrPattern #-}

-- | A unique name representing this regular expression.
orrLabel :: Lens' OptionRestrictionRegex (Maybe Text)
orrLabel = lens _orrLabel (\s a -> s { _orrLabel = a })
{-# INLINE orrLabel #-}

instance FromXML OptionRestrictionRegex where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionRestrictionRegex"

instance ToQuery OptionRestrictionRegex where
    toQuery = genericQuery def

-- | A specification identifying an individual configuration option.
data OptionSpecification = OptionSpecification
    { _osNamespace :: Maybe Text
    , _osOptionName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OptionSpecification' data type to populate a request.
mkOptionSpecification :: OptionSpecification
mkOptionSpecification = OptionSpecification
    { _osNamespace = Nothing
    , _osOptionName = Nothing
    }
{-# INLINE mkOptionSpecification #-}

-- | A unique namespace identifying the option's associated AWS resource.
osNamespace :: Lens' OptionSpecification (Maybe Text)
osNamespace = lens _osNamespace (\s a -> s { _osNamespace = a })
{-# INLINE osNamespace #-}

-- | The name of the configuration option.
osOptionName :: Lens' OptionSpecification (Maybe Text)
osOptionName = lens _osOptionName (\s a -> s { _osOptionName = a })
{-# INLINE osOptionName #-}

instance ToQuery OptionSpecification where
    toQuery = genericQuery def

-- | Describes a queue.
data Queue = Queue
    { _qName :: Maybe Text
    , _qURL :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Queue' data type to populate a request.
mkQueue :: Queue
mkQueue = Queue
    { _qName = Nothing
    , _qURL = Nothing
    }
{-# INLINE mkQueue #-}

-- | The name of the queue.
qName :: Lens' Queue (Maybe Text)
qName = lens _qName (\s a -> s { _qName = a })
{-# INLINE qName #-}

-- | The URL of the queue.
qURL :: Lens' Queue (Maybe Text)
qURL = lens _qURL (\s a -> s { _qURL = a })
{-# INLINE qURL #-}

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
    , _slS3Key :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'S3Location' data type to populate a request.
mkS3Location :: S3Location
mkS3Location = S3Location
    { _slS3Bucket = Nothing
    , _slS3Key = Nothing
    }
{-# INLINE mkS3Location #-}

-- | The Amazon S3 bucket where the data is located.
slS3Bucket :: Lens' S3Location (Maybe Text)
slS3Bucket = lens _slS3Bucket (\s a -> s { _slS3Bucket = a })
{-# INLINE slS3Bucket #-}

-- | The Amazon S3 key where the data is located.
slS3Key :: Lens' S3Location (Maybe Text)
slS3Key = lens _slS3Key (\s a -> s { _slS3Key = a })
{-# INLINE slS3Key #-}

instance FromXML S3Location where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "S3Location"

instance ToQuery S3Location where
    toQuery = genericQuery def

-- | Describes the solution stack.
data SolutionStackDescription = SolutionStackDescription
    { _ssdSolutionStackName :: Maybe Text
    , _ssdPermittedFileTypes :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SolutionStackDescription' data type to populate a request.
mkSolutionStackDescription :: SolutionStackDescription
mkSolutionStackDescription = SolutionStackDescription
    { _ssdSolutionStackName = Nothing
    , _ssdPermittedFileTypes = mempty
    }
{-# INLINE mkSolutionStackDescription #-}

-- | The name of the solution stack.
ssdSolutionStackName :: Lens' SolutionStackDescription (Maybe Text)
ssdSolutionStackName =
    lens _ssdSolutionStackName (\s a -> s { _ssdSolutionStackName = a })
{-# INLINE ssdSolutionStackName #-}

-- | The permitted file types allowed for a solution stack.
ssdPermittedFileTypes :: Lens' SolutionStackDescription [Text]
ssdPermittedFileTypes =
    lens _ssdPermittedFileTypes (\s a -> s { _ssdPermittedFileTypes = a })
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
    { _scApplicationName :: Maybe Text
    , _scTemplateName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SourceConfiguration' data type to populate a request.
mkSourceConfiguration :: SourceConfiguration
mkSourceConfiguration = SourceConfiguration
    { _scApplicationName = Nothing
    , _scTemplateName = Nothing
    }
{-# INLINE mkSourceConfiguration #-}

-- | The name of the application associated with the configuration.
scApplicationName :: Lens' SourceConfiguration (Maybe Text)
scApplicationName =
    lens _scApplicationName (\s a -> s { _scApplicationName = a })
{-# INLINE scApplicationName #-}

-- | The name of the configuration template.
scTemplateName :: Lens' SourceConfiguration (Maybe Text)
scTemplateName = lens _scTemplateName (\s a -> s { _scTemplateName = a })
{-# INLINE scTemplateName #-}

instance ToQuery SourceConfiguration where
    toQuery = genericQuery def

-- | Describes a tag applied to a resource in an environment.
data Tag = Tag
    { _tKey :: Maybe Text
    , _tValue :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tag' data type to populate a request.
mkTag :: Tag
mkTag = Tag
    { _tKey = Nothing
    , _tValue = Nothing
    }
{-# INLINE mkTag #-}

-- | The key of the tag.
tKey :: Lens' Tag (Maybe Text)
tKey = lens _tKey (\s a -> s { _tKey = a })
{-# INLINE tKey #-}

-- | The value of the tag.
tValue :: Lens' Tag (Maybe Text)
tValue = lens _tValue (\s a -> s { _tValue = a })
{-# INLINE tValue #-}

instance ToQuery Tag where
    toQuery = genericQuery def

-- | An error or warning for a desired configuration option value.
data ValidationMessage = ValidationMessage
    { _vmMessage :: Maybe Text
    , _vmSeverity :: Maybe ValidationSeverity
    , _vmNamespace :: Maybe Text
    , _vmOptionName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ValidationMessage' data type to populate a request.
mkValidationMessage :: ValidationMessage
mkValidationMessage = ValidationMessage
    { _vmMessage = Nothing
    , _vmSeverity = Nothing
    , _vmNamespace = Nothing
    , _vmOptionName = Nothing
    }
{-# INLINE mkValidationMessage #-}

-- | A message describing the error or warning.
vmMessage :: Lens' ValidationMessage (Maybe Text)
vmMessage = lens _vmMessage (\s a -> s { _vmMessage = a })
{-# INLINE vmMessage #-}

-- | An indication of the severity of this message: error: This message
-- indicates that this is not a valid setting for an option. warning: This
-- message is providing information you should take into account. error: This
-- message indicates that this is not a valid setting for an option. warning:
-- This message is providing information you should take into account.
vmSeverity :: Lens' ValidationMessage (Maybe ValidationSeverity)
vmSeverity = lens _vmSeverity (\s a -> s { _vmSeverity = a })
{-# INLINE vmSeverity #-}

-- | 
vmNamespace :: Lens' ValidationMessage (Maybe Text)
vmNamespace = lens _vmNamespace (\s a -> s { _vmNamespace = a })
{-# INLINE vmNamespace #-}

-- | 
vmOptionName :: Lens' ValidationMessage (Maybe Text)
vmOptionName = lens _vmOptionName (\s a -> s { _vmOptionName = a })
{-# INLINE vmOptionName #-}

instance FromXML ValidationMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ValidationMessage"
