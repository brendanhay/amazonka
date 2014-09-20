{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.Types
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
module Network.AWS.ElasticBeanstalk.Types
    (
    -- * Service
      ElasticBeanstalk
    -- ** Errors
    , ElasticBeanstalkError (..)
    , _ElasticBeanstalkClient
    , _ElasticBeanstalkSerializer
    , _ElasticBeanstalkService
    , _InsufficientPrivilegesException
    , _OperationInProgressException
    , _S3LocationNotInServiceRegionException
    , _S3SubscriptionRequiredException
    , _SourceBundleDeletionException
    , _TooManyApplicationVersionsException
    , _TooManyApplicationsException
    , _TooManyBucketsException
    , _TooManyConfigurationTemplatesException
    , _TooManyEnvironmentsException
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
    , autoScalingGroup
    , asgName

    -- * EnvironmentResourcesDescription
    , EnvironmentResourcesDescription
    , environmentResourcesDescription
    , erdLoadBalancer

    -- * Instance
    , Instance
    , instance'
    , iId

    -- * LaunchConfiguration
    , LaunchConfiguration
    , launchConfiguration
    , lcName

    -- * LoadBalancer
    , LoadBalancer
    , loadBalancer
    , lbName

    -- * Trigger
    , Trigger
    , trigger
    , trName

    -- * ApplicationDescription
    , ApplicationDescription
    , applicationDescription
    , adApplicationName
    , adDescription
    , adDateCreated
    , adDateUpdated
    , adVersions
    , adConfigurationTemplates

    -- * ApplicationVersionDescription
    , ApplicationVersionDescription
    , applicationVersionDescription
    , avdApplicationName
    , avdDescription
    , avdVersionLabel
    , avdSourceBundle
    , avdDateCreated
    , avdDateUpdated

    -- * ConfigurationOptionDescription
    , ConfigurationOptionDescription
    , configurationOptionDescription
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
    , configurationOptionSetting
    , cosNamespace
    , cosOptionName
    , cosValue

    -- * ConfigurationSettingsDescription
    , ConfigurationSettingsDescription
    , configurationSettingsDescription
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
    , environmentDescription
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
    , environmentInfoDescription
    , eidInfoType
    , eidEc2InstanceId
    , eidSampleTimestamp
    , eidMessage

    -- * EnvironmentResourceDescription
    , EnvironmentResourceDescription
    , environmentResourceDescription
    , erdrEnvironmentName
    , erdrAutoScalingGroups
    , erdrInstances
    , erdrLaunchConfigurations
    , erdrLoadBalancers
    , erdrTriggers
    , erdrQueues

    -- * EnvironmentTier
    , EnvironmentTier
    , environmentTier
    , etName
    , etType
    , etVersion

    -- * EventDescription
    , EventDescription
    , eventDescription
    , edrEventDate
    , edrMessage
    , edrApplicationName
    , edrVersionLabel
    , edrTemplateName
    , edrEnvironmentName
    , edrRequestId
    , edrSeverity

    -- * Listener
    , Listener
    , listener
    , lProtocol
    , lPort

    -- * LoadBalancerDescription
    , LoadBalancerDescription
    , loadBalancerDescription
    , lbdLoadBalancerName
    , lbdDomain
    , lbdListeners

    -- * OptionRestrictionRegex
    , OptionRestrictionRegex
    , optionRestrictionRegex
    , orrPattern
    , orrLabel

    -- * OptionSpecification
    , OptionSpecification
    , optionSpecification
    , osNamespace
    , osOptionName

    -- * Queue
    , Queue
    , queue
    , qName
    , qURL

    -- * S3Location
    , S3Location
    , s3Location
    , slS3Bucket
    , slS3Key

    -- * SolutionStackDescription
    , SolutionStackDescription
    , solutionStackDescription
    , ssdSolutionStackName
    , ssdPermittedFileTypes

    -- * SourceConfiguration
    , SourceConfiguration
    , sourceConfiguration
    , scApplicationName
    , scTemplateName

    -- * Tag
    , Tag
    , tag
    , tKey
    , tValue

    -- * ValidationMessage
    , ValidationMessage
    , validationMessage
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
    type Er ElasticBeanstalk = ElasticBeanstalkError

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "elasticbeanstalk"
        , _svcVersion  = "2010-12-01"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'ElasticBeanstalk' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data ElasticBeanstalkError
    = ElasticBeanstalkClient HttpException
    | ElasticBeanstalkSerializer String
    | ElasticBeanstalkService String
      -- | Unable to perform the specified operation because the user does
      -- not have enough privileges for one of more downstream aws
      -- services.
    | InsufficientPrivilegesException
      -- | Unable to perform the specified operation because another
      -- operation is already in progress affecting an an element in this
      -- activity.
    | OperationInProgressException
      -- | The specified S3 bucket does not belong to the S3 region in which
      -- the service is running.
    | S3LocationNotInServiceRegionException
      -- | The caller does not have a subscription to Amazon S3.
    | S3SubscriptionRequiredException
      -- | Unable to delete the Amazon S3 source bundle associated with the
      -- application version, although the application version deleted
      -- successfully.
    | SourceBundleDeletionException
      -- | The caller has exceeded the limit on the number of application
      -- versions associated with their account.
    | TooManyApplicationVersionsException
      -- | The caller has exceeded the limit on the number of applications
      -- associated with their account.
    | TooManyApplicationsException
      -- | The web service attempted to create a bucket in an Amazon S3
      -- account that already has 100 buckets.
    | TooManyBucketsException
      -- | The caller has exceeded the limit on the number of configuration
      -- templates associated with their account.
    | TooManyConfigurationTemplatesException
      -- | The caller has exceeded the limit of allowed environments
      -- associated with the account.
    | TooManyEnvironmentsException
      deriving (Show, Typeable, Generic)

instance AWSError ElasticBeanstalkError where
    awsError = const "ElasticBeanstalkError"

instance AWSServiceError ElasticBeanstalkError where
    serviceError    = ElasticBeanstalkService
    clientError     = ElasticBeanstalkClient
    serializerError = ElasticBeanstalkSerializer

instance Exception ElasticBeanstalkError

-- | See: 'ElasticBeanstalkClient'
_ElasticBeanstalkClient :: Prism' ElasticBeanstalkError HttpException
_ElasticBeanstalkClient = prism
    ElasticBeanstalkClient
    (\case
        ElasticBeanstalkClient p1 -> Right p1
        x -> Left x)

-- | See: 'ElasticBeanstalkSerializer'
_ElasticBeanstalkSerializer :: Prism' ElasticBeanstalkError String
_ElasticBeanstalkSerializer = prism
    ElasticBeanstalkSerializer
    (\case
        ElasticBeanstalkSerializer p1 -> Right p1
        x -> Left x)

-- | See: 'ElasticBeanstalkService'
_ElasticBeanstalkService :: Prism' ElasticBeanstalkError String
_ElasticBeanstalkService = prism
    ElasticBeanstalkService
    (\case
        ElasticBeanstalkService p1 -> Right p1
        x -> Left x)

-- | Unable to perform the specified operation because the user does not have
-- enough privileges for one of more downstream aws services.
--
-- See: 'InsufficientPrivilegesException'
_InsufficientPrivilegesException :: Prism' ElasticBeanstalkError ()
_InsufficientPrivilegesException = prism
    (const InsufficientPrivilegesException)
    (\case
        InsufficientPrivilegesException -> Right ()
        x -> Left x)

-- | Unable to perform the specified operation because another operation is
-- already in progress affecting an an element in this activity.
--
-- See: 'OperationInProgressException'
_OperationInProgressException :: Prism' ElasticBeanstalkError ()
_OperationInProgressException = prism
    (const OperationInProgressException)
    (\case
        OperationInProgressException -> Right ()
        x -> Left x)

-- | The specified S3 bucket does not belong to the S3 region in which the
-- service is running.
--
-- See: 'S3LocationNotInServiceRegionException'
_S3LocationNotInServiceRegionException :: Prism' ElasticBeanstalkError ()
_S3LocationNotInServiceRegionException = prism
    (const S3LocationNotInServiceRegionException)
    (\case
        S3LocationNotInServiceRegionException -> Right ()
        x -> Left x)

-- | The caller does not have a subscription to Amazon S3.
--
-- See: 'S3SubscriptionRequiredException'
_S3SubscriptionRequiredException :: Prism' ElasticBeanstalkError ()
_S3SubscriptionRequiredException = prism
    (const S3SubscriptionRequiredException)
    (\case
        S3SubscriptionRequiredException -> Right ()
        x -> Left x)

-- | Unable to delete the Amazon S3 source bundle associated with the
-- application version, although the application version deleted successfully.
--
-- See: 'SourceBundleDeletionException'
_SourceBundleDeletionException :: Prism' ElasticBeanstalkError ()
_SourceBundleDeletionException = prism
    (const SourceBundleDeletionException)
    (\case
        SourceBundleDeletionException -> Right ()
        x -> Left x)

-- | The caller has exceeded the limit on the number of application versions
-- associated with their account.
--
-- See: 'TooManyApplicationVersionsException'
_TooManyApplicationVersionsException :: Prism' ElasticBeanstalkError ()
_TooManyApplicationVersionsException = prism
    (const TooManyApplicationVersionsException)
    (\case
        TooManyApplicationVersionsException -> Right ()
        x -> Left x)

-- | The caller has exceeded the limit on the number of applications associated
-- with their account.
--
-- See: 'TooManyApplicationsException'
_TooManyApplicationsException :: Prism' ElasticBeanstalkError ()
_TooManyApplicationsException = prism
    (const TooManyApplicationsException)
    (\case
        TooManyApplicationsException -> Right ()
        x -> Left x)

-- | The web service attempted to create a bucket in an Amazon S3 account that
-- already has 100 buckets.
--
-- See: 'TooManyBucketsException'
_TooManyBucketsException :: Prism' ElasticBeanstalkError ()
_TooManyBucketsException = prism
    (const TooManyBucketsException)
    (\case
        TooManyBucketsException -> Right ()
        x -> Left x)

-- | The caller has exceeded the limit on the number of configuration templates
-- associated with their account.
--
-- See: 'TooManyConfigurationTemplatesException'
_TooManyConfigurationTemplatesException :: Prism' ElasticBeanstalkError ()
_TooManyConfigurationTemplatesException = prism
    (const TooManyConfigurationTemplatesException)
    (\case
        TooManyConfigurationTemplatesException -> Right ()
        x -> Left x)

-- | The caller has exceeded the limit of allowed environments associated with
-- the account.
--
-- See: 'TooManyEnvironmentsException'
_TooManyEnvironmentsException :: Prism' ElasticBeanstalkError ()
_TooManyEnvironmentsException = prism
    (const TooManyEnvironmentsException)
    (\case
        TooManyEnvironmentsException -> Right ()
        x -> Left x)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def

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
    = EnvironmentInfoTypeBundle -- ^ bundle
    | EnvironmentInfoTypeTail -- ^ tail
      deriving (Eq, Show, Generic)

instance Hashable EnvironmentInfoType

instance FromText EnvironmentInfoType where
    parser = match "bundle" EnvironmentInfoTypeBundle
         <|> match "tail" EnvironmentInfoTypeTail

instance ToText EnvironmentInfoType where
    toText EnvironmentInfoTypeBundle = "bundle"
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
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AutoScalingGroup' data type.
--
-- 'AutoScalingGroup' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe Text@
--
autoScalingGroup :: AutoScalingGroup
autoScalingGroup = AutoScalingGroup
    { _asgName = Nothing
    }

-- | The name of the AutoScalingGroup .
asgName :: Lens' AutoScalingGroup (Maybe Text)
asgName = lens _asgName (\s a -> s { _asgName = a })

instance FromXML AutoScalingGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AutoScalingGroup"

-- | The description of the AWS resources used by this environment.
newtype EnvironmentResourcesDescription = EnvironmentResourcesDescription
    { _erdLoadBalancer :: Maybe LoadBalancerDescription
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EnvironmentResourcesDescription' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancer ::@ @Maybe LoadBalancerDescription@
--
environmentResourcesDescription :: EnvironmentResourcesDescription
environmentResourcesDescription = EnvironmentResourcesDescription
    { _erdLoadBalancer = Nothing
    }

-- | Describes the LoadBalancer.
erdLoadBalancer :: Lens' EnvironmentResourcesDescription (Maybe LoadBalancerDescription)
erdLoadBalancer = lens _erdLoadBalancer (\s a -> s { _erdLoadBalancer = a })

instance FromXML EnvironmentResourcesDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentResourcesDescription"

instance ToQuery EnvironmentResourcesDescription where
    toQuery = genericQuery def

-- | The description of an Amazon EC2 instance.
newtype Instance = Instance
    { _iId :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Instance' data type.
--
-- 'Instance' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Maybe Text@
--
instance' :: Instance
instance' = Instance
    { _iId = Nothing
    }

-- | The ID of the Amazon EC2 instance.
iId :: Lens' Instance (Maybe Text)
iId = lens _iId (\s a -> s { _iId = a })

instance FromXML Instance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Instance"

-- | Describes an Auto Scaling launch configuration.
newtype LaunchConfiguration = LaunchConfiguration
    { _lcName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LaunchConfiguration' data type.
--
-- 'LaunchConfiguration' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe Text@
--
launchConfiguration :: LaunchConfiguration
launchConfiguration = LaunchConfiguration
    { _lcName = Nothing
    }

-- | The name of the launch configuration.
lcName :: Lens' LaunchConfiguration (Maybe Text)
lcName = lens _lcName (\s a -> s { _lcName = a })

instance FromXML LaunchConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LaunchConfiguration"

-- | Describes a LoadBalancer.
newtype LoadBalancer = LoadBalancer
    { _lbName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LoadBalancer' data type.
--
-- 'LoadBalancer' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe Text@
--
loadBalancer :: LoadBalancer
loadBalancer = LoadBalancer
    { _lbName = Nothing
    }

-- | The name of the LoadBalancer.
lbName :: Lens' LoadBalancer (Maybe Text)
lbName = lens _lbName (\s a -> s { _lbName = a })

instance FromXML LoadBalancer where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LoadBalancer"

-- | Describes a trigger.
newtype Trigger = Trigger
    { _trName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Trigger' data type.
--
-- 'Trigger' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe Text@
--
trigger :: Trigger
trigger = Trigger
    { _trName = Nothing
    }

-- | The name of the trigger.
trName :: Lens' Trigger (Maybe Text)
trName = lens _trName (\s a -> s { _trName = a })

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
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ApplicationDescription' data type.
--
-- 'ApplicationDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ApplicationName ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @DateCreated ::@ @Maybe ISO8601@
--
-- * @DateUpdated ::@ @Maybe ISO8601@
--
-- * @Versions ::@ @[Text]@
--
-- * @ConfigurationTemplates ::@ @[Text]@
--
applicationDescription :: ApplicationDescription
applicationDescription = ApplicationDescription
    { _adApplicationName = Nothing
    , _adDescription = Nothing
    , _adDateCreated = Nothing
    , _adDateUpdated = Nothing
    , _adVersions = mempty
    , _adConfigurationTemplates = mempty
    }

-- | The name of the application.
adApplicationName :: Lens' ApplicationDescription (Maybe Text)
adApplicationName =
    lens _adApplicationName (\s a -> s { _adApplicationName = a })

-- | User-defined description of the application.
adDescription :: Lens' ApplicationDescription (Maybe Text)
adDescription = lens _adDescription (\s a -> s { _adDescription = a })

-- | The date when the application was created.
adDateCreated :: Lens' ApplicationDescription (Maybe ISO8601)
adDateCreated = lens _adDateCreated (\s a -> s { _adDateCreated = a })

-- | The date when the application was last modified.
adDateUpdated :: Lens' ApplicationDescription (Maybe ISO8601)
adDateUpdated = lens _adDateUpdated (\s a -> s { _adDateUpdated = a })

-- | The names of the versions for this application.
adVersions :: Lens' ApplicationDescription [Text]
adVersions = lens _adVersions (\s a -> s { _adVersions = a })

-- | The names of the configuration templates associated with this application.
adConfigurationTemplates :: Lens' ApplicationDescription [Text]
adConfigurationTemplates =
    lens _adConfigurationTemplates
         (\s a -> s { _adConfigurationTemplates = a })

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
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ApplicationVersionDescription' data type.
--
-- 'ApplicationVersionDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ApplicationName ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @VersionLabel ::@ @Maybe Text@
--
-- * @SourceBundle ::@ @Maybe S3Location@
--
-- * @DateCreated ::@ @Maybe ISO8601@
--
-- * @DateUpdated ::@ @Maybe ISO8601@
--
applicationVersionDescription :: ApplicationVersionDescription
applicationVersionDescription = ApplicationVersionDescription
    { _avdApplicationName = Nothing
    , _avdDescription = Nothing
    , _avdVersionLabel = Nothing
    , _avdSourceBundle = Nothing
    , _avdDateCreated = Nothing
    , _avdDateUpdated = Nothing
    }

-- | The name of the application associated with this release.
avdApplicationName :: Lens' ApplicationVersionDescription (Maybe Text)
avdApplicationName =
    lens _avdApplicationName (\s a -> s { _avdApplicationName = a })

-- | The description of this application version.
avdDescription :: Lens' ApplicationVersionDescription (Maybe Text)
avdDescription = lens _avdDescription (\s a -> s { _avdDescription = a })

-- | A label uniquely identifying the version for the associated application.
avdVersionLabel :: Lens' ApplicationVersionDescription (Maybe Text)
avdVersionLabel = lens _avdVersionLabel (\s a -> s { _avdVersionLabel = a })

-- | The location where the source bundle is located for this version.
avdSourceBundle :: Lens' ApplicationVersionDescription (Maybe S3Location)
avdSourceBundle = lens _avdSourceBundle (\s a -> s { _avdSourceBundle = a })

-- | The creation date of the application version.
avdDateCreated :: Lens' ApplicationVersionDescription (Maybe ISO8601)
avdDateCreated = lens _avdDateCreated (\s a -> s { _avdDateCreated = a })

-- | The last modified date of the application version.
avdDateUpdated :: Lens' ApplicationVersionDescription (Maybe ISO8601)
avdDateUpdated = lens _avdDateUpdated (\s a -> s { _avdDateUpdated = a })

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
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ConfigurationOptionDescription' data type.
--
-- 'ConfigurationOptionDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Namespace ::@ @Maybe Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @DefaultValue ::@ @Maybe Text@
--
-- * @ChangeSeverity ::@ @Maybe Text@
--
-- * @UserDefined ::@ @Maybe Bool@
--
-- * @ValueType ::@ @Maybe ConfigurationOptionValueType@
--
-- * @ValueOptions ::@ @[Text]@
--
-- * @MinValue ::@ @Maybe Integer@
--
-- * @MaxValue ::@ @Maybe Integer@
--
-- * @MaxLength ::@ @Maybe Integer@
--
-- * @Regex ::@ @Maybe OptionRestrictionRegex@
--
configurationOptionDescription :: ConfigurationOptionDescription
configurationOptionDescription = ConfigurationOptionDescription
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

-- | A unique namespace identifying the option's associated AWS resource.
codNamespace :: Lens' ConfigurationOptionDescription (Maybe Text)
codNamespace = lens _codNamespace (\s a -> s { _codNamespace = a })

-- | The name of the configuration option.
codName :: Lens' ConfigurationOptionDescription (Maybe Text)
codName = lens _codName (\s a -> s { _codName = a })

-- | The default value for this configuration option.
codDefaultValue :: Lens' ConfigurationOptionDescription (Maybe Text)
codDefaultValue = lens _codDefaultValue (\s a -> s { _codDefaultValue = a })

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

-- | If specified, values for the configuration option are selected from this
-- list.
codValueOptions :: Lens' ConfigurationOptionDescription [Text]
codValueOptions = lens _codValueOptions (\s a -> s { _codValueOptions = a })

-- | If specified, the configuration option must be a numeric value greater than
-- this value.
codMinValue :: Lens' ConfigurationOptionDescription (Maybe Integer)
codMinValue = lens _codMinValue (\s a -> s { _codMinValue = a })

-- | If specified, the configuration option must be a numeric value less than
-- this value.
codMaxValue :: Lens' ConfigurationOptionDescription (Maybe Integer)
codMaxValue = lens _codMaxValue (\s a -> s { _codMaxValue = a })

-- | If specified, the configuration option must be a string value no longer
-- than this value.
codMaxLength :: Lens' ConfigurationOptionDescription (Maybe Integer)
codMaxLength = lens _codMaxLength (\s a -> s { _codMaxLength = a })

-- | If specified, the configuration option must be a string value that
-- satisfies this regular expression.
codRegex :: Lens' ConfigurationOptionDescription (Maybe OptionRestrictionRegex)
codRegex = lens _codRegex (\s a -> s { _codRegex = a })

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
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ConfigurationOptionSetting' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Namespace ::@ @Maybe Text@
--
-- * @OptionName ::@ @Maybe Text@
--
-- * @Value ::@ @Maybe Text@
--
configurationOptionSetting :: ConfigurationOptionSetting
configurationOptionSetting = ConfigurationOptionSetting
    { _cosNamespace = Nothing
    , _cosOptionName = Nothing
    , _cosValue = Nothing
    }

-- | A unique namespace identifying the option's associated AWS resource.
cosNamespace :: Lens' ConfigurationOptionSetting (Maybe Text)
cosNamespace = lens _cosNamespace (\s a -> s { _cosNamespace = a })

-- | The name of the configuration option.
cosOptionName :: Lens' ConfigurationOptionSetting (Maybe Text)
cosOptionName = lens _cosOptionName (\s a -> s { _cosOptionName = a })

-- | The current value for the configuration option.
cosValue :: Lens' ConfigurationOptionSetting (Maybe Text)
cosValue = lens _cosValue (\s a -> s { _cosValue = a })

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
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ConfigurationSettingsDescription' data type.
--
-- 'ConfigurationSettingsDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SolutionStackName ::@ @Maybe Text@
--
-- * @ApplicationName ::@ @Maybe Text@
--
-- * @TemplateName ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @EnvironmentName ::@ @Maybe Text@
--
-- * @DeploymentStatus ::@ @Maybe ConfigurationDeploymentStatus@
--
-- * @DateCreated ::@ @Maybe ISO8601@
--
-- * @DateUpdated ::@ @Maybe ISO8601@
--
-- * @OptionSettings ::@ @[ConfigurationOptionSetting]@
--
configurationSettingsDescription :: ConfigurationSettingsDescription
configurationSettingsDescription = ConfigurationSettingsDescription
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

-- | The name of the solution stack this configuration set uses.
csdSolutionStackName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdSolutionStackName =
    lens _csdSolutionStackName (\s a -> s { _csdSolutionStackName = a })

-- | The name of the application associated with this configuration set.
csdApplicationName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdApplicationName =
    lens _csdApplicationName (\s a -> s { _csdApplicationName = a })

-- | If not null, the name of the configuration template for this configuration
-- set.
csdTemplateName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdTemplateName = lens _csdTemplateName (\s a -> s { _csdTemplateName = a })

-- | Describes this configuration set.
csdDescription :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdDescription = lens _csdDescription (\s a -> s { _csdDescription = a })

-- | If not null, the name of the environment for this configuration set.
csdEnvironmentName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdEnvironmentName =
    lens _csdEnvironmentName (\s a -> s { _csdEnvironmentName = a })

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

-- | The date (in UTC time) when this configuration set was created.
csdDateCreated :: Lens' ConfigurationSettingsDescription (Maybe ISO8601)
csdDateCreated = lens _csdDateCreated (\s a -> s { _csdDateCreated = a })

-- | The date (in UTC time) when this configuration set was last modified.
csdDateUpdated :: Lens' ConfigurationSettingsDescription (Maybe ISO8601)
csdDateUpdated = lens _csdDateUpdated (\s a -> s { _csdDateUpdated = a })

-- | A list of the configuration options and their values in this configuration
-- set.
csdOptionSettings :: Lens' ConfigurationSettingsDescription [ConfigurationOptionSetting]
csdOptionSettings =
    lens _csdOptionSettings (\s a -> s { _csdOptionSettings = a })

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
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EnvironmentDescription' data type.
--
-- 'EnvironmentDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EnvironmentName ::@ @Maybe Text@
--
-- * @EnvironmentId ::@ @Maybe Text@
--
-- * @ApplicationName ::@ @Maybe Text@
--
-- * @VersionLabel ::@ @Maybe Text@
--
-- * @SolutionStackName ::@ @Maybe Text@
--
-- * @TemplateName ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @EndpointURL ::@ @Maybe Text@
--
-- * @CNAME ::@ @Maybe Text@
--
-- * @DateCreated ::@ @Maybe ISO8601@
--
-- * @DateUpdated ::@ @Maybe ISO8601@
--
-- * @Status ::@ @Maybe EnvironmentStatus@
--
-- * @Health ::@ @Maybe EnvironmentHealth@
--
-- * @Resources ::@ @Maybe EnvironmentResourcesDescription@
--
-- * @Tier ::@ @Maybe EnvironmentTier@
--
environmentDescription :: EnvironmentDescription
environmentDescription = EnvironmentDescription
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

-- | The name of this environment.
edEnvironmentName :: Lens' EnvironmentDescription (Maybe Text)
edEnvironmentName =
    lens _edEnvironmentName (\s a -> s { _edEnvironmentName = a })

-- | The ID of this environment.
edEnvironmentId :: Lens' EnvironmentDescription (Maybe Text)
edEnvironmentId = lens _edEnvironmentId (\s a -> s { _edEnvironmentId = a })

-- | The name of the application associated with this environment.
edApplicationName :: Lens' EnvironmentDescription (Maybe Text)
edApplicationName =
    lens _edApplicationName (\s a -> s { _edApplicationName = a })

-- | The application version deployed in this environment.
edVersionLabel :: Lens' EnvironmentDescription (Maybe Text)
edVersionLabel = lens _edVersionLabel (\s a -> s { _edVersionLabel = a })

-- | The name of the SolutionStack deployed with this environment.
edSolutionStackName :: Lens' EnvironmentDescription (Maybe Text)
edSolutionStackName =
    lens _edSolutionStackName (\s a -> s { _edSolutionStackName = a })

-- | The name of the configuration template used to originally launch this
-- environment.
edTemplateName :: Lens' EnvironmentDescription (Maybe Text)
edTemplateName = lens _edTemplateName (\s a -> s { _edTemplateName = a })

-- | Describes this environment.
edDescription :: Lens' EnvironmentDescription (Maybe Text)
edDescription = lens _edDescription (\s a -> s { _edDescription = a })

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer.
-- For single-instance environments, the IP address of the instance.
edEndpointURL :: Lens' EnvironmentDescription (Maybe Text)
edEndpointURL = lens _edEndpointURL (\s a -> s { _edEndpointURL = a })

-- | The URL to the CNAME for this environment.
edCNAME :: Lens' EnvironmentDescription (Maybe Text)
edCNAME = lens _edCNAME (\s a -> s { _edCNAME = a })

-- | The creation date for this environment.
edDateCreated :: Lens' EnvironmentDescription (Maybe ISO8601)
edDateCreated = lens _edDateCreated (\s a -> s { _edDateCreated = a })

-- | The last modified date for this environment.
edDateUpdated :: Lens' EnvironmentDescription (Maybe ISO8601)
edDateUpdated = lens _edDateUpdated (\s a -> s { _edDateUpdated = a })

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such as
-- update or terminate. Terminating: Environment is in the shut-down process.
-- Terminated: Environment is not running.
edStatus :: Lens' EnvironmentDescription (Maybe EnvironmentStatus)
edStatus = lens _edStatus (\s a -> s { _edStatus = a })

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

-- | The description of the AWS resources used by this environment.
edResources :: Lens' EnvironmentDescription (Maybe EnvironmentResourcesDescription)
edResources = lens _edResources (\s a -> s { _edResources = a })

-- | Describes the current tier of this environment.
edTier :: Lens' EnvironmentDescription (Maybe EnvironmentTier)
edTier = lens _edTier (\s a -> s { _edTier = a })

instance FromXML EnvironmentDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentDescription"

-- | The information retrieved from the Amazon EC2 instances.
data EnvironmentInfoDescription = EnvironmentInfoDescription
    { _eidInfoType :: Maybe EnvironmentInfoType
    , _eidEc2InstanceId :: Maybe Text
    , _eidSampleTimestamp :: Maybe ISO8601
    , _eidMessage :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EnvironmentInfoDescription' data type.
--
-- 'EnvironmentInfoDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InfoType ::@ @Maybe EnvironmentInfoType@
--
-- * @Ec2InstanceId ::@ @Maybe Text@
--
-- * @SampleTimestamp ::@ @Maybe ISO8601@
--
-- * @Message ::@ @Maybe Text@
--
environmentInfoDescription :: EnvironmentInfoDescription
environmentInfoDescription = EnvironmentInfoDescription
    { _eidInfoType = Nothing
    , _eidEc2InstanceId = Nothing
    , _eidSampleTimestamp = Nothing
    , _eidMessage = Nothing
    }

-- | The type of information retrieved.
eidInfoType :: Lens' EnvironmentInfoDescription (Maybe EnvironmentInfoType)
eidInfoType = lens _eidInfoType (\s a -> s { _eidInfoType = a })

-- | The Amazon EC2 Instance ID for this information.
eidEc2InstanceId :: Lens' EnvironmentInfoDescription (Maybe Text)
eidEc2InstanceId =
    lens _eidEc2InstanceId (\s a -> s { _eidEc2InstanceId = a })

-- | The time stamp when this information was retrieved.
eidSampleTimestamp :: Lens' EnvironmentInfoDescription (Maybe ISO8601)
eidSampleTimestamp =
    lens _eidSampleTimestamp (\s a -> s { _eidSampleTimestamp = a })

-- | The retrieved information.
eidMessage :: Lens' EnvironmentInfoDescription (Maybe Text)
eidMessage = lens _eidMessage (\s a -> s { _eidMessage = a })

instance FromXML EnvironmentInfoDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentInfoDescription"

-- | A list of EnvironmentResourceDescription.
data EnvironmentResourceDescription = EnvironmentResourceDescription
    { _erdrEnvironmentName :: Maybe Text
    , _erdrAutoScalingGroups :: [AutoScalingGroup]
    , _erdrInstances :: [Instance]
    , _erdrLaunchConfigurations :: [LaunchConfiguration]
    , _erdrLoadBalancers :: [LoadBalancer]
    , _erdrTriggers :: [Trigger]
    , _erdrQueues :: [Queue]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EnvironmentResourceDescription' data type.
--
-- 'EnvironmentResourceDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EnvironmentName ::@ @Maybe Text@
--
-- * @AutoScalingGroups ::@ @[AutoScalingGroup]@
--
-- * @Instances ::@ @[Instance]@
--
-- * @LaunchConfigurations ::@ @[LaunchConfiguration]@
--
-- * @LoadBalancers ::@ @[LoadBalancer]@
--
-- * @Triggers ::@ @[Trigger]@
--
-- * @Queues ::@ @[Queue]@
--
environmentResourceDescription :: EnvironmentResourceDescription
environmentResourceDescription = EnvironmentResourceDescription
    { _erdrEnvironmentName = Nothing
    , _erdrAutoScalingGroups = mempty
    , _erdrInstances = mempty
    , _erdrLaunchConfigurations = mempty
    , _erdrLoadBalancers = mempty
    , _erdrTriggers = mempty
    , _erdrQueues = mempty
    }

-- | The name of the environment.
erdrEnvironmentName :: Lens' EnvironmentResourceDescription (Maybe Text)
erdrEnvironmentName =
    lens _erdrEnvironmentName (\s a -> s { _erdrEnvironmentName = a })

-- | The AutoScalingGroups used by this environment.
erdrAutoScalingGroups :: Lens' EnvironmentResourceDescription [AutoScalingGroup]
erdrAutoScalingGroups =
    lens _erdrAutoScalingGroups (\s a -> s { _erdrAutoScalingGroups = a })

-- | The Amazon EC2 instances used by this environment.
erdrInstances :: Lens' EnvironmentResourceDescription [Instance]
erdrInstances = lens _erdrInstances (\s a -> s { _erdrInstances = a })

-- | The Auto Scaling launch configurations in use by this environment.
erdrLaunchConfigurations :: Lens' EnvironmentResourceDescription [LaunchConfiguration]
erdrLaunchConfigurations =
    lens _erdrLaunchConfigurations
         (\s a -> s { _erdrLaunchConfigurations = a })

-- | The LoadBalancers in use by this environment.
erdrLoadBalancers :: Lens' EnvironmentResourceDescription [LoadBalancer]
erdrLoadBalancers =
    lens _erdrLoadBalancers (\s a -> s { _erdrLoadBalancers = a })

-- | The AutoScaling triggers in use by this environment.
erdrTriggers :: Lens' EnvironmentResourceDescription [Trigger]
erdrTriggers = lens _erdrTriggers (\s a -> s { _erdrTriggers = a })

-- | The queues used by this environment.
erdrQueues :: Lens' EnvironmentResourceDescription [Queue]
erdrQueues = lens _erdrQueues (\s a -> s { _erdrQueues = a })

instance FromXML EnvironmentResourceDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentResourceDescription"

-- | This specifies the tier to use for creating this environment.
data EnvironmentTier = EnvironmentTier
    { _etName :: Maybe Text
    , _etType :: Maybe Text
    , _etVersion :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EnvironmentTier' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe Text@
--
-- * @Type ::@ @Maybe Text@
--
-- * @Version ::@ @Maybe Text@
--
environmentTier :: EnvironmentTier
environmentTier = EnvironmentTier
    { _etName = Nothing
    , _etType = Nothing
    , _etVersion = Nothing
    }

-- | The name of this environment tier.
etName :: Lens' EnvironmentTier (Maybe Text)
etName = lens _etName (\s a -> s { _etName = a })

-- | The type of this environment tier.
etType :: Lens' EnvironmentTier (Maybe Text)
etType = lens _etType (\s a -> s { _etType = a })

-- | The version of this environment tier.
etVersion :: Lens' EnvironmentTier (Maybe Text)
etVersion = lens _etVersion (\s a -> s { _etVersion = a })

instance FromXML EnvironmentTier where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentTier"

instance ToQuery EnvironmentTier where
    toQuery = genericQuery def

-- | Describes an event.
data EventDescription = EventDescription
    { _edrEventDate :: Maybe ISO8601
    , _edrMessage :: Maybe Text
    , _edrApplicationName :: Maybe Text
    , _edrVersionLabel :: Maybe Text
    , _edrTemplateName :: Maybe Text
    , _edrEnvironmentName :: Maybe Text
    , _edrRequestId :: Maybe Text
    , _edrSeverity :: Maybe EventSeverity
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EventDescription' data type.
--
-- 'EventDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EventDate ::@ @Maybe ISO8601@
--
-- * @Message ::@ @Maybe Text@
--
-- * @ApplicationName ::@ @Maybe Text@
--
-- * @VersionLabel ::@ @Maybe Text@
--
-- * @TemplateName ::@ @Maybe Text@
--
-- * @EnvironmentName ::@ @Maybe Text@
--
-- * @RequestId ::@ @Maybe Text@
--
-- * @Severity ::@ @Maybe EventSeverity@
--
eventDescription :: EventDescription
eventDescription = EventDescription
    { _edrEventDate = Nothing
    , _edrMessage = Nothing
    , _edrApplicationName = Nothing
    , _edrVersionLabel = Nothing
    , _edrTemplateName = Nothing
    , _edrEnvironmentName = Nothing
    , _edrRequestId = Nothing
    , _edrSeverity = Nothing
    }

-- | The date when the event occurred.
edrEventDate :: Lens' EventDescription (Maybe ISO8601)
edrEventDate = lens _edrEventDate (\s a -> s { _edrEventDate = a })

-- | The event message.
edrMessage :: Lens' EventDescription (Maybe Text)
edrMessage = lens _edrMessage (\s a -> s { _edrMessage = a })

-- | The application associated with the event.
edrApplicationName :: Lens' EventDescription (Maybe Text)
edrApplicationName =
    lens _edrApplicationName (\s a -> s { _edrApplicationName = a })

-- | The release label for the application version associated with this event.
edrVersionLabel :: Lens' EventDescription (Maybe Text)
edrVersionLabel = lens _edrVersionLabel (\s a -> s { _edrVersionLabel = a })

-- | The name of the configuration associated with this event.
edrTemplateName :: Lens' EventDescription (Maybe Text)
edrTemplateName = lens _edrTemplateName (\s a -> s { _edrTemplateName = a })

-- | The name of the environment associated with this event.
edrEnvironmentName :: Lens' EventDescription (Maybe Text)
edrEnvironmentName =
    lens _edrEnvironmentName (\s a -> s { _edrEnvironmentName = a })

-- | The web service request ID for the activity of this event.
edrRequestId :: Lens' EventDescription (Maybe Text)
edrRequestId = lens _edrRequestId (\s a -> s { _edrRequestId = a })

-- | The severity level of this event.
edrSeverity :: Lens' EventDescription (Maybe EventSeverity)
edrSeverity = lens _edrSeverity (\s a -> s { _edrSeverity = a })

instance FromXML EventDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventDescription"

-- | Describes the properties of a Listener for the LoadBalancer.
data Listener = Listener
    { _lProtocol :: Maybe Text
    , _lPort :: Maybe Integer
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Listener' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Protocol ::@ @Maybe Text@
--
-- * @Port ::@ @Maybe Integer@
--
listener :: Listener
listener = Listener
    { _lProtocol = Nothing
    , _lPort = Nothing
    }

-- | The protocol that is used by the Listener.
lProtocol :: Lens' Listener (Maybe Text)
lProtocol = lens _lProtocol (\s a -> s { _lProtocol = a })

-- | The port that is used by the Listener.
lPort :: Lens' Listener (Maybe Integer)
lPort = lens _lPort (\s a -> s { _lPort = a })

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
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'LoadBalancerDescription' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerName ::@ @Maybe Text@
--
-- * @Domain ::@ @Maybe Text@
--
-- * @Listeners ::@ @[Listener]@
--
loadBalancerDescription :: LoadBalancerDescription
loadBalancerDescription = LoadBalancerDescription
    { _lbdLoadBalancerName = Nothing
    , _lbdDomain = Nothing
    , _lbdListeners = mempty
    }

-- | The name of the LoadBalancer.
lbdLoadBalancerName :: Lens' LoadBalancerDescription (Maybe Text)
lbdLoadBalancerName =
    lens _lbdLoadBalancerName (\s a -> s { _lbdLoadBalancerName = a })

-- | The domain name of the LoadBalancer.
lbdDomain :: Lens' LoadBalancerDescription (Maybe Text)
lbdDomain = lens _lbdDomain (\s a -> s { _lbdDomain = a })

-- | A list of Listeners used by the LoadBalancer.
lbdListeners :: Lens' LoadBalancerDescription [Listener]
lbdListeners = lens _lbdListeners (\s a -> s { _lbdListeners = a })

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
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OptionRestrictionRegex' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Pattern ::@ @Maybe Text@
--
-- * @Label ::@ @Maybe Text@
--
optionRestrictionRegex :: OptionRestrictionRegex
optionRestrictionRegex = OptionRestrictionRegex
    { _orrPattern = Nothing
    , _orrLabel = Nothing
    }

-- | The regular expression pattern that a string configuration option value
-- with this restriction must match.
orrPattern :: Lens' OptionRestrictionRegex (Maybe Text)
orrPattern = lens _orrPattern (\s a -> s { _orrPattern = a })

-- | A unique name representing this regular expression.
orrLabel :: Lens' OptionRestrictionRegex (Maybe Text)
orrLabel = lens _orrLabel (\s a -> s { _orrLabel = a })

instance FromXML OptionRestrictionRegex where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionRestrictionRegex"

instance ToQuery OptionRestrictionRegex where
    toQuery = genericQuery def

-- | A specification identifying an individual configuration option.
data OptionSpecification = OptionSpecification
    { _osNamespace :: Maybe Text
    , _osOptionName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OptionSpecification' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Namespace ::@ @Maybe Text@
--
-- * @OptionName ::@ @Maybe Text@
--
optionSpecification :: OptionSpecification
optionSpecification = OptionSpecification
    { _osNamespace = Nothing
    , _osOptionName = Nothing
    }

-- | A unique namespace identifying the option's associated AWS resource.
osNamespace :: Lens' OptionSpecification (Maybe Text)
osNamespace = lens _osNamespace (\s a -> s { _osNamespace = a })

-- | The name of the configuration option.
osOptionName :: Lens' OptionSpecification (Maybe Text)
osOptionName = lens _osOptionName (\s a -> s { _osOptionName = a })

instance ToQuery OptionSpecification where
    toQuery = genericQuery def

-- | Describes a queue.
data Queue = Queue
    { _qName :: Maybe Text
    , _qURL :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Queue' data type.
--
-- 'Queue' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe Text@
--
-- * @URL ::@ @Maybe Text@
--
queue :: Queue
queue = Queue
    { _qName = Nothing
    , _qURL = Nothing
    }

-- | The name of the queue.
qName :: Lens' Queue (Maybe Text)
qName = lens _qName (\s a -> s { _qName = a })

-- | The URL of the queue.
qURL :: Lens' Queue (Maybe Text)
qURL = lens _qURL (\s a -> s { _qURL = a })

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
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'S3Location' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @S3Bucket ::@ @Maybe Text@
--
-- * @S3Key ::@ @Maybe Text@
--
s3Location :: S3Location
s3Location = S3Location
    { _slS3Bucket = Nothing
    , _slS3Key = Nothing
    }

-- | The Amazon S3 bucket where the data is located.
slS3Bucket :: Lens' S3Location (Maybe Text)
slS3Bucket = lens _slS3Bucket (\s a -> s { _slS3Bucket = a })

-- | The Amazon S3 key where the data is located.
slS3Key :: Lens' S3Location (Maybe Text)
slS3Key = lens _slS3Key (\s a -> s { _slS3Key = a })

instance FromXML S3Location where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "S3Location"

instance ToQuery S3Location where
    toQuery = genericQuery def

-- | Describes the solution stack.
data SolutionStackDescription = SolutionStackDescription
    { _ssdSolutionStackName :: Maybe Text
    , _ssdPermittedFileTypes :: [Text]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SolutionStackDescription' data type.
--
-- 'SolutionStackDescription' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SolutionStackName ::@ @Maybe Text@
--
-- * @PermittedFileTypes ::@ @[Text]@
--
solutionStackDescription :: SolutionStackDescription
solutionStackDescription = SolutionStackDescription
    { _ssdSolutionStackName = Nothing
    , _ssdPermittedFileTypes = mempty
    }

-- | The name of the solution stack.
ssdSolutionStackName :: Lens' SolutionStackDescription (Maybe Text)
ssdSolutionStackName =
    lens _ssdSolutionStackName (\s a -> s { _ssdSolutionStackName = a })

-- | The permitted file types allowed for a solution stack.
ssdPermittedFileTypes :: Lens' SolutionStackDescription [Text]
ssdPermittedFileTypes =
    lens _ssdPermittedFileTypes (\s a -> s { _ssdPermittedFileTypes = a })

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
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SourceConfiguration' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ApplicationName ::@ @Maybe Text@
--
-- * @TemplateName ::@ @Maybe Text@
--
sourceConfiguration :: SourceConfiguration
sourceConfiguration = SourceConfiguration
    { _scApplicationName = Nothing
    , _scTemplateName = Nothing
    }

-- | The name of the application associated with the configuration.
scApplicationName :: Lens' SourceConfiguration (Maybe Text)
scApplicationName =
    lens _scApplicationName (\s a -> s { _scApplicationName = a })

-- | The name of the configuration template.
scTemplateName :: Lens' SourceConfiguration (Maybe Text)
scTemplateName = lens _scTemplateName (\s a -> s { _scTemplateName = a })

instance ToQuery SourceConfiguration where
    toQuery = genericQuery def

-- | Describes a tag applied to a resource in an environment.
data Tag = Tag
    { _tKey :: Maybe Text
    , _tValue :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Tag' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Key ::@ @Maybe Text@
--
-- * @Value ::@ @Maybe Text@
--
tag :: Tag
tag = Tag
    { _tKey = Nothing
    , _tValue = Nothing
    }

-- | The key of the tag.
tKey :: Lens' Tag (Maybe Text)
tKey = lens _tKey (\s a -> s { _tKey = a })

-- | The value of the tag.
tValue :: Lens' Tag (Maybe Text)
tValue = lens _tValue (\s a -> s { _tValue = a })

instance ToQuery Tag where
    toQuery = genericQuery def

-- | An error or warning for a desired configuration option value.
data ValidationMessage = ValidationMessage
    { _vmMessage :: Maybe Text
    , _vmSeverity :: Maybe ValidationSeverity
    , _vmNamespace :: Maybe Text
    , _vmOptionName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ValidationMessage' data type.
--
-- 'ValidationMessage' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Message ::@ @Maybe Text@
--
-- * @Severity ::@ @Maybe ValidationSeverity@
--
-- * @Namespace ::@ @Maybe Text@
--
-- * @OptionName ::@ @Maybe Text@
--
validationMessage :: ValidationMessage
validationMessage = ValidationMessage
    { _vmMessage = Nothing
    , _vmSeverity = Nothing
    , _vmNamespace = Nothing
    , _vmOptionName = Nothing
    }

-- | A message describing the error or warning.
vmMessage :: Lens' ValidationMessage (Maybe Text)
vmMessage = lens _vmMessage (\s a -> s { _vmMessage = a })

-- | An indication of the severity of this message: error: This message
-- indicates that this is not a valid setting for an option. warning: This
-- message is providing information you should take into account. error: This
-- message indicates that this is not a valid setting for an option. warning:
-- This message is providing information you should take into account.
vmSeverity :: Lens' ValidationMessage (Maybe ValidationSeverity)
vmSeverity = lens _vmSeverity (\s a -> s { _vmSeverity = a })

-- | 
vmNamespace :: Lens' ValidationMessage (Maybe Text)
vmNamespace = lens _vmNamespace (\s a -> s { _vmNamespace = a })

-- | 
vmOptionName :: Lens' ValidationMessage (Maybe Text)
vmOptionName = lens _vmOptionName (\s a -> s { _vmOptionName = a })

instance FromXML ValidationMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ValidationMessage"
