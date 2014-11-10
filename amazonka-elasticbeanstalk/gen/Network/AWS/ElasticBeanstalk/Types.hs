{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.ElasticBeanstalk.Types
    (
    -- * Service
      ElasticBeanstalk
    -- ** Errors
    , ElasticBeanstalkError (..)
    , _ElasticBeanstalkHttp
    , _ElasticBeanstalkSerializer
    , _ElasticBeanstalkService
    -- ** XML
    , xmlOptions

    -- * ApplicationDescription
    , ApplicationDescription
    , applicationDescription
    , adApplicationName
    , adConfigurationTemplates
    , adDateCreated
    , adDateUpdated
    , adDescription
    , adVersions

    -- * EventSeverity
    , EventSeverity (..)

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * EventDescription
    , EventDescription
    , eventDescription
    , edApplicationName
    , edEnvironmentName
    , edEventDate
    , edMessage
    , edRequestId
    , edSeverity
    , edTemplateName
    , edVersionLabel

    -- * LaunchConfiguration
    , LaunchConfiguration
    , launchConfiguration
    , lcName

    -- * ApplicationVersionDescriptionMessage
    , ApplicationVersionDescriptionMessage
    , applicationVersionDescriptionMessage
    , avdmApplicationVersion

    -- * AutoScalingGroup
    , AutoScalingGroup
    , autoScalingGroup
    , asgName

    -- * ConfigurationDeploymentStatus
    , ConfigurationDeploymentStatus (..)

    -- * ConfigurationOptionSetting
    , ConfigurationOptionSetting
    , configurationOptionSetting
    , cosNamespace
    , cosOptionName
    , cosValue

    -- * ConfigurationOptionValueType
    , ConfigurationOptionValueType (..)

    -- * ConfigurationSettingsDescription
    , ConfigurationSettingsDescription
    , configurationSettingsDescription
    , csdApplicationName
    , csdDateCreated
    , csdDateUpdated
    , csdDeploymentStatus
    , csdDescription
    , csdEnvironmentName
    , csdOptionSettings
    , csdSolutionStackName
    , csdTemplateName

    -- * ApplicationVersionDescription
    , ApplicationVersionDescription
    , applicationVersionDescription
    , avdApplicationName
    , avdDateCreated
    , avdDateUpdated
    , avdDescription
    , avdSourceBundle
    , avdVersionLabel

    -- * OptionSpecification
    , OptionSpecification
    , optionSpecification
    , osNamespace
    , osOptionName

    -- * EnvironmentResourceDescription
    , EnvironmentResourceDescription
    , environmentResourceDescription
    , erdAutoScalingGroups
    , erdEnvironmentName
    , erdInstances
    , erdLaunchConfigurations
    , erdLoadBalancers
    , erdQueues
    , erdTriggers

    -- * Queue
    , Queue
    , queue
    , qName
    , qURL

    -- * EnvironmentStatus
    , EnvironmentStatus (..)

    -- * LoadBalancerDescription
    , LoadBalancerDescription
    , loadBalancerDescription
    , lbdDomain
    , lbdListeners
    , lbdLoadBalancerName

    -- * ApplicationDescriptionMessage
    , ApplicationDescriptionMessage
    , applicationDescriptionMessage
    , admApplication

    -- * EnvironmentTier
    , EnvironmentTier
    , environmentTier
    , etName
    , etType
    , etVersion

    -- * LoadBalancer
    , LoadBalancer
    , loadBalancer
    , lbName

    -- * EnvironmentResourcesDescription
    , EnvironmentResourcesDescription
    , environmentResourcesDescription
    , erdLoadBalancer

    -- * OptionRestrictionRegex
    , OptionRestrictionRegex
    , optionRestrictionRegex
    , orrLabel
    , orrPattern

    -- * ConfigurationOptionDescription
    , ConfigurationOptionDescription
    , configurationOptionDescription
    , codChangeSeverity
    , codDefaultValue
    , codMaxLength
    , codMaxValue
    , codMinValue
    , codName
    , codNamespace
    , codRegex
    , codUserDefined
    , codValueOptions
    , codValueType

    -- * SourceConfiguration
    , SourceConfiguration
    , sourceConfiguration
    , scApplicationName
    , scTemplateName

    -- * EnvironmentInfoDescription
    , EnvironmentInfoDescription
    , environmentInfoDescription
    , eidEc2InstanceId
    , eidInfoType
    , eidMessage
    , eidSampleTimestamp

    -- * S3Location
    , S3Location
    , s3Location
    , slS3Bucket
    , slS3Key

    -- * ValidationMessage
    , ValidationMessage
    , validationMessage
    , vmMessage
    , vmNamespace
    , vmOptionName
    , vmSeverity

    -- * ValidationSeverity
    , ValidationSeverity (..)

    -- * Trigger
    , Trigger
    , trigger
    , tName

    -- * EnvironmentInfoType
    , EnvironmentInfoType (..)

    -- * EnvironmentDescription
    , EnvironmentDescription
    , environmentDescription
    , ed1ApplicationName
    , ed1CNAME
    , ed1DateCreated
    , ed1DateUpdated
    , ed1Description
    , ed1EndpointURL
    , ed1EnvironmentId
    , ed1EnvironmentName
    , ed1Health
    , ed1Resources
    , ed1SolutionStackName
    , ed1Status
    , ed1TemplateName
    , ed1Tier
    , ed1VersionLabel

    -- * Listener
    , Listener
    , listener
    , lPort
    , lProtocol

    -- * EnvironmentHealth
    , EnvironmentHealth (..)

    -- * Instance
    , Instance
    , instance'
    , iId

    -- * SolutionStackDescription
    , SolutionStackDescription
    , solutionStackDescription
    , ssdPermittedFileTypes
    , ssdSolutionStackName
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2010-12-01@) of the Amazon Elastic Beanstalk.
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

    handle = xmlError alwaysFail

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def

data ApplicationDescription = ApplicationDescription
    { _adApplicationName        :: Maybe Text
    , _adConfigurationTemplates :: [Text]
    , _adDateCreated            :: Maybe RFC822
    , _adDateUpdated            :: Maybe RFC822
    , _adDescription            :: Maybe Text
    , _adVersions               :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'ApplicationDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'adApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'adConfigurationTemplates' @::@ ['Text']
--
-- * 'adDateCreated' @::@ 'Maybe' 'UTCTime'
--
-- * 'adDateUpdated' @::@ 'Maybe' 'UTCTime'
--
-- * 'adDescription' @::@ 'Maybe' 'Text'
--
-- * 'adVersions' @::@ ['Text']
--
applicationDescription :: ApplicationDescription
applicationDescription = ApplicationDescription
    { _adApplicationName        = Nothing
    , _adDescription            = Nothing
    , _adDateCreated            = Nothing
    , _adDateUpdated            = Nothing
    , _adVersions               = mempty
    , _adConfigurationTemplates = mempty
    }

-- | The name of the application.
adApplicationName :: Lens' ApplicationDescription (Maybe Text)
adApplicationName =
    lens _adApplicationName (\s a -> s { _adApplicationName = a })

-- | The names of the configuration templates associated with this
-- application.
adConfigurationTemplates :: Lens' ApplicationDescription [Text]
adConfigurationTemplates =
    lens _adConfigurationTemplates
        (\s a -> s { _adConfigurationTemplates = a })

-- | The date when the application was created.
adDateCreated :: Lens' ApplicationDescription (Maybe UTCTime)
adDateCreated = lens _adDateCreated (\s a -> s { _adDateCreated = a })
    . mapping _Time

-- | The date when the application was last modified.
adDateUpdated :: Lens' ApplicationDescription (Maybe UTCTime)
adDateUpdated = lens _adDateUpdated (\s a -> s { _adDateUpdated = a })
    . mapping _Time

-- | User-defined description of the application.
adDescription :: Lens' ApplicationDescription (Maybe Text)
adDescription = lens _adDescription (\s a -> s { _adDescription = a })

-- | The names of the versions for this application.
adVersions :: Lens' ApplicationDescription [Text]
adVersions = lens _adVersions (\s a -> s { _adVersions = a })

instance FromXML ApplicationDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ApplicationDescription"

instance ToQuery ApplicationDescription

data EventSeverity
    = Debug -- ^ DEBUG
    | Error -- ^ ERROR
    | Fatal -- ^ FATAL
    | Info  -- ^ INFO
    | Trace -- ^ TRACE
    | Warn  -- ^ WARN
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable EventSeverity

instance FromText EventSeverity where
    parser = match "DEBUG" Debug
         <|> match "ERROR" Error
         <|> match "FATAL" Fatal
         <|> match "INFO"  Info
         <|> match "TRACE" Trace
         <|> match "WARN"  Warn

instance ToText EventSeverity where
    toText = \case
        Debug -> "DEBUG"
        Error -> "ERROR"
        Fatal -> "FATAL"
        Info  -> "INFO"
        Trace -> "TRACE"
        Warn  -> "WARN"

instance FromXML EventSeverity where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventSeverity"

instance ToQuery EventSeverity

data Tag = Tag
    { _tagKey   :: Maybe Text
    , _tagValue :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Tag' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagKey' @::@ 'Maybe' 'Text'
--
-- * 'tagValue' @::@ 'Maybe' 'Text'
--
tag :: Tag
tag = Tag
    { _tagKey   = Nothing
    , _tagValue = Nothing
    }

-- | The key of the tag.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\s a -> s { _tagKey = a })

-- | The value of the tag.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\s a -> s { _tagValue = a })

instance FromXML Tag where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Tag"

instance ToQuery Tag

data EventDescription = EventDescription
    { _edApplicationName :: Maybe Text
    , _edEnvironmentName :: Maybe Text
    , _edEventDate       :: Maybe RFC822
    , _edMessage         :: Maybe Text
    , _edRequestId       :: Maybe Text
    , _edSeverity        :: Maybe Text
    , _edTemplateName    :: Maybe Text
    , _edVersionLabel    :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'EventDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'edApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'edEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'edEventDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'edMessage' @::@ 'Maybe' 'Text'
--
-- * 'edRequestId' @::@ 'Maybe' 'Text'
--
-- * 'edSeverity' @::@ 'Maybe' 'Text'
--
-- * 'edTemplateName' @::@ 'Maybe' 'Text'
--
-- * 'edVersionLabel' @::@ 'Maybe' 'Text'
--
eventDescription :: EventDescription
eventDescription = EventDescription
    { _edEventDate       = Nothing
    , _edMessage         = Nothing
    , _edApplicationName = Nothing
    , _edVersionLabel    = Nothing
    , _edTemplateName    = Nothing
    , _edEnvironmentName = Nothing
    , _edRequestId       = Nothing
    , _edSeverity        = Nothing
    }

-- | The application associated with the event.
edApplicationName :: Lens' EventDescription (Maybe Text)
edApplicationName =
    lens _edApplicationName (\s a -> s { _edApplicationName = a })

-- | The name of the environment associated with this event.
edEnvironmentName :: Lens' EventDescription (Maybe Text)
edEnvironmentName =
    lens _edEnvironmentName (\s a -> s { _edEnvironmentName = a })

-- | The date when the event occurred.
edEventDate :: Lens' EventDescription (Maybe UTCTime)
edEventDate = lens _edEventDate (\s a -> s { _edEventDate = a })
    . mapping _Time

-- | The event message.
edMessage :: Lens' EventDescription (Maybe Text)
edMessage = lens _edMessage (\s a -> s { _edMessage = a })

-- | The web service request ID for the activity of this event.
edRequestId :: Lens' EventDescription (Maybe Text)
edRequestId = lens _edRequestId (\s a -> s { _edRequestId = a })

-- | The severity level of this event.
edSeverity :: Lens' EventDescription (Maybe Text)
edSeverity = lens _edSeverity (\s a -> s { _edSeverity = a })

-- | The name of the configuration associated with this event.
edTemplateName :: Lens' EventDescription (Maybe Text)
edTemplateName = lens _edTemplateName (\s a -> s { _edTemplateName = a })

-- | The release label for the application version associated with this event.
edVersionLabel :: Lens' EventDescription (Maybe Text)
edVersionLabel = lens _edVersionLabel (\s a -> s { _edVersionLabel = a })

instance FromXML EventDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventDescription"

instance ToQuery EventDescription

newtype LaunchConfiguration = LaunchConfiguration
    { _lcName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'LaunchConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcName' @::@ 'Maybe' 'Text'
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

instance ToQuery LaunchConfiguration

newtype ApplicationVersionDescriptionMessage = ApplicationVersionDescriptionMessage
    { _avdmApplicationVersion :: Maybe ApplicationVersionDescription
    } deriving (Eq, Show, Generic)

-- | 'ApplicationVersionDescriptionMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avdmApplicationVersion' @::@ 'Maybe' 'ApplicationVersionDescription'
--
applicationVersionDescriptionMessage :: ApplicationVersionDescriptionMessage
applicationVersionDescriptionMessage = ApplicationVersionDescriptionMessage
    { _avdmApplicationVersion = Nothing
    }

-- | The ApplicationVersionDescription of the application version.
avdmApplicationVersion :: Lens' ApplicationVersionDescriptionMessage (Maybe ApplicationVersionDescription)
avdmApplicationVersion =
    lens _avdmApplicationVersion (\s a -> s { _avdmApplicationVersion = a })

instance FromXML ApplicationVersionDescriptionMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ApplicationVersionDescriptionMessage"

instance ToQuery ApplicationVersionDescriptionMessage

newtype AutoScalingGroup = AutoScalingGroup
    { _asgName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AutoScalingGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asgName' @::@ 'Maybe' 'Text'
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

instance ToQuery AutoScalingGroup

data ConfigurationDeploymentStatus
    = Deployed -- ^ deployed
    | Failed   -- ^ failed
    | Pending  -- ^ pending
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable ConfigurationDeploymentStatus

instance FromText ConfigurationDeploymentStatus where
    parser = match "deployed" Deployed
         <|> match "failed"   Failed
         <|> match "pending"  Pending

instance ToText ConfigurationDeploymentStatus where
    toText = \case
        Deployed -> "deployed"
        Failed   -> "failed"
        Pending  -> "pending"

instance FromXML ConfigurationDeploymentStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ConfigurationDeploymentStatus"

instance ToQuery ConfigurationDeploymentStatus

data ConfigurationOptionSetting = ConfigurationOptionSetting
    { _cosNamespace  :: Maybe Text
    , _cosOptionName :: Maybe Text
    , _cosValue      :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ConfigurationOptionSetting' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cosNamespace' @::@ 'Maybe' 'Text'
--
-- * 'cosOptionName' @::@ 'Maybe' 'Text'
--
-- * 'cosValue' @::@ 'Maybe' 'Text'
--
configurationOptionSetting :: ConfigurationOptionSetting
configurationOptionSetting = ConfigurationOptionSetting
    { _cosNamespace  = Nothing
    , _cosOptionName = Nothing
    , _cosValue      = Nothing
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

instance ToQuery ConfigurationOptionSetting

data ConfigurationOptionValueType
    = List   -- ^ List
    | Scalar -- ^ Scalar
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable ConfigurationOptionValueType

instance FromText ConfigurationOptionValueType where
    parser = match "List"   List
         <|> match "Scalar" Scalar

instance ToText ConfigurationOptionValueType where
    toText = \case
        List   -> "List"
        Scalar -> "Scalar"

instance FromXML ConfigurationOptionValueType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ConfigurationOptionValueType"

instance ToQuery ConfigurationOptionValueType

data ConfigurationSettingsDescription = ConfigurationSettingsDescription
    { _csdApplicationName   :: Maybe Text
    , _csdDateCreated       :: Maybe RFC822
    , _csdDateUpdated       :: Maybe RFC822
    , _csdDeploymentStatus  :: Maybe Text
    , _csdDescription       :: Maybe Text
    , _csdEnvironmentName   :: Maybe Text
    , _csdOptionSettings    :: [ConfigurationOptionSetting]
    , _csdSolutionStackName :: Maybe Text
    , _csdTemplateName      :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ConfigurationSettingsDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csdApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'csdDateCreated' @::@ 'Maybe' 'UTCTime'
--
-- * 'csdDateUpdated' @::@ 'Maybe' 'UTCTime'
--
-- * 'csdDeploymentStatus' @::@ 'Maybe' 'Text'
--
-- * 'csdDescription' @::@ 'Maybe' 'Text'
--
-- * 'csdEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'csdOptionSettings' @::@ ['ConfigurationOptionSetting']
--
-- * 'csdSolutionStackName' @::@ 'Maybe' 'Text'
--
-- * 'csdTemplateName' @::@ 'Maybe' 'Text'
--
configurationSettingsDescription :: ConfigurationSettingsDescription
configurationSettingsDescription = ConfigurationSettingsDescription
    { _csdSolutionStackName = Nothing
    , _csdApplicationName   = Nothing
    , _csdTemplateName      = Nothing
    , _csdDescription       = Nothing
    , _csdEnvironmentName   = Nothing
    , _csdDeploymentStatus  = Nothing
    , _csdDateCreated       = Nothing
    , _csdDateUpdated       = Nothing
    , _csdOptionSettings    = mempty
    }

-- | The name of the application associated with this configuration set.
csdApplicationName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdApplicationName =
    lens _csdApplicationName (\s a -> s { _csdApplicationName = a })

-- | The date (in UTC time) when this configuration set was created.
csdDateCreated :: Lens' ConfigurationSettingsDescription (Maybe UTCTime)
csdDateCreated = lens _csdDateCreated (\s a -> s { _csdDateCreated = a })
    . mapping _Time

-- | The date (in UTC time) when this configuration set was last modified.
csdDateUpdated :: Lens' ConfigurationSettingsDescription (Maybe UTCTime)
csdDateUpdated = lens _csdDateUpdated (\s a -> s { _csdDateUpdated = a })
    . mapping _Time

-- | If this configuration set is associated with an environment, the
-- DeploymentStatus parameter indicates the deployment status of this
-- configuration set: null: This configuration is not associated with a
-- running environment. pending: This is a draft configuration that is not
-- deployed to the associated environment but is in the process of
-- deploying. deployed: This is the configuration that is currently deployed
-- to the associated running environment. failed: This is a draft
-- configuration, that failed to successfully deploy. null: This
-- configuration is not associated with a running environment. pending: This
-- is a draft configuration that is not deployed to the associated
-- environment but is in the process of deploying. deployed: This is the
-- configuration that is currently deployed to the associated running
-- environment. failed: This is a draft configuration that failed to
-- successfully deploy.
csdDeploymentStatus :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdDeploymentStatus =
    lens _csdDeploymentStatus (\s a -> s { _csdDeploymentStatus = a })

-- | Describes this configuration set.
csdDescription :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdDescription = lens _csdDescription (\s a -> s { _csdDescription = a })

-- | If not null, the name of the environment for this configuration set.
csdEnvironmentName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdEnvironmentName =
    lens _csdEnvironmentName (\s a -> s { _csdEnvironmentName = a })

-- | A list of the configuration options and their values in this
-- configuration set.
csdOptionSettings :: Lens' ConfigurationSettingsDescription [ConfigurationOptionSetting]
csdOptionSettings =
    lens _csdOptionSettings (\s a -> s { _csdOptionSettings = a })

-- | The name of the solution stack this configuration set uses.
csdSolutionStackName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdSolutionStackName =
    lens _csdSolutionStackName (\s a -> s { _csdSolutionStackName = a })

-- | If not null, the name of the configuration template for this
-- configuration set.
csdTemplateName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdTemplateName = lens _csdTemplateName (\s a -> s { _csdTemplateName = a })

instance FromXML ConfigurationSettingsDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ConfigurationSettingsDescription"

instance ToQuery ConfigurationSettingsDescription

data ApplicationVersionDescription = ApplicationVersionDescription
    { _avdApplicationName :: Maybe Text
    , _avdDateCreated     :: Maybe RFC822
    , _avdDateUpdated     :: Maybe RFC822
    , _avdDescription     :: Maybe Text
    , _avdSourceBundle    :: Maybe S3Location
    , _avdVersionLabel    :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ApplicationVersionDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avdApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'avdDateCreated' @::@ 'Maybe' 'UTCTime'
--
-- * 'avdDateUpdated' @::@ 'Maybe' 'UTCTime'
--
-- * 'avdDescription' @::@ 'Maybe' 'Text'
--
-- * 'avdSourceBundle' @::@ 'Maybe' 'S3Location'
--
-- * 'avdVersionLabel' @::@ 'Maybe' 'Text'
--
applicationVersionDescription :: ApplicationVersionDescription
applicationVersionDescription = ApplicationVersionDescription
    { _avdApplicationName = Nothing
    , _avdDescription     = Nothing
    , _avdVersionLabel    = Nothing
    , _avdSourceBundle    = Nothing
    , _avdDateCreated     = Nothing
    , _avdDateUpdated     = Nothing
    }

-- | The name of the application associated with this release.
avdApplicationName :: Lens' ApplicationVersionDescription (Maybe Text)
avdApplicationName =
    lens _avdApplicationName (\s a -> s { _avdApplicationName = a })

-- | The creation date of the application version.
avdDateCreated :: Lens' ApplicationVersionDescription (Maybe UTCTime)
avdDateCreated = lens _avdDateCreated (\s a -> s { _avdDateCreated = a })
    . mapping _Time

-- | The last modified date of the application version.
avdDateUpdated :: Lens' ApplicationVersionDescription (Maybe UTCTime)
avdDateUpdated = lens _avdDateUpdated (\s a -> s { _avdDateUpdated = a })
    . mapping _Time

-- | The description of this application version.
avdDescription :: Lens' ApplicationVersionDescription (Maybe Text)
avdDescription = lens _avdDescription (\s a -> s { _avdDescription = a })

-- | The location where the source bundle is located for this version.
avdSourceBundle :: Lens' ApplicationVersionDescription (Maybe S3Location)
avdSourceBundle = lens _avdSourceBundle (\s a -> s { _avdSourceBundle = a })

-- | A label uniquely identifying the version for the associated application.
avdVersionLabel :: Lens' ApplicationVersionDescription (Maybe Text)
avdVersionLabel = lens _avdVersionLabel (\s a -> s { _avdVersionLabel = a })

instance FromXML ApplicationVersionDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ApplicationVersionDescription"

instance ToQuery ApplicationVersionDescription

data OptionSpecification = OptionSpecification
    { _osNamespace  :: Maybe Text
    , _osOptionName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'OptionSpecification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'osNamespace' @::@ 'Maybe' 'Text'
--
-- * 'osOptionName' @::@ 'Maybe' 'Text'
--
optionSpecification :: OptionSpecification
optionSpecification = OptionSpecification
    { _osNamespace  = Nothing
    , _osOptionName = Nothing
    }

-- | A unique namespace identifying the option's associated AWS resource.
osNamespace :: Lens' OptionSpecification (Maybe Text)
osNamespace = lens _osNamespace (\s a -> s { _osNamespace = a })

-- | The name of the configuration option.
osOptionName :: Lens' OptionSpecification (Maybe Text)
osOptionName = lens _osOptionName (\s a -> s { _osOptionName = a })

instance FromXML OptionSpecification where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionSpecification"

instance ToQuery OptionSpecification

data EnvironmentResourceDescription = EnvironmentResourceDescription
    { _erdAutoScalingGroups    :: [AutoScalingGroup]
    , _erdEnvironmentName      :: Maybe Text
    , _erdInstances            :: [Instance]
    , _erdLaunchConfigurations :: [LaunchConfiguration]
    , _erdLoadBalancers        :: [LoadBalancer]
    , _erdQueues               :: [Queue]
    , _erdTriggers             :: [Trigger]
    } deriving (Eq, Show, Generic)

-- | 'EnvironmentResourceDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'erdAutoScalingGroups' @::@ ['AutoScalingGroup']
--
-- * 'erdEnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'erdInstances' @::@ ['Instance']
--
-- * 'erdLaunchConfigurations' @::@ ['LaunchConfiguration']
--
-- * 'erdLoadBalancers' @::@ ['LoadBalancer']
--
-- * 'erdQueues' @::@ ['Queue']
--
-- * 'erdTriggers' @::@ ['Trigger']
--
environmentResourceDescription :: EnvironmentResourceDescription
environmentResourceDescription = EnvironmentResourceDescription
    { _erdEnvironmentName      = Nothing
    , _erdAutoScalingGroups    = mempty
    , _erdInstances            = mempty
    , _erdLaunchConfigurations = mempty
    , _erdLoadBalancers        = mempty
    , _erdTriggers             = mempty
    , _erdQueues               = mempty
    }

-- | The AutoScalingGroups used by this environment.
erdAutoScalingGroups :: Lens' EnvironmentResourceDescription [AutoScalingGroup]
erdAutoScalingGroups =
    lens _erdAutoScalingGroups (\s a -> s { _erdAutoScalingGroups = a })

-- | The name of the environment.
erdEnvironmentName :: Lens' EnvironmentResourceDescription (Maybe Text)
erdEnvironmentName =
    lens _erdEnvironmentName (\s a -> s { _erdEnvironmentName = a })

-- | The Amazon EC2 instances used by this environment.
erdInstances :: Lens' EnvironmentResourceDescription [Instance]
erdInstances = lens _erdInstances (\s a -> s { _erdInstances = a })

-- | The Auto Scaling launch configurations in use by this environment.
erdLaunchConfigurations :: Lens' EnvironmentResourceDescription [LaunchConfiguration]
erdLaunchConfigurations =
    lens _erdLaunchConfigurations (\s a -> s { _erdLaunchConfigurations = a })

-- | The LoadBalancers in use by this environment.
erdLoadBalancers :: Lens' EnvironmentResourceDescription [LoadBalancer]
erdLoadBalancers = lens _erdLoadBalancers (\s a -> s { _erdLoadBalancers = a })

-- | The queues used by this environment.
erdQueues :: Lens' EnvironmentResourceDescription [Queue]
erdQueues = lens _erdQueues (\s a -> s { _erdQueues = a })

-- | The AutoScaling triggers in use by this environment.
erdTriggers :: Lens' EnvironmentResourceDescription [Trigger]
erdTriggers = lens _erdTriggers (\s a -> s { _erdTriggers = a })

instance FromXML EnvironmentResourceDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentResourceDescription"

instance ToQuery EnvironmentResourceDescription

data Queue = Queue
    { _qName :: Maybe Text
    , _qURL  :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Queue' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'qName' @::@ 'Maybe' 'Text'
--
-- * 'qURL' @::@ 'Maybe' 'Text'
--
queue :: Queue
queue = Queue
    { _qName = Nothing
    , _qURL  = Nothing
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

instance ToQuery Queue

data EnvironmentStatus
    = Launching   -- ^ Launching
    | Ready       -- ^ Ready
    | Terminated  -- ^ Terminated
    | Terminating -- ^ Terminating
    | Updating    -- ^ Updating
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable EnvironmentStatus

instance FromText EnvironmentStatus where
    parser = match "Launching"   Launching
         <|> match "Ready"       Ready
         <|> match "Terminated"  Terminated
         <|> match "Terminating" Terminating
         <|> match "Updating"    Updating

instance ToText EnvironmentStatus where
    toText = \case
        Launching   -> "Launching"
        Ready       -> "Ready"
        Terminated  -> "Terminated"
        Terminating -> "Terminating"
        Updating    -> "Updating"

instance FromXML EnvironmentStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentStatus"

instance ToQuery EnvironmentStatus

data LoadBalancerDescription = LoadBalancerDescription
    { _lbdDomain           :: Maybe Text
    , _lbdListeners        :: [Listener]
    , _lbdLoadBalancerName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'LoadBalancerDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbdDomain' @::@ 'Maybe' 'Text'
--
-- * 'lbdListeners' @::@ ['Listener']
--
-- * 'lbdLoadBalancerName' @::@ 'Maybe' 'Text'
--
loadBalancerDescription :: LoadBalancerDescription
loadBalancerDescription = LoadBalancerDescription
    { _lbdLoadBalancerName = Nothing
    , _lbdDomain           = Nothing
    , _lbdListeners        = mempty
    }

-- | The domain name of the LoadBalancer.
lbdDomain :: Lens' LoadBalancerDescription (Maybe Text)
lbdDomain = lens _lbdDomain (\s a -> s { _lbdDomain = a })

-- | A list of Listeners used by the LoadBalancer.
lbdListeners :: Lens' LoadBalancerDescription [Listener]
lbdListeners = lens _lbdListeners (\s a -> s { _lbdListeners = a })

-- | The name of the LoadBalancer.
lbdLoadBalancerName :: Lens' LoadBalancerDescription (Maybe Text)
lbdLoadBalancerName =
    lens _lbdLoadBalancerName (\s a -> s { _lbdLoadBalancerName = a })

instance FromXML LoadBalancerDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "LoadBalancerDescription"

instance ToQuery LoadBalancerDescription

newtype ApplicationDescriptionMessage = ApplicationDescriptionMessage
    { _admApplication :: Maybe ApplicationDescription
    } deriving (Eq, Show, Generic)

-- | 'ApplicationDescriptionMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'admApplication' @::@ 'Maybe' 'ApplicationDescription'
--
applicationDescriptionMessage :: ApplicationDescriptionMessage
applicationDescriptionMessage = ApplicationDescriptionMessage
    { _admApplication = Nothing
    }

-- | The ApplicationDescription of the application.
admApplication :: Lens' ApplicationDescriptionMessage (Maybe ApplicationDescription)
admApplication = lens _admApplication (\s a -> s { _admApplication = a })

instance FromXML ApplicationDescriptionMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ApplicationDescriptionMessage"

instance ToQuery ApplicationDescriptionMessage

data EnvironmentTier = EnvironmentTier
    { _etName    :: Maybe Text
    , _etType    :: Maybe Text
    , _etVersion :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'EnvironmentTier' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'etName' @::@ 'Maybe' 'Text'
--
-- * 'etType' @::@ 'Maybe' 'Text'
--
-- * 'etVersion' @::@ 'Maybe' 'Text'
--
environmentTier :: EnvironmentTier
environmentTier = EnvironmentTier
    { _etName    = Nothing
    , _etType    = Nothing
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

instance ToQuery EnvironmentTier

newtype LoadBalancer = LoadBalancer
    { _lbName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'LoadBalancer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbName' @::@ 'Maybe' 'Text'
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

instance ToQuery LoadBalancer

newtype EnvironmentResourcesDescription = EnvironmentResourcesDescription
    { _erdLoadBalancer :: Maybe LoadBalancerDescription
    } deriving (Eq, Show, Generic)

-- | 'EnvironmentResourcesDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'erdLoadBalancer' @::@ 'Maybe' 'LoadBalancerDescription'
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

instance ToQuery EnvironmentResourcesDescription

data OptionRestrictionRegex = OptionRestrictionRegex
    { _orrLabel   :: Maybe Text
    , _orrPattern :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'OptionRestrictionRegex' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'orrLabel' @::@ 'Maybe' 'Text'
--
-- * 'orrPattern' @::@ 'Maybe' 'Text'
--
optionRestrictionRegex :: OptionRestrictionRegex
optionRestrictionRegex = OptionRestrictionRegex
    { _orrPattern = Nothing
    , _orrLabel   = Nothing
    }

-- | A unique name representing this regular expression.
orrLabel :: Lens' OptionRestrictionRegex (Maybe Text)
orrLabel = lens _orrLabel (\s a -> s { _orrLabel = a })

-- | The regular expression pattern that a string configuration option value
-- with this restriction must match.
orrPattern :: Lens' OptionRestrictionRegex (Maybe Text)
orrPattern = lens _orrPattern (\s a -> s { _orrPattern = a })

instance FromXML OptionRestrictionRegex where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionRestrictionRegex"

instance ToQuery OptionRestrictionRegex

data ConfigurationOptionDescription = ConfigurationOptionDescription
    { _codChangeSeverity :: Maybe Text
    , _codDefaultValue   :: Maybe Text
    , _codMaxLength      :: Maybe Int
    , _codMaxValue       :: Maybe Int
    , _codMinValue       :: Maybe Int
    , _codName           :: Maybe Text
    , _codNamespace      :: Maybe Text
    , _codRegex          :: Maybe OptionRestrictionRegex
    , _codUserDefined    :: Maybe Bool
    , _codValueOptions   :: [Text]
    , _codValueType      :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ConfigurationOptionDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'codChangeSeverity' @::@ 'Maybe' 'Text'
--
-- * 'codDefaultValue' @::@ 'Maybe' 'Text'
--
-- * 'codMaxLength' @::@ 'Maybe' 'Int'
--
-- * 'codMaxValue' @::@ 'Maybe' 'Int'
--
-- * 'codMinValue' @::@ 'Maybe' 'Int'
--
-- * 'codName' @::@ 'Maybe' 'Text'
--
-- * 'codNamespace' @::@ 'Maybe' 'Text'
--
-- * 'codRegex' @::@ 'Maybe' 'OptionRestrictionRegex'
--
-- * 'codUserDefined' @::@ 'Maybe' 'Bool'
--
-- * 'codValueOptions' @::@ ['Text']
--
-- * 'codValueType' @::@ 'Maybe' 'Text'
--
configurationOptionDescription :: ConfigurationOptionDescription
configurationOptionDescription = ConfigurationOptionDescription
    { _codNamespace      = Nothing
    , _codName           = Nothing
    , _codDefaultValue   = Nothing
    , _codChangeSeverity = Nothing
    , _codUserDefined    = Nothing
    , _codValueType      = Nothing
    , _codValueOptions   = mempty
    , _codMinValue       = Nothing
    , _codMaxValue       = Nothing
    , _codMaxLength      = Nothing
    , _codRegex          = Nothing
    }

-- | An indication of which action is required if the value for this
-- configuration option changes: NoInterruption - There is no interruption
-- to the environment or application availability. RestartEnvironment - The
-- environment is restarted, all AWS resources are deleted and recreated,
-- and the environment is unavailable during the process.
-- RestartApplicationServer - The environment is available the entire time.
-- However, a short application outage occurs when the application servers
-- on the running Amazon EC2 instances are restarted. NoInterruption : There
-- is no interruption to the environment or application availability.
-- RestartEnvironment : The environment is entirely restarted, all AWS
-- resources are deleted and recreated, and the environment is unavailable
-- during the process. RestartApplicationServer : The environment is
-- available the entire time. However, a short application outage occurs
-- when the application servers on the running Amazon EC2 instances are
-- restarted.
codChangeSeverity :: Lens' ConfigurationOptionDescription (Maybe Text)
codChangeSeverity =
    lens _codChangeSeverity (\s a -> s { _codChangeSeverity = a })

-- | The default value for this configuration option.
codDefaultValue :: Lens' ConfigurationOptionDescription (Maybe Text)
codDefaultValue = lens _codDefaultValue (\s a -> s { _codDefaultValue = a })

-- | If specified, the configuration option must be a string value no longer
-- than this value.
codMaxLength :: Lens' ConfigurationOptionDescription (Maybe Int)
codMaxLength = lens _codMaxLength (\s a -> s { _codMaxLength = a })

-- | If specified, the configuration option must be a numeric value less than
-- this value.
codMaxValue :: Lens' ConfigurationOptionDescription (Maybe Int)
codMaxValue = lens _codMaxValue (\s a -> s { _codMaxValue = a })

-- | If specified, the configuration option must be a numeric value greater
-- than this value.
codMinValue :: Lens' ConfigurationOptionDescription (Maybe Int)
codMinValue = lens _codMinValue (\s a -> s { _codMinValue = a })

-- | The name of the configuration option.
codName :: Lens' ConfigurationOptionDescription (Maybe Text)
codName = lens _codName (\s a -> s { _codName = a })

-- | A unique namespace identifying the option's associated AWS resource.
codNamespace :: Lens' ConfigurationOptionDescription (Maybe Text)
codNamespace = lens _codNamespace (\s a -> s { _codNamespace = a })

-- | If specified, the configuration option must be a string value that
-- satisfies this regular expression.
codRegex :: Lens' ConfigurationOptionDescription (Maybe OptionRestrictionRegex)
codRegex = lens _codRegex (\s a -> s { _codRegex = a })

-- | An indication of whether the user defined this configuration option: true
-- : This configuration option was defined by the user. It is a valid choice
-- for specifying this as an Option to Remove when updating configuration
-- settings. false : This configuration was not defined by the user. true :
-- This configuration option was defined by the user. It is a valid choice
-- for specifying if this as an Option to Remove when updating configuration
-- settings. false : This configuration was not defined by the user.
-- Constraint: You can remove only UserDefined options from a configuration.
-- Valid Values: true | false.
codUserDefined :: Lens' ConfigurationOptionDescription (Maybe Bool)
codUserDefined = lens _codUserDefined (\s a -> s { _codUserDefined = a })

-- | If specified, values for the configuration option are selected from this
-- list.
codValueOptions :: Lens' ConfigurationOptionDescription [Text]
codValueOptions = lens _codValueOptions (\s a -> s { _codValueOptions = a })

-- | An indication of which type of values this option has and whether it is
-- allowable to select one or more than one of the possible values: Scalar :
-- Values for this option are a single selection from the possible values,
-- or a unformatted string or numeric value governed by the MIN/MAX/Regex
-- constraints: List : Values for this option are multiple selections of the
-- possible values. Boolean : Values for this option are either true or
-- false . Scalar : Values for this option are a single selection from the
-- possible values, or an unformatted string, or numeric value governed by
-- the MIN/MAX/Regex constraints. List : Values for this option are multiple
-- selections from the possible values. Boolean : Values for this option are
-- either true or false .
codValueType :: Lens' ConfigurationOptionDescription (Maybe Text)
codValueType = lens _codValueType (\s a -> s { _codValueType = a })

instance FromXML ConfigurationOptionDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ConfigurationOptionDescription"

instance ToQuery ConfigurationOptionDescription

data SourceConfiguration = SourceConfiguration
    { _scApplicationName :: Maybe Text
    , _scTemplateName    :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SourceConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'scTemplateName' @::@ 'Maybe' 'Text'
--
sourceConfiguration :: SourceConfiguration
sourceConfiguration = SourceConfiguration
    { _scApplicationName = Nothing
    , _scTemplateName    = Nothing
    }

-- | The name of the application associated with the configuration.
scApplicationName :: Lens' SourceConfiguration (Maybe Text)
scApplicationName =
    lens _scApplicationName (\s a -> s { _scApplicationName = a })

-- | The name of the configuration template.
scTemplateName :: Lens' SourceConfiguration (Maybe Text)
scTemplateName = lens _scTemplateName (\s a -> s { _scTemplateName = a })

instance FromXML SourceConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SourceConfiguration"

instance ToQuery SourceConfiguration

data EnvironmentInfoDescription = EnvironmentInfoDescription
    { _eidEc2InstanceId   :: Maybe Text
    , _eidInfoType        :: Maybe Text
    , _eidMessage         :: Maybe Text
    , _eidSampleTimestamp :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'EnvironmentInfoDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eidEc2InstanceId' @::@ 'Maybe' 'Text'
--
-- * 'eidInfoType' @::@ 'Maybe' 'Text'
--
-- * 'eidMessage' @::@ 'Maybe' 'Text'
--
-- * 'eidSampleTimestamp' @::@ 'Maybe' 'UTCTime'
--
environmentInfoDescription :: EnvironmentInfoDescription
environmentInfoDescription = EnvironmentInfoDescription
    { _eidInfoType        = Nothing
    , _eidEc2InstanceId   = Nothing
    , _eidSampleTimestamp = Nothing
    , _eidMessage         = Nothing
    }

-- | The Amazon EC2 Instance ID for this information.
eidEc2InstanceId :: Lens' EnvironmentInfoDescription (Maybe Text)
eidEc2InstanceId = lens _eidEc2InstanceId (\s a -> s { _eidEc2InstanceId = a })

-- | The type of information retrieved.
eidInfoType :: Lens' EnvironmentInfoDescription (Maybe Text)
eidInfoType = lens _eidInfoType (\s a -> s { _eidInfoType = a })

-- | The retrieved information.
eidMessage :: Lens' EnvironmentInfoDescription (Maybe Text)
eidMessage = lens _eidMessage (\s a -> s { _eidMessage = a })

-- | The time stamp when this information was retrieved.
eidSampleTimestamp :: Lens' EnvironmentInfoDescription (Maybe UTCTime)
eidSampleTimestamp =
    lens _eidSampleTimestamp (\s a -> s { _eidSampleTimestamp = a })
        . mapping _Time

instance FromXML EnvironmentInfoDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentInfoDescription"

instance ToQuery EnvironmentInfoDescription

data S3Location = S3Location
    { _slS3Bucket :: Maybe Text
    , _slS3Key    :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'S3Location' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slS3Bucket' @::@ 'Maybe' 'Text'
--
-- * 'slS3Key' @::@ 'Maybe' 'Text'
--
s3Location :: S3Location
s3Location = S3Location
    { _slS3Bucket = Nothing
    , _slS3Key    = Nothing
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

instance ToQuery S3Location

data ValidationMessage = ValidationMessage
    { _vmMessage    :: Maybe Text
    , _vmNamespace  :: Maybe Text
    , _vmOptionName :: Maybe Text
    , _vmSeverity   :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ValidationMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vmMessage' @::@ 'Maybe' 'Text'
--
-- * 'vmNamespace' @::@ 'Maybe' 'Text'
--
-- * 'vmOptionName' @::@ 'Maybe' 'Text'
--
-- * 'vmSeverity' @::@ 'Maybe' 'Text'
--
validationMessage :: ValidationMessage
validationMessage = ValidationMessage
    { _vmMessage    = Nothing
    , _vmSeverity   = Nothing
    , _vmNamespace  = Nothing
    , _vmOptionName = Nothing
    }

-- | A message describing the error or warning.
vmMessage :: Lens' ValidationMessage (Maybe Text)
vmMessage = lens _vmMessage (\s a -> s { _vmMessage = a })

-- | 
vmNamespace :: Lens' ValidationMessage (Maybe Text)
vmNamespace = lens _vmNamespace (\s a -> s { _vmNamespace = a })

-- | 
vmOptionName :: Lens' ValidationMessage (Maybe Text)
vmOptionName = lens _vmOptionName (\s a -> s { _vmOptionName = a })

-- | An indication of the severity of this message: error: This message
-- indicates that this is not a valid setting for an option. warning: This
-- message is providing information you should take into account. error:
-- This message indicates that this is not a valid setting for an option.
-- warning: This message is providing information you should take into
-- account.
vmSeverity :: Lens' ValidationMessage (Maybe Text)
vmSeverity = lens _vmSeverity (\s a -> s { _vmSeverity = a })

instance FromXML ValidationMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ValidationMessage"

instance ToQuery ValidationMessage

data ValidationSeverity
    = VSError   -- ^ error
    | VSWarning -- ^ warning
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable ValidationSeverity

instance FromText ValidationSeverity where
    parser = match "error"   VSError
         <|> match "warning" VSWarning

instance ToText ValidationSeverity where
    toText = \case
        VSError   -> "error"
        VSWarning -> "warning"

instance FromXML ValidationSeverity where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ValidationSeverity"

instance ToQuery ValidationSeverity

newtype Trigger = Trigger
    { _tName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Trigger' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tName' @::@ 'Maybe' 'Text'
--
trigger :: Trigger
trigger = Trigger
    { _tName = Nothing
    }

-- | The name of the trigger.
tName :: Lens' Trigger (Maybe Text)
tName = lens _tName (\s a -> s { _tName = a })

instance FromXML Trigger where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Trigger"

instance ToQuery Trigger

data EnvironmentInfoType
    = Tail -- ^ tail
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable EnvironmentInfoType

instance FromText EnvironmentInfoType where
    parser = match "tail" Tail

instance ToText EnvironmentInfoType where
    toText Tail = "tail"

instance FromXML EnvironmentInfoType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentInfoType"

instance ToQuery EnvironmentInfoType

data EnvironmentDescription = EnvironmentDescription
    { _ed1ApplicationName   :: Maybe Text
    , _ed1CNAME             :: Maybe Text
    , _ed1DateCreated       :: Maybe RFC822
    , _ed1DateUpdated       :: Maybe RFC822
    , _ed1Description       :: Maybe Text
    , _ed1EndpointURL       :: Maybe Text
    , _ed1EnvironmentId     :: Maybe Text
    , _ed1EnvironmentName   :: Maybe Text
    , _ed1Health            :: Maybe Text
    , _ed1Resources         :: Maybe EnvironmentResourcesDescription
    , _ed1SolutionStackName :: Maybe Text
    , _ed1Status            :: Maybe Text
    , _ed1TemplateName      :: Maybe Text
    , _ed1Tier              :: Maybe EnvironmentTier
    , _ed1VersionLabel      :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'EnvironmentDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ed1ApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'ed1CNAME' @::@ 'Maybe' 'Text'
--
-- * 'ed1DateCreated' @::@ 'Maybe' 'UTCTime'
--
-- * 'ed1DateUpdated' @::@ 'Maybe' 'UTCTime'
--
-- * 'ed1Description' @::@ 'Maybe' 'Text'
--
-- * 'ed1EndpointURL' @::@ 'Maybe' 'Text'
--
-- * 'ed1EnvironmentId' @::@ 'Maybe' 'Text'
--
-- * 'ed1EnvironmentName' @::@ 'Maybe' 'Text'
--
-- * 'ed1Health' @::@ 'Maybe' 'Text'
--
-- * 'ed1Resources' @::@ 'Maybe' 'EnvironmentResourcesDescription'
--
-- * 'ed1SolutionStackName' @::@ 'Maybe' 'Text'
--
-- * 'ed1Status' @::@ 'Maybe' 'Text'
--
-- * 'ed1TemplateName' @::@ 'Maybe' 'Text'
--
-- * 'ed1Tier' @::@ 'Maybe' 'EnvironmentTier'
--
-- * 'ed1VersionLabel' @::@ 'Maybe' 'Text'
--
environmentDescription :: EnvironmentDescription
environmentDescription = EnvironmentDescription
    { _ed1EnvironmentName   = Nothing
    , _ed1EnvironmentId     = Nothing
    , _ed1ApplicationName   = Nothing
    , _ed1VersionLabel      = Nothing
    , _ed1SolutionStackName = Nothing
    , _ed1TemplateName      = Nothing
    , _ed1Description       = Nothing
    , _ed1EndpointURL       = Nothing
    , _ed1CNAME             = Nothing
    , _ed1DateCreated       = Nothing
    , _ed1DateUpdated       = Nothing
    , _ed1Status            = Nothing
    , _ed1Health            = Nothing
    , _ed1Resources         = Nothing
    , _ed1Tier              = Nothing
    }

-- | The name of the application associated with this environment.
ed1ApplicationName :: Lens' EnvironmentDescription (Maybe Text)
ed1ApplicationName =
    lens _ed1ApplicationName (\s a -> s { _ed1ApplicationName = a })

-- | The URL to the CNAME for this environment.
ed1CNAME :: Lens' EnvironmentDescription (Maybe Text)
ed1CNAME = lens _ed1CNAME (\s a -> s { _ed1CNAME = a })

-- | The creation date for this environment.
ed1DateCreated :: Lens' EnvironmentDescription (Maybe UTCTime)
ed1DateCreated = lens _ed1DateCreated (\s a -> s { _ed1DateCreated = a })
    . mapping _Time

-- | The last modified date for this environment.
ed1DateUpdated :: Lens' EnvironmentDescription (Maybe UTCTime)
ed1DateUpdated = lens _ed1DateUpdated (\s a -> s { _ed1DateUpdated = a })
    . mapping _Time

-- | Describes this environment.
ed1Description :: Lens' EnvironmentDescription (Maybe Text)
ed1Description = lens _ed1Description (\s a -> s { _ed1Description = a })

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer.
-- For single-instance environments, the IP address of the instance.
ed1EndpointURL :: Lens' EnvironmentDescription (Maybe Text)
ed1EndpointURL = lens _ed1EndpointURL (\s a -> s { _ed1EndpointURL = a })

-- | The ID of this environment.
ed1EnvironmentId :: Lens' EnvironmentDescription (Maybe Text)
ed1EnvironmentId = lens _ed1EnvironmentId (\s a -> s { _ed1EnvironmentId = a })

-- | The name of this environment.
ed1EnvironmentName :: Lens' EnvironmentDescription (Maybe Text)
ed1EnvironmentName =
    lens _ed1EnvironmentName (\s a -> s { _ed1EnvironmentName = a })

-- | Describes the health status of the environment. AWS Elastic Beanstalk
-- indicates the failure levels for a running environment: Red : Indicates
-- the environment is not working. Yellow: Indicates that something is
-- wrong, the application might not be available, but the instances appear
-- running. Green: Indicates the environment is healthy and fully
-- functional. Red: Indicates the environment is not responsive. Occurs when
-- three or more consecutive failures occur for an environment. Yellow:
-- Indicates that something is wrong. Occurs when two consecutive failures
-- occur for an environment. Green: Indicates the environment is healthy and
-- fully functional. Grey: Default health for a new environment. The
-- environment is not fully launched and health checks have not started or
-- health checks are suspended during an UpdateEnvironment or
-- RestartEnvironement request. Default: Grey.
ed1Health :: Lens' EnvironmentDescription (Maybe Text)
ed1Health = lens _ed1Health (\s a -> s { _ed1Health = a })

-- | The description of the AWS resources used by this environment.
ed1Resources :: Lens' EnvironmentDescription (Maybe EnvironmentResourcesDescription)
ed1Resources = lens _ed1Resources (\s a -> s { _ed1Resources = a })

-- | The name of the SolutionStack deployed with this environment.
ed1SolutionStackName :: Lens' EnvironmentDescription (Maybe Text)
ed1SolutionStackName =
    lens _ed1SolutionStackName (\s a -> s { _ed1SolutionStackName = a })

-- | The current operational status of the environment: Launching: Environment
-- is in the process of initial deployment. Updating: Environment is in the
-- process of updating its configuration settings or application version.
-- Ready: Environment is available to have an action performed on it, such
-- as update or terminate. Terminating: Environment is in the shut-down
-- process. Terminated: Environment is not running.
ed1Status :: Lens' EnvironmentDescription (Maybe Text)
ed1Status = lens _ed1Status (\s a -> s { _ed1Status = a })

-- | The name of the configuration template used to originally launch this
-- environment.
ed1TemplateName :: Lens' EnvironmentDescription (Maybe Text)
ed1TemplateName = lens _ed1TemplateName (\s a -> s { _ed1TemplateName = a })

-- | Describes the current tier of this environment.
ed1Tier :: Lens' EnvironmentDescription (Maybe EnvironmentTier)
ed1Tier = lens _ed1Tier (\s a -> s { _ed1Tier = a })

-- | The application version deployed in this environment.
ed1VersionLabel :: Lens' EnvironmentDescription (Maybe Text)
ed1VersionLabel = lens _ed1VersionLabel (\s a -> s { _ed1VersionLabel = a })

instance FromXML EnvironmentDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentDescription"

instance ToQuery EnvironmentDescription

data Listener = Listener
    { _lPort     :: Maybe Int
    , _lProtocol :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Listener' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lPort' @::@ 'Maybe' 'Int'
--
-- * 'lProtocol' @::@ 'Maybe' 'Text'
--
listener :: Listener
listener = Listener
    { _lProtocol = Nothing
    , _lPort     = Nothing
    }

-- | The port that is used by the Listener.
lPort :: Lens' Listener (Maybe Int)
lPort = lens _lPort (\s a -> s { _lPort = a })

-- | The protocol that is used by the Listener.
lProtocol :: Lens' Listener (Maybe Text)
lProtocol = lens _lProtocol (\s a -> s { _lProtocol = a })

instance FromXML Listener where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Listener"

instance ToQuery Listener

data EnvironmentHealth
    = Green  -- ^ Green
    | Grey   -- ^ Grey
    | Red    -- ^ Red
    | Yellow -- ^ Yellow
      deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable EnvironmentHealth

instance FromText EnvironmentHealth where
    parser = match "Green"  Green
         <|> match "Grey"   Grey
         <|> match "Red"    Red
         <|> match "Yellow" Yellow

instance ToText EnvironmentHealth where
    toText = \case
        Green  -> "Green"
        Grey   -> "Grey"
        Red    -> "Red"
        Yellow -> "Yellow"

instance FromXML EnvironmentHealth where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnvironmentHealth"

instance ToQuery EnvironmentHealth

newtype Instance = Instance
    { _iId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Instance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iId' @::@ 'Maybe' 'Text'
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

instance ToQuery Instance

data SolutionStackDescription = SolutionStackDescription
    { _ssdPermittedFileTypes :: [Text]
    , _ssdSolutionStackName  :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SolutionStackDescription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssdPermittedFileTypes' @::@ ['Text']
--
-- * 'ssdSolutionStackName' @::@ 'Maybe' 'Text'
--
solutionStackDescription :: SolutionStackDescription
solutionStackDescription = SolutionStackDescription
    { _ssdSolutionStackName  = Nothing
    , _ssdPermittedFileTypes = mempty
    }

-- | The permitted file types allowed for a solution stack.
ssdPermittedFileTypes :: Lens' SolutionStackDescription [Text]
ssdPermittedFileTypes =
    lens _ssdPermittedFileTypes (\s a -> s { _ssdPermittedFileTypes = a })

-- | The name of the solution stack.
ssdSolutionStackName :: Lens' SolutionStackDescription (Maybe Text)
ssdSolutionStackName =
    lens _ssdSolutionStackName (\s a -> s { _ssdSolutionStackName = a })

instance FromXML SolutionStackDescription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SolutionStackDescription"

instance ToQuery SolutionStackDescription
