{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.Product where

import           Network.AWS.ElasticBeanstalk.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | Describes the properties of an application.
--
-- /See:/ 'applicationDescription' smart constructor.
data ApplicationDescription = ApplicationDescription'
    { _adVersions               :: !(Maybe [Text])
    , _adDateUpdated            :: !(Maybe ISO8601)
    , _adDateCreated            :: !(Maybe ISO8601)
    , _adApplicationName        :: !(Maybe Text)
    , _adConfigurationTemplates :: !(Maybe [Text])
    , _adDescription            :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ApplicationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adVersions'
--
-- * 'adDateUpdated'
--
-- * 'adDateCreated'
--
-- * 'adApplicationName'
--
-- * 'adConfigurationTemplates'
--
-- * 'adDescription'
applicationDescription
    :: ApplicationDescription
applicationDescription =
    ApplicationDescription'
    { _adVersions = Nothing
    , _adDateUpdated = Nothing
    , _adDateCreated = Nothing
    , _adApplicationName = Nothing
    , _adConfigurationTemplates = Nothing
    , _adDescription = Nothing
    }

-- | The names of the versions for this application.
adVersions :: Lens' ApplicationDescription [Text]
adVersions = lens _adVersions (\ s a -> s{_adVersions = a}) . _Default . _Coerce;

-- | The date when the application was last modified.
adDateUpdated :: Lens' ApplicationDescription (Maybe UTCTime)
adDateUpdated = lens _adDateUpdated (\ s a -> s{_adDateUpdated = a}) . mapping _Time;

-- | The date when the application was created.
adDateCreated :: Lens' ApplicationDescription (Maybe UTCTime)
adDateCreated = lens _adDateCreated (\ s a -> s{_adDateCreated = a}) . mapping _Time;

-- | The name of the application.
adApplicationName :: Lens' ApplicationDescription (Maybe Text)
adApplicationName = lens _adApplicationName (\ s a -> s{_adApplicationName = a});

-- | The names of the configuration templates associated with this
-- application.
adConfigurationTemplates :: Lens' ApplicationDescription [Text]
adConfigurationTemplates = lens _adConfigurationTemplates (\ s a -> s{_adConfigurationTemplates = a}) . _Default . _Coerce;

-- | User-defined description of the application.
adDescription :: Lens' ApplicationDescription (Maybe Text)
adDescription = lens _adDescription (\ s a -> s{_adDescription = a});

instance FromXML ApplicationDescription where
        parseXML x
          = ApplicationDescription' <$>
              (x .@? "Versions" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "DateUpdated")
                <*> (x .@? "DateCreated")
                <*> (x .@? "ApplicationName")
                <*>
                (x .@? "ConfigurationTemplates" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "Description")

instance Hashable ApplicationDescription

-- | Result message containing a single description of an application.
--
-- /See:/ 'applicationDescriptionMessage' smart constructor.
newtype ApplicationDescriptionMessage = ApplicationDescriptionMessage'
    { _admApplication :: Maybe ApplicationDescription
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ApplicationDescriptionMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'admApplication'
applicationDescriptionMessage
    :: ApplicationDescriptionMessage
applicationDescriptionMessage =
    ApplicationDescriptionMessage'
    { _admApplication = Nothing
    }

-- | The < ApplicationDescription> of the application.
admApplication :: Lens' ApplicationDescriptionMessage (Maybe ApplicationDescription)
admApplication = lens _admApplication (\ s a -> s{_admApplication = a});

instance FromXML ApplicationDescriptionMessage where
        parseXML x
          = ApplicationDescriptionMessage' <$>
              (x .@? "Application")

instance Hashable ApplicationDescriptionMessage

-- | Represents the application metrics for a specified environment.
--
-- /See:/ 'applicationMetrics' smart constructor.
data ApplicationMetrics = ApplicationMetrics'
    { _amRequestCount :: !(Maybe Int)
    , _amLatency      :: !(Maybe Latency)
    , _amStatusCodes  :: !(Maybe StatusCodes)
    , _amDuration     :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ApplicationMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amRequestCount'
--
-- * 'amLatency'
--
-- * 'amStatusCodes'
--
-- * 'amDuration'
applicationMetrics
    :: ApplicationMetrics
applicationMetrics =
    ApplicationMetrics'
    { _amRequestCount = Nothing
    , _amLatency = Nothing
    , _amStatusCodes = Nothing
    , _amDuration = Nothing
    }

-- | Average number of requests handled by the web server per second over the
-- last 10 seconds.
amRequestCount :: Lens' ApplicationMetrics (Maybe Int)
amRequestCount = lens _amRequestCount (\ s a -> s{_amRequestCount = a});

-- | Represents the average latency for the slowest X percent of requests
-- over the last 10 seconds. Latencies are in seconds with one milisecond
-- resolution.
amLatency :: Lens' ApplicationMetrics (Maybe Latency)
amLatency = lens _amLatency (\ s a -> s{_amLatency = a});

-- | Represents the percentage of requests over the last 10 seconds that
-- resulted in each type of status code response.
amStatusCodes :: Lens' ApplicationMetrics (Maybe StatusCodes)
amStatusCodes = lens _amStatusCodes (\ s a -> s{_amStatusCodes = a});

-- | The amount of time that the metrics cover (usually 10 seconds). For
-- example, you might have 5 requests ('request_count') within the most
-- recent time slice of 10 seconds ('duration').
amDuration :: Lens' ApplicationMetrics (Maybe Int)
amDuration = lens _amDuration (\ s a -> s{_amDuration = a});

instance FromXML ApplicationMetrics where
        parseXML x
          = ApplicationMetrics' <$>
              (x .@? "RequestCount") <*> (x .@? "Latency") <*>
                (x .@? "StatusCodes")
                <*> (x .@? "Duration")

instance Hashable ApplicationMetrics

-- | Describes the properties of an application version.
--
-- /See:/ 'applicationVersionDescription' smart constructor.
data ApplicationVersionDescription = ApplicationVersionDescription'
    { _avdStatus          :: !(Maybe ApplicationVersionStatus)
    , _avdSourceBundle    :: !(Maybe S3Location)
    , _avdDateUpdated     :: !(Maybe ISO8601)
    , _avdDateCreated     :: !(Maybe ISO8601)
    , _avdVersionLabel    :: !(Maybe Text)
    , _avdApplicationName :: !(Maybe Text)
    , _avdDescription     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ApplicationVersionDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avdStatus'
--
-- * 'avdSourceBundle'
--
-- * 'avdDateUpdated'
--
-- * 'avdDateCreated'
--
-- * 'avdVersionLabel'
--
-- * 'avdApplicationName'
--
-- * 'avdDescription'
applicationVersionDescription
    :: ApplicationVersionDescription
applicationVersionDescription =
    ApplicationVersionDescription'
    { _avdStatus = Nothing
    , _avdSourceBundle = Nothing
    , _avdDateUpdated = Nothing
    , _avdDateCreated = Nothing
    , _avdVersionLabel = Nothing
    , _avdApplicationName = Nothing
    , _avdDescription = Nothing
    }

-- | The processing status of the application version.
avdStatus :: Lens' ApplicationVersionDescription (Maybe ApplicationVersionStatus)
avdStatus = lens _avdStatus (\ s a -> s{_avdStatus = a});

-- | The location where the source bundle is located for this version.
avdSourceBundle :: Lens' ApplicationVersionDescription (Maybe S3Location)
avdSourceBundle = lens _avdSourceBundle (\ s a -> s{_avdSourceBundle = a});

-- | The last modified date of the application version.
avdDateUpdated :: Lens' ApplicationVersionDescription (Maybe UTCTime)
avdDateUpdated = lens _avdDateUpdated (\ s a -> s{_avdDateUpdated = a}) . mapping _Time;

-- | The creation date of the application version.
avdDateCreated :: Lens' ApplicationVersionDescription (Maybe UTCTime)
avdDateCreated = lens _avdDateCreated (\ s a -> s{_avdDateCreated = a}) . mapping _Time;

-- | A label uniquely identifying the version for the associated application.
avdVersionLabel :: Lens' ApplicationVersionDescription (Maybe Text)
avdVersionLabel = lens _avdVersionLabel (\ s a -> s{_avdVersionLabel = a});

-- | The name of the application associated with this release.
avdApplicationName :: Lens' ApplicationVersionDescription (Maybe Text)
avdApplicationName = lens _avdApplicationName (\ s a -> s{_avdApplicationName = a});

-- | The description of this application version.
avdDescription :: Lens' ApplicationVersionDescription (Maybe Text)
avdDescription = lens _avdDescription (\ s a -> s{_avdDescription = a});

instance FromXML ApplicationVersionDescription where
        parseXML x
          = ApplicationVersionDescription' <$>
              (x .@? "Status") <*> (x .@? "SourceBundle") <*>
                (x .@? "DateUpdated")
                <*> (x .@? "DateCreated")
                <*> (x .@? "VersionLabel")
                <*> (x .@? "ApplicationName")
                <*> (x .@? "Description")

instance Hashable ApplicationVersionDescription

-- | Result message wrapping a single description of an application version.
--
-- /See:/ 'applicationVersionDescriptionMessage' smart constructor.
newtype ApplicationVersionDescriptionMessage = ApplicationVersionDescriptionMessage'
    { _avdmApplicationVersion :: Maybe ApplicationVersionDescription
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ApplicationVersionDescriptionMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avdmApplicationVersion'
applicationVersionDescriptionMessage
    :: ApplicationVersionDescriptionMessage
applicationVersionDescriptionMessage =
    ApplicationVersionDescriptionMessage'
    { _avdmApplicationVersion = Nothing
    }

-- | The < ApplicationVersionDescription> of the application version.
avdmApplicationVersion :: Lens' ApplicationVersionDescriptionMessage (Maybe ApplicationVersionDescription)
avdmApplicationVersion = lens _avdmApplicationVersion (\ s a -> s{_avdmApplicationVersion = a});

instance FromXML ApplicationVersionDescriptionMessage
         where
        parseXML x
          = ApplicationVersionDescriptionMessage' <$>
              (x .@? "ApplicationVersion")

instance Hashable
         ApplicationVersionDescriptionMessage

-- | Describes an Auto Scaling launch configuration.
--
-- /See:/ 'autoScalingGroup' smart constructor.
newtype AutoScalingGroup = AutoScalingGroup'
    { _asgName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AutoScalingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asgName'
autoScalingGroup
    :: AutoScalingGroup
autoScalingGroup =
    AutoScalingGroup'
    { _asgName = Nothing
    }

-- | The name of the 'AutoScalingGroup' .
asgName :: Lens' AutoScalingGroup (Maybe Text)
asgName = lens _asgName (\ s a -> s{_asgName = a});

instance FromXML AutoScalingGroup where
        parseXML x = AutoScalingGroup' <$> (x .@? "Name")

instance Hashable AutoScalingGroup

-- | Represents CPU utilization information from the specified instance that
-- belongs to the AWS Elastic Beanstalk environment. Use the 'instanceId'
-- property to specify the application instance for which you\'d like to
-- return data.
--
-- /See:/ 'cpuUtilization' smart constructor.
data CPUUtilization = CPUUtilization'
    { _cuSoftIRQ :: !(Maybe Double)
    , _cuIdle    :: !(Maybe Double)
    , _cuIRQ     :: !(Maybe Double)
    , _cuSystem  :: !(Maybe Double)
    , _cuUser    :: !(Maybe Double)
    , _cuIOWait  :: !(Maybe Double)
    , _cuNice    :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CPUUtilization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cuSoftIRQ'
--
-- * 'cuIdle'
--
-- * 'cuIRQ'
--
-- * 'cuSystem'
--
-- * 'cuUser'
--
-- * 'cuIOWait'
--
-- * 'cuNice'
cpuUtilization
    :: CPUUtilization
cpuUtilization =
    CPUUtilization'
    { _cuSoftIRQ = Nothing
    , _cuIdle = Nothing
    , _cuIRQ = Nothing
    , _cuSystem = Nothing
    , _cuUser = Nothing
    , _cuIOWait = Nothing
    , _cuNice = Nothing
    }

-- | Percentage of time that the CPU has spent in the 'SoftIRQ' state over
-- the last 10 seconds.
cuSoftIRQ :: Lens' CPUUtilization (Maybe Double)
cuSoftIRQ = lens _cuSoftIRQ (\ s a -> s{_cuSoftIRQ = a});

-- | Percentage of time that the CPU has spent in the 'Idle' state over the
-- last 10 seconds.
cuIdle :: Lens' CPUUtilization (Maybe Double)
cuIdle = lens _cuIdle (\ s a -> s{_cuIdle = a});

-- | Percentage of time that the CPU has spent in the 'IRQ' state over the
-- last 10 seconds.
cuIRQ :: Lens' CPUUtilization (Maybe Double)
cuIRQ = lens _cuIRQ (\ s a -> s{_cuIRQ = a});

-- | Percentage of time that the CPU has spent in the 'System' state over the
-- last 10 seconds.
cuSystem :: Lens' CPUUtilization (Maybe Double)
cuSystem = lens _cuSystem (\ s a -> s{_cuSystem = a});

-- | Percentage of time that the CPU has spent in the 'User' state over the
-- last 10 seconds.
cuUser :: Lens' CPUUtilization (Maybe Double)
cuUser = lens _cuUser (\ s a -> s{_cuUser = a});

-- | Percentage of time that the CPU has spent in the 'I\/O Wait' state over
-- the last 10 seconds.
cuIOWait :: Lens' CPUUtilization (Maybe Double)
cuIOWait = lens _cuIOWait (\ s a -> s{_cuIOWait = a});

-- | Percentage of time that the CPU has spent in the 'Nice' state over the
-- last 10 seconds.
cuNice :: Lens' CPUUtilization (Maybe Double)
cuNice = lens _cuNice (\ s a -> s{_cuNice = a});

instance FromXML CPUUtilization where
        parseXML x
          = CPUUtilization' <$>
              (x .@? "SoftIRQ") <*> (x .@? "Idle") <*>
                (x .@? "IRQ")
                <*> (x .@? "System")
                <*> (x .@? "User")
                <*> (x .@? "IOWait")
                <*> (x .@? "Nice")

instance Hashable CPUUtilization

-- | Describes the possible values for a configuration option.
--
-- /See:/ 'configurationOptionDescription' smart constructor.
data ConfigurationOptionDescription = ConfigurationOptionDescription'
    { _codMaxValue       :: !(Maybe Int)
    , _codRegex          :: !(Maybe OptionRestrictionRegex)
    , _codMaxLength      :: !(Maybe Int)
    , _codUserDefined    :: !(Maybe Bool)
    , _codNamespace      :: !(Maybe Text)
    , _codValueOptions   :: !(Maybe [Text])
    , _codName           :: !(Maybe Text)
    , _codChangeSeverity :: !(Maybe Text)
    , _codDefaultValue   :: !(Maybe Text)
    , _codValueType      :: !(Maybe ConfigurationOptionValueType)
    , _codMinValue       :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfigurationOptionDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'codMaxValue'
--
-- * 'codRegex'
--
-- * 'codMaxLength'
--
-- * 'codUserDefined'
--
-- * 'codNamespace'
--
-- * 'codValueOptions'
--
-- * 'codName'
--
-- * 'codChangeSeverity'
--
-- * 'codDefaultValue'
--
-- * 'codValueType'
--
-- * 'codMinValue'
configurationOptionDescription
    :: ConfigurationOptionDescription
configurationOptionDescription =
    ConfigurationOptionDescription'
    { _codMaxValue = Nothing
    , _codRegex = Nothing
    , _codMaxLength = Nothing
    , _codUserDefined = Nothing
    , _codNamespace = Nothing
    , _codValueOptions = Nothing
    , _codName = Nothing
    , _codChangeSeverity = Nothing
    , _codDefaultValue = Nothing
    , _codValueType = Nothing
    , _codMinValue = Nothing
    }

-- | If specified, the configuration option must be a numeric value less than
-- this value.
codMaxValue :: Lens' ConfigurationOptionDescription (Maybe Int)
codMaxValue = lens _codMaxValue (\ s a -> s{_codMaxValue = a});

-- | If specified, the configuration option must be a string value that
-- satisfies this regular expression.
codRegex :: Lens' ConfigurationOptionDescription (Maybe OptionRestrictionRegex)
codRegex = lens _codRegex (\ s a -> s{_codRegex = a});

-- | If specified, the configuration option must be a string value no longer
-- than this value.
codMaxLength :: Lens' ConfigurationOptionDescription (Maybe Int)
codMaxLength = lens _codMaxLength (\ s a -> s{_codMaxLength = a});

-- | An indication of whether the user defined this configuration option:
--
-- -   'true' : This configuration option was defined by the user. It is a
--     valid choice for specifying if this as an 'Option to Remove' when
--     updating configuration settings.
--
-- -   'false' : This configuration was not defined by the user.
--
-- Constraint: You can remove only 'UserDefined' options from a
-- configuration.
--
-- Valid Values: 'true' | 'false'
codUserDefined :: Lens' ConfigurationOptionDescription (Maybe Bool)
codUserDefined = lens _codUserDefined (\ s a -> s{_codUserDefined = a});

-- | A unique namespace identifying the option\'s associated AWS resource.
codNamespace :: Lens' ConfigurationOptionDescription (Maybe Text)
codNamespace = lens _codNamespace (\ s a -> s{_codNamespace = a});

-- | If specified, values for the configuration option are selected from this
-- list.
codValueOptions :: Lens' ConfigurationOptionDescription [Text]
codValueOptions = lens _codValueOptions (\ s a -> s{_codValueOptions = a}) . _Default . _Coerce;

-- | The name of the configuration option.
codName :: Lens' ConfigurationOptionDescription (Maybe Text)
codName = lens _codName (\ s a -> s{_codName = a});

-- | An indication of which action is required if the value for this
-- configuration option changes:
--
-- -   'NoInterruption' : There is no interruption to the environment or
--     application availability.
-- -   'RestartEnvironment' : The environment is entirely restarted, all
--     AWS resources are deleted and recreated, and the environment is
--     unavailable during the process.
-- -   'RestartApplicationServer' : The environment is available the entire
--     time. However, a short application outage occurs when the
--     application servers on the running Amazon EC2 instances are
--     restarted.
codChangeSeverity :: Lens' ConfigurationOptionDescription (Maybe Text)
codChangeSeverity = lens _codChangeSeverity (\ s a -> s{_codChangeSeverity = a});

-- | The default value for this configuration option.
codDefaultValue :: Lens' ConfigurationOptionDescription (Maybe Text)
codDefaultValue = lens _codDefaultValue (\ s a -> s{_codDefaultValue = a});

-- | An indication of which type of values this option has and whether it is
-- allowable to select one or more than one of the possible values:
--
-- -   'Scalar' : Values for this option are a single selection from the
--     possible values, or an unformatted string, or numeric value governed
--     by the 'MIN\/MAX\/Regex' constraints.
-- -   'List' : Values for this option are multiple selections from the
--     possible values.
-- -   'Boolean' : Values for this option are either 'true' or 'false' .
-- -   'Json' : Values for this option are a JSON representation of a
--     'ConfigDocument'.
codValueType :: Lens' ConfigurationOptionDescription (Maybe ConfigurationOptionValueType)
codValueType = lens _codValueType (\ s a -> s{_codValueType = a});

-- | If specified, the configuration option must be a numeric value greater
-- than this value.
codMinValue :: Lens' ConfigurationOptionDescription (Maybe Int)
codMinValue = lens _codMinValue (\ s a -> s{_codMinValue = a});

instance FromXML ConfigurationOptionDescription where
        parseXML x
          = ConfigurationOptionDescription' <$>
              (x .@? "MaxValue") <*> (x .@? "Regex") <*>
                (x .@? "MaxLength")
                <*> (x .@? "UserDefined")
                <*> (x .@? "Namespace")
                <*>
                (x .@? "ValueOptions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "Name")
                <*> (x .@? "ChangeSeverity")
                <*> (x .@? "DefaultValue")
                <*> (x .@? "ValueType")
                <*> (x .@? "MinValue")

instance Hashable ConfigurationOptionDescription

-- | A specification identifying an individual configuration option along
-- with its current value. For a list of possible option values, go to
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/command-options.html Option Values>
-- in the /AWS Elastic Beanstalk Developer Guide/.
--
-- /See:/ 'configurationOptionSetting' smart constructor.
data ConfigurationOptionSetting = ConfigurationOptionSetting'
    { _cosOptionName   :: !(Maybe Text)
    , _cosResourceName :: !(Maybe Text)
    , _cosNamespace    :: !(Maybe Text)
    , _cosValue        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfigurationOptionSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cosOptionName'
--
-- * 'cosResourceName'
--
-- * 'cosNamespace'
--
-- * 'cosValue'
configurationOptionSetting
    :: ConfigurationOptionSetting
configurationOptionSetting =
    ConfigurationOptionSetting'
    { _cosOptionName = Nothing
    , _cosResourceName = Nothing
    , _cosNamespace = Nothing
    , _cosValue = Nothing
    }

-- | The name of the configuration option.
cosOptionName :: Lens' ConfigurationOptionSetting (Maybe Text)
cosOptionName = lens _cosOptionName (\ s a -> s{_cosOptionName = a});

-- | A unique resource name for a time-based scaling configuration option.
cosResourceName :: Lens' ConfigurationOptionSetting (Maybe Text)
cosResourceName = lens _cosResourceName (\ s a -> s{_cosResourceName = a});

-- | A unique namespace identifying the option\'s associated AWS resource.
cosNamespace :: Lens' ConfigurationOptionSetting (Maybe Text)
cosNamespace = lens _cosNamespace (\ s a -> s{_cosNamespace = a});

-- | The current value for the configuration option.
cosValue :: Lens' ConfigurationOptionSetting (Maybe Text)
cosValue = lens _cosValue (\ s a -> s{_cosValue = a});

instance FromXML ConfigurationOptionSetting where
        parseXML x
          = ConfigurationOptionSetting' <$>
              (x .@? "OptionName") <*> (x .@? "ResourceName") <*>
                (x .@? "Namespace")
                <*> (x .@? "Value")

instance Hashable ConfigurationOptionSetting

instance ToQuery ConfigurationOptionSetting where
        toQuery ConfigurationOptionSetting'{..}
          = mconcat
              ["OptionName" =: _cosOptionName,
               "ResourceName" =: _cosResourceName,
               "Namespace" =: _cosNamespace, "Value" =: _cosValue]

-- | Describes the settings for a configuration set.
--
-- /See:/ 'configurationSettingsDescription' smart constructor.
data ConfigurationSettingsDescription = ConfigurationSettingsDescription'
    { _csdTemplateName      :: !(Maybe Text)
    , _csdOptionSettings    :: !(Maybe [ConfigurationOptionSetting])
    , _csdDateUpdated       :: !(Maybe ISO8601)
    , _csdDateCreated       :: !(Maybe ISO8601)
    , _csdEnvironmentName   :: !(Maybe Text)
    , _csdApplicationName   :: !(Maybe Text)
    , _csdDeploymentStatus  :: !(Maybe ConfigurationDeploymentStatus)
    , _csdSolutionStackName :: !(Maybe Text)
    , _csdDescription       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfigurationSettingsDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdTemplateName'
--
-- * 'csdOptionSettings'
--
-- * 'csdDateUpdated'
--
-- * 'csdDateCreated'
--
-- * 'csdEnvironmentName'
--
-- * 'csdApplicationName'
--
-- * 'csdDeploymentStatus'
--
-- * 'csdSolutionStackName'
--
-- * 'csdDescription'
configurationSettingsDescription
    :: ConfigurationSettingsDescription
configurationSettingsDescription =
    ConfigurationSettingsDescription'
    { _csdTemplateName = Nothing
    , _csdOptionSettings = Nothing
    , _csdDateUpdated = Nothing
    , _csdDateCreated = Nothing
    , _csdEnvironmentName = Nothing
    , _csdApplicationName = Nothing
    , _csdDeploymentStatus = Nothing
    , _csdSolutionStackName = Nothing
    , _csdDescription = Nothing
    }

-- | If not 'null', the name of the configuration template for this
-- configuration set.
csdTemplateName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdTemplateName = lens _csdTemplateName (\ s a -> s{_csdTemplateName = a});

-- | A list of the configuration options and their values in this
-- configuration set.
csdOptionSettings :: Lens' ConfigurationSettingsDescription [ConfigurationOptionSetting]
csdOptionSettings = lens _csdOptionSettings (\ s a -> s{_csdOptionSettings = a}) . _Default . _Coerce;

-- | The date (in UTC time) when this configuration set was last modified.
csdDateUpdated :: Lens' ConfigurationSettingsDescription (Maybe UTCTime)
csdDateUpdated = lens _csdDateUpdated (\ s a -> s{_csdDateUpdated = a}) . mapping _Time;

-- | The date (in UTC time) when this configuration set was created.
csdDateCreated :: Lens' ConfigurationSettingsDescription (Maybe UTCTime)
csdDateCreated = lens _csdDateCreated (\ s a -> s{_csdDateCreated = a}) . mapping _Time;

-- | If not 'null', the name of the environment for this configuration set.
csdEnvironmentName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdEnvironmentName = lens _csdEnvironmentName (\ s a -> s{_csdEnvironmentName = a});

-- | The name of the application associated with this configuration set.
csdApplicationName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdApplicationName = lens _csdApplicationName (\ s a -> s{_csdApplicationName = a});

-- | If this configuration set is associated with an environment, the
-- 'DeploymentStatus' parameter indicates the deployment status of this
-- configuration set:
--
-- -   'null': This configuration is not associated with a running
--     environment.
-- -   'pending': This is a draft configuration that is not deployed to the
--     associated environment but is in the process of deploying.
-- -   'deployed': This is the configuration that is currently deployed to
--     the associated running environment.
-- -   'failed': This is a draft configuration that failed to successfully
--     deploy.
csdDeploymentStatus :: Lens' ConfigurationSettingsDescription (Maybe ConfigurationDeploymentStatus)
csdDeploymentStatus = lens _csdDeploymentStatus (\ s a -> s{_csdDeploymentStatus = a});

-- | The name of the solution stack this configuration set uses.
csdSolutionStackName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdSolutionStackName = lens _csdSolutionStackName (\ s a -> s{_csdSolutionStackName = a});

-- | Describes this configuration set.
csdDescription :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdDescription = lens _csdDescription (\ s a -> s{_csdDescription = a});

instance FromXML ConfigurationSettingsDescription
         where
        parseXML x
          = ConfigurationSettingsDescription' <$>
              (x .@? "TemplateName") <*>
                (x .@? "OptionSettings" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "DateUpdated")
                <*> (x .@? "DateCreated")
                <*> (x .@? "EnvironmentName")
                <*> (x .@? "ApplicationName")
                <*> (x .@? "DeploymentStatus")
                <*> (x .@? "SolutionStackName")
                <*> (x .@? "Description")

instance Hashable ConfigurationSettingsDescription

-- | Describes the properties of an environment.
--
-- /See:/ 'environmentDescription' smart constructor.
data EnvironmentDescription = EnvironmentDescription'
    { _eStatus                       :: !(Maybe EnvironmentStatus)
    , _eCNAME                        :: !(Maybe Text)
    , _eTemplateName                 :: !(Maybe Text)
    , _eAbortableOperationInProgress :: !(Maybe Bool)
    , _eEndpointURL                  :: !(Maybe Text)
    , _eResources                    :: !(Maybe EnvironmentResourcesDescription)
    , _eDateUpdated                  :: !(Maybe ISO8601)
    , _eDateCreated                  :: !(Maybe ISO8601)
    , _eHealth                       :: !(Maybe EnvironmentHealth)
    , _eVersionLabel                 :: !(Maybe Text)
    , _eTier                         :: !(Maybe EnvironmentTier)
    , _eEnvironmentName              :: !(Maybe Text)
    , _eApplicationName              :: !(Maybe Text)
    , _eSolutionStackName            :: !(Maybe Text)
    , _eEnvironmentId                :: !(Maybe Text)
    , _eHealthStatus                 :: !(Maybe EnvironmentHealthStatus)
    , _eEnvironmentLinks             :: !(Maybe [EnvironmentLink])
    , _eDescription                  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnvironmentDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eStatus'
--
-- * 'eCNAME'
--
-- * 'eTemplateName'
--
-- * 'eAbortableOperationInProgress'
--
-- * 'eEndpointURL'
--
-- * 'eResources'
--
-- * 'eDateUpdated'
--
-- * 'eDateCreated'
--
-- * 'eHealth'
--
-- * 'eVersionLabel'
--
-- * 'eTier'
--
-- * 'eEnvironmentName'
--
-- * 'eApplicationName'
--
-- * 'eSolutionStackName'
--
-- * 'eEnvironmentId'
--
-- * 'eHealthStatus'
--
-- * 'eEnvironmentLinks'
--
-- * 'eDescription'
environmentDescription
    :: EnvironmentDescription
environmentDescription =
    EnvironmentDescription'
    { _eStatus = Nothing
    , _eCNAME = Nothing
    , _eTemplateName = Nothing
    , _eAbortableOperationInProgress = Nothing
    , _eEndpointURL = Nothing
    , _eResources = Nothing
    , _eDateUpdated = Nothing
    , _eDateCreated = Nothing
    , _eHealth = Nothing
    , _eVersionLabel = Nothing
    , _eTier = Nothing
    , _eEnvironmentName = Nothing
    , _eApplicationName = Nothing
    , _eSolutionStackName = Nothing
    , _eEnvironmentId = Nothing
    , _eHealthStatus = Nothing
    , _eEnvironmentLinks = Nothing
    , _eDescription = Nothing
    }

-- | The current operational status of the environment:
--
-- -   'Launching': Environment is in the process of initial deployment.
-- -   'Updating': Environment is in the process of updating its
--     configuration settings or application version.
-- -   'Ready': Environment is available to have an action performed on it,
--     such as update or terminate.
-- -   'Terminating': Environment is in the shut-down process.
-- -   'Terminated': Environment is not running.
eStatus :: Lens' EnvironmentDescription (Maybe EnvironmentStatus)
eStatus = lens _eStatus (\ s a -> s{_eStatus = a});

-- | The URL to the CNAME for this environment.
eCNAME :: Lens' EnvironmentDescription (Maybe Text)
eCNAME = lens _eCNAME (\ s a -> s{_eCNAME = a});

-- | The name of the configuration template used to originally launch this
-- environment.
eTemplateName :: Lens' EnvironmentDescription (Maybe Text)
eTemplateName = lens _eTemplateName (\ s a -> s{_eTemplateName = a});

-- | Indicates if there is an in-progress environment configuration update or
-- application version deployment that you can cancel.
--
-- 'true:' There is an update in progress.
--
-- 'false:' There are no updates currently in progress.
eAbortableOperationInProgress :: Lens' EnvironmentDescription (Maybe Bool)
eAbortableOperationInProgress = lens _eAbortableOperationInProgress (\ s a -> s{_eAbortableOperationInProgress = a});

-- | For load-balanced, autoscaling environments, the URL to the
-- LoadBalancer. For single-instance environments, the IP address of the
-- instance.
eEndpointURL :: Lens' EnvironmentDescription (Maybe Text)
eEndpointURL = lens _eEndpointURL (\ s a -> s{_eEndpointURL = a});

-- | The description of the AWS resources used by this environment.
eResources :: Lens' EnvironmentDescription (Maybe EnvironmentResourcesDescription)
eResources = lens _eResources (\ s a -> s{_eResources = a});

-- | The last modified date for this environment.
eDateUpdated :: Lens' EnvironmentDescription (Maybe UTCTime)
eDateUpdated = lens _eDateUpdated (\ s a -> s{_eDateUpdated = a}) . mapping _Time;

-- | The creation date for this environment.
eDateCreated :: Lens' EnvironmentDescription (Maybe UTCTime)
eDateCreated = lens _eDateCreated (\ s a -> s{_eDateCreated = a}) . mapping _Time;

-- | Describes the health status of the environment. AWS Elastic Beanstalk
-- indicates the failure levels for a running environment:
--
-- -   'Red': Indicates the environment is not responsive. Occurs when
--     three or more consecutive failures occur for an environment.
-- -   'Yellow': Indicates that something is wrong. Occurs when two
--     consecutive failures occur for an environment.
-- -   'Green': Indicates the environment is healthy and fully functional.
-- -   'Grey': Default health for a new environment. The environment is not
--     fully launched and health checks have not started or health checks
--     are suspended during an 'UpdateEnvironment' or 'RestartEnvironement'
--     request.
--
-- Default: 'Grey'
eHealth :: Lens' EnvironmentDescription (Maybe EnvironmentHealth)
eHealth = lens _eHealth (\ s a -> s{_eHealth = a});

-- | The application version deployed in this environment.
eVersionLabel :: Lens' EnvironmentDescription (Maybe Text)
eVersionLabel = lens _eVersionLabel (\ s a -> s{_eVersionLabel = a});

-- | Describes the current tier of this environment.
eTier :: Lens' EnvironmentDescription (Maybe EnvironmentTier)
eTier = lens _eTier (\ s a -> s{_eTier = a});

-- | The name of this environment.
eEnvironmentName :: Lens' EnvironmentDescription (Maybe Text)
eEnvironmentName = lens _eEnvironmentName (\ s a -> s{_eEnvironmentName = a});

-- | The name of the application associated with this environment.
eApplicationName :: Lens' EnvironmentDescription (Maybe Text)
eApplicationName = lens _eApplicationName (\ s a -> s{_eApplicationName = a});

-- | The name of the 'SolutionStack' deployed with this environment.
eSolutionStackName :: Lens' EnvironmentDescription (Maybe Text)
eSolutionStackName = lens _eSolutionStackName (\ s a -> s{_eSolutionStackName = a});

-- | The ID of this environment.
eEnvironmentId :: Lens' EnvironmentDescription (Maybe Text)
eEnvironmentId = lens _eEnvironmentId (\ s a -> s{_eEnvironmentId = a});

-- | Returns the health status of the application running in your
-- environment. For more information, see
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses>.
eHealthStatus :: Lens' EnvironmentDescription (Maybe EnvironmentHealthStatus)
eHealthStatus = lens _eHealthStatus (\ s a -> s{_eHealthStatus = a});

-- | A list of links to other environments in the same group.
eEnvironmentLinks :: Lens' EnvironmentDescription [EnvironmentLink]
eEnvironmentLinks = lens _eEnvironmentLinks (\ s a -> s{_eEnvironmentLinks = a}) . _Default . _Coerce;

-- | Describes this environment.
eDescription :: Lens' EnvironmentDescription (Maybe Text)
eDescription = lens _eDescription (\ s a -> s{_eDescription = a});

instance FromXML EnvironmentDescription where
        parseXML x
          = EnvironmentDescription' <$>
              (x .@? "Status") <*> (x .@? "CNAME") <*>
                (x .@? "TemplateName")
                <*> (x .@? "AbortableOperationInProgress")
                <*> (x .@? "EndpointURL")
                <*> (x .@? "Resources")
                <*> (x .@? "DateUpdated")
                <*> (x .@? "DateCreated")
                <*> (x .@? "Health")
                <*> (x .@? "VersionLabel")
                <*> (x .@? "Tier")
                <*> (x .@? "EnvironmentName")
                <*> (x .@? "ApplicationName")
                <*> (x .@? "SolutionStackName")
                <*> (x .@? "EnvironmentId")
                <*> (x .@? "HealthStatus")
                <*>
                (x .@? "EnvironmentLinks" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "Description")

instance Hashable EnvironmentDescription

-- | Result message containing a list of environment descriptions.
--
-- /See:/ 'environmentDescriptionsMessage' smart constructor.
newtype EnvironmentDescriptionsMessage = EnvironmentDescriptionsMessage'
    { _edmEnvironments :: Maybe [EnvironmentDescription]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnvironmentDescriptionsMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edmEnvironments'
environmentDescriptionsMessage
    :: EnvironmentDescriptionsMessage
environmentDescriptionsMessage =
    EnvironmentDescriptionsMessage'
    { _edmEnvironments = Nothing
    }

-- | Returns an < EnvironmentDescription> list.
edmEnvironments :: Lens' EnvironmentDescriptionsMessage [EnvironmentDescription]
edmEnvironments = lens _edmEnvironments (\ s a -> s{_edmEnvironments = a}) . _Default . _Coerce;

instance FromXML EnvironmentDescriptionsMessage where
        parseXML x
          = EnvironmentDescriptionsMessage' <$>
              (x .@? "Environments" .!@ mempty >>=
                 may (parseXMLList "member"))

instance Hashable EnvironmentDescriptionsMessage

-- | The information retrieved from the Amazon EC2 instances.
--
-- /See:/ 'environmentInfoDescription' smart constructor.
data EnvironmentInfoDescription = EnvironmentInfoDescription'
    { _eidSampleTimestamp :: !(Maybe ISO8601)
    , _eidEC2InstanceId   :: !(Maybe Text)
    , _eidInfoType        :: !(Maybe EnvironmentInfoType)
    , _eidMessage         :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnvironmentInfoDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eidSampleTimestamp'
--
-- * 'eidEC2InstanceId'
--
-- * 'eidInfoType'
--
-- * 'eidMessage'
environmentInfoDescription
    :: EnvironmentInfoDescription
environmentInfoDescription =
    EnvironmentInfoDescription'
    { _eidSampleTimestamp = Nothing
    , _eidEC2InstanceId = Nothing
    , _eidInfoType = Nothing
    , _eidMessage = Nothing
    }

-- | The time stamp when this information was retrieved.
eidSampleTimestamp :: Lens' EnvironmentInfoDescription (Maybe UTCTime)
eidSampleTimestamp = lens _eidSampleTimestamp (\ s a -> s{_eidSampleTimestamp = a}) . mapping _Time;

-- | The Amazon EC2 Instance ID for this information.
eidEC2InstanceId :: Lens' EnvironmentInfoDescription (Maybe Text)
eidEC2InstanceId = lens _eidEC2InstanceId (\ s a -> s{_eidEC2InstanceId = a});

-- | The type of information retrieved.
eidInfoType :: Lens' EnvironmentInfoDescription (Maybe EnvironmentInfoType)
eidInfoType = lens _eidInfoType (\ s a -> s{_eidInfoType = a});

-- | The retrieved information.
eidMessage :: Lens' EnvironmentInfoDescription (Maybe Text)
eidMessage = lens _eidMessage (\ s a -> s{_eidMessage = a});

instance FromXML EnvironmentInfoDescription where
        parseXML x
          = EnvironmentInfoDescription' <$>
              (x .@? "SampleTimestamp") <*> (x .@? "Ec2InstanceId")
                <*> (x .@? "InfoType")
                <*> (x .@? "Message")

instance Hashable EnvironmentInfoDescription

-- | A link to another environment, defined in the environment\'s manifest.
-- Links provide connection information in system properties that can be
-- used to connect to another environment in the same group. See
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-mgmt-compose.html#environment-mgmt-compose-envyaml Environment Manifest (env.yaml)>
-- for details.
--
-- /See:/ 'environmentLink' smart constructor.
data EnvironmentLink = EnvironmentLink'
    { _elLinkName        :: !(Maybe Text)
    , _elEnvironmentName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnvironmentLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'elLinkName'
--
-- * 'elEnvironmentName'
environmentLink
    :: EnvironmentLink
environmentLink =
    EnvironmentLink'
    { _elLinkName = Nothing
    , _elEnvironmentName = Nothing
    }

-- | The name of the link.
elLinkName :: Lens' EnvironmentLink (Maybe Text)
elLinkName = lens _elLinkName (\ s a -> s{_elLinkName = a});

-- | The name of the linked environment (the dependency).
elEnvironmentName :: Lens' EnvironmentLink (Maybe Text)
elEnvironmentName = lens _elEnvironmentName (\ s a -> s{_elEnvironmentName = a});

instance FromXML EnvironmentLink where
        parseXML x
          = EnvironmentLink' <$>
              (x .@? "LinkName") <*> (x .@? "EnvironmentName")

instance Hashable EnvironmentLink

-- | Describes the AWS resources in use by this environment. This data is
-- live.
--
-- /See:/ 'environmentResourceDescription' smart constructor.
data EnvironmentResourceDescription = EnvironmentResourceDescription'
    { _erdQueues               :: !(Maybe [Queue])
    , _erdTriggers             :: !(Maybe [Trigger])
    , _erdLoadBalancers        :: !(Maybe [LoadBalancer])
    , _erdEnvironmentName      :: !(Maybe Text)
    , _erdInstances            :: !(Maybe [Instance])
    , _erdLaunchConfigurations :: !(Maybe [LaunchConfiguration])
    , _erdAutoScalingGroups    :: !(Maybe [AutoScalingGroup])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnvironmentResourceDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erdQueues'
--
-- * 'erdTriggers'
--
-- * 'erdLoadBalancers'
--
-- * 'erdEnvironmentName'
--
-- * 'erdInstances'
--
-- * 'erdLaunchConfigurations'
--
-- * 'erdAutoScalingGroups'
environmentResourceDescription
    :: EnvironmentResourceDescription
environmentResourceDescription =
    EnvironmentResourceDescription'
    { _erdQueues = Nothing
    , _erdTriggers = Nothing
    , _erdLoadBalancers = Nothing
    , _erdEnvironmentName = Nothing
    , _erdInstances = Nothing
    , _erdLaunchConfigurations = Nothing
    , _erdAutoScalingGroups = Nothing
    }

-- | The queues used by this environment.
erdQueues :: Lens' EnvironmentResourceDescription [Queue]
erdQueues = lens _erdQueues (\ s a -> s{_erdQueues = a}) . _Default . _Coerce;

-- | The 'AutoScaling' triggers in use by this environment.
erdTriggers :: Lens' EnvironmentResourceDescription [Trigger]
erdTriggers = lens _erdTriggers (\ s a -> s{_erdTriggers = a}) . _Default . _Coerce;

-- | The LoadBalancers in use by this environment.
erdLoadBalancers :: Lens' EnvironmentResourceDescription [LoadBalancer]
erdLoadBalancers = lens _erdLoadBalancers (\ s a -> s{_erdLoadBalancers = a}) . _Default . _Coerce;

-- | The name of the environment.
erdEnvironmentName :: Lens' EnvironmentResourceDescription (Maybe Text)
erdEnvironmentName = lens _erdEnvironmentName (\ s a -> s{_erdEnvironmentName = a});

-- | The Amazon EC2 instances used by this environment.
erdInstances :: Lens' EnvironmentResourceDescription [Instance]
erdInstances = lens _erdInstances (\ s a -> s{_erdInstances = a}) . _Default . _Coerce;

-- | The Auto Scaling launch configurations in use by this environment.
erdLaunchConfigurations :: Lens' EnvironmentResourceDescription [LaunchConfiguration]
erdLaunchConfigurations = lens _erdLaunchConfigurations (\ s a -> s{_erdLaunchConfigurations = a}) . _Default . _Coerce;

-- | The 'AutoScalingGroups' used by this environment.
erdAutoScalingGroups :: Lens' EnvironmentResourceDescription [AutoScalingGroup]
erdAutoScalingGroups = lens _erdAutoScalingGroups (\ s a -> s{_erdAutoScalingGroups = a}) . _Default . _Coerce;

instance FromXML EnvironmentResourceDescription where
        parseXML x
          = EnvironmentResourceDescription' <$>
              (x .@? "Queues" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*>
                (x .@? "Triggers" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "LoadBalancers" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "EnvironmentName")
                <*>
                (x .@? "Instances" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "LaunchConfigurations" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "AutoScalingGroups" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable EnvironmentResourceDescription

-- | Describes the AWS resources in use by this environment. This data is not
-- live data.
--
-- /See:/ 'environmentResourcesDescription' smart constructor.
newtype EnvironmentResourcesDescription = EnvironmentResourcesDescription'
    { _erdLoadBalancer :: Maybe LoadBalancerDescription
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnvironmentResourcesDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erdLoadBalancer'
environmentResourcesDescription
    :: EnvironmentResourcesDescription
environmentResourcesDescription =
    EnvironmentResourcesDescription'
    { _erdLoadBalancer = Nothing
    }

-- | Describes the LoadBalancer.
erdLoadBalancer :: Lens' EnvironmentResourcesDescription (Maybe LoadBalancerDescription)
erdLoadBalancer = lens _erdLoadBalancer (\ s a -> s{_erdLoadBalancer = a});

instance FromXML EnvironmentResourcesDescription
         where
        parseXML x
          = EnvironmentResourcesDescription' <$>
              (x .@? "LoadBalancer")

instance Hashable EnvironmentResourcesDescription

-- | Describes the properties of an environment tier
--
-- /See:/ 'environmentTier' smart constructor.
data EnvironmentTier = EnvironmentTier'
    { _etName    :: !(Maybe Text)
    , _etVersion :: !(Maybe Text)
    , _etType    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnvironmentTier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etName'
--
-- * 'etVersion'
--
-- * 'etType'
environmentTier
    :: EnvironmentTier
environmentTier =
    EnvironmentTier'
    { _etName = Nothing
    , _etVersion = Nothing
    , _etType = Nothing
    }

-- | The name of this environment tier.
etName :: Lens' EnvironmentTier (Maybe Text)
etName = lens _etName (\ s a -> s{_etName = a});

-- | The version of this environment tier.
etVersion :: Lens' EnvironmentTier (Maybe Text)
etVersion = lens _etVersion (\ s a -> s{_etVersion = a});

-- | The type of this environment tier.
etType :: Lens' EnvironmentTier (Maybe Text)
etType = lens _etType (\ s a -> s{_etType = a});

instance FromXML EnvironmentTier where
        parseXML x
          = EnvironmentTier' <$>
              (x .@? "Name") <*> (x .@? "Version") <*>
                (x .@? "Type")

instance Hashable EnvironmentTier

instance ToQuery EnvironmentTier where
        toQuery EnvironmentTier'{..}
          = mconcat
              ["Name" =: _etName, "Version" =: _etVersion,
               "Type" =: _etType]

-- | Describes an event.
--
-- /See:/ 'eventDescription' smart constructor.
data EventDescription = EventDescription'
    { _edRequestId       :: !(Maybe Text)
    , _edTemplateName    :: !(Maybe Text)
    , _edSeverity        :: !(Maybe EventSeverity)
    , _edVersionLabel    :: !(Maybe Text)
    , _edEnvironmentName :: !(Maybe Text)
    , _edApplicationName :: !(Maybe Text)
    , _edEventDate       :: !(Maybe ISO8601)
    , _edMessage         :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EventDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edRequestId'
--
-- * 'edTemplateName'
--
-- * 'edSeverity'
--
-- * 'edVersionLabel'
--
-- * 'edEnvironmentName'
--
-- * 'edApplicationName'
--
-- * 'edEventDate'
--
-- * 'edMessage'
eventDescription
    :: EventDescription
eventDescription =
    EventDescription'
    { _edRequestId = Nothing
    , _edTemplateName = Nothing
    , _edSeverity = Nothing
    , _edVersionLabel = Nothing
    , _edEnvironmentName = Nothing
    , _edApplicationName = Nothing
    , _edEventDate = Nothing
    , _edMessage = Nothing
    }

-- | The web service request ID for the activity of this event.
edRequestId :: Lens' EventDescription (Maybe Text)
edRequestId = lens _edRequestId (\ s a -> s{_edRequestId = a});

-- | The name of the configuration associated with this event.
edTemplateName :: Lens' EventDescription (Maybe Text)
edTemplateName = lens _edTemplateName (\ s a -> s{_edTemplateName = a});

-- | The severity level of this event.
edSeverity :: Lens' EventDescription (Maybe EventSeverity)
edSeverity = lens _edSeverity (\ s a -> s{_edSeverity = a});

-- | The release label for the application version associated with this
-- event.
edVersionLabel :: Lens' EventDescription (Maybe Text)
edVersionLabel = lens _edVersionLabel (\ s a -> s{_edVersionLabel = a});

-- | The name of the environment associated with this event.
edEnvironmentName :: Lens' EventDescription (Maybe Text)
edEnvironmentName = lens _edEnvironmentName (\ s a -> s{_edEnvironmentName = a});

-- | The application associated with the event.
edApplicationName :: Lens' EventDescription (Maybe Text)
edApplicationName = lens _edApplicationName (\ s a -> s{_edApplicationName = a});

-- | The date when the event occurred.
edEventDate :: Lens' EventDescription (Maybe UTCTime)
edEventDate = lens _edEventDate (\ s a -> s{_edEventDate = a}) . mapping _Time;

-- | The event message.
edMessage :: Lens' EventDescription (Maybe Text)
edMessage = lens _edMessage (\ s a -> s{_edMessage = a});

instance FromXML EventDescription where
        parseXML x
          = EventDescription' <$>
              (x .@? "RequestId") <*> (x .@? "TemplateName") <*>
                (x .@? "Severity")
                <*> (x .@? "VersionLabel")
                <*> (x .@? "EnvironmentName")
                <*> (x .@? "ApplicationName")
                <*> (x .@? "EventDate")
                <*> (x .@? "Message")

instance Hashable EventDescription

-- | The description of an Amazon EC2 instance.
--
-- /See:/ 'instance'' smart constructor.
newtype Instance = Instance'
    { _iId :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iId'
instance'
    :: Instance
instance' =
    Instance'
    { _iId = Nothing
    }

-- | The ID of the Amazon EC2 instance.
iId :: Lens' Instance (Maybe Text)
iId = lens _iId (\ s a -> s{_iId = a});

instance FromXML Instance where
        parseXML x = Instance' <$> (x .@? "Id")

instance Hashable Instance

-- | Represents summary information about the health of an instance. For more
-- information, see
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses>.
--
-- /See:/ 'instanceHealthSummary' smart constructor.
data InstanceHealthSummary = InstanceHealthSummary'
    { _ihsOK       :: !(Maybe Int)
    , _ihsPending  :: !(Maybe Int)
    , _ihsSevere   :: !(Maybe Int)
    , _ihsUnknown  :: !(Maybe Int)
    , _ihsNoData   :: !(Maybe Int)
    , _ihsWarning  :: !(Maybe Int)
    , _ihsDegraded :: !(Maybe Int)
    , _ihsInfo     :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InstanceHealthSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ihsOK'
--
-- * 'ihsPending'
--
-- * 'ihsSevere'
--
-- * 'ihsUnknown'
--
-- * 'ihsNoData'
--
-- * 'ihsWarning'
--
-- * 'ihsDegraded'
--
-- * 'ihsInfo'
instanceHealthSummary
    :: InstanceHealthSummary
instanceHealthSummary =
    InstanceHealthSummary'
    { _ihsOK = Nothing
    , _ihsPending = Nothing
    , _ihsSevere = Nothing
    , _ihsUnknown = Nothing
    , _ihsNoData = Nothing
    , _ihsWarning = Nothing
    , _ihsDegraded = Nothing
    , _ihsInfo = Nothing
    }

-- | __Green.__ An instance is passing health checks and the health agent is
-- not reporting any problems.
ihsOK :: Lens' InstanceHealthSummary (Maybe Int)
ihsOK = lens _ihsOK (\ s a -> s{_ihsOK = a});

-- | __Grey.__ An operation is in progress on an instance within the command
-- timeout.
ihsPending :: Lens' InstanceHealthSummary (Maybe Int)
ihsPending = lens _ihsPending (\ s a -> s{_ihsPending = a});

-- | __Red.__ The health agent is reporting a very high number of request
-- failures or other issues for an instance or environment.
ihsSevere :: Lens' InstanceHealthSummary (Maybe Int)
ihsSevere = lens _ihsSevere (\ s a -> s{_ihsSevere = a});

-- | __Grey.__ AWS Elastic Beanstalk and the health agent are reporting an
-- insufficient amount of data on an instance.
ihsUnknown :: Lens' InstanceHealthSummary (Maybe Int)
ihsUnknown = lens _ihsUnknown (\ s a -> s{_ihsUnknown = a});

-- | __Grey.__ AWS Elastic Beanstalk and the health agent are reporting no
-- data on an instance.
ihsNoData :: Lens' InstanceHealthSummary (Maybe Int)
ihsNoData = lens _ihsNoData (\ s a -> s{_ihsNoData = a});

-- | __Yellow.__ The health agent is reporting a moderate number of request
-- failures or other issues for an instance or environment.
ihsWarning :: Lens' InstanceHealthSummary (Maybe Int)
ihsWarning = lens _ihsWarning (\ s a -> s{_ihsWarning = a});

-- | __Red.__ The health agent is reporting a high number of request failures
-- or other issues for an instance or environment.
ihsDegraded :: Lens' InstanceHealthSummary (Maybe Int)
ihsDegraded = lens _ihsDegraded (\ s a -> s{_ihsDegraded = a});

-- | __Green.__ An operation is in progress on an instance.
ihsInfo :: Lens' InstanceHealthSummary (Maybe Int)
ihsInfo = lens _ihsInfo (\ s a -> s{_ihsInfo = a});

instance FromXML InstanceHealthSummary where
        parseXML x
          = InstanceHealthSummary' <$>
              (x .@? "Ok") <*> (x .@? "Pending") <*>
                (x .@? "Severe")
                <*> (x .@? "Unknown")
                <*> (x .@? "NoData")
                <*> (x .@? "Warning")
                <*> (x .@? "Degraded")
                <*> (x .@? "Info")

instance Hashable InstanceHealthSummary

-- | Represents the average latency for the slowest X percent of requests
-- over the last 10 seconds.
--
-- /See:/ 'latency' smart constructor.
data Latency = Latency'
    { _lP75  :: !(Maybe Double)
    , _lP50  :: !(Maybe Double)
    , _lP85  :: !(Maybe Double)
    , _lP999 :: !(Maybe Double)
    , _lP90  :: !(Maybe Double)
    , _lP95  :: !(Maybe Double)
    , _lP99  :: !(Maybe Double)
    , _lP10  :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Latency' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lP75'
--
-- * 'lP50'
--
-- * 'lP85'
--
-- * 'lP999'
--
-- * 'lP90'
--
-- * 'lP95'
--
-- * 'lP99'
--
-- * 'lP10'
latency
    :: Latency
latency =
    Latency'
    { _lP75 = Nothing
    , _lP50 = Nothing
    , _lP85 = Nothing
    , _lP999 = Nothing
    , _lP90 = Nothing
    , _lP95 = Nothing
    , _lP99 = Nothing
    , _lP10 = Nothing
    }

-- | The average latency for the slowest 25 percent of requests over the last
-- 10 seconds.
lP75 :: Lens' Latency (Maybe Double)
lP75 = lens _lP75 (\ s a -> s{_lP75 = a});

-- | The average latency for the slowest 50 percent of requests over the last
-- 10 seconds.
lP50 :: Lens' Latency (Maybe Double)
lP50 = lens _lP50 (\ s a -> s{_lP50 = a});

-- | The average latency for the slowest 15 percent of requests over the last
-- 10 seconds.
lP85 :: Lens' Latency (Maybe Double)
lP85 = lens _lP85 (\ s a -> s{_lP85 = a});

-- | The average latency for the slowest 0.1 percent of requests over the
-- last 10 seconds.
lP999 :: Lens' Latency (Maybe Double)
lP999 = lens _lP999 (\ s a -> s{_lP999 = a});

-- | The average latency for the slowest 10 percent of requests over the last
-- 10 seconds.
lP90 :: Lens' Latency (Maybe Double)
lP90 = lens _lP90 (\ s a -> s{_lP90 = a});

-- | The average latency for the slowest 5 percent of requests over the last
-- 10 seconds.
lP95 :: Lens' Latency (Maybe Double)
lP95 = lens _lP95 (\ s a -> s{_lP95 = a});

-- | The average latency for the slowest 1 percent of requests over the last
-- 10 seconds.
lP99 :: Lens' Latency (Maybe Double)
lP99 = lens _lP99 (\ s a -> s{_lP99 = a});

-- | The average latency for the slowest 90 percent of requests over the last
-- 10 seconds.
lP10 :: Lens' Latency (Maybe Double)
lP10 = lens _lP10 (\ s a -> s{_lP10 = a});

instance FromXML Latency where
        parseXML x
          = Latency' <$>
              (x .@? "P75") <*> (x .@? "P50") <*> (x .@? "P85") <*>
                (x .@? "P999")
                <*> (x .@? "P90")
                <*> (x .@? "P95")
                <*> (x .@? "P99")
                <*> (x .@? "P10")

instance Hashable Latency

-- | Describes an Auto Scaling launch configuration.
--
-- /See:/ 'launchConfiguration' smart constructor.
newtype LaunchConfiguration = LaunchConfiguration'
    { _lcName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LaunchConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcName'
launchConfiguration
    :: LaunchConfiguration
launchConfiguration =
    LaunchConfiguration'
    { _lcName = Nothing
    }

-- | The name of the launch configuration.
lcName :: Lens' LaunchConfiguration (Maybe Text)
lcName = lens _lcName (\ s a -> s{_lcName = a});

instance FromXML LaunchConfiguration where
        parseXML x = LaunchConfiguration' <$> (x .@? "Name")

instance Hashable LaunchConfiguration

-- | Describes the properties of a Listener for the LoadBalancer.
--
-- /See:/ 'listener' smart constructor.
data Listener = Listener'
    { _lProtocol :: !(Maybe Text)
    , _lPort     :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Listener' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lProtocol'
--
-- * 'lPort'
listener
    :: Listener
listener =
    Listener'
    { _lProtocol = Nothing
    , _lPort = Nothing
    }

-- | The protocol that is used by the Listener.
lProtocol :: Lens' Listener (Maybe Text)
lProtocol = lens _lProtocol (\ s a -> s{_lProtocol = a});

-- | The port that is used by the Listener.
lPort :: Lens' Listener (Maybe Int)
lPort = lens _lPort (\ s a -> s{_lPort = a});

instance FromXML Listener where
        parseXML x
          = Listener' <$> (x .@? "Protocol") <*> (x .@? "Port")

instance Hashable Listener

-- | Describes a LoadBalancer.
--
-- /See:/ 'loadBalancer' smart constructor.
newtype LoadBalancer = LoadBalancer'
    { _lbName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbName'
loadBalancer
    :: LoadBalancer
loadBalancer =
    LoadBalancer'
    { _lbName = Nothing
    }

-- | The name of the LoadBalancer.
lbName :: Lens' LoadBalancer (Maybe Text)
lbName = lens _lbName (\ s a -> s{_lbName = a});

instance FromXML LoadBalancer where
        parseXML x = LoadBalancer' <$> (x .@? "Name")

instance Hashable LoadBalancer

-- | Describes the details of a LoadBalancer.
--
-- /See:/ 'loadBalancerDescription' smart constructor.
data LoadBalancerDescription = LoadBalancerDescription'
    { _lbdLoadBalancerName :: !(Maybe Text)
    , _lbdDomain           :: !(Maybe Text)
    , _lbdListeners        :: !(Maybe [Listener])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LoadBalancerDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbdLoadBalancerName'
--
-- * 'lbdDomain'
--
-- * 'lbdListeners'
loadBalancerDescription
    :: LoadBalancerDescription
loadBalancerDescription =
    LoadBalancerDescription'
    { _lbdLoadBalancerName = Nothing
    , _lbdDomain = Nothing
    , _lbdListeners = Nothing
    }

-- | The name of the LoadBalancer.
lbdLoadBalancerName :: Lens' LoadBalancerDescription (Maybe Text)
lbdLoadBalancerName = lens _lbdLoadBalancerName (\ s a -> s{_lbdLoadBalancerName = a});

-- | The domain name of the LoadBalancer.
lbdDomain :: Lens' LoadBalancerDescription (Maybe Text)
lbdDomain = lens _lbdDomain (\ s a -> s{_lbdDomain = a});

-- | A list of Listeners used by the LoadBalancer.
lbdListeners :: Lens' LoadBalancerDescription [Listener]
lbdListeners = lens _lbdListeners (\ s a -> s{_lbdListeners = a}) . _Default . _Coerce;

instance FromXML LoadBalancerDescription where
        parseXML x
          = LoadBalancerDescription' <$>
              (x .@? "LoadBalancerName") <*> (x .@? "Domain") <*>
                (x .@? "Listeners" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable LoadBalancerDescription

-- | A regular expression representing a restriction on a string
-- configuration option value.
--
-- /See:/ 'optionRestrictionRegex' smart constructor.
data OptionRestrictionRegex = OptionRestrictionRegex'
    { _orrPattern :: !(Maybe Text)
    , _orrLabel   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'OptionRestrictionRegex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orrPattern'
--
-- * 'orrLabel'
optionRestrictionRegex
    :: OptionRestrictionRegex
optionRestrictionRegex =
    OptionRestrictionRegex'
    { _orrPattern = Nothing
    , _orrLabel = Nothing
    }

-- | The regular expression pattern that a string configuration option value
-- with this restriction must match.
orrPattern :: Lens' OptionRestrictionRegex (Maybe Text)
orrPattern = lens _orrPattern (\ s a -> s{_orrPattern = a});

-- | A unique name representing this regular expression.
orrLabel :: Lens' OptionRestrictionRegex (Maybe Text)
orrLabel = lens _orrLabel (\ s a -> s{_orrLabel = a});

instance FromXML OptionRestrictionRegex where
        parseXML x
          = OptionRestrictionRegex' <$>
              (x .@? "Pattern") <*> (x .@? "Label")

instance Hashable OptionRestrictionRegex

-- | A specification identifying an individual configuration option.
--
-- /See:/ 'optionSpecification' smart constructor.
data OptionSpecification = OptionSpecification'
    { _osOptionName   :: !(Maybe Text)
    , _osResourceName :: !(Maybe Text)
    , _osNamespace    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'OptionSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osOptionName'
--
-- * 'osResourceName'
--
-- * 'osNamespace'
optionSpecification
    :: OptionSpecification
optionSpecification =
    OptionSpecification'
    { _osOptionName = Nothing
    , _osResourceName = Nothing
    , _osNamespace = Nothing
    }

-- | The name of the configuration option.
osOptionName :: Lens' OptionSpecification (Maybe Text)
osOptionName = lens _osOptionName (\ s a -> s{_osOptionName = a});

-- | A unique resource name for a time-based scaling configuration option.
osResourceName :: Lens' OptionSpecification (Maybe Text)
osResourceName = lens _osResourceName (\ s a -> s{_osResourceName = a});

-- | A unique namespace identifying the option\'s associated AWS resource.
osNamespace :: Lens' OptionSpecification (Maybe Text)
osNamespace = lens _osNamespace (\ s a -> s{_osNamespace = a});

instance Hashable OptionSpecification

instance ToQuery OptionSpecification where
        toQuery OptionSpecification'{..}
          = mconcat
              ["OptionName" =: _osOptionName,
               "ResourceName" =: _osResourceName,
               "Namespace" =: _osNamespace]

-- | Describes a queue.
--
-- /See:/ 'queue' smart constructor.
data Queue = Queue'
    { _qURL  :: !(Maybe Text)
    , _qName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Queue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qURL'
--
-- * 'qName'
queue
    :: Queue
queue =
    Queue'
    { _qURL = Nothing
    , _qName = Nothing
    }

-- | The URL of the queue.
qURL :: Lens' Queue (Maybe Text)
qURL = lens _qURL (\ s a -> s{_qURL = a});

-- | The name of the queue.
qName :: Lens' Queue (Maybe Text)
qName = lens _qName (\ s a -> s{_qName = a});

instance FromXML Queue where
        parseXML x
          = Queue' <$> (x .@? "URL") <*> (x .@? "Name")

instance Hashable Queue

-- | A specification of a location in Amazon S3.
--
-- /See:/ 's3Location' smart constructor.
data S3Location = S3Location'
    { _slS3Key    :: !(Maybe Text)
    , _slS3Bucket :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slS3Key'
--
-- * 'slS3Bucket'
s3Location
    :: S3Location
s3Location =
    S3Location'
    { _slS3Key = Nothing
    , _slS3Bucket = Nothing
    }

-- | The Amazon S3 key where the data is located.
slS3Key :: Lens' S3Location (Maybe Text)
slS3Key = lens _slS3Key (\ s a -> s{_slS3Key = a});

-- | The Amazon S3 bucket where the data is located.
slS3Bucket :: Lens' S3Location (Maybe Text)
slS3Bucket = lens _slS3Bucket (\ s a -> s{_slS3Bucket = a});

instance FromXML S3Location where
        parseXML x
          = S3Location' <$>
              (x .@? "S3Key") <*> (x .@? "S3Bucket")

instance Hashable S3Location

instance ToQuery S3Location where
        toQuery S3Location'{..}
          = mconcat
              ["S3Key" =: _slS3Key, "S3Bucket" =: _slS3Bucket]

-- | Represents health information from the specified instance that belongs
-- to the AWS Elastic Beanstalk environment. Use the 'InstanceId' property
-- to specify the application instance for which you\'d like to return
-- data.
--
-- /See:/ 'singleInstanceHealth' smart constructor.
data SingleInstanceHealth = SingleInstanceHealth'
    { _sihInstanceId         :: !(Maybe Text)
    , _sihCauses             :: !(Maybe [Text])
    , _sihSystem             :: !(Maybe SystemStatus)
    , _sihApplicationMetrics :: !(Maybe ApplicationMetrics)
    , _sihColor              :: !(Maybe Text)
    , _sihHealthStatus       :: !(Maybe Text)
    , _sihLaunchedAt         :: !(Maybe ISO8601)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SingleInstanceHealth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sihInstanceId'
--
-- * 'sihCauses'
--
-- * 'sihSystem'
--
-- * 'sihApplicationMetrics'
--
-- * 'sihColor'
--
-- * 'sihHealthStatus'
--
-- * 'sihLaunchedAt'
singleInstanceHealth
    :: SingleInstanceHealth
singleInstanceHealth =
    SingleInstanceHealth'
    { _sihInstanceId = Nothing
    , _sihCauses = Nothing
    , _sihSystem = Nothing
    , _sihApplicationMetrics = Nothing
    , _sihColor = Nothing
    , _sihHealthStatus = Nothing
    , _sihLaunchedAt = Nothing
    }

-- | The ID of the Amazon EC2 instance.
sihInstanceId :: Lens' SingleInstanceHealth (Maybe Text)
sihInstanceId = lens _sihInstanceId (\ s a -> s{_sihInstanceId = a});

-- | Represents the causes, which provide more information about the current
-- health status.
sihCauses :: Lens' SingleInstanceHealth [Text]
sihCauses = lens _sihCauses (\ s a -> s{_sihCauses = a}) . _Default . _Coerce;

-- | Undocumented member.
sihSystem :: Lens' SingleInstanceHealth (Maybe SystemStatus)
sihSystem = lens _sihSystem (\ s a -> s{_sihSystem = a});

-- | Undocumented member.
sihApplicationMetrics :: Lens' SingleInstanceHealth (Maybe ApplicationMetrics)
sihApplicationMetrics = lens _sihApplicationMetrics (\ s a -> s{_sihApplicationMetrics = a});

-- | Represents the color indicator that gives you information about the
-- health of the EC2 instance. For more information, see
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses>.
sihColor :: Lens' SingleInstanceHealth (Maybe Text)
sihColor = lens _sihColor (\ s a -> s{_sihColor = a});

-- | Returns the health status of the specified instance. For more
-- information, see
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses>.
sihHealthStatus :: Lens' SingleInstanceHealth (Maybe Text)
sihHealthStatus = lens _sihHealthStatus (\ s a -> s{_sihHealthStatus = a});

-- | The time at which the EC2 instance was launched.
sihLaunchedAt :: Lens' SingleInstanceHealth (Maybe UTCTime)
sihLaunchedAt = lens _sihLaunchedAt (\ s a -> s{_sihLaunchedAt = a}) . mapping _Time;

instance FromXML SingleInstanceHealth where
        parseXML x
          = SingleInstanceHealth' <$>
              (x .@? "InstanceId") <*>
                (x .@? "Causes" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "System")
                <*> (x .@? "ApplicationMetrics")
                <*> (x .@? "Color")
                <*> (x .@? "HealthStatus")
                <*> (x .@? "LaunchedAt")

instance Hashable SingleInstanceHealth

-- | Describes the solution stack.
--
-- /See:/ 'solutionStackDescription' smart constructor.
data SolutionStackDescription = SolutionStackDescription'
    { _ssdPermittedFileTypes :: !(Maybe [Text])
    , _ssdSolutionStackName  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SolutionStackDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssdPermittedFileTypes'
--
-- * 'ssdSolutionStackName'
solutionStackDescription
    :: SolutionStackDescription
solutionStackDescription =
    SolutionStackDescription'
    { _ssdPermittedFileTypes = Nothing
    , _ssdSolutionStackName = Nothing
    }

-- | The permitted file types allowed for a solution stack.
ssdPermittedFileTypes :: Lens' SolutionStackDescription [Text]
ssdPermittedFileTypes = lens _ssdPermittedFileTypes (\ s a -> s{_ssdPermittedFileTypes = a}) . _Default . _Coerce;

-- | The name of the solution stack.
ssdSolutionStackName :: Lens' SolutionStackDescription (Maybe Text)
ssdSolutionStackName = lens _ssdSolutionStackName (\ s a -> s{_ssdSolutionStackName = a});

instance FromXML SolutionStackDescription where
        parseXML x
          = SolutionStackDescription' <$>
              (x .@? "PermittedFileTypes" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "SolutionStackName")

instance Hashable SolutionStackDescription

-- | A specification for an environment configuration
--
-- /See:/ 'sourceConfiguration' smart constructor.
data SourceConfiguration = SourceConfiguration'
    { _scTemplateName    :: !(Maybe Text)
    , _scApplicationName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SourceConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scTemplateName'
--
-- * 'scApplicationName'
sourceConfiguration
    :: SourceConfiguration
sourceConfiguration =
    SourceConfiguration'
    { _scTemplateName = Nothing
    , _scApplicationName = Nothing
    }

-- | The name of the configuration template.
scTemplateName :: Lens' SourceConfiguration (Maybe Text)
scTemplateName = lens _scTemplateName (\ s a -> s{_scTemplateName = a});

-- | The name of the application associated with the configuration.
scApplicationName :: Lens' SourceConfiguration (Maybe Text)
scApplicationName = lens _scApplicationName (\ s a -> s{_scApplicationName = a});

instance Hashable SourceConfiguration

instance ToQuery SourceConfiguration where
        toQuery SourceConfiguration'{..}
          = mconcat
              ["TemplateName" =: _scTemplateName,
               "ApplicationName" =: _scApplicationName]

-- | Represents the percentage of requests over the last 10 seconds that
-- resulted in each type of status code response. For more information, see
-- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html Status Code Definitions>.
--
-- /See:/ 'statusCodes' smart constructor.
data StatusCodes = StatusCodes'
    { _scStatus2xx :: !(Maybe Int)
    , _scStatus3xx :: !(Maybe Int)
    , _scStatus4xx :: !(Maybe Int)
    , _scStatus5xx :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'StatusCodes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scStatus2xx'
--
-- * 'scStatus3xx'
--
-- * 'scStatus4xx'
--
-- * 'scStatus5xx'
statusCodes
    :: StatusCodes
statusCodes =
    StatusCodes'
    { _scStatus2xx = Nothing
    , _scStatus3xx = Nothing
    , _scStatus4xx = Nothing
    , _scStatus5xx = Nothing
    }

-- | The percentage of requests over the last 10 seconds that resulted in a
-- 2xx (200, 201, etc.) status code.
scStatus2xx :: Lens' StatusCodes (Maybe Int)
scStatus2xx = lens _scStatus2xx (\ s a -> s{_scStatus2xx = a});

-- | The percentage of requests over the last 10 seconds that resulted in a
-- 3xx (300, 301, etc.) status code.
scStatus3xx :: Lens' StatusCodes (Maybe Int)
scStatus3xx = lens _scStatus3xx (\ s a -> s{_scStatus3xx = a});

-- | The percentage of requests over the last 10 seconds that resulted in a
-- 4xx (400, 401, etc.) status code.
scStatus4xx :: Lens' StatusCodes (Maybe Int)
scStatus4xx = lens _scStatus4xx (\ s a -> s{_scStatus4xx = a});

-- | The percentage of requests over the last 10 seconds that resulted in a
-- 5xx (500, 501, etc.) status code.
scStatus5xx :: Lens' StatusCodes (Maybe Int)
scStatus5xx = lens _scStatus5xx (\ s a -> s{_scStatus5xx = a});

instance FromXML StatusCodes where
        parseXML x
          = StatusCodes' <$>
              (x .@? "Status2xx") <*> (x .@? "Status3xx") <*>
                (x .@? "Status4xx")
                <*> (x .@? "Status5xx")

instance Hashable StatusCodes

-- | Represents CPU utilization and load average information for applications
-- running in the specified environment.
--
-- /See:/ 'systemStatus' smart constructor.
data SystemStatus = SystemStatus'
    { _ssCPUUtilization :: !(Maybe CPUUtilization)
    , _ssLoadAverage    :: !(Maybe [Double])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SystemStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssCPUUtilization'
--
-- * 'ssLoadAverage'
systemStatus
    :: SystemStatus
systemStatus =
    SystemStatus'
    { _ssCPUUtilization = Nothing
    , _ssLoadAverage = Nothing
    }

-- | Undocumented member.
ssCPUUtilization :: Lens' SystemStatus (Maybe CPUUtilization)
ssCPUUtilization = lens _ssCPUUtilization (\ s a -> s{_ssCPUUtilization = a});

-- | Load average in the last 1-minute and 5-minute periods. For more
-- information, see
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-metrics.html#health-enhanced-metrics-os Operating System Metrics>.
ssLoadAverage :: Lens' SystemStatus [Double]
ssLoadAverage = lens _ssLoadAverage (\ s a -> s{_ssLoadAverage = a}) . _Default . _Coerce;

instance FromXML SystemStatus where
        parseXML x
          = SystemStatus' <$>
              (x .@? "CPUUtilization") <*>
                (x .@? "LoadAverage" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable SystemStatus

-- | Describes a tag applied to a resource in an environment.
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
    { _tagValue :: !(Maybe Text)
    , _tagKey   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue'
--
-- * 'tagKey'
tag
    :: Tag
tag =
    Tag'
    { _tagValue = Nothing
    , _tagKey = Nothing
    }

-- | The value of the tag.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | The key of the tag.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance Hashable Tag

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat ["Value" =: _tagValue, "Key" =: _tagKey]

-- | Describes a trigger.
--
-- /See:/ 'trigger' smart constructor.
newtype Trigger = Trigger'
    { _tName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Trigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tName'
trigger
    :: Trigger
trigger =
    Trigger'
    { _tName = Nothing
    }

-- | The name of the trigger.
tName :: Lens' Trigger (Maybe Text)
tName = lens _tName (\ s a -> s{_tName = a});

instance FromXML Trigger where
        parseXML x = Trigger' <$> (x .@? "Name")

instance Hashable Trigger

-- | An error or warning for a desired configuration option value.
--
-- /See:/ 'validationMessage' smart constructor.
data ValidationMessage = ValidationMessage'
    { _vmOptionName :: !(Maybe Text)
    , _vmSeverity   :: !(Maybe ValidationSeverity)
    , _vmNamespace  :: !(Maybe Text)
    , _vmMessage    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ValidationMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vmOptionName'
--
-- * 'vmSeverity'
--
-- * 'vmNamespace'
--
-- * 'vmMessage'
validationMessage
    :: ValidationMessage
validationMessage =
    ValidationMessage'
    { _vmOptionName = Nothing
    , _vmSeverity = Nothing
    , _vmNamespace = Nothing
    , _vmMessage = Nothing
    }

-- |
vmOptionName :: Lens' ValidationMessage (Maybe Text)
vmOptionName = lens _vmOptionName (\ s a -> s{_vmOptionName = a});

-- | An indication of the severity of this message:
--
-- -   'error': This message indicates that this is not a valid setting for
--     an option.
-- -   'warning': This message is providing information you should take
--     into account.
vmSeverity :: Lens' ValidationMessage (Maybe ValidationSeverity)
vmSeverity = lens _vmSeverity (\ s a -> s{_vmSeverity = a});

-- |
vmNamespace :: Lens' ValidationMessage (Maybe Text)
vmNamespace = lens _vmNamespace (\ s a -> s{_vmNamespace = a});

-- | A message describing the error or warning.
vmMessage :: Lens' ValidationMessage (Maybe Text)
vmMessage = lens _vmMessage (\ s a -> s{_vmMessage = a});

instance FromXML ValidationMessage where
        parseXML x
          = ValidationMessage' <$>
              (x .@? "OptionName") <*> (x .@? "Severity") <*>
                (x .@? "Namespace")
                <*> (x .@? "Message")

instance Hashable ValidationMessage
