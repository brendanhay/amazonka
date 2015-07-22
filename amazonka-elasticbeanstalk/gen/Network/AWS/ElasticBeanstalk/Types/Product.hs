{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.Product where

import           Network.AWS.ElasticBeanstalk.Types.Sum
import           Network.AWS.Prelude

-- | Describes the properties of an application.
--
-- /See:/ 'applicationDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'adDateUpdated'
--
-- * 'adVersions'
--
-- * 'adDateCreated'
--
-- * 'adApplicationName'
--
-- * 'adConfigurationTemplates'
--
-- * 'adDescription'
data ApplicationDescription = ApplicationDescription'
    { _adDateUpdated            :: !(Maybe ISO8601)
    , _adVersions               :: !(Maybe [Text])
    , _adDateCreated            :: !(Maybe ISO8601)
    , _adApplicationName        :: !(Maybe Text)
    , _adConfigurationTemplates :: !(Maybe [Text])
    , _adDescription            :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ApplicationDescription' smart constructor.
applicationDescription :: ApplicationDescription
applicationDescription =
    ApplicationDescription'
    { _adDateUpdated = Nothing
    , _adVersions = Nothing
    , _adDateCreated = Nothing
    , _adApplicationName = Nothing
    , _adConfigurationTemplates = Nothing
    , _adDescription = Nothing
    }

-- | The date when the application was last modified.
adDateUpdated :: Lens' ApplicationDescription (Maybe UTCTime)
adDateUpdated = lens _adDateUpdated (\ s a -> s{_adDateUpdated = a}) . mapping _Time;

-- | The names of the versions for this application.
adVersions :: Lens' ApplicationDescription [Text]
adVersions = lens _adVersions (\ s a -> s{_adVersions = a}) . _Default;

-- | The date when the application was created.
adDateCreated :: Lens' ApplicationDescription (Maybe UTCTime)
adDateCreated = lens _adDateCreated (\ s a -> s{_adDateCreated = a}) . mapping _Time;

-- | The name of the application.
adApplicationName :: Lens' ApplicationDescription (Maybe Text)
adApplicationName = lens _adApplicationName (\ s a -> s{_adApplicationName = a});

-- | The names of the configuration templates associated with this
-- application.
adConfigurationTemplates :: Lens' ApplicationDescription [Text]
adConfigurationTemplates = lens _adConfigurationTemplates (\ s a -> s{_adConfigurationTemplates = a}) . _Default;

-- | User-defined description of the application.
adDescription :: Lens' ApplicationDescription (Maybe Text)
adDescription = lens _adDescription (\ s a -> s{_adDescription = a});

instance FromXML ApplicationDescription where
        parseXML x
          = ApplicationDescription' <$>
              (x .@? "DateUpdated") <*>
                (x .@? "Versions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "DateCreated")
                <*> (x .@? "ApplicationName")
                <*>
                (x .@? "ConfigurationTemplates" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "Description")

-- | Result message containing a single description of an application.
--
-- /See:/ 'applicationDescriptionMessage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'admApplication'
newtype ApplicationDescriptionMessage = ApplicationDescriptionMessage'
    { _admApplication :: Maybe ApplicationDescription
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ApplicationDescriptionMessage' smart constructor.
applicationDescriptionMessage :: ApplicationDescriptionMessage
applicationDescriptionMessage =
    ApplicationDescriptionMessage'
    { _admApplication = Nothing
    }

-- | The ApplicationDescription of the application.
admApplication :: Lens' ApplicationDescriptionMessage (Maybe ApplicationDescription)
admApplication = lens _admApplication (\ s a -> s{_admApplication = a});

instance FromXML ApplicationDescriptionMessage where
        parseXML x
          = ApplicationDescriptionMessage' <$>
              (x .@? "Application")

-- | Describes the properties of an application version.
--
-- /See:/ 'applicationVersionDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avdDateUpdated'
--
-- * 'avdSourceBundle'
--
-- * 'avdVersionLabel'
--
-- * 'avdDateCreated'
--
-- * 'avdApplicationName'
--
-- * 'avdDescription'
data ApplicationVersionDescription = ApplicationVersionDescription'
    { _avdDateUpdated     :: !(Maybe ISO8601)
    , _avdSourceBundle    :: !(Maybe S3Location)
    , _avdVersionLabel    :: !(Maybe Text)
    , _avdDateCreated     :: !(Maybe ISO8601)
    , _avdApplicationName :: !(Maybe Text)
    , _avdDescription     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ApplicationVersionDescription' smart constructor.
applicationVersionDescription :: ApplicationVersionDescription
applicationVersionDescription =
    ApplicationVersionDescription'
    { _avdDateUpdated = Nothing
    , _avdSourceBundle = Nothing
    , _avdVersionLabel = Nothing
    , _avdDateCreated = Nothing
    , _avdApplicationName = Nothing
    , _avdDescription = Nothing
    }

-- | The last modified date of the application version.
avdDateUpdated :: Lens' ApplicationVersionDescription (Maybe UTCTime)
avdDateUpdated = lens _avdDateUpdated (\ s a -> s{_avdDateUpdated = a}) . mapping _Time;

-- | The location where the source bundle is located for this version.
avdSourceBundle :: Lens' ApplicationVersionDescription (Maybe S3Location)
avdSourceBundle = lens _avdSourceBundle (\ s a -> s{_avdSourceBundle = a});

-- | A label uniquely identifying the version for the associated application.
avdVersionLabel :: Lens' ApplicationVersionDescription (Maybe Text)
avdVersionLabel = lens _avdVersionLabel (\ s a -> s{_avdVersionLabel = a});

-- | The creation date of the application version.
avdDateCreated :: Lens' ApplicationVersionDescription (Maybe UTCTime)
avdDateCreated = lens _avdDateCreated (\ s a -> s{_avdDateCreated = a}) . mapping _Time;

-- | The name of the application associated with this release.
avdApplicationName :: Lens' ApplicationVersionDescription (Maybe Text)
avdApplicationName = lens _avdApplicationName (\ s a -> s{_avdApplicationName = a});

-- | The description of this application version.
avdDescription :: Lens' ApplicationVersionDescription (Maybe Text)
avdDescription = lens _avdDescription (\ s a -> s{_avdDescription = a});

instance FromXML ApplicationVersionDescription where
        parseXML x
          = ApplicationVersionDescription' <$>
              (x .@? "DateUpdated") <*> (x .@? "SourceBundle") <*>
                (x .@? "VersionLabel")
                <*> (x .@? "DateCreated")
                <*> (x .@? "ApplicationName")
                <*> (x .@? "Description")

-- | Result message wrapping a single description of an application version.
--
-- /See:/ 'applicationVersionDescriptionMessage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avdmApplicationVersion'
newtype ApplicationVersionDescriptionMessage = ApplicationVersionDescriptionMessage'
    { _avdmApplicationVersion :: Maybe ApplicationVersionDescription
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ApplicationVersionDescriptionMessage' smart constructor.
applicationVersionDescriptionMessage :: ApplicationVersionDescriptionMessage
applicationVersionDescriptionMessage =
    ApplicationVersionDescriptionMessage'
    { _avdmApplicationVersion = Nothing
    }

-- | The ApplicationVersionDescription of the application version.
avdmApplicationVersion :: Lens' ApplicationVersionDescriptionMessage (Maybe ApplicationVersionDescription)
avdmApplicationVersion = lens _avdmApplicationVersion (\ s a -> s{_avdmApplicationVersion = a});

instance FromXML ApplicationVersionDescriptionMessage
         where
        parseXML x
          = ApplicationVersionDescriptionMessage' <$>
              (x .@? "ApplicationVersion")

-- | Describes an Auto Scaling launch configuration.
--
-- /See:/ 'autoScalingGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asgName'
newtype AutoScalingGroup = AutoScalingGroup'
    { _asgName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AutoScalingGroup' smart constructor.
autoScalingGroup :: AutoScalingGroup
autoScalingGroup =
    AutoScalingGroup'
    { _asgName = Nothing
    }

-- | The name of the @AutoScalingGroup@ .
asgName :: Lens' AutoScalingGroup (Maybe Text)
asgName = lens _asgName (\ s a -> s{_asgName = a});

instance FromXML AutoScalingGroup where
        parseXML x = AutoScalingGroup' <$> (x .@? "Name")

-- | Describes the possible values for a configuration option.
--
-- /See:/ 'configurationOptionDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'codMaxValue'
--
-- * 'codRegex'
--
-- * 'codUserDefined'
--
-- * 'codMaxLength'
--
-- * 'codValueOptions'
--
-- * 'codNamespace'
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
data ConfigurationOptionDescription = ConfigurationOptionDescription'
    { _codMaxValue       :: !(Maybe Int)
    , _codRegex          :: !(Maybe OptionRestrictionRegex)
    , _codUserDefined    :: !(Maybe Bool)
    , _codMaxLength      :: !(Maybe Int)
    , _codValueOptions   :: !(Maybe [Text])
    , _codNamespace      :: !(Maybe Text)
    , _codName           :: !(Maybe Text)
    , _codChangeSeverity :: !(Maybe Text)
    , _codDefaultValue   :: !(Maybe Text)
    , _codValueType      :: !(Maybe ConfigurationOptionValueType)
    , _codMinValue       :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ConfigurationOptionDescription' smart constructor.
configurationOptionDescription :: ConfigurationOptionDescription
configurationOptionDescription =
    ConfigurationOptionDescription'
    { _codMaxValue = Nothing
    , _codRegex = Nothing
    , _codUserDefined = Nothing
    , _codMaxLength = Nothing
    , _codValueOptions = Nothing
    , _codNamespace = Nothing
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

-- | An indication of whether the user defined this configuration option:
--
-- @true@ : This configuration option was defined by the user. It is a
-- valid choice for specifying this as an Option to Remove when updating
-- configuration settings.
--
-- @false@ : This configuration was not defined by the user.
--
-- -   @true@ : This configuration option was defined by the user. It is a
--     valid choice for specifying if this as an @Option to Remove@ when
--     updating configuration settings.
--
-- -   @false@ : This configuration was not defined by the user.
--
-- Constraint: You can remove only @UserDefined@ options from a
-- configuration.
--
-- Valid Values: @true@ | @false@
codUserDefined :: Lens' ConfigurationOptionDescription (Maybe Bool)
codUserDefined = lens _codUserDefined (\ s a -> s{_codUserDefined = a});

-- | If specified, the configuration option must be a string value no longer
-- than this value.
codMaxLength :: Lens' ConfigurationOptionDescription (Maybe Int)
codMaxLength = lens _codMaxLength (\ s a -> s{_codMaxLength = a});

-- | If specified, values for the configuration option are selected from this
-- list.
codValueOptions :: Lens' ConfigurationOptionDescription [Text]
codValueOptions = lens _codValueOptions (\ s a -> s{_codValueOptions = a}) . _Default;

-- | A unique namespace identifying the option\'s associated AWS resource.
codNamespace :: Lens' ConfigurationOptionDescription (Maybe Text)
codNamespace = lens _codNamespace (\ s a -> s{_codNamespace = a});

-- | The name of the configuration option.
codName :: Lens' ConfigurationOptionDescription (Maybe Text)
codName = lens _codName (\ s a -> s{_codName = a});

-- | An indication of which action is required if the value for this
-- configuration option changes:
--
-- NoInterruption - There is no interruption to the environment or
-- application availability.
--
-- RestartEnvironment - The environment is restarted, all AWS resources are
-- deleted and recreated, and the environment is unavailable during the
-- process.
--
-- RestartApplicationServer - The environment is available the entire time.
-- However, a short application outage occurs when the application servers
-- on the running Amazon EC2 instances are restarted.
--
-- -   @NoInterruption@ : There is no interruption to the environment or
--     application availability.
-- -   @RestartEnvironment@ : The environment is entirely restarted, all
--     AWS resources are deleted and recreated, and the environment is
--     unavailable during the process.
-- -   @RestartApplicationServer@ : The environment is available the entire
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
-- @Scalar@ : Values for this option are a single selection from the
-- possible values, or a unformatted string or numeric value governed by
-- the MIN\/MAX\/Regex constraints:
--
-- @List@ : Values for this option are multiple selections of the possible
-- values.
--
-- @Boolean@ : Values for this option are either @true@ or @false@ .
--
-- -   @Scalar@ : Values for this option are a single selection from the
--     possible values, or an unformatted string, or numeric value governed
--     by the @MIN\/MAX\/Regex@ constraints.
-- -   @List@ : Values for this option are multiple selections from the
--     possible values.
-- -   @Boolean@ : Values for this option are either @true@ or @false@ .
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
                (x .@? "UserDefined")
                <*> (x .@? "MaxLength")
                <*>
                (x .@? "ValueOptions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "Namespace")
                <*> (x .@? "Name")
                <*> (x .@? "ChangeSeverity")
                <*> (x .@? "DefaultValue")
                <*> (x .@? "ValueType")
                <*> (x .@? "MinValue")

-- | A specification identifying an individual configuration option along
-- with its current value. For a list of possible option values, go to
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/command-options.html Option Values>
-- in the /AWS Elastic Beanstalk Developer Guide/.
--
-- /See:/ 'configurationOptionSetting' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cosOptionName'
--
-- * 'cosResourceName'
--
-- * 'cosValue'
--
-- * 'cosNamespace'
data ConfigurationOptionSetting = ConfigurationOptionSetting'
    { _cosOptionName   :: !(Maybe Text)
    , _cosResourceName :: !(Maybe Text)
    , _cosValue        :: !(Maybe Text)
    , _cosNamespace    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ConfigurationOptionSetting' smart constructor.
configurationOptionSetting :: ConfigurationOptionSetting
configurationOptionSetting =
    ConfigurationOptionSetting'
    { _cosOptionName = Nothing
    , _cosResourceName = Nothing
    , _cosValue = Nothing
    , _cosNamespace = Nothing
    }

-- | The name of the configuration option.
cosOptionName :: Lens' ConfigurationOptionSetting (Maybe Text)
cosOptionName = lens _cosOptionName (\ s a -> s{_cosOptionName = a});

-- | A unique resource name for a time-based scaling configuration option.
cosResourceName :: Lens' ConfigurationOptionSetting (Maybe Text)
cosResourceName = lens _cosResourceName (\ s a -> s{_cosResourceName = a});

-- | The current value for the configuration option.
cosValue :: Lens' ConfigurationOptionSetting (Maybe Text)
cosValue = lens _cosValue (\ s a -> s{_cosValue = a});

-- | A unique namespace identifying the option\'s associated AWS resource.
cosNamespace :: Lens' ConfigurationOptionSetting (Maybe Text)
cosNamespace = lens _cosNamespace (\ s a -> s{_cosNamespace = a});

instance FromXML ConfigurationOptionSetting where
        parseXML x
          = ConfigurationOptionSetting' <$>
              (x .@? "OptionName") <*> (x .@? "ResourceName") <*>
                (x .@? "Value")
                <*> (x .@? "Namespace")

instance ToQuery ConfigurationOptionSetting where
        toQuery ConfigurationOptionSetting'{..}
          = mconcat
              ["OptionName" =: _cosOptionName,
               "ResourceName" =: _cosResourceName,
               "Value" =: _cosValue, "Namespace" =: _cosNamespace]

-- | Describes the settings for a configuration set.
--
-- /See:/ 'configurationSettingsDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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

-- | 'ConfigurationSettingsDescription' smart constructor.
configurationSettingsDescription :: ConfigurationSettingsDescription
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

-- | If not @null@, the name of the configuration template for this
-- configuration set.
csdTemplateName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdTemplateName = lens _csdTemplateName (\ s a -> s{_csdTemplateName = a});

-- | A list of the configuration options and their values in this
-- configuration set.
csdOptionSettings :: Lens' ConfigurationSettingsDescription [ConfigurationOptionSetting]
csdOptionSettings = lens _csdOptionSettings (\ s a -> s{_csdOptionSettings = a}) . _Default;

-- | The date (in UTC time) when this configuration set was last modified.
csdDateUpdated :: Lens' ConfigurationSettingsDescription (Maybe UTCTime)
csdDateUpdated = lens _csdDateUpdated (\ s a -> s{_csdDateUpdated = a}) . mapping _Time;

-- | The date (in UTC time) when this configuration set was created.
csdDateCreated :: Lens' ConfigurationSettingsDescription (Maybe UTCTime)
csdDateCreated = lens _csdDateCreated (\ s a -> s{_csdDateCreated = a}) . mapping _Time;

-- | If not @null@, the name of the environment for this configuration set.
csdEnvironmentName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdEnvironmentName = lens _csdEnvironmentName (\ s a -> s{_csdEnvironmentName = a});

-- | The name of the application associated with this configuration set.
csdApplicationName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdApplicationName = lens _csdApplicationName (\ s a -> s{_csdApplicationName = a});

-- | If this configuration set is associated with an environment, the
-- @DeploymentStatus@ parameter indicates the deployment status of this
-- configuration set:
--
-- @null@: This configuration is not associated with a running environment.
--
-- @pending@: This is a draft configuration that is not deployed to the
-- associated environment but is in the process of deploying.
--
-- @deployed@: This is the configuration that is currently deployed to the
-- associated running environment.
--
-- @failed@: This is a draft configuration, that failed to successfully
-- deploy.
--
-- -   @null@: This configuration is not associated with a running
--     environment.
-- -   @pending@: This is a draft configuration that is not deployed to the
--     associated environment but is in the process of deploying.
-- -   @deployed@: This is the configuration that is currently deployed to
--     the associated running environment.
-- -   @failed@: This is a draft configuration that failed to successfully
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

-- | Describes the properties of an environment.
--
-- /See:/ 'environmentDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eCNAME'
--
-- * 'eStatus'
--
-- * 'eTemplateName'
--
-- * 'eAbortableOperationInProgress'
--
-- * 'eEndpointURL'
--
-- * 'eDateUpdated'
--
-- * 'eResources'
--
-- * 'eHealth'
--
-- * 'eVersionLabel'
--
-- * 'eDateCreated'
--
-- * 'eTier'
--
-- * 'eEnvironmentName'
--
-- * 'eApplicationName'
--
-- * 'eEnvironmentId'
--
-- * 'eSolutionStackName'
--
-- * 'eDescription'
data EnvironmentDescription = EnvironmentDescription'
    { _eCNAME                        :: !(Maybe Text)
    , _eStatus                       :: !(Maybe EnvironmentStatus)
    , _eTemplateName                 :: !(Maybe Text)
    , _eAbortableOperationInProgress :: !(Maybe Bool)
    , _eEndpointURL                  :: !(Maybe Text)
    , _eDateUpdated                  :: !(Maybe ISO8601)
    , _eResources                    :: !(Maybe EnvironmentResourcesDescription)
    , _eHealth                       :: !(Maybe EnvironmentHealth)
    , _eVersionLabel                 :: !(Maybe Text)
    , _eDateCreated                  :: !(Maybe ISO8601)
    , _eTier                         :: !(Maybe EnvironmentTier)
    , _eEnvironmentName              :: !(Maybe Text)
    , _eApplicationName              :: !(Maybe Text)
    , _eEnvironmentId                :: !(Maybe Text)
    , _eSolutionStackName            :: !(Maybe Text)
    , _eDescription                  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnvironmentDescription' smart constructor.
environmentDescription :: EnvironmentDescription
environmentDescription =
    EnvironmentDescription'
    { _eCNAME = Nothing
    , _eStatus = Nothing
    , _eTemplateName = Nothing
    , _eAbortableOperationInProgress = Nothing
    , _eEndpointURL = Nothing
    , _eDateUpdated = Nothing
    , _eResources = Nothing
    , _eHealth = Nothing
    , _eVersionLabel = Nothing
    , _eDateCreated = Nothing
    , _eTier = Nothing
    , _eEnvironmentName = Nothing
    , _eApplicationName = Nothing
    , _eEnvironmentId = Nothing
    , _eSolutionStackName = Nothing
    , _eDescription = Nothing
    }

-- | The URL to the CNAME for this environment.
eCNAME :: Lens' EnvironmentDescription (Maybe Text)
eCNAME = lens _eCNAME (\ s a -> s{_eCNAME = a});

-- | The current operational status of the environment:
--
-- -   @Launching@: Environment is in the process of initial deployment.
-- -   @Updating@: Environment is in the process of updating its
--     configuration settings or application version.
-- -   @Ready@: Environment is available to have an action performed on it,
--     such as update or terminate.
-- -   @Terminating@: Environment is in the shut-down process.
-- -   @Terminated@: Environment is not running.
eStatus :: Lens' EnvironmentDescription (Maybe EnvironmentStatus)
eStatus = lens _eStatus (\ s a -> s{_eStatus = a});

-- | The name of the configuration template used to originally launch this
-- environment.
eTemplateName :: Lens' EnvironmentDescription (Maybe Text)
eTemplateName = lens _eTemplateName (\ s a -> s{_eTemplateName = a});

-- | Indicates if there is an in-progress environment configuration update or
-- application version deployment that you can cancel.
--
-- @true:@ There is an update in progress.
--
-- @false:@ There are no updates currently in progress.
eAbortableOperationInProgress :: Lens' EnvironmentDescription (Maybe Bool)
eAbortableOperationInProgress = lens _eAbortableOperationInProgress (\ s a -> s{_eAbortableOperationInProgress = a});

-- | For load-balanced, autoscaling environments, the URL to the
-- LoadBalancer. For single-instance environments, the IP address of the
-- instance.
eEndpointURL :: Lens' EnvironmentDescription (Maybe Text)
eEndpointURL = lens _eEndpointURL (\ s a -> s{_eEndpointURL = a});

-- | The last modified date for this environment.
eDateUpdated :: Lens' EnvironmentDescription (Maybe UTCTime)
eDateUpdated = lens _eDateUpdated (\ s a -> s{_eDateUpdated = a}) . mapping _Time;

-- | The description of the AWS resources used by this environment.
eResources :: Lens' EnvironmentDescription (Maybe EnvironmentResourcesDescription)
eResources = lens _eResources (\ s a -> s{_eResources = a});

-- | Describes the health status of the environment. AWS Elastic Beanstalk
-- indicates the failure levels for a running environment:
--
-- @Red@ : Indicates the environment is not working.
--
-- @Yellow@: Indicates that something is wrong, the application might not
-- be available, but the instances appear running.
--
-- @Green@: Indicates the environment is healthy and fully functional.
--
-- -   @Red@: Indicates the environment is not responsive. Occurs when
--     three or more consecutive failures occur for an environment.
-- -   @Yellow@: Indicates that something is wrong. Occurs when two
--     consecutive failures occur for an environment.
-- -   @Green@: Indicates the environment is healthy and fully functional.
-- -   @Grey@: Default health for a new environment. The environment is not
--     fully launched and health checks have not started or health checks
--     are suspended during an @UpdateEnvironment@ or @RestartEnvironement@
--     request.
--
-- Default: @Grey@
eHealth :: Lens' EnvironmentDescription (Maybe EnvironmentHealth)
eHealth = lens _eHealth (\ s a -> s{_eHealth = a});

-- | The application version deployed in this environment.
eVersionLabel :: Lens' EnvironmentDescription (Maybe Text)
eVersionLabel = lens _eVersionLabel (\ s a -> s{_eVersionLabel = a});

-- | The creation date for this environment.
eDateCreated :: Lens' EnvironmentDescription (Maybe UTCTime)
eDateCreated = lens _eDateCreated (\ s a -> s{_eDateCreated = a}) . mapping _Time;

-- | Describes the current tier of this environment.
eTier :: Lens' EnvironmentDescription (Maybe EnvironmentTier)
eTier = lens _eTier (\ s a -> s{_eTier = a});

-- | The name of this environment.
eEnvironmentName :: Lens' EnvironmentDescription (Maybe Text)
eEnvironmentName = lens _eEnvironmentName (\ s a -> s{_eEnvironmentName = a});

-- | The name of the application associated with this environment.
eApplicationName :: Lens' EnvironmentDescription (Maybe Text)
eApplicationName = lens _eApplicationName (\ s a -> s{_eApplicationName = a});

-- | The ID of this environment.
eEnvironmentId :: Lens' EnvironmentDescription (Maybe Text)
eEnvironmentId = lens _eEnvironmentId (\ s a -> s{_eEnvironmentId = a});

-- | The name of the @SolutionStack@ deployed with this environment.
eSolutionStackName :: Lens' EnvironmentDescription (Maybe Text)
eSolutionStackName = lens _eSolutionStackName (\ s a -> s{_eSolutionStackName = a});

-- | Describes this environment.
eDescription :: Lens' EnvironmentDescription (Maybe Text)
eDescription = lens _eDescription (\ s a -> s{_eDescription = a});

instance FromXML EnvironmentDescription where
        parseXML x
          = EnvironmentDescription' <$>
              (x .@? "CNAME") <*> (x .@? "Status") <*>
                (x .@? "TemplateName")
                <*> (x .@? "AbortableOperationInProgress")
                <*> (x .@? "EndpointURL")
                <*> (x .@? "DateUpdated")
                <*> (x .@? "Resources")
                <*> (x .@? "Health")
                <*> (x .@? "VersionLabel")
                <*> (x .@? "DateCreated")
                <*> (x .@? "Tier")
                <*> (x .@? "EnvironmentName")
                <*> (x .@? "ApplicationName")
                <*> (x .@? "EnvironmentId")
                <*> (x .@? "SolutionStackName")
                <*> (x .@? "Description")

-- | The information retrieved from the Amazon EC2 instances.
--
-- /See:/ 'environmentInfoDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eidSampleTimestamp'
--
-- * 'eidEC2InstanceId'
--
-- * 'eidInfoType'
--
-- * 'eidMessage'
data EnvironmentInfoDescription = EnvironmentInfoDescription'
    { _eidSampleTimestamp :: !(Maybe ISO8601)
    , _eidEC2InstanceId   :: !(Maybe Text)
    , _eidInfoType        :: !(Maybe EnvironmentInfoType)
    , _eidMessage         :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnvironmentInfoDescription' smart constructor.
environmentInfoDescription :: EnvironmentInfoDescription
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

-- | Describes the AWS resources in use by this environment. This data is
-- live.
--
-- /See:/ 'environmentResourceDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'erdQueues'
--
-- * 'erdTriggers'
--
-- * 'erdLoadBalancers'
--
-- * 'erdInstances'
--
-- * 'erdEnvironmentName'
--
-- * 'erdLaunchConfigurations'
--
-- * 'erdAutoScalingGroups'
data EnvironmentResourceDescription = EnvironmentResourceDescription'
    { _erdQueues               :: !(Maybe [Queue])
    , _erdTriggers             :: !(Maybe [Trigger])
    , _erdLoadBalancers        :: !(Maybe [LoadBalancer])
    , _erdInstances            :: !(Maybe [Instance])
    , _erdEnvironmentName      :: !(Maybe Text)
    , _erdLaunchConfigurations :: !(Maybe [LaunchConfiguration])
    , _erdAutoScalingGroups    :: !(Maybe [AutoScalingGroup])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnvironmentResourceDescription' smart constructor.
environmentResourceDescription :: EnvironmentResourceDescription
environmentResourceDescription =
    EnvironmentResourceDescription'
    { _erdQueues = Nothing
    , _erdTriggers = Nothing
    , _erdLoadBalancers = Nothing
    , _erdInstances = Nothing
    , _erdEnvironmentName = Nothing
    , _erdLaunchConfigurations = Nothing
    , _erdAutoScalingGroups = Nothing
    }

-- | The queues used by this environment.
erdQueues :: Lens' EnvironmentResourceDescription [Queue]
erdQueues = lens _erdQueues (\ s a -> s{_erdQueues = a}) . _Default;

-- | The @AutoScaling@ triggers in use by this environment.
erdTriggers :: Lens' EnvironmentResourceDescription [Trigger]
erdTriggers = lens _erdTriggers (\ s a -> s{_erdTriggers = a}) . _Default;

-- | The LoadBalancers in use by this environment.
erdLoadBalancers :: Lens' EnvironmentResourceDescription [LoadBalancer]
erdLoadBalancers = lens _erdLoadBalancers (\ s a -> s{_erdLoadBalancers = a}) . _Default;

-- | The Amazon EC2 instances used by this environment.
erdInstances :: Lens' EnvironmentResourceDescription [Instance]
erdInstances = lens _erdInstances (\ s a -> s{_erdInstances = a}) . _Default;

-- | The name of the environment.
erdEnvironmentName :: Lens' EnvironmentResourceDescription (Maybe Text)
erdEnvironmentName = lens _erdEnvironmentName (\ s a -> s{_erdEnvironmentName = a});

-- | The Auto Scaling launch configurations in use by this environment.
erdLaunchConfigurations :: Lens' EnvironmentResourceDescription [LaunchConfiguration]
erdLaunchConfigurations = lens _erdLaunchConfigurations (\ s a -> s{_erdLaunchConfigurations = a}) . _Default;

-- | The @AutoScalingGroups@ used by this environment.
erdAutoScalingGroups :: Lens' EnvironmentResourceDescription [AutoScalingGroup]
erdAutoScalingGroups = lens _erdAutoScalingGroups (\ s a -> s{_erdAutoScalingGroups = a}) . _Default;

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
                <*>
                (x .@? "Instances" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "EnvironmentName")
                <*>
                (x .@? "LaunchConfigurations" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*>
                (x .@? "AutoScalingGroups" .!@ mempty >>=
                   may (parseXMLList "member"))

-- | Describes the AWS resources in use by this environment. This data is not
-- live data.
--
-- /See:/ 'environmentResourcesDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'erdLoadBalancer'
newtype EnvironmentResourcesDescription = EnvironmentResourcesDescription'
    { _erdLoadBalancer :: Maybe LoadBalancerDescription
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnvironmentResourcesDescription' smart constructor.
environmentResourcesDescription :: EnvironmentResourcesDescription
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

-- | Describes the properties of an environment tier
--
-- /See:/ 'environmentTier' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'etName'
--
-- * 'etVersion'
--
-- * 'etType'
data EnvironmentTier = EnvironmentTier'
    { _etName    :: !(Maybe Text)
    , _etVersion :: !(Maybe Text)
    , _etType    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnvironmentTier' smart constructor.
environmentTier :: EnvironmentTier
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

instance ToQuery EnvironmentTier where
        toQuery EnvironmentTier'{..}
          = mconcat
              ["Name" =: _etName, "Version" =: _etVersion,
               "Type" =: _etType]

-- | Describes an event.
--
-- /See:/ 'eventDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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

-- | 'EventDescription' smart constructor.
eventDescription :: EventDescription
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

-- | The description of an Amazon EC2 instance.
--
-- /See:/ 'instance'' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iId'
newtype Instance = Instance'
    { _iId :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Instance' smart constructor.
instance' :: Instance
instance' =
    Instance'
    { _iId = Nothing
    }

-- | The ID of the Amazon EC2 instance.
iId :: Lens' Instance (Maybe Text)
iId = lens _iId (\ s a -> s{_iId = a});

instance FromXML Instance where
        parseXML x = Instance' <$> (x .@? "Id")

-- | Describes an Auto Scaling launch configuration.
--
-- /See:/ 'launchConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcName'
newtype LaunchConfiguration = LaunchConfiguration'
    { _lcName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'LaunchConfiguration' smart constructor.
launchConfiguration :: LaunchConfiguration
launchConfiguration =
    LaunchConfiguration'
    { _lcName = Nothing
    }

-- | The name of the launch configuration.
lcName :: Lens' LaunchConfiguration (Maybe Text)
lcName = lens _lcName (\ s a -> s{_lcName = a});

instance FromXML LaunchConfiguration where
        parseXML x = LaunchConfiguration' <$> (x .@? "Name")

-- | Describes the properties of a Listener for the LoadBalancer.
--
-- /See:/ 'listener' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lProtocol'
--
-- * 'lPort'
data Listener = Listener'
    { _lProtocol :: !(Maybe Text)
    , _lPort     :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Listener' smart constructor.
listener :: Listener
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

-- | Describes a LoadBalancer.
--
-- /See:/ 'loadBalancer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbName'
newtype LoadBalancer = LoadBalancer'
    { _lbName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'LoadBalancer' smart constructor.
loadBalancer :: LoadBalancer
loadBalancer =
    LoadBalancer'
    { _lbName = Nothing
    }

-- | The name of the LoadBalancer.
lbName :: Lens' LoadBalancer (Maybe Text)
lbName = lens _lbName (\ s a -> s{_lbName = a});

instance FromXML LoadBalancer where
        parseXML x = LoadBalancer' <$> (x .@? "Name")

-- | Describes the details of a LoadBalancer.
--
-- /See:/ 'loadBalancerDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbdLoadBalancerName'
--
-- * 'lbdDomain'
--
-- * 'lbdListeners'
data LoadBalancerDescription = LoadBalancerDescription'
    { _lbdLoadBalancerName :: !(Maybe Text)
    , _lbdDomain           :: !(Maybe Text)
    , _lbdListeners        :: !(Maybe [Listener])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'LoadBalancerDescription' smart constructor.
loadBalancerDescription :: LoadBalancerDescription
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
lbdListeners = lens _lbdListeners (\ s a -> s{_lbdListeners = a}) . _Default;

instance FromXML LoadBalancerDescription where
        parseXML x
          = LoadBalancerDescription' <$>
              (x .@? "LoadBalancerName") <*> (x .@? "Domain") <*>
                (x .@? "Listeners" .!@ mempty >>=
                   may (parseXMLList "member"))

-- | A regular expression representing a restriction on a string
-- configuration option value.
--
-- /See:/ 'optionRestrictionRegex' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'orrPattern'
--
-- * 'orrLabel'
data OptionRestrictionRegex = OptionRestrictionRegex'
    { _orrPattern :: !(Maybe Text)
    , _orrLabel   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'OptionRestrictionRegex' smart constructor.
optionRestrictionRegex :: OptionRestrictionRegex
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

-- | A specification identifying an individual configuration option.
--
-- /See:/ 'optionSpecification' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'osOptionName'
--
-- * 'osResourceName'
--
-- * 'osNamespace'
data OptionSpecification = OptionSpecification'
    { _osOptionName   :: !(Maybe Text)
    , _osResourceName :: !(Maybe Text)
    , _osNamespace    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'OptionSpecification' smart constructor.
optionSpecification :: OptionSpecification
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

instance ToQuery OptionSpecification where
        toQuery OptionSpecification'{..}
          = mconcat
              ["OptionName" =: _osOptionName,
               "ResourceName" =: _osResourceName,
               "Namespace" =: _osNamespace]

-- | Describes a queue.
--
-- /See:/ 'queue' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'qURL'
--
-- * 'qName'
data Queue = Queue'
    { _qURL  :: !(Maybe Text)
    , _qName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Queue' smart constructor.
queue :: Queue
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

-- | A specification of a location in Amazon S3.
--
-- /See:/ 's3Location' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slS3Key'
--
-- * 'slS3Bucket'
data S3Location = S3Location'
    { _slS3Key    :: !(Maybe Text)
    , _slS3Bucket :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'S3Location' smart constructor.
s3Location :: S3Location
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

instance ToQuery S3Location where
        toQuery S3Location'{..}
          = mconcat
              ["S3Key" =: _slS3Key, "S3Bucket" =: _slS3Bucket]

-- | Describes the solution stack.
--
-- /See:/ 'solutionStackDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssdPermittedFileTypes'
--
-- * 'ssdSolutionStackName'
data SolutionStackDescription = SolutionStackDescription'
    { _ssdPermittedFileTypes :: !(Maybe [Text])
    , _ssdSolutionStackName  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SolutionStackDescription' smart constructor.
solutionStackDescription :: SolutionStackDescription
solutionStackDescription =
    SolutionStackDescription'
    { _ssdPermittedFileTypes = Nothing
    , _ssdSolutionStackName = Nothing
    }

-- | The permitted file types allowed for a solution stack.
ssdPermittedFileTypes :: Lens' SolutionStackDescription [Text]
ssdPermittedFileTypes = lens _ssdPermittedFileTypes (\ s a -> s{_ssdPermittedFileTypes = a}) . _Default;

-- | The name of the solution stack.
ssdSolutionStackName :: Lens' SolutionStackDescription (Maybe Text)
ssdSolutionStackName = lens _ssdSolutionStackName (\ s a -> s{_ssdSolutionStackName = a});

instance FromXML SolutionStackDescription where
        parseXML x
          = SolutionStackDescription' <$>
              (x .@? "PermittedFileTypes" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "SolutionStackName")

-- | A specification for an environment configuration
--
-- /See:/ 'sourceConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scTemplateName'
--
-- * 'scApplicationName'
data SourceConfiguration = SourceConfiguration'
    { _scTemplateName    :: !(Maybe Text)
    , _scApplicationName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SourceConfiguration' smart constructor.
sourceConfiguration :: SourceConfiguration
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

instance ToQuery SourceConfiguration where
        toQuery SourceConfiguration'{..}
          = mconcat
              ["TemplateName" =: _scTemplateName,
               "ApplicationName" =: _scApplicationName]

-- | Describes a tag applied to a resource in an environment.
--
-- /See:/ 'tag' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagValue'
--
-- * 'tagKey'
data Tag = Tag'
    { _tagValue :: !(Maybe Text)
    , _tagKey   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Tag' smart constructor.
tag :: Tag
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

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat ["Value" =: _tagValue, "Key" =: _tagKey]

-- | Describes a trigger.
--
-- /See:/ 'trigger' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tName'
newtype Trigger = Trigger'
    { _tName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Trigger' smart constructor.
trigger :: Trigger
trigger =
    Trigger'
    { _tName = Nothing
    }

-- | The name of the trigger.
tName :: Lens' Trigger (Maybe Text)
tName = lens _tName (\ s a -> s{_tName = a});

instance FromXML Trigger where
        parseXML x = Trigger' <$> (x .@? "Name")

-- | An error or warning for a desired configuration option value.
--
-- /See:/ 'validationMessage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vmOptionName'
--
-- * 'vmSeverity'
--
-- * 'vmNamespace'
--
-- * 'vmMessage'
data ValidationMessage = ValidationMessage'
    { _vmOptionName :: !(Maybe Text)
    , _vmSeverity   :: !(Maybe ValidationSeverity)
    , _vmNamespace  :: !(Maybe Text)
    , _vmMessage    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ValidationMessage' smart constructor.
validationMessage :: ValidationMessage
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
-- error: This message indicates that this is not a valid setting for an
-- option.
--
-- warning: This message is providing information you should take into
-- account.
--
-- -   error: This message indicates that this is not a valid setting for
--     an option.
-- -   warning: This message is providing information you should take into
--     account.
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
