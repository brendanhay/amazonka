{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.ElasticBeanstalk.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.ElasticBeanstalk.Types
    (
    -- * Service
      ElasticBeanstalk
    -- ** Errors
    , RESTError

    -- * ApplicationDescription
    , ApplicationDescription
    , applicationDescription
    , adDateUpdated
    , adVersions
    , adDateCreated
    , adConfigurationTemplates
    , adDescription
    , adApplicationName

    -- * ApplicationDescriptionMessage
    , ApplicationDescriptionMessage
    , applicationDescriptionMessage
    , admApplication

    -- * ApplicationVersionDescription
    , ApplicationVersionDescription
    , applicationVersionDescription
    , avdDateUpdated
    , avdSourceBundle
    , avdDateCreated
    , avdDescription
    , avdVersionLabel
    , avdApplicationName

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

    -- * ConfigurationOptionDescription
    , ConfigurationOptionDescription
    , configurationOptionDescription
    , codMaxValue
    , codRegex
    , codUserDefined
    , codMaxLength
    , codValueOptions
    , codNamespace
    , codName
    , codChangeSeverity
    , codDefaultValue
    , codValueType
    , codMinValue

    -- * ConfigurationOptionSetting
    , ConfigurationOptionSetting
    , configurationOptionSetting
    , cosOptionName
    , cosValue
    , cosNamespace
    , cosResourceName

    -- * ConfigurationOptionValueType
    , ConfigurationOptionValueType (..)

    -- * ConfigurationSettingsDescription
    , ConfigurationSettingsDescription
    , configurationSettingsDescription
    , csdOptionSettings
    , csdDateUpdated
    , csdDateCreated
    , csdDeploymentStatus
    , csdSolutionStackName
    , csdDescription
    , csdTemplateName
    , csdEnvironmentName
    , csdApplicationName

    -- * EnvironmentDescription
    , EnvironmentDescription
    , environmentDescription
    , envStatus
    , envAbortableOperationInProgress
    , envEndpointURL
    , envDateUpdated
    , envResources
    , envHealth
    , envDateCreated
    , envTier
    , envEnvironmentId
    , envSolutionStackName
    , envDescription
    , envCNAME
    , envTemplateName
    , envVersionLabel
    , envEnvironmentName
    , envApplicationName

    -- * EnvironmentHealth
    , EnvironmentHealth (..)

    -- * EnvironmentInfoDescription
    , EnvironmentInfoDescription
    , environmentInfoDescription
    , eidSampleTimestamp
    , eidEC2InstanceId
    , eidInfoType
    , eidMessage

    -- * EnvironmentInfoType
    , EnvironmentInfoType (..)

    -- * EnvironmentResourceDescription
    , EnvironmentResourceDescription
    , environmentResourceDescription
    , erdQueues
    , erdTriggers
    , erdLoadBalancers
    , erdInstances
    , erdLaunchConfigurations
    , erdAutoScalingGroups
    , erdEnvironmentName

    -- * EnvironmentResourcesDescription
    , EnvironmentResourcesDescription
    , environmentResourcesDescription
    , erdLoadBalancer

    -- * EnvironmentStatus
    , EnvironmentStatus (..)

    -- * EnvironmentTier
    , EnvironmentTier
    , environmentTier
    , etName
    , etVersion
    , etType

    -- * EventDescription
    , EventDescription
    , eventDescription
    , edRequestId
    , edSeverity
    , edEventDate
    , edMessage
    , edTemplateName
    , edVersionLabel
    , edEnvironmentName
    , edApplicationName

    -- * EventSeverity
    , EventSeverity (..)

    -- * Instance
    , Instance
    , instance'
    , insId

    -- * LaunchConfiguration
    , LaunchConfiguration
    , launchConfiguration
    , lcName

    -- * Listener
    , Listener
    , listener
    , lisProtocol
    , lisPort

    -- * LoadBalancer
    , LoadBalancer
    , loadBalancer
    , lbName

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
    , osOptionName
    , osNamespace
    , osResourceName

    -- * Queue
    , Queue
    , queue
    , queURL
    , queName

    -- * S3Location
    , S3Location
    , s3Location
    , slS3Key
    , slS3Bucket

    -- * SolutionStackDescription
    , SolutionStackDescription
    , solutionStackDescription
    , ssdPermittedFileTypes
    , ssdSolutionStackName

    -- * SourceConfiguration
    , SourceConfiguration
    , sourceConfiguration
    , scTemplateName
    , scApplicationName

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * Trigger
    , Trigger
    , trigger
    , triName

    -- * ValidationMessage
    , ValidationMessage
    , validationMessage
    , vmOptionName
    , vmSeverity
    , vmNamespace
    , vmMessage

    -- * ValidationSeverity
    , ValidationSeverity (..)
    ) where

import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | Version @2010-12-01@ of the Amazon Elastic Beanstalk SDK.
data ElasticBeanstalk

instance AWSService ElasticBeanstalk where
    type Sg ElasticBeanstalk = V4
    type Er ElasticBeanstalk = RESTError

    service = service'
      where
        service' :: Service ElasticBeanstalk
        service' = Service
            { _svcAbbrev  = "ElasticBeanstalk"
            , _svcPrefix  = "elasticbeanstalk"
            , _svcVersion = "2010-12-01"
            , _svcHandle  = handle
            , _svcRetry   = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError RESTError)
        handle = restError statusSuccess service'

        retry :: Retry ElasticBeanstalk
        retry = undefined

        check :: Status
              -> RESTError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e) = undefined

-- | /See:/ 'applicationDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'adDateUpdated'
--
-- * 'adVersions'
--
-- * 'adDateCreated'
--
-- * 'adConfigurationTemplates'
--
-- * 'adDescription'
--
-- * 'adApplicationName'
data ApplicationDescription = ApplicationDescription'{_adDateUpdated :: Maybe ISO8601, _adVersions :: [Text], _adDateCreated :: Maybe ISO8601, _adConfigurationTemplates :: [Text], _adDescription :: Maybe Text, _adApplicationName :: Text} deriving (Eq, Read, Show)

-- | 'ApplicationDescription' smart constructor.
applicationDescription :: Text -> ApplicationDescription
applicationDescription pApplicationName = ApplicationDescription'{_adDateUpdated = Nothing, _adVersions = mempty, _adDateCreated = Nothing, _adConfigurationTemplates = mempty, _adDescription = Nothing, _adApplicationName = pApplicationName};

-- | The date when the application was last modified.
adDateUpdated :: Lens' ApplicationDescription (Maybe UTCTime)
adDateUpdated = lens _adDateUpdated (\ s a -> s{_adDateUpdated = a}) . mapping _Time;

-- | The names of the versions for this application.
adVersions :: Lens' ApplicationDescription [Text]
adVersions = lens _adVersions (\ s a -> s{_adVersions = a});

-- | The date when the application was created.
adDateCreated :: Lens' ApplicationDescription (Maybe UTCTime)
adDateCreated = lens _adDateCreated (\ s a -> s{_adDateCreated = a}) . mapping _Time;

-- | The names of the configuration templates associated with this
-- application.
adConfigurationTemplates :: Lens' ApplicationDescription [Text]
adConfigurationTemplates = lens _adConfigurationTemplates (\ s a -> s{_adConfigurationTemplates = a});

-- | User-defined description of the application.
adDescription :: Lens' ApplicationDescription (Maybe Text)
adDescription = lens _adDescription (\ s a -> s{_adDescription = a});

-- | The name of the application.
adApplicationName :: Lens' ApplicationDescription Text
adApplicationName = lens _adApplicationName (\ s a -> s{_adApplicationName = a});

instance FromXML ApplicationDescription where
        parseXML x
          = ApplicationDescription' <$>
              x .@? "DateUpdated" <*>
                (x .@? "Versions" .!@ mempty >>=
                   parseXMLList "member")
                <*> x .@? "DateCreated"
                <*>
                (x .@? "ConfigurationTemplates" .!@ mempty >>=
                   parseXMLList "member")
                <*> x .@? "Description"
                <*> x .@ "ApplicationName"

-- | /See:/ 'applicationDescriptionMessage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'admApplication'
newtype ApplicationDescriptionMessage = ApplicationDescriptionMessage'{_admApplication :: Maybe ApplicationDescription} deriving (Eq, Read, Show)

-- | 'ApplicationDescriptionMessage' smart constructor.
applicationDescriptionMessage :: ApplicationDescriptionMessage
applicationDescriptionMessage = ApplicationDescriptionMessage'{_admApplication = Nothing};

-- | The ApplicationDescription of the application.
admApplication :: Lens' ApplicationDescriptionMessage (Maybe ApplicationDescription)
admApplication = lens _admApplication (\ s a -> s{_admApplication = a});

instance FromXML ApplicationDescriptionMessage where
        parseXML x
          = ApplicationDescriptionMessage' <$>
              x .@? "Application"

-- | /See:/ 'applicationVersionDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avdDateUpdated'
--
-- * 'avdSourceBundle'
--
-- * 'avdDateCreated'
--
-- * 'avdDescription'
--
-- * 'avdVersionLabel'
--
-- * 'avdApplicationName'
data ApplicationVersionDescription = ApplicationVersionDescription'{_avdDateUpdated :: Maybe ISO8601, _avdSourceBundle :: Maybe S3Location, _avdDateCreated :: Maybe ISO8601, _avdDescription :: Maybe Text, _avdVersionLabel :: Text, _avdApplicationName :: Text} deriving (Eq, Read, Show)

-- | 'ApplicationVersionDescription' smart constructor.
applicationVersionDescription :: Text -> Text -> ApplicationVersionDescription
applicationVersionDescription pVersionLabel pApplicationName = ApplicationVersionDescription'{_avdDateUpdated = Nothing, _avdSourceBundle = Nothing, _avdDateCreated = Nothing, _avdDescription = Nothing, _avdVersionLabel = pVersionLabel, _avdApplicationName = pApplicationName};

-- | The last modified date of the application version.
avdDateUpdated :: Lens' ApplicationVersionDescription (Maybe UTCTime)
avdDateUpdated = lens _avdDateUpdated (\ s a -> s{_avdDateUpdated = a}) . mapping _Time;

-- | The location where the source bundle is located for this version.
avdSourceBundle :: Lens' ApplicationVersionDescription (Maybe S3Location)
avdSourceBundle = lens _avdSourceBundle (\ s a -> s{_avdSourceBundle = a});

-- | The creation date of the application version.
avdDateCreated :: Lens' ApplicationVersionDescription (Maybe UTCTime)
avdDateCreated = lens _avdDateCreated (\ s a -> s{_avdDateCreated = a}) . mapping _Time;

-- | The description of this application version.
avdDescription :: Lens' ApplicationVersionDescription (Maybe Text)
avdDescription = lens _avdDescription (\ s a -> s{_avdDescription = a});

-- | A label uniquely identifying the version for the associated application.
avdVersionLabel :: Lens' ApplicationVersionDescription Text
avdVersionLabel = lens _avdVersionLabel (\ s a -> s{_avdVersionLabel = a});

-- | The name of the application associated with this release.
avdApplicationName :: Lens' ApplicationVersionDescription Text
avdApplicationName = lens _avdApplicationName (\ s a -> s{_avdApplicationName = a});

instance FromXML ApplicationVersionDescription where
        parseXML x
          = ApplicationVersionDescription' <$>
              x .@? "DateUpdated" <*> x .@? "SourceBundle" <*>
                x .@? "DateCreated"
                <*> x .@? "Description"
                <*> x .@ "VersionLabel"
                <*> x .@ "ApplicationName"

-- | /See:/ 'applicationVersionDescriptionMessage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avdmApplicationVersion'
newtype ApplicationVersionDescriptionMessage = ApplicationVersionDescriptionMessage'{_avdmApplicationVersion :: Maybe ApplicationVersionDescription} deriving (Eq, Read, Show)

-- | 'ApplicationVersionDescriptionMessage' smart constructor.
applicationVersionDescriptionMessage :: ApplicationVersionDescriptionMessage
applicationVersionDescriptionMessage = ApplicationVersionDescriptionMessage'{_avdmApplicationVersion = Nothing};

-- | The ApplicationVersionDescription of the application version.
avdmApplicationVersion :: Lens' ApplicationVersionDescriptionMessage (Maybe ApplicationVersionDescription)
avdmApplicationVersion = lens _avdmApplicationVersion (\ s a -> s{_avdmApplicationVersion = a});

instance FromXML ApplicationVersionDescriptionMessage
         where
        parseXML x
          = ApplicationVersionDescriptionMessage' <$>
              x .@? "ApplicationVersion"

-- | /See:/ 'autoScalingGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asgName'
newtype AutoScalingGroup = AutoScalingGroup'{_asgName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'AutoScalingGroup' smart constructor.
autoScalingGroup :: AutoScalingGroup
autoScalingGroup = AutoScalingGroup'{_asgName = Nothing};

-- | The name of the @AutoScalingGroup@ .
asgName :: Lens' AutoScalingGroup (Maybe Text)
asgName = lens _asgName (\ s a -> s{_asgName = a});

instance FromXML AutoScalingGroup where
        parseXML x = AutoScalingGroup' <$> x .@? "Name"

data ConfigurationDeploymentStatus = Pending | Deployed | Failed deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ConfigurationDeploymentStatus where
    parser = takeLowerText >>= \case
        "deployed" -> pure Deployed
        "failed" -> pure Failed
        "pending" -> pure Pending
        e -> fail ("Failure parsing ConfigurationDeploymentStatus from " ++ show e)

instance ToText ConfigurationDeploymentStatus where
    toText = \case
        Deployed -> "deployed"
        Failed -> "failed"
        Pending -> "pending"

instance Hashable ConfigurationDeploymentStatus
instance ToQuery ConfigurationDeploymentStatus
instance ToHeader ConfigurationDeploymentStatus

instance FromXML ConfigurationDeploymentStatus where
    parseXML = parseXMLText "ConfigurationDeploymentStatus"

-- | /See:/ 'configurationOptionDescription' smart constructor.
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
data ConfigurationOptionDescription = ConfigurationOptionDescription'{_codMaxValue :: Maybe Int, _codRegex :: Maybe OptionRestrictionRegex, _codUserDefined :: Maybe Bool, _codMaxLength :: Maybe Int, _codValueOptions :: [Text], _codNamespace :: Maybe Text, _codName :: Maybe Text, _codChangeSeverity :: Maybe Text, _codDefaultValue :: Maybe Text, _codValueType :: Maybe ConfigurationOptionValueType, _codMinValue :: Maybe Int} deriving (Eq, Read, Show)

-- | 'ConfigurationOptionDescription' smart constructor.
configurationOptionDescription :: ConfigurationOptionDescription
configurationOptionDescription = ConfigurationOptionDescription'{_codMaxValue = Nothing, _codRegex = Nothing, _codUserDefined = Nothing, _codMaxLength = Nothing, _codValueOptions = mempty, _codNamespace = Nothing, _codName = Nothing, _codChangeSeverity = Nothing, _codDefaultValue = Nothing, _codValueType = Nothing, _codMinValue = Nothing};

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
codValueOptions = lens _codValueOptions (\ s a -> s{_codValueOptions = a});

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
              x .@? "MaxValue" <*> x .@? "Regex" <*>
                x .@? "UserDefined"
                <*> x .@? "MaxLength"
                <*>
                (x .@? "ValueOptions" .!@ mempty >>=
                   parseXMLList "member")
                <*> x .@? "Namespace"
                <*> x .@? "Name"
                <*> x .@? "ChangeSeverity"
                <*> x .@? "DefaultValue"
                <*> x .@? "ValueType"
                <*> x .@? "MinValue"

-- | /See:/ 'configurationOptionSetting' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cosOptionName'
--
-- * 'cosValue'
--
-- * 'cosNamespace'
--
-- * 'cosResourceName'
data ConfigurationOptionSetting = ConfigurationOptionSetting'{_cosOptionName :: Maybe Text, _cosValue :: Maybe Text, _cosNamespace :: Maybe Text, _cosResourceName :: Text} deriving (Eq, Read, Show)

-- | 'ConfigurationOptionSetting' smart constructor.
configurationOptionSetting :: Text -> ConfigurationOptionSetting
configurationOptionSetting pResourceName = ConfigurationOptionSetting'{_cosOptionName = Nothing, _cosValue = Nothing, _cosNamespace = Nothing, _cosResourceName = pResourceName};

-- | The name of the configuration option.
cosOptionName :: Lens' ConfigurationOptionSetting (Maybe Text)
cosOptionName = lens _cosOptionName (\ s a -> s{_cosOptionName = a});

-- | The current value for the configuration option.
cosValue :: Lens' ConfigurationOptionSetting (Maybe Text)
cosValue = lens _cosValue (\ s a -> s{_cosValue = a});

-- | A unique namespace identifying the option\'s associated AWS resource.
cosNamespace :: Lens' ConfigurationOptionSetting (Maybe Text)
cosNamespace = lens _cosNamespace (\ s a -> s{_cosNamespace = a});

-- | A unique resource name for a time-based scaling configuration option.
cosResourceName :: Lens' ConfigurationOptionSetting Text
cosResourceName = lens _cosResourceName (\ s a -> s{_cosResourceName = a});

instance FromXML ConfigurationOptionSetting where
        parseXML x
          = ConfigurationOptionSetting' <$>
              x .@? "OptionName" <*> x .@? "Value" <*>
                x .@? "Namespace"
                <*> x .@ "ResourceName"

instance ToQuery ConfigurationOptionSetting where
        toQuery ConfigurationOptionSetting'{..}
          = mconcat
              ["OptionName" =: _cosOptionName,
               "Value" =: _cosValue, "Namespace" =: _cosNamespace,
               "ResourceName" =: _cosResourceName]

data ConfigurationOptionValueType = List | Scalar deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ConfigurationOptionValueType where
    parser = takeLowerText >>= \case
        "List" -> pure List
        "Scalar" -> pure Scalar
        e -> fail ("Failure parsing ConfigurationOptionValueType from " ++ show e)

instance ToText ConfigurationOptionValueType where
    toText = \case
        List -> "List"
        Scalar -> "Scalar"

instance Hashable ConfigurationOptionValueType
instance ToQuery ConfigurationOptionValueType
instance ToHeader ConfigurationOptionValueType

instance FromXML ConfigurationOptionValueType where
    parseXML = parseXMLText "ConfigurationOptionValueType"

-- | /See:/ 'configurationSettingsDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csdOptionSettings'
--
-- * 'csdDateUpdated'
--
-- * 'csdDateCreated'
--
-- * 'csdDeploymentStatus'
--
-- * 'csdSolutionStackName'
--
-- * 'csdDescription'
--
-- * 'csdTemplateName'
--
-- * 'csdEnvironmentName'
--
-- * 'csdApplicationName'
data ConfigurationSettingsDescription = ConfigurationSettingsDescription'{_csdOptionSettings :: [ConfigurationOptionSetting], _csdDateUpdated :: Maybe ISO8601, _csdDateCreated :: Maybe ISO8601, _csdDeploymentStatus :: Maybe ConfigurationDeploymentStatus, _csdSolutionStackName :: Maybe Text, _csdDescription :: Maybe Text, _csdTemplateName :: Text, _csdEnvironmentName :: Text, _csdApplicationName :: Text} deriving (Eq, Read, Show)

-- | 'ConfigurationSettingsDescription' smart constructor.
configurationSettingsDescription :: Text -> Text -> Text -> ConfigurationSettingsDescription
configurationSettingsDescription pTemplateName pEnvironmentName pApplicationName = ConfigurationSettingsDescription'{_csdOptionSettings = mempty, _csdDateUpdated = Nothing, _csdDateCreated = Nothing, _csdDeploymentStatus = Nothing, _csdSolutionStackName = Nothing, _csdDescription = Nothing, _csdTemplateName = pTemplateName, _csdEnvironmentName = pEnvironmentName, _csdApplicationName = pApplicationName};

-- | A list of the configuration options and their values in this
-- configuration set.
csdOptionSettings :: Lens' ConfigurationSettingsDescription [ConfigurationOptionSetting]
csdOptionSettings = lens _csdOptionSettings (\ s a -> s{_csdOptionSettings = a});

-- | The date (in UTC time) when this configuration set was last modified.
csdDateUpdated :: Lens' ConfigurationSettingsDescription (Maybe UTCTime)
csdDateUpdated = lens _csdDateUpdated (\ s a -> s{_csdDateUpdated = a}) . mapping _Time;

-- | The date (in UTC time) when this configuration set was created.
csdDateCreated :: Lens' ConfigurationSettingsDescription (Maybe UTCTime)
csdDateCreated = lens _csdDateCreated (\ s a -> s{_csdDateCreated = a}) . mapping _Time;

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

-- | If not @null@, the name of the configuration template for this
-- configuration set.
csdTemplateName :: Lens' ConfigurationSettingsDescription Text
csdTemplateName = lens _csdTemplateName (\ s a -> s{_csdTemplateName = a});

-- | If not @null@, the name of the environment for this configuration set.
csdEnvironmentName :: Lens' ConfigurationSettingsDescription Text
csdEnvironmentName = lens _csdEnvironmentName (\ s a -> s{_csdEnvironmentName = a});

-- | The name of the application associated with this configuration set.
csdApplicationName :: Lens' ConfigurationSettingsDescription Text
csdApplicationName = lens _csdApplicationName (\ s a -> s{_csdApplicationName = a});

instance FromXML ConfigurationSettingsDescription
         where
        parseXML x
          = ConfigurationSettingsDescription' <$>
              (x .@? "OptionSettings" .!@ mempty >>=
                 parseXMLList "member")
                <*> x .@? "DateUpdated"
                <*> x .@? "DateCreated"
                <*> x .@? "DeploymentStatus"
                <*> x .@? "SolutionStackName"
                <*> x .@? "Description"
                <*> x .@ "TemplateName"
                <*> x .@ "EnvironmentName"
                <*> x .@ "ApplicationName"

-- | /See:/ 'environmentDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'envStatus'
--
-- * 'envAbortableOperationInProgress'
--
-- * 'envEndpointURL'
--
-- * 'envDateUpdated'
--
-- * 'envResources'
--
-- * 'envHealth'
--
-- * 'envDateCreated'
--
-- * 'envTier'
--
-- * 'envEnvironmentId'
--
-- * 'envSolutionStackName'
--
-- * 'envDescription'
--
-- * 'envCNAME'
--
-- * 'envTemplateName'
--
-- * 'envVersionLabel'
--
-- * 'envEnvironmentName'
--
-- * 'envApplicationName'
data EnvironmentDescription = EnvironmentDescription'{_envStatus :: Maybe EnvironmentStatus, _envAbortableOperationInProgress :: Maybe Bool, _envEndpointURL :: Maybe Text, _envDateUpdated :: Maybe ISO8601, _envResources :: Maybe EnvironmentResourcesDescription, _envHealth :: Maybe EnvironmentHealth, _envDateCreated :: Maybe ISO8601, _envTier :: Maybe EnvironmentTier, _envEnvironmentId :: Maybe Text, _envSolutionStackName :: Maybe Text, _envDescription :: Maybe Text, _envCNAME :: Text, _envTemplateName :: Text, _envVersionLabel :: Text, _envEnvironmentName :: Text, _envApplicationName :: Text} deriving (Eq, Read, Show)

-- | 'EnvironmentDescription' smart constructor.
environmentDescription :: Text -> Text -> Text -> Text -> Text -> EnvironmentDescription
environmentDescription pCNAME pTemplateName pVersionLabel pEnvironmentName pApplicationName = EnvironmentDescription'{_envStatus = Nothing, _envAbortableOperationInProgress = Nothing, _envEndpointURL = Nothing, _envDateUpdated = Nothing, _envResources = Nothing, _envHealth = Nothing, _envDateCreated = Nothing, _envTier = Nothing, _envEnvironmentId = Nothing, _envSolutionStackName = Nothing, _envDescription = Nothing, _envCNAME = pCNAME, _envTemplateName = pTemplateName, _envVersionLabel = pVersionLabel, _envEnvironmentName = pEnvironmentName, _envApplicationName = pApplicationName};

-- | The current operational status of the environment:
--
-- -   @Launching@: Environment is in the process of initial deployment.
-- -   @Updating@: Environment is in the process of updating its
--     configuration settings or application version.
-- -   @Ready@: Environment is available to have an action performed on it,
--     such as update or terminate.
-- -   @Terminating@: Environment is in the shut-down process.
-- -   @Terminated@: Environment is not running.
envStatus :: Lens' EnvironmentDescription (Maybe EnvironmentStatus)
envStatus = lens _envStatus (\ s a -> s{_envStatus = a});

-- | Indicates if there is an in-progress environment configuration update or
-- application version deployment that you can cancel.
--
-- @true:@ There is an update in progress.
--
-- @false:@ There are no updates currently in progress.
envAbortableOperationInProgress :: Lens' EnvironmentDescription (Maybe Bool)
envAbortableOperationInProgress = lens _envAbortableOperationInProgress (\ s a -> s{_envAbortableOperationInProgress = a});

-- | For load-balanced, autoscaling environments, the URL to the
-- LoadBalancer. For single-instance environments, the IP address of the
-- instance.
envEndpointURL :: Lens' EnvironmentDescription (Maybe Text)
envEndpointURL = lens _envEndpointURL (\ s a -> s{_envEndpointURL = a});

-- | The last modified date for this environment.
envDateUpdated :: Lens' EnvironmentDescription (Maybe UTCTime)
envDateUpdated = lens _envDateUpdated (\ s a -> s{_envDateUpdated = a}) . mapping _Time;

-- | The description of the AWS resources used by this environment.
envResources :: Lens' EnvironmentDescription (Maybe EnvironmentResourcesDescription)
envResources = lens _envResources (\ s a -> s{_envResources = a});

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
envHealth :: Lens' EnvironmentDescription (Maybe EnvironmentHealth)
envHealth = lens _envHealth (\ s a -> s{_envHealth = a});

-- | The creation date for this environment.
envDateCreated :: Lens' EnvironmentDescription (Maybe UTCTime)
envDateCreated = lens _envDateCreated (\ s a -> s{_envDateCreated = a}) . mapping _Time;

-- | Describes the current tier of this environment.
envTier :: Lens' EnvironmentDescription (Maybe EnvironmentTier)
envTier = lens _envTier (\ s a -> s{_envTier = a});

-- | The ID of this environment.
envEnvironmentId :: Lens' EnvironmentDescription (Maybe Text)
envEnvironmentId = lens _envEnvironmentId (\ s a -> s{_envEnvironmentId = a});

-- | The name of the @SolutionStack@ deployed with this environment.
envSolutionStackName :: Lens' EnvironmentDescription (Maybe Text)
envSolutionStackName = lens _envSolutionStackName (\ s a -> s{_envSolutionStackName = a});

-- | Describes this environment.
envDescription :: Lens' EnvironmentDescription (Maybe Text)
envDescription = lens _envDescription (\ s a -> s{_envDescription = a});

-- | The URL to the CNAME for this environment.
envCNAME :: Lens' EnvironmentDescription Text
envCNAME = lens _envCNAME (\ s a -> s{_envCNAME = a});

-- | The name of the configuration template used to originally launch this
-- environment.
envTemplateName :: Lens' EnvironmentDescription Text
envTemplateName = lens _envTemplateName (\ s a -> s{_envTemplateName = a});

-- | The application version deployed in this environment.
envVersionLabel :: Lens' EnvironmentDescription Text
envVersionLabel = lens _envVersionLabel (\ s a -> s{_envVersionLabel = a});

-- | The name of this environment.
envEnvironmentName :: Lens' EnvironmentDescription Text
envEnvironmentName = lens _envEnvironmentName (\ s a -> s{_envEnvironmentName = a});

-- | The name of the application associated with this environment.
envApplicationName :: Lens' EnvironmentDescription Text
envApplicationName = lens _envApplicationName (\ s a -> s{_envApplicationName = a});

instance FromXML EnvironmentDescription where
        parseXML x
          = EnvironmentDescription' <$>
              x .@? "Status" <*>
                x .@? "AbortableOperationInProgress"
                <*> x .@? "EndpointURL"
                <*> x .@? "DateUpdated"
                <*> x .@? "Resources"
                <*> x .@? "Health"
                <*> x .@? "DateCreated"
                <*> x .@? "Tier"
                <*> x .@? "EnvironmentId"
                <*> x .@? "SolutionStackName"
                <*> x .@? "Description"
                <*> x .@ "CNAME"
                <*> x .@ "TemplateName"
                <*> x .@ "VersionLabel"
                <*> x .@ "EnvironmentName"
                <*> x .@ "ApplicationName"

data EnvironmentHealth = Red | Yellow | Green | Grey deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText EnvironmentHealth where
    parser = takeLowerText >>= \case
        "Green" -> pure Green
        "Grey" -> pure Grey
        "Red" -> pure Red
        "Yellow" -> pure Yellow
        e -> fail ("Failure parsing EnvironmentHealth from " ++ show e)

instance ToText EnvironmentHealth where
    toText = \case
        Green -> "Green"
        Grey -> "Grey"
        Red -> "Red"
        Yellow -> "Yellow"

instance Hashable EnvironmentHealth
instance ToQuery EnvironmentHealth
instance ToHeader EnvironmentHealth

instance FromXML EnvironmentHealth where
    parseXML = parseXMLText "EnvironmentHealth"

-- | /See:/ 'environmentInfoDescription' smart constructor.
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
data EnvironmentInfoDescription = EnvironmentInfoDescription'{_eidSampleTimestamp :: Maybe ISO8601, _eidEC2InstanceId :: Maybe Text, _eidInfoType :: Maybe EnvironmentInfoType, _eidMessage :: Maybe Text} deriving (Eq, Read, Show)

-- | 'EnvironmentInfoDescription' smart constructor.
environmentInfoDescription :: EnvironmentInfoDescription
environmentInfoDescription = EnvironmentInfoDescription'{_eidSampleTimestamp = Nothing, _eidEC2InstanceId = Nothing, _eidInfoType = Nothing, _eidMessage = Nothing};

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
              x .@? "SampleTimestamp" <*> x .@? "Ec2InstanceId" <*>
                x .@? "InfoType"
                <*> x .@? "Message"

data EnvironmentInfoType = Bundle | Tail deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText EnvironmentInfoType where
    parser = takeLowerText >>= \case
        "bundle" -> pure Bundle
        "tail" -> pure Tail
        e -> fail ("Failure parsing EnvironmentInfoType from " ++ show e)

instance ToText EnvironmentInfoType where
    toText = \case
        Bundle -> "bundle"
        Tail -> "tail"

instance Hashable EnvironmentInfoType
instance ToQuery EnvironmentInfoType
instance ToHeader EnvironmentInfoType

instance FromXML EnvironmentInfoType where
    parseXML = parseXMLText "EnvironmentInfoType"

-- | /See:/ 'environmentResourceDescription' smart constructor.
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
-- * 'erdLaunchConfigurations'
--
-- * 'erdAutoScalingGroups'
--
-- * 'erdEnvironmentName'
data EnvironmentResourceDescription = EnvironmentResourceDescription'{_erdQueues :: [Queue], _erdTriggers :: [Trigger], _erdLoadBalancers :: [LoadBalancer], _erdInstances :: [Instance], _erdLaunchConfigurations :: [LaunchConfiguration], _erdAutoScalingGroups :: [AutoScalingGroup], _erdEnvironmentName :: Text} deriving (Eq, Read, Show)

-- | 'EnvironmentResourceDescription' smart constructor.
environmentResourceDescription :: Text -> EnvironmentResourceDescription
environmentResourceDescription pEnvironmentName = EnvironmentResourceDescription'{_erdQueues = mempty, _erdTriggers = mempty, _erdLoadBalancers = mempty, _erdInstances = mempty, _erdLaunchConfigurations = mempty, _erdAutoScalingGroups = mempty, _erdEnvironmentName = pEnvironmentName};

-- | The queues used by this environment.
erdQueues :: Lens' EnvironmentResourceDescription [Queue]
erdQueues = lens _erdQueues (\ s a -> s{_erdQueues = a});

-- | The @AutoScaling@ triggers in use by this environment.
erdTriggers :: Lens' EnvironmentResourceDescription [Trigger]
erdTriggers = lens _erdTriggers (\ s a -> s{_erdTriggers = a});

-- | The LoadBalancers in use by this environment.
erdLoadBalancers :: Lens' EnvironmentResourceDescription [LoadBalancer]
erdLoadBalancers = lens _erdLoadBalancers (\ s a -> s{_erdLoadBalancers = a});

-- | The Amazon EC2 instances used by this environment.
erdInstances :: Lens' EnvironmentResourceDescription [Instance]
erdInstances = lens _erdInstances (\ s a -> s{_erdInstances = a});

-- | The Auto Scaling launch configurations in use by this environment.
erdLaunchConfigurations :: Lens' EnvironmentResourceDescription [LaunchConfiguration]
erdLaunchConfigurations = lens _erdLaunchConfigurations (\ s a -> s{_erdLaunchConfigurations = a});

-- | The @AutoScalingGroups@ used by this environment.
erdAutoScalingGroups :: Lens' EnvironmentResourceDescription [AutoScalingGroup]
erdAutoScalingGroups = lens _erdAutoScalingGroups (\ s a -> s{_erdAutoScalingGroups = a});

-- | The name of the environment.
erdEnvironmentName :: Lens' EnvironmentResourceDescription Text
erdEnvironmentName = lens _erdEnvironmentName (\ s a -> s{_erdEnvironmentName = a});

instance FromXML EnvironmentResourceDescription where
        parseXML x
          = EnvironmentResourceDescription' <$>
              (x .@? "Queues" .!@ mempty >>= parseXMLList "member")
                <*>
                (x .@? "Triggers" .!@ mempty >>=
                   parseXMLList "member")
                <*>
                (x .@? "LoadBalancers" .!@ mempty >>=
                   parseXMLList "member")
                <*>
                (x .@? "Instances" .!@ mempty >>=
                   parseXMLList "member")
                <*>
                (x .@? "LaunchConfigurations" .!@ mempty >>=
                   parseXMLList "member")
                <*>
                (x .@? "AutoScalingGroups" .!@ mempty >>=
                   parseXMLList "member")
                <*> x .@ "EnvironmentName"

-- | /See:/ 'environmentResourcesDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'erdLoadBalancer'
newtype EnvironmentResourcesDescription = EnvironmentResourcesDescription'{_erdLoadBalancer :: Maybe LoadBalancerDescription} deriving (Eq, Read, Show)

-- | 'EnvironmentResourcesDescription' smart constructor.
environmentResourcesDescription :: EnvironmentResourcesDescription
environmentResourcesDescription = EnvironmentResourcesDescription'{_erdLoadBalancer = Nothing};

-- | Describes the LoadBalancer.
erdLoadBalancer :: Lens' EnvironmentResourcesDescription (Maybe LoadBalancerDescription)
erdLoadBalancer = lens _erdLoadBalancer (\ s a -> s{_erdLoadBalancer = a});

instance FromXML EnvironmentResourcesDescription
         where
        parseXML x
          = EnvironmentResourcesDescription' <$>
              x .@? "LoadBalancer"

data EnvironmentStatus = Updating | Terminating | Launching | Terminated | Ready deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText EnvironmentStatus where
    parser = takeLowerText >>= \case
        "Launching" -> pure Launching
        "Ready" -> pure Ready
        "Terminated" -> pure Terminated
        "Terminating" -> pure Terminating
        "Updating" -> pure Updating
        e -> fail ("Failure parsing EnvironmentStatus from " ++ show e)

instance ToText EnvironmentStatus where
    toText = \case
        Launching -> "Launching"
        Ready -> "Ready"
        Terminated -> "Terminated"
        Terminating -> "Terminating"
        Updating -> "Updating"

instance Hashable EnvironmentStatus
instance ToQuery EnvironmentStatus
instance ToHeader EnvironmentStatus

instance FromXML EnvironmentStatus where
    parseXML = parseXMLText "EnvironmentStatus"

-- | /See:/ 'environmentTier' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'etName'
--
-- * 'etVersion'
--
-- * 'etType'
data EnvironmentTier = EnvironmentTier'{_etName :: Maybe Text, _etVersion :: Maybe Text, _etType :: Maybe Text} deriving (Eq, Read, Show)

-- | 'EnvironmentTier' smart constructor.
environmentTier :: EnvironmentTier
environmentTier = EnvironmentTier'{_etName = Nothing, _etVersion = Nothing, _etType = Nothing};

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
              x .@? "Name" <*> x .@? "Version" <*> x .@? "Type"

instance ToQuery EnvironmentTier where
        toQuery EnvironmentTier'{..}
          = mconcat
              ["Name" =: _etName, "Version" =: _etVersion,
               "Type" =: _etType]

-- | /See:/ 'eventDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'edRequestId'
--
-- * 'edSeverity'
--
-- * 'edEventDate'
--
-- * 'edMessage'
--
-- * 'edTemplateName'
--
-- * 'edVersionLabel'
--
-- * 'edEnvironmentName'
--
-- * 'edApplicationName'
data EventDescription = EventDescription'{_edRequestId :: Maybe Text, _edSeverity :: Maybe EventSeverity, _edEventDate :: Maybe ISO8601, _edMessage :: Maybe Text, _edTemplateName :: Text, _edVersionLabel :: Text, _edEnvironmentName :: Text, _edApplicationName :: Text} deriving (Eq, Read, Show)

-- | 'EventDescription' smart constructor.
eventDescription :: Text -> Text -> Text -> Text -> EventDescription
eventDescription pTemplateName pVersionLabel pEnvironmentName pApplicationName = EventDescription'{_edRequestId = Nothing, _edSeverity = Nothing, _edEventDate = Nothing, _edMessage = Nothing, _edTemplateName = pTemplateName, _edVersionLabel = pVersionLabel, _edEnvironmentName = pEnvironmentName, _edApplicationName = pApplicationName};

-- | The web service request ID for the activity of this event.
edRequestId :: Lens' EventDescription (Maybe Text)
edRequestId = lens _edRequestId (\ s a -> s{_edRequestId = a});

-- | The severity level of this event.
edSeverity :: Lens' EventDescription (Maybe EventSeverity)
edSeverity = lens _edSeverity (\ s a -> s{_edSeverity = a});

-- | The date when the event occurred.
edEventDate :: Lens' EventDescription (Maybe UTCTime)
edEventDate = lens _edEventDate (\ s a -> s{_edEventDate = a}) . mapping _Time;

-- | The event message.
edMessage :: Lens' EventDescription (Maybe Text)
edMessage = lens _edMessage (\ s a -> s{_edMessage = a});

-- | The name of the configuration associated with this event.
edTemplateName :: Lens' EventDescription Text
edTemplateName = lens _edTemplateName (\ s a -> s{_edTemplateName = a});

-- | The release label for the application version associated with this
-- event.
edVersionLabel :: Lens' EventDescription Text
edVersionLabel = lens _edVersionLabel (\ s a -> s{_edVersionLabel = a});

-- | The name of the environment associated with this event.
edEnvironmentName :: Lens' EventDescription Text
edEnvironmentName = lens _edEnvironmentName (\ s a -> s{_edEnvironmentName = a});

-- | The application associated with the event.
edApplicationName :: Lens' EventDescription Text
edApplicationName = lens _edApplicationName (\ s a -> s{_edApplicationName = a});

instance FromXML EventDescription where
        parseXML x
          = EventDescription' <$>
              x .@? "RequestId" <*> x .@? "Severity" <*>
                x .@? "EventDate"
                <*> x .@? "Message"
                <*> x .@ "TemplateName"
                <*> x .@ "VersionLabel"
                <*> x .@ "EnvironmentName"
                <*> x .@ "ApplicationName"

data EventSeverity = Error | Fatal | Debug | Warn | Info | Trace deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText EventSeverity where
    parser = takeLowerText >>= \case
        "DEBUG" -> pure Debug
        "ERROR" -> pure Error
        "FATAL" -> pure Fatal
        "INFO" -> pure Info
        "TRACE" -> pure Trace
        "WARN" -> pure Warn
        e -> fail ("Failure parsing EventSeverity from " ++ show e)

instance ToText EventSeverity where
    toText = \case
        Debug -> "DEBUG"
        Error -> "ERROR"
        Fatal -> "FATAL"
        Info -> "INFO"
        Trace -> "TRACE"
        Warn -> "WARN"

instance Hashable EventSeverity
instance ToQuery EventSeverity
instance ToHeader EventSeverity

instance FromXML EventSeverity where
    parseXML = parseXMLText "EventSeverity"

-- | /See:/ 'instance'' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'insId'
newtype Instance = Instance'{_insId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Instance' smart constructor.
instance' :: Instance
instance' = Instance'{_insId = Nothing};

-- | The ID of the Amazon EC2 instance.
insId :: Lens' Instance (Maybe Text)
insId = lens _insId (\ s a -> s{_insId = a});

instance FromXML Instance where
        parseXML x = Instance' <$> x .@? "Id"

-- | /See:/ 'launchConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcName'
newtype LaunchConfiguration = LaunchConfiguration'{_lcName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'LaunchConfiguration' smart constructor.
launchConfiguration :: LaunchConfiguration
launchConfiguration = LaunchConfiguration'{_lcName = Nothing};

-- | The name of the launch configuration.
lcName :: Lens' LaunchConfiguration (Maybe Text)
lcName = lens _lcName (\ s a -> s{_lcName = a});

instance FromXML LaunchConfiguration where
        parseXML x = LaunchConfiguration' <$> x .@? "Name"

-- | /See:/ 'listener' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lisProtocol'
--
-- * 'lisPort'
data Listener = Listener'{_lisProtocol :: Maybe Text, _lisPort :: Maybe Int} deriving (Eq, Read, Show)

-- | 'Listener' smart constructor.
listener :: Listener
listener = Listener'{_lisProtocol = Nothing, _lisPort = Nothing};

-- | The protocol that is used by the Listener.
lisProtocol :: Lens' Listener (Maybe Text)
lisProtocol = lens _lisProtocol (\ s a -> s{_lisProtocol = a});

-- | The port that is used by the Listener.
lisPort :: Lens' Listener (Maybe Int)
lisPort = lens _lisPort (\ s a -> s{_lisPort = a});

instance FromXML Listener where
        parseXML x
          = Listener' <$> x .@? "Protocol" <*> x .@? "Port"

-- | /See:/ 'loadBalancer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbName'
newtype LoadBalancer = LoadBalancer'{_lbName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'LoadBalancer' smart constructor.
loadBalancer :: LoadBalancer
loadBalancer = LoadBalancer'{_lbName = Nothing};

-- | The name of the LoadBalancer.
lbName :: Lens' LoadBalancer (Maybe Text)
lbName = lens _lbName (\ s a -> s{_lbName = a});

instance FromXML LoadBalancer where
        parseXML x = LoadBalancer' <$> x .@? "Name"

-- | /See:/ 'loadBalancerDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbdLoadBalancerName'
--
-- * 'lbdDomain'
--
-- * 'lbdListeners'
data LoadBalancerDescription = LoadBalancerDescription'{_lbdLoadBalancerName :: Maybe Text, _lbdDomain :: Maybe Text, _lbdListeners :: [Listener]} deriving (Eq, Read, Show)

-- | 'LoadBalancerDescription' smart constructor.
loadBalancerDescription :: LoadBalancerDescription
loadBalancerDescription = LoadBalancerDescription'{_lbdLoadBalancerName = Nothing, _lbdDomain = Nothing, _lbdListeners = mempty};

-- | The name of the LoadBalancer.
lbdLoadBalancerName :: Lens' LoadBalancerDescription (Maybe Text)
lbdLoadBalancerName = lens _lbdLoadBalancerName (\ s a -> s{_lbdLoadBalancerName = a});

-- | The domain name of the LoadBalancer.
lbdDomain :: Lens' LoadBalancerDescription (Maybe Text)
lbdDomain = lens _lbdDomain (\ s a -> s{_lbdDomain = a});

-- | A list of Listeners used by the LoadBalancer.
lbdListeners :: Lens' LoadBalancerDescription [Listener]
lbdListeners = lens _lbdListeners (\ s a -> s{_lbdListeners = a});

instance FromXML LoadBalancerDescription where
        parseXML x
          = LoadBalancerDescription' <$>
              x .@? "LoadBalancerName" <*> x .@? "Domain" <*>
                (x .@? "Listeners" .!@ mempty >>=
                   parseXMLList "member")

-- | /See:/ 'optionRestrictionRegex' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'orrPattern'
--
-- * 'orrLabel'
data OptionRestrictionRegex = OptionRestrictionRegex'{_orrPattern :: Maybe Text, _orrLabel :: Maybe Text} deriving (Eq, Read, Show)

-- | 'OptionRestrictionRegex' smart constructor.
optionRestrictionRegex :: OptionRestrictionRegex
optionRestrictionRegex = OptionRestrictionRegex'{_orrPattern = Nothing, _orrLabel = Nothing};

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
              x .@? "Pattern" <*> x .@? "Label"

-- | /See:/ 'optionSpecification' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'osOptionName'
--
-- * 'osNamespace'
--
-- * 'osResourceName'
data OptionSpecification = OptionSpecification'{_osOptionName :: Maybe Text, _osNamespace :: Maybe Text, _osResourceName :: Text} deriving (Eq, Read, Show)

-- | 'OptionSpecification' smart constructor.
optionSpecification :: Text -> OptionSpecification
optionSpecification pResourceName = OptionSpecification'{_osOptionName = Nothing, _osNamespace = Nothing, _osResourceName = pResourceName};

-- | The name of the configuration option.
osOptionName :: Lens' OptionSpecification (Maybe Text)
osOptionName = lens _osOptionName (\ s a -> s{_osOptionName = a});

-- | A unique namespace identifying the option\'s associated AWS resource.
osNamespace :: Lens' OptionSpecification (Maybe Text)
osNamespace = lens _osNamespace (\ s a -> s{_osNamespace = a});

-- | A unique resource name for a time-based scaling configuration option.
osResourceName :: Lens' OptionSpecification Text
osResourceName = lens _osResourceName (\ s a -> s{_osResourceName = a});

instance ToQuery OptionSpecification where
        toQuery OptionSpecification'{..}
          = mconcat
              ["OptionName" =: _osOptionName,
               "Namespace" =: _osNamespace,
               "ResourceName" =: _osResourceName]

-- | /See:/ 'queue' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'queURL'
--
-- * 'queName'
data Queue = Queue'{_queURL :: Maybe Text, _queName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Queue' smart constructor.
queue :: Queue
queue = Queue'{_queURL = Nothing, _queName = Nothing};

-- | The URL of the queue.
queURL :: Lens' Queue (Maybe Text)
queURL = lens _queURL (\ s a -> s{_queURL = a});

-- | The name of the queue.
queName :: Lens' Queue (Maybe Text)
queName = lens _queName (\ s a -> s{_queName = a});

instance FromXML Queue where
        parseXML x = Queue' <$> x .@? "URL" <*> x .@? "Name"

-- | /See:/ 's3Location' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slS3Key'
--
-- * 'slS3Bucket'
data S3Location = S3Location'{_slS3Key :: Maybe Text, _slS3Bucket :: Maybe Text} deriving (Eq, Read, Show)

-- | 'S3Location' smart constructor.
s3Location :: S3Location
s3Location = S3Location'{_slS3Key = Nothing, _slS3Bucket = Nothing};

-- | The Amazon S3 key where the data is located.
slS3Key :: Lens' S3Location (Maybe Text)
slS3Key = lens _slS3Key (\ s a -> s{_slS3Key = a});

-- | The Amazon S3 bucket where the data is located.
slS3Bucket :: Lens' S3Location (Maybe Text)
slS3Bucket = lens _slS3Bucket (\ s a -> s{_slS3Bucket = a});

instance FromXML S3Location where
        parseXML x
          = S3Location' <$> x .@? "S3Key" <*> x .@? "S3Bucket"

instance ToQuery S3Location where
        toQuery S3Location'{..}
          = mconcat
              ["S3Key" =: _slS3Key, "S3Bucket" =: _slS3Bucket]

-- | /See:/ 'solutionStackDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssdPermittedFileTypes'
--
-- * 'ssdSolutionStackName'
data SolutionStackDescription = SolutionStackDescription'{_ssdPermittedFileTypes :: [Text], _ssdSolutionStackName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'SolutionStackDescription' smart constructor.
solutionStackDescription :: SolutionStackDescription
solutionStackDescription = SolutionStackDescription'{_ssdPermittedFileTypes = mempty, _ssdSolutionStackName = Nothing};

-- | The permitted file types allowed for a solution stack.
ssdPermittedFileTypes :: Lens' SolutionStackDescription [Text]
ssdPermittedFileTypes = lens _ssdPermittedFileTypes (\ s a -> s{_ssdPermittedFileTypes = a});

-- | The name of the solution stack.
ssdSolutionStackName :: Lens' SolutionStackDescription (Maybe Text)
ssdSolutionStackName = lens _ssdSolutionStackName (\ s a -> s{_ssdSolutionStackName = a});

instance FromXML SolutionStackDescription where
        parseXML x
          = SolutionStackDescription' <$>
              (x .@? "PermittedFileTypes" .!@ mempty >>=
                 parseXMLList "member")
                <*> x .@? "SolutionStackName"

-- | /See:/ 'sourceConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scTemplateName'
--
-- * 'scApplicationName'
data SourceConfiguration = SourceConfiguration'{_scTemplateName :: Text, _scApplicationName :: Text} deriving (Eq, Read, Show)

-- | 'SourceConfiguration' smart constructor.
sourceConfiguration :: Text -> Text -> SourceConfiguration
sourceConfiguration pTemplateName pApplicationName = SourceConfiguration'{_scTemplateName = pTemplateName, _scApplicationName = pApplicationName};

-- | The name of the configuration template.
scTemplateName :: Lens' SourceConfiguration Text
scTemplateName = lens _scTemplateName (\ s a -> s{_scTemplateName = a});

-- | The name of the application associated with the configuration.
scApplicationName :: Lens' SourceConfiguration Text
scApplicationName = lens _scApplicationName (\ s a -> s{_scApplicationName = a});

instance ToQuery SourceConfiguration where
        toQuery SourceConfiguration'{..}
          = mconcat
              ["TemplateName" =: _scTemplateName,
               "ApplicationName" =: _scApplicationName]

-- | /See:/ 'tag' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagValue'
--
-- * 'tagKey'
data Tag = Tag'{_tagValue :: Text, _tagKey :: Text} deriving (Eq, Read, Show)

-- | 'Tag' smart constructor.
tag :: Text -> Text -> Tag
tag pValue pKey = Tag'{_tagValue = pValue, _tagKey = pKey};

-- | The value of the tag.
tagValue :: Lens' Tag Text
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | The key of the tag.
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat ["Value" =: _tagValue, "Key" =: _tagKey]

-- | /See:/ 'trigger' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'triName'
newtype Trigger = Trigger'{_triName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Trigger' smart constructor.
trigger :: Trigger
trigger = Trigger'{_triName = Nothing};

-- | The name of the trigger.
triName :: Lens' Trigger (Maybe Text)
triName = lens _triName (\ s a -> s{_triName = a});

instance FromXML Trigger where
        parseXML x = Trigger' <$> x .@? "Name"

-- | /See:/ 'validationMessage' smart constructor.
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
data ValidationMessage = ValidationMessage'{_vmOptionName :: Maybe Text, _vmSeverity :: Maybe ValidationSeverity, _vmNamespace :: Maybe Text, _vmMessage :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ValidationMessage' smart constructor.
validationMessage :: ValidationMessage
validationMessage = ValidationMessage'{_vmOptionName = Nothing, _vmSeverity = Nothing, _vmNamespace = Nothing, _vmMessage = Nothing};

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
              x .@? "OptionName" <*> x .@? "Severity" <*>
                x .@? "Namespace"
                <*> x .@? "Message"

data ValidationSeverity = VSError | VSWarning deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ValidationSeverity where
    parser = takeLowerText >>= \case
        "error" -> pure VSError
        "warning" -> pure VSWarning
        e -> fail ("Failure parsing ValidationSeverity from " ++ show e)

instance ToText ValidationSeverity where
    toText = \case
        VSError -> "error"
        VSWarning -> "warning"

instance Hashable ValidationSeverity
instance ToQuery ValidationSeverity
instance ToHeader ValidationSeverity

instance FromXML ValidationSeverity where
    parseXML = parseXMLText "ValidationSeverity"
