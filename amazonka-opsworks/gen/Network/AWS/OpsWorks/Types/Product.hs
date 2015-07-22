{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.Product where

import           Network.AWS.OpsWorks.Types.Sum
import           Network.AWS.Prelude

-- | Describes an agent version.
--
-- /See:/ 'agentVersion' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avVersion'
--
-- * 'avConfigurationManager'
data AgentVersion = AgentVersion'
    { _avVersion              :: !(Maybe Text)
    , _avConfigurationManager :: !(Maybe StackConfigurationManager)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AgentVersion' smart constructor.
agentVersion :: AgentVersion
agentVersion =
    AgentVersion'
    { _avVersion = Nothing
    , _avConfigurationManager = Nothing
    }

-- | The agent version.
avVersion :: Lens' AgentVersion (Maybe Text)
avVersion = lens _avVersion (\ s a -> s{_avVersion = a});

-- | The configuration manager.
avConfigurationManager :: Lens' AgentVersion (Maybe StackConfigurationManager)
avConfigurationManager = lens _avConfigurationManager (\ s a -> s{_avConfigurationManager = a});

instance FromJSON AgentVersion where
        parseJSON
          = withObject "AgentVersion"
              (\ x ->
                 AgentVersion' <$>
                   (x .:? "Version") <*> (x .:? "ConfigurationManager"))

-- | A description of the app.
--
-- /See:/ 'app' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'appSSLConfiguration'
--
-- * 'appShortname'
--
-- * 'appEnableSSL'
--
-- * 'appCreatedAt'
--
-- * 'appEnvironment'
--
-- * 'appDataSources'
--
-- * 'appAppId'
--
-- * 'appAppSource'
--
-- * 'appName'
--
-- * 'appAttributes'
--
-- * 'appType'
--
-- * 'appDomains'
--
-- * 'appStackId'
--
-- * 'appDescription'
data App = App'
    { _appSSLConfiguration :: !(Maybe SSLConfiguration)
    , _appShortname        :: !(Maybe Text)
    , _appEnableSSL        :: !(Maybe Bool)
    , _appCreatedAt        :: !(Maybe Text)
    , _appEnvironment      :: !(Maybe [EnvironmentVariable])
    , _appDataSources      :: !(Maybe [DataSource])
    , _appAppId            :: !(Maybe Text)
    , _appAppSource        :: !(Maybe Source)
    , _appName             :: !(Maybe Text)
    , _appAttributes       :: !(Maybe (Map AppAttributesKeys Text))
    , _appType             :: !(Maybe AppType)
    , _appDomains          :: !(Maybe [Text])
    , _appStackId          :: !(Maybe Text)
    , _appDescription      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'App' smart constructor.
app :: App
app =
    App'
    { _appSSLConfiguration = Nothing
    , _appShortname = Nothing
    , _appEnableSSL = Nothing
    , _appCreatedAt = Nothing
    , _appEnvironment = Nothing
    , _appDataSources = Nothing
    , _appAppId = Nothing
    , _appAppSource = Nothing
    , _appName = Nothing
    , _appAttributes = Nothing
    , _appType = Nothing
    , _appDomains = Nothing
    , _appStackId = Nothing
    , _appDescription = Nothing
    }

-- | An @SslConfiguration@ object with the SSL configuration.
appSSLConfiguration :: Lens' App (Maybe SSLConfiguration)
appSSLConfiguration = lens _appSSLConfiguration (\ s a -> s{_appSSLConfiguration = a});

-- | The app\'s short name.
appShortname :: Lens' App (Maybe Text)
appShortname = lens _appShortname (\ s a -> s{_appShortname = a});

-- | Whether to enable SSL for the app.
appEnableSSL :: Lens' App (Maybe Bool)
appEnableSSL = lens _appEnableSSL (\ s a -> s{_appEnableSSL = a});

-- | When the app was created.
appCreatedAt :: Lens' App (Maybe Text)
appCreatedAt = lens _appCreatedAt (\ s a -> s{_appCreatedAt = a});

-- | An array of @EnvironmentVariable@ objects that specify environment
-- variables to be associated with the app. After you deploy the app, these
-- variables are defined on the associated app server instances. For more
-- information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables>.
--
-- There is no specific limit on the number of environment variables.
-- However, the size of the associated data structure - which includes the
-- variables\' names, values, and protected flag values - cannot exceed 10
-- KB (10240 Bytes). This limit should accommodate most if not all use
-- cases, but if you do exceed it, you will cause an exception (API) with
-- an \"Environment: is too large (maximum is 10KB)\" message.
appEnvironment :: Lens' App [EnvironmentVariable]
appEnvironment = lens _appEnvironment (\ s a -> s{_appEnvironment = a}) . _Default;

-- | The app\'s data sources.
appDataSources :: Lens' App [DataSource]
appDataSources = lens _appDataSources (\ s a -> s{_appDataSources = a}) . _Default;

-- | The app ID.
appAppId :: Lens' App (Maybe Text)
appAppId = lens _appAppId (\ s a -> s{_appAppId = a});

-- | A @Source@ object that describes the app repository.
appAppSource :: Lens' App (Maybe Source)
appAppSource = lens _appAppSource (\ s a -> s{_appAppSource = a});

-- | The app name.
appName :: Lens' App (Maybe Text)
appName = lens _appName (\ s a -> s{_appName = a});

-- | The stack attributes.
appAttributes :: Lens' App (HashMap AppAttributesKeys Text)
appAttributes = lens _appAttributes (\ s a -> s{_appAttributes = a}) . _Default . _Map;

-- | The app type.
appType :: Lens' App (Maybe AppType)
appType = lens _appType (\ s a -> s{_appType = a});

-- | The app vhost settings with multiple domains separated by commas. For
-- example: @\'www.example.com, example.com\'@
appDomains :: Lens' App [Text]
appDomains = lens _appDomains (\ s a -> s{_appDomains = a}) . _Default;

-- | The app stack ID.
appStackId :: Lens' App (Maybe Text)
appStackId = lens _appStackId (\ s a -> s{_appStackId = a});

-- | A description of the app.
appDescription :: Lens' App (Maybe Text)
appDescription = lens _appDescription (\ s a -> s{_appDescription = a});

instance FromJSON App where
        parseJSON
          = withObject "App"
              (\ x ->
                 App' <$>
                   (x .:? "SslConfiguration") <*> (x .:? "Shortname")
                     <*> (x .:? "EnableSsl")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "Environment" .!= mempty)
                     <*> (x .:? "DataSources" .!= mempty)
                     <*> (x .:? "AppId")
                     <*> (x .:? "AppSource")
                     <*> (x .:? "Name")
                     <*> (x .:? "Attributes" .!= mempty)
                     <*> (x .:? "Type")
                     <*> (x .:? "Domains" .!= mempty)
                     <*> (x .:? "StackId")
                     <*> (x .:? "Description"))

-- | Describes a load-based auto scaling upscaling or downscaling threshold
-- configuration, which specifies when AWS OpsWorks starts or stops
-- load-based instances.
--
-- /See:/ 'autoScalingThresholds' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'astInstanceCount'
--
-- * 'astIgnoreMetricsTime'
--
-- * 'astLoadThreshold'
--
-- * 'astThresholdsWaitTime'
--
-- * 'astAlarms'
--
-- * 'astMemoryThreshold'
--
-- * 'astCPUThreshold'
data AutoScalingThresholds = AutoScalingThresholds'
    { _astInstanceCount      :: !(Maybe Int)
    , _astIgnoreMetricsTime  :: !(Maybe Nat)
    , _astLoadThreshold      :: !(Maybe Double)
    , _astThresholdsWaitTime :: !(Maybe Nat)
    , _astAlarms             :: !(Maybe [Text])
    , _astMemoryThreshold    :: !(Maybe Double)
    , _astCPUThreshold       :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AutoScalingThresholds' smart constructor.
autoScalingThresholds :: AutoScalingThresholds
autoScalingThresholds =
    AutoScalingThresholds'
    { _astInstanceCount = Nothing
    , _astIgnoreMetricsTime = Nothing
    , _astLoadThreshold = Nothing
    , _astThresholdsWaitTime = Nothing
    , _astAlarms = Nothing
    , _astMemoryThreshold = Nothing
    , _astCPUThreshold = Nothing
    }

-- | The number of instances to add or remove when the load exceeds a
-- threshold.
astInstanceCount :: Lens' AutoScalingThresholds (Maybe Int)
astInstanceCount = lens _astInstanceCount (\ s a -> s{_astInstanceCount = a});

-- | The amount of time (in minutes) after a scaling event occurs that AWS
-- OpsWorks should ignore metrics and suppress additional scaling events.
-- For example, AWS OpsWorks adds new instances following an upscaling
-- event but the instances won\'t start reducing the load until they have
-- been booted and configured. There is no point in raising additional
-- scaling events during that operation, which typically takes several
-- minutes. @IgnoreMetricsTime@ allows you to direct AWS OpsWorks to
-- suppress scaling events long enough to get the new instances online.
astIgnoreMetricsTime :: Lens' AutoScalingThresholds (Maybe Natural)
astIgnoreMetricsTime = lens _astIgnoreMetricsTime (\ s a -> s{_astIgnoreMetricsTime = a}) . mapping _Nat;

-- | The load threshold. For more information about how load is computed, see
-- <http://en.wikipedia.org/wiki/Load_%28computing%29 Load (computing)>.
astLoadThreshold :: Lens' AutoScalingThresholds (Maybe Double)
astLoadThreshold = lens _astLoadThreshold (\ s a -> s{_astLoadThreshold = a});

-- | The amount of time, in minutes, that the load must exceed a threshold
-- before more instances are added or removed.
astThresholdsWaitTime :: Lens' AutoScalingThresholds (Maybe Natural)
astThresholdsWaitTime = lens _astThresholdsWaitTime (\ s a -> s{_astThresholdsWaitTime = a}) . mapping _Nat;

-- | Custom Cloudwatch auto scaling alarms, to be used as thresholds. This
-- parameter takes a list of up to five alarm names, which are case
-- sensitive and must be in the same region as the stack.
--
-- To use custom alarms, you must update your service role to allow
-- @cloudwatch:DescribeAlarms@. You can either have AWS OpsWorks update the
-- role for you when you first use this feature or you can edit the role
-- manually. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-servicerole.html Allowing AWS OpsWorks to Act on Your Behalf>.
astAlarms :: Lens' AutoScalingThresholds [Text]
astAlarms = lens _astAlarms (\ s a -> s{_astAlarms = a}) . _Default;

-- | The memory utilization threshold, as a percent of the available memory.
astMemoryThreshold :: Lens' AutoScalingThresholds (Maybe Double)
astMemoryThreshold = lens _astMemoryThreshold (\ s a -> s{_astMemoryThreshold = a});

-- | The CPU utilization threshold, as a percent of the available CPU.
astCPUThreshold :: Lens' AutoScalingThresholds (Maybe Double)
astCPUThreshold = lens _astCPUThreshold (\ s a -> s{_astCPUThreshold = a});

instance FromJSON AutoScalingThresholds where
        parseJSON
          = withObject "AutoScalingThresholds"
              (\ x ->
                 AutoScalingThresholds' <$>
                   (x .:? "InstanceCount") <*>
                     (x .:? "IgnoreMetricsTime")
                     <*> (x .:? "LoadThreshold")
                     <*> (x .:? "ThresholdsWaitTime")
                     <*> (x .:? "Alarms" .!= mempty)
                     <*> (x .:? "MemoryThreshold")
                     <*> (x .:? "CpuThreshold"))

instance ToJSON AutoScalingThresholds where
        toJSON AutoScalingThresholds'{..}
          = object
              ["InstanceCount" .= _astInstanceCount,
               "IgnoreMetricsTime" .= _astIgnoreMetricsTime,
               "LoadThreshold" .= _astLoadThreshold,
               "ThresholdsWaitTime" .= _astThresholdsWaitTime,
               "Alarms" .= _astAlarms,
               "MemoryThreshold" .= _astMemoryThreshold,
               "CpuThreshold" .= _astCPUThreshold]

-- | Describes a block device mapping. This data type maps directly to the
-- Amazon EC2
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_BlockDeviceMapping.html BlockDeviceMapping>
-- data type.
--
-- /See:/ 'blockDeviceMapping' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bdmVirtualName'
--
-- * 'bdmNoDevice'
--
-- * 'bdmEBS'
--
-- * 'bdmDeviceName'
data BlockDeviceMapping = BlockDeviceMapping'
    { _bdmVirtualName :: !(Maybe Text)
    , _bdmNoDevice    :: !(Maybe Text)
    , _bdmEBS         :: !(Maybe EBSBlockDevice)
    , _bdmDeviceName  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BlockDeviceMapping' smart constructor.
blockDeviceMapping :: BlockDeviceMapping
blockDeviceMapping =
    BlockDeviceMapping'
    { _bdmVirtualName = Nothing
    , _bdmNoDevice = Nothing
    , _bdmEBS = Nothing
    , _bdmDeviceName = Nothing
    }

-- | The virtual device name. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_BlockDeviceMapping.html BlockDeviceMapping>.
bdmVirtualName :: Lens' BlockDeviceMapping (Maybe Text)
bdmVirtualName = lens _bdmVirtualName (\ s a -> s{_bdmVirtualName = a});

-- | Suppresses the specified device included in the AMI\'s block device
-- mapping.
bdmNoDevice :: Lens' BlockDeviceMapping (Maybe Text)
bdmNoDevice = lens _bdmNoDevice (\ s a -> s{_bdmNoDevice = a});

-- | An @EBSBlockDevice@ that defines how to configure an Amazon EBS volume
-- when the instance is launched.
bdmEBS :: Lens' BlockDeviceMapping (Maybe EBSBlockDevice)
bdmEBS = lens _bdmEBS (\ s a -> s{_bdmEBS = a});

-- | The device name that is exposed to the instance, such as @\/dev\/sdh@.
-- For the root device, you can use the explicit device name or you can set
-- this parameter to @ROOT_DEVICE@ and AWS OpsWorks will provide the
-- correct device name.
bdmDeviceName :: Lens' BlockDeviceMapping (Maybe Text)
bdmDeviceName = lens _bdmDeviceName (\ s a -> s{_bdmDeviceName = a});

instance FromJSON BlockDeviceMapping where
        parseJSON
          = withObject "BlockDeviceMapping"
              (\ x ->
                 BlockDeviceMapping' <$>
                   (x .:? "VirtualName") <*> (x .:? "NoDevice") <*>
                     (x .:? "Ebs")
                     <*> (x .:? "DeviceName"))

instance ToJSON BlockDeviceMapping where
        toJSON BlockDeviceMapping'{..}
          = object
              ["VirtualName" .= _bdmVirtualName,
               "NoDevice" .= _bdmNoDevice, "Ebs" .= _bdmEBS,
               "DeviceName" .= _bdmDeviceName]

-- | Describes the Chef configuration.
--
-- /See:/ 'chefConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccBerkshelfVersion'
--
-- * 'ccManageBerkshelf'
data ChefConfiguration = ChefConfiguration'
    { _ccBerkshelfVersion :: !(Maybe Text)
    , _ccManageBerkshelf  :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ChefConfiguration' smart constructor.
chefConfiguration :: ChefConfiguration
chefConfiguration =
    ChefConfiguration'
    { _ccBerkshelfVersion = Nothing
    , _ccManageBerkshelf = Nothing
    }

-- | The Berkshelf version.
ccBerkshelfVersion :: Lens' ChefConfiguration (Maybe Text)
ccBerkshelfVersion = lens _ccBerkshelfVersion (\ s a -> s{_ccBerkshelfVersion = a});

-- | Whether to enable Berkshelf.
ccManageBerkshelf :: Lens' ChefConfiguration (Maybe Bool)
ccManageBerkshelf = lens _ccManageBerkshelf (\ s a -> s{_ccManageBerkshelf = a});

instance FromJSON ChefConfiguration where
        parseJSON
          = withObject "ChefConfiguration"
              (\ x ->
                 ChefConfiguration' <$>
                   (x .:? "BerkshelfVersion") <*>
                     (x .:? "ManageBerkshelf"))

instance ToJSON ChefConfiguration where
        toJSON ChefConfiguration'{..}
          = object
              ["BerkshelfVersion" .= _ccBerkshelfVersion,
               "ManageBerkshelf" .= _ccManageBerkshelf]

-- | Describes a command.
--
-- /See:/ 'command' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cInstanceId'
--
-- * 'cDeploymentId'
--
-- * 'cStatus'
--
-- * 'cLogURL'
--
-- * 'cCreatedAt'
--
-- * 'cCommandId'
--
-- * 'cExitCode'
--
-- * 'cType'
--
-- * 'cCompletedAt'
--
-- * 'cAcknowledgedAt'
data Command = Command'
    { _cInstanceId     :: !(Maybe Text)
    , _cDeploymentId   :: !(Maybe Text)
    , _cStatus         :: !(Maybe Text)
    , _cLogURL         :: !(Maybe Text)
    , _cCreatedAt      :: !(Maybe Text)
    , _cCommandId      :: !(Maybe Text)
    , _cExitCode       :: !(Maybe Int)
    , _cType           :: !(Maybe Text)
    , _cCompletedAt    :: !(Maybe Text)
    , _cAcknowledgedAt :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Command' smart constructor.
command :: Command
command =
    Command'
    { _cInstanceId = Nothing
    , _cDeploymentId = Nothing
    , _cStatus = Nothing
    , _cLogURL = Nothing
    , _cCreatedAt = Nothing
    , _cCommandId = Nothing
    , _cExitCode = Nothing
    , _cType = Nothing
    , _cCompletedAt = Nothing
    , _cAcknowledgedAt = Nothing
    }

-- | The ID of the instance where the command was executed.
cInstanceId :: Lens' Command (Maybe Text)
cInstanceId = lens _cInstanceId (\ s a -> s{_cInstanceId = a});

-- | The command deployment ID.
cDeploymentId :: Lens' Command (Maybe Text)
cDeploymentId = lens _cDeploymentId (\ s a -> s{_cDeploymentId = a});

-- | The command status:
--
-- -   failed
-- -   successful
-- -   skipped
-- -   pending
cStatus :: Lens' Command (Maybe Text)
cStatus = lens _cStatus (\ s a -> s{_cStatus = a});

-- | The URL of the command log.
cLogURL :: Lens' Command (Maybe Text)
cLogURL = lens _cLogURL (\ s a -> s{_cLogURL = a});

-- | Date and time when the command was run.
cCreatedAt :: Lens' Command (Maybe Text)
cCreatedAt = lens _cCreatedAt (\ s a -> s{_cCreatedAt = a});

-- | The command ID.
cCommandId :: Lens' Command (Maybe Text)
cCommandId = lens _cCommandId (\ s a -> s{_cCommandId = a});

-- | The command exit code.
cExitCode :: Lens' Command (Maybe Int)
cExitCode = lens _cExitCode (\ s a -> s{_cExitCode = a});

-- | The command type:
--
-- -   @deploy@
-- -   @rollback@
-- -   @start@
-- -   @stop@
-- -   @restart@
-- -   @undeploy@
-- -   @update_dependencies@
-- -   @install_dependencies@
-- -   @update_custom_cookbooks@
-- -   @execute_recipes@
cType :: Lens' Command (Maybe Text)
cType = lens _cType (\ s a -> s{_cType = a});

-- | Date when the command completed.
cCompletedAt :: Lens' Command (Maybe Text)
cCompletedAt = lens _cCompletedAt (\ s a -> s{_cCompletedAt = a});

-- | Date and time when the command was acknowledged.
cAcknowledgedAt :: Lens' Command (Maybe Text)
cAcknowledgedAt = lens _cAcknowledgedAt (\ s a -> s{_cAcknowledgedAt = a});

instance FromJSON Command where
        parseJSON
          = withObject "Command"
              (\ x ->
                 Command' <$>
                   (x .:? "InstanceId") <*> (x .:? "DeploymentId") <*>
                     (x .:? "Status")
                     <*> (x .:? "LogUrl")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "CommandId")
                     <*> (x .:? "ExitCode")
                     <*> (x .:? "Type")
                     <*> (x .:? "CompletedAt")
                     <*> (x .:? "AcknowledgedAt"))

-- | Describes an app\'s data source.
--
-- /See:/ 'dataSource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsARN'
--
-- * 'dsDatabaseName'
--
-- * 'dsType'
data DataSource = DataSource'
    { _dsARN          :: !(Maybe Text)
    , _dsDatabaseName :: !(Maybe Text)
    , _dsType         :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DataSource' smart constructor.
dataSource :: DataSource
dataSource =
    DataSource'
    { _dsARN = Nothing
    , _dsDatabaseName = Nothing
    , _dsType = Nothing
    }

-- | The data source\'s ARN.
dsARN :: Lens' DataSource (Maybe Text)
dsARN = lens _dsARN (\ s a -> s{_dsARN = a});

-- | The database name.
dsDatabaseName :: Lens' DataSource (Maybe Text)
dsDatabaseName = lens _dsDatabaseName (\ s a -> s{_dsDatabaseName = a});

-- | The data source\'s type, @AutoSelectOpsworksMysqlInstance@,
-- @OpsworksMysqlInstance@, or @RdsDbInstance@.
dsType :: Lens' DataSource (Maybe Text)
dsType = lens _dsType (\ s a -> s{_dsType = a});

instance FromJSON DataSource where
        parseJSON
          = withObject "DataSource"
              (\ x ->
                 DataSource' <$>
                   (x .:? "Arn") <*> (x .:? "DatabaseName") <*>
                     (x .:? "Type"))

instance ToJSON DataSource where
        toJSON DataSource'{..}
          = object
              ["Arn" .= _dsARN, "DatabaseName" .= _dsDatabaseName,
               "Type" .= _dsType]

-- | Describes a deployment of a stack or app.
--
-- /See:/ 'deployment' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dDeploymentId'
--
-- * 'dStatus'
--
-- * 'dCommand'
--
-- * 'dCreatedAt'
--
-- * 'dCustomJSON'
--
-- * 'dIAMUserARN'
--
-- * 'dAppId'
--
-- * 'dInstanceIds'
--
-- * 'dCompletedAt'
--
-- * 'dStackId'
--
-- * 'dComment'
--
-- * 'dDuration'
data Deployment = Deployment'
    { _dDeploymentId :: !(Maybe Text)
    , _dStatus       :: !(Maybe Text)
    , _dCommand      :: !(Maybe DeploymentCommand)
    , _dCreatedAt    :: !(Maybe Text)
    , _dCustomJSON   :: !(Maybe Text)
    , _dIAMUserARN   :: !(Maybe Text)
    , _dAppId        :: !(Maybe Text)
    , _dInstanceIds  :: !(Maybe [Text])
    , _dCompletedAt  :: !(Maybe Text)
    , _dStackId      :: !(Maybe Text)
    , _dComment      :: !(Maybe Text)
    , _dDuration     :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Deployment' smart constructor.
deployment :: Deployment
deployment =
    Deployment'
    { _dDeploymentId = Nothing
    , _dStatus = Nothing
    , _dCommand = Nothing
    , _dCreatedAt = Nothing
    , _dCustomJSON = Nothing
    , _dIAMUserARN = Nothing
    , _dAppId = Nothing
    , _dInstanceIds = Nothing
    , _dCompletedAt = Nothing
    , _dStackId = Nothing
    , _dComment = Nothing
    , _dDuration = Nothing
    }

-- | The deployment ID.
dDeploymentId :: Lens' Deployment (Maybe Text)
dDeploymentId = lens _dDeploymentId (\ s a -> s{_dDeploymentId = a});

-- | The deployment status:
--
-- -   running
-- -   successful
-- -   failed
dStatus :: Lens' Deployment (Maybe Text)
dStatus = lens _dStatus (\ s a -> s{_dStatus = a});

-- | FIXME: Undocumented member.
dCommand :: Lens' Deployment (Maybe DeploymentCommand)
dCommand = lens _dCommand (\ s a -> s{_dCommand = a});

-- | Date when the deployment was created.
dCreatedAt :: Lens' Deployment (Maybe Text)
dCreatedAt = lens _dCreatedAt (\ s a -> s{_dCreatedAt = a});

-- | A string that contains user-defined custom JSON. It can be used to
-- override the corresponding default stack configuration attribute values
-- for stack or to pass data to recipes. The string should be in the
-- following format and must escape characters such as \'\"\':
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information on custom JSON, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>.
dCustomJSON :: Lens' Deployment (Maybe Text)
dCustomJSON = lens _dCustomJSON (\ s a -> s{_dCustomJSON = a});

-- | The user\'s IAM ARN.
dIAMUserARN :: Lens' Deployment (Maybe Text)
dIAMUserARN = lens _dIAMUserARN (\ s a -> s{_dIAMUserARN = a});

-- | The app ID.
dAppId :: Lens' Deployment (Maybe Text)
dAppId = lens _dAppId (\ s a -> s{_dAppId = a});

-- | The IDs of the target instances.
dInstanceIds :: Lens' Deployment [Text]
dInstanceIds = lens _dInstanceIds (\ s a -> s{_dInstanceIds = a}) . _Default;

-- | Date when the deployment completed.
dCompletedAt :: Lens' Deployment (Maybe Text)
dCompletedAt = lens _dCompletedAt (\ s a -> s{_dCompletedAt = a});

-- | The stack ID.
dStackId :: Lens' Deployment (Maybe Text)
dStackId = lens _dStackId (\ s a -> s{_dStackId = a});

-- | A user-defined comment.
dComment :: Lens' Deployment (Maybe Text)
dComment = lens _dComment (\ s a -> s{_dComment = a});

-- | The deployment duration.
dDuration :: Lens' Deployment (Maybe Int)
dDuration = lens _dDuration (\ s a -> s{_dDuration = a});

instance FromJSON Deployment where
        parseJSON
          = withObject "Deployment"
              (\ x ->
                 Deployment' <$>
                   (x .:? "DeploymentId") <*> (x .:? "Status") <*>
                     (x .:? "Command")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "CustomJson")
                     <*> (x .:? "IamUserArn")
                     <*> (x .:? "AppId")
                     <*> (x .:? "InstanceIds" .!= mempty)
                     <*> (x .:? "CompletedAt")
                     <*> (x .:? "StackId")
                     <*> (x .:? "Comment")
                     <*> (x .:? "Duration"))

-- | Used to specify a stack or deployment command.
--
-- /See:/ 'deploymentCommand' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcArgs'
--
-- * 'dcName'
data DeploymentCommand = DeploymentCommand'
    { _dcArgs :: !(Maybe (Map Text [Text]))
    , _dcName :: !DeploymentCommandName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeploymentCommand' smart constructor.
deploymentCommand :: DeploymentCommandName -> DeploymentCommand
deploymentCommand pName =
    DeploymentCommand'
    { _dcArgs = Nothing
    , _dcName = pName
    }

-- | The arguments of those commands that take arguments. It should be set to
-- a JSON object with the following format:
--
-- @{\"arg_name1\" : [\"value1\", \"value2\", ...], \"arg_name2\" : [\"value1\", \"value2\", ...], ...}@
--
-- The @update_dependencies@ command takes two arguments:
--
-- -   @upgrade_os_to@ - Specifies the desired Amazon Linux version for
--     instances whose OS you want to upgrade, such as
--     @Amazon Linux 2014.09@. You must also set the @allow_reboot@
--     argument to true.
-- -   @allow_reboot@ - Specifies whether to allow AWS OpsWorks to reboot
--     the instances if necessary, after installing the updates. This
--     argument can be set to either @true@ or @false@. The default value
--     is @false@.
--
-- For example, to upgrade an instance to Amazon Linux 2014.09, set @Args@
-- to the following.
--
-- @ { \"upgrade_os_to\":[\"Amazon Linux 2014.09\"], \"allow_reboot\":[\"true\"] } @
dcArgs :: Lens' DeploymentCommand (HashMap Text [Text])
dcArgs = lens _dcArgs (\ s a -> s{_dcArgs = a}) . _Default . _Map;

-- | Specifies the operation. You can specify only one command.
--
-- For stacks, the following commands are available:
--
-- -   @execute_recipes@: Execute one or more recipes. To specify the
--     recipes, set an @Args@ parameter named @recipes@ to the list of
--     recipes to be executed. For example, to execute @phpapp::appsetup@,
--     set @Args@ to @{\"recipes\":[\"phpapp::appsetup\"]}@.
-- -   @install_dependencies@: Install the stack\'s dependencies.
-- -   @update_custom_cookbooks@: Update the stack\'s custom cookbooks.
-- -   @update_dependencies@: Update the stack\'s dependencies.
--
-- The update_dependencies and install_dependencies commands are supported
-- only for Linux instances. You can run the commands successfully on
-- Windows instances, but they do nothing.
--
-- For apps, the following commands are available:
--
-- -   @deploy@: Deploy an app. Ruby on Rails apps have an optional @Args@
--     parameter named @migrate@. Set @Args@ to {\"migrate\":[\"true\"]} to
--     migrate the database. The default setting is
--     {\"migrate\":[\"false\"]}.
-- -   @rollback@ Roll the app back to the previous version. When you
--     update an app, AWS OpsWorks stores the previous version, up to a
--     maximum of five versions. You can use this command to roll an app
--     back as many as four versions.
-- -   @start@: Start the app\'s web or application server.
-- -   @stop@: Stop the app\'s web or application server.
-- -   @restart@: Restart the app\'s web or application server.
-- -   @undeploy@: Undeploy the app.
dcName :: Lens' DeploymentCommand DeploymentCommandName
dcName = lens _dcName (\ s a -> s{_dcName = a});

instance FromJSON DeploymentCommand where
        parseJSON
          = withObject "DeploymentCommand"
              (\ x ->
                 DeploymentCommand' <$>
                   (x .:? "Args" .!= mempty) <*> (x .: "Name"))

instance ToJSON DeploymentCommand where
        toJSON DeploymentCommand'{..}
          = object ["Args" .= _dcArgs, "Name" .= _dcName]

-- | Describes an Amazon EBS volume. This data type maps directly to the
-- Amazon EC2
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice>
-- data type.
--
-- /See:/ 'ebsBlockDevice' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ebdDeleteOnTermination'
--
-- * 'ebdVolumeSize'
--
-- * 'ebdIOPS'
--
-- * 'ebdVolumeType'
--
-- * 'ebdSnapshotId'
data EBSBlockDevice = EBSBlockDevice'
    { _ebdDeleteOnTermination :: !(Maybe Bool)
    , _ebdVolumeSize          :: !(Maybe Int)
    , _ebdIOPS                :: !(Maybe Int)
    , _ebdVolumeType          :: !(Maybe VolumeType)
    , _ebdSnapshotId          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EBSBlockDevice' smart constructor.
ebsBlockDevice :: EBSBlockDevice
ebsBlockDevice =
    EBSBlockDevice'
    { _ebdDeleteOnTermination = Nothing
    , _ebdVolumeSize = Nothing
    , _ebdIOPS = Nothing
    , _ebdVolumeType = Nothing
    , _ebdSnapshotId = Nothing
    }

-- | Whether the volume is deleted on instance termination.
ebdDeleteOnTermination :: Lens' EBSBlockDevice (Maybe Bool)
ebdDeleteOnTermination = lens _ebdDeleteOnTermination (\ s a -> s{_ebdDeleteOnTermination = a});

-- | The volume size, in GiB. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice>.
ebdVolumeSize :: Lens' EBSBlockDevice (Maybe Int)
ebdVolumeSize = lens _ebdVolumeSize (\ s a -> s{_ebdVolumeSize = a});

-- | The number of I\/O operations per second (IOPS) that the volume
-- supports. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice>.
ebdIOPS :: Lens' EBSBlockDevice (Maybe Int)
ebdIOPS = lens _ebdIOPS (\ s a -> s{_ebdIOPS = a});

-- | The volume type. @gp2@ for General Purpose (SSD) volumes, @io1@ for
-- Provisioned IOPS (SSD) volumes, and @standard@ for Magnetic volumes.
ebdVolumeType :: Lens' EBSBlockDevice (Maybe VolumeType)
ebdVolumeType = lens _ebdVolumeType (\ s a -> s{_ebdVolumeType = a});

-- | The snapshot ID.
ebdSnapshotId :: Lens' EBSBlockDevice (Maybe Text)
ebdSnapshotId = lens _ebdSnapshotId (\ s a -> s{_ebdSnapshotId = a});

instance FromJSON EBSBlockDevice where
        parseJSON
          = withObject "EBSBlockDevice"
              (\ x ->
                 EBSBlockDevice' <$>
                   (x .:? "DeleteOnTermination") <*>
                     (x .:? "VolumeSize")
                     <*> (x .:? "Iops")
                     <*> (x .:? "VolumeType")
                     <*> (x .:? "SnapshotId"))

instance ToJSON EBSBlockDevice where
        toJSON EBSBlockDevice'{..}
          = object
              ["DeleteOnTermination" .= _ebdDeleteOnTermination,
               "VolumeSize" .= _ebdVolumeSize, "Iops" .= _ebdIOPS,
               "VolumeType" .= _ebdVolumeType,
               "SnapshotId" .= _ebdSnapshotId]

-- | Describes an Elastic IP address.
--
-- /See:/ 'elasticIP' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eiInstanceId'
--
-- * 'eiDomain'
--
-- * 'eiIP'
--
-- * 'eiName'
--
-- * 'eiRegion'
data ElasticIP = ElasticIP'
    { _eiInstanceId :: !(Maybe Text)
    , _eiDomain     :: !(Maybe Text)
    , _eiIP         :: !(Maybe Text)
    , _eiName       :: !(Maybe Text)
    , _eiRegion     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ElasticIP' smart constructor.
elasticIP :: ElasticIP
elasticIP =
    ElasticIP'
    { _eiInstanceId = Nothing
    , _eiDomain = Nothing
    , _eiIP = Nothing
    , _eiName = Nothing
    , _eiRegion = Nothing
    }

-- | The ID of the instance that the address is attached to.
eiInstanceId :: Lens' ElasticIP (Maybe Text)
eiInstanceId = lens _eiInstanceId (\ s a -> s{_eiInstanceId = a});

-- | The domain.
eiDomain :: Lens' ElasticIP (Maybe Text)
eiDomain = lens _eiDomain (\ s a -> s{_eiDomain = a});

-- | The IP address.
eiIP :: Lens' ElasticIP (Maybe Text)
eiIP = lens _eiIP (\ s a -> s{_eiIP = a});

-- | The name.
eiName :: Lens' ElasticIP (Maybe Text)
eiName = lens _eiName (\ s a -> s{_eiName = a});

-- | The AWS region. For more information, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
eiRegion :: Lens' ElasticIP (Maybe Text)
eiRegion = lens _eiRegion (\ s a -> s{_eiRegion = a});

instance FromJSON ElasticIP where
        parseJSON
          = withObject "ElasticIP"
              (\ x ->
                 ElasticIP' <$>
                   (x .:? "InstanceId") <*> (x .:? "Domain") <*>
                     (x .:? "Ip")
                     <*> (x .:? "Name")
                     <*> (x .:? "Region"))

-- | Describes an Elastic Load Balancing instance.
--
-- /See:/ 'elasticLoadBalancer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'elbSubnetIds'
--
-- * 'elbVPCId'
--
-- * 'elbAvailabilityZones'
--
-- * 'elbRegion'
--
-- * 'elbElasticLoadBalancerName'
--
-- * 'elbEC2InstanceIds'
--
-- * 'elbStackId'
--
-- * 'elbLayerId'
--
-- * 'elbDNSName'
data ElasticLoadBalancer = ElasticLoadBalancer'
    { _elbSubnetIds               :: !(Maybe [Text])
    , _elbVPCId                   :: !(Maybe Text)
    , _elbAvailabilityZones       :: !(Maybe [Text])
    , _elbRegion                  :: !(Maybe Text)
    , _elbElasticLoadBalancerName :: !(Maybe Text)
    , _elbEC2InstanceIds          :: !(Maybe [Text])
    , _elbStackId                 :: !(Maybe Text)
    , _elbLayerId                 :: !(Maybe Text)
    , _elbDNSName                 :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ElasticLoadBalancer' smart constructor.
elasticLoadBalancer :: ElasticLoadBalancer
elasticLoadBalancer =
    ElasticLoadBalancer'
    { _elbSubnetIds = Nothing
    , _elbVPCId = Nothing
    , _elbAvailabilityZones = Nothing
    , _elbRegion = Nothing
    , _elbElasticLoadBalancerName = Nothing
    , _elbEC2InstanceIds = Nothing
    , _elbStackId = Nothing
    , _elbLayerId = Nothing
    , _elbDNSName = Nothing
    }

-- | A list of subnet IDs, if the stack is running in a VPC.
elbSubnetIds :: Lens' ElasticLoadBalancer [Text]
elbSubnetIds = lens _elbSubnetIds (\ s a -> s{_elbSubnetIds = a}) . _Default;

-- | The VPC ID.
elbVPCId :: Lens' ElasticLoadBalancer (Maybe Text)
elbVPCId = lens _elbVPCId (\ s a -> s{_elbVPCId = a});

-- | A list of Availability Zones.
elbAvailabilityZones :: Lens' ElasticLoadBalancer [Text]
elbAvailabilityZones = lens _elbAvailabilityZones (\ s a -> s{_elbAvailabilityZones = a}) . _Default;

-- | The instance\'s AWS region.
elbRegion :: Lens' ElasticLoadBalancer (Maybe Text)
elbRegion = lens _elbRegion (\ s a -> s{_elbRegion = a});

-- | The Elastic Load Balancing instance\'s name.
elbElasticLoadBalancerName :: Lens' ElasticLoadBalancer (Maybe Text)
elbElasticLoadBalancerName = lens _elbElasticLoadBalancerName (\ s a -> s{_elbElasticLoadBalancerName = a});

-- | A list of the EC2 instances that the Elastic Load Balancing instance is
-- managing traffic for.
elbEC2InstanceIds :: Lens' ElasticLoadBalancer [Text]
elbEC2InstanceIds = lens _elbEC2InstanceIds (\ s a -> s{_elbEC2InstanceIds = a}) . _Default;

-- | The ID of the stack that the instance is associated with.
elbStackId :: Lens' ElasticLoadBalancer (Maybe Text)
elbStackId = lens _elbStackId (\ s a -> s{_elbStackId = a});

-- | The ID of the layer that the instance is attached to.
elbLayerId :: Lens' ElasticLoadBalancer (Maybe Text)
elbLayerId = lens _elbLayerId (\ s a -> s{_elbLayerId = a});

-- | The instance\'s public DNS name.
elbDNSName :: Lens' ElasticLoadBalancer (Maybe Text)
elbDNSName = lens _elbDNSName (\ s a -> s{_elbDNSName = a});

instance FromJSON ElasticLoadBalancer where
        parseJSON
          = withObject "ElasticLoadBalancer"
              (\ x ->
                 ElasticLoadBalancer' <$>
                   (x .:? "SubnetIds" .!= mempty) <*> (x .:? "VpcId")
                     <*> (x .:? "AvailabilityZones" .!= mempty)
                     <*> (x .:? "Region")
                     <*> (x .:? "ElasticLoadBalancerName")
                     <*> (x .:? "Ec2InstanceIds" .!= mempty)
                     <*> (x .:? "StackId")
                     <*> (x .:? "LayerId")
                     <*> (x .:? "DnsName"))

-- | Represents an app\'s environment variable.
--
-- /See:/ 'environmentVariable' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'evSecure'
--
-- * 'evKey'
--
-- * 'evValue'
data EnvironmentVariable = EnvironmentVariable'
    { _evSecure :: !(Maybe Bool)
    , _evKey    :: !Text
    , _evValue  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnvironmentVariable' smart constructor.
environmentVariable :: Text -> Text -> EnvironmentVariable
environmentVariable pKey pValue =
    EnvironmentVariable'
    { _evSecure = Nothing
    , _evKey = pKey
    , _evValue = pValue
    }

-- | (Optional) Whether the variable\'s value will be returned by the
-- DescribeApps action. To conceal an environment variable\'s value, set
-- @Secure@ to @true@. @DescribeApps@ then returns @*****FILTERED*****@
-- instead of the actual value. The default value for @Secure@ is @false@.
evSecure :: Lens' EnvironmentVariable (Maybe Bool)
evSecure = lens _evSecure (\ s a -> s{_evSecure = a});

-- | (Required) The environment variable\'s name, which can consist of up to
-- 64 characters and must be specified. The name can contain upper- and
-- lowercase letters, numbers, and underscores (_), but it must start with
-- a letter or underscore.
evKey :: Lens' EnvironmentVariable Text
evKey = lens _evKey (\ s a -> s{_evKey = a});

-- | (Optional) The environment variable\'s value, which can be left empty.
-- If you specify a value, it can contain up to 256 characters, which must
-- all be printable.
evValue :: Lens' EnvironmentVariable Text
evValue = lens _evValue (\ s a -> s{_evValue = a});

instance FromJSON EnvironmentVariable where
        parseJSON
          = withObject "EnvironmentVariable"
              (\ x ->
                 EnvironmentVariable' <$>
                   (x .:? "Secure") <*> (x .: "Key") <*> (x .: "Value"))

instance ToJSON EnvironmentVariable where
        toJSON EnvironmentVariable'{..}
          = object
              ["Secure" .= _evSecure, "Key" .= _evKey,
               "Value" .= _evValue]

-- | Describes an instance.
--
-- /See:/ 'instance'' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iInstanceId'
--
-- * 'iPrivateIP'
--
-- * 'iInstallUpdatesOnBoot'
--
-- * 'iReportedAgentVersion'
--
-- * 'iStatus'
--
-- * 'iPrivateDNS'
--
-- * 'iVirtualizationType'
--
-- * 'iSecurityGroupIds'
--
-- * 'iSSHHostRsaKeyFingerprint'
--
-- * 'iInstanceProfileARN'
--
-- * 'iPlatform'
--
-- * 'iHostname'
--
-- * 'iCreatedAt'
--
-- * 'iSSHKeyName'
--
-- * 'iEC2InstanceId'
--
-- * 'iAgentVersion'
--
-- * 'iRootDeviceVolumeId'
--
-- * 'iSubnetId'
--
-- * 'iInstanceType'
--
-- * 'iInfrastructureClass'
--
-- * 'iEBSOptimized'
--
-- * 'iSSHHostDsaKeyFingerprint'
--
-- * 'iElasticIP'
--
-- * 'iOS'
--
-- * 'iAvailabilityZone'
--
-- * 'iLastServiceErrorId'
--
-- * 'iAutoScalingType'
--
-- * 'iLayerIds'
--
-- * 'iArchitecture'
--
-- * 'iPublicDNS'
--
-- * 'iPublicIP'
--
-- * 'iAMIId'
--
-- * 'iReportedOS'
--
-- * 'iStackId'
--
-- * 'iRegisteredBy'
--
-- * 'iBlockDeviceMappings'
--
-- * 'iRootDeviceType'
data Instance = Instance'
    { _iInstanceId               :: !(Maybe Text)
    , _iPrivateIP                :: !(Maybe Text)
    , _iInstallUpdatesOnBoot     :: !(Maybe Bool)
    , _iReportedAgentVersion     :: !(Maybe Text)
    , _iStatus                   :: !(Maybe Text)
    , _iPrivateDNS               :: !(Maybe Text)
    , _iVirtualizationType       :: !(Maybe VirtualizationType)
    , _iSecurityGroupIds         :: !(Maybe [Text])
    , _iSSHHostRsaKeyFingerprint :: !(Maybe Text)
    , _iInstanceProfileARN       :: !(Maybe Text)
    , _iPlatform                 :: !(Maybe Text)
    , _iHostname                 :: !(Maybe Text)
    , _iCreatedAt                :: !(Maybe Text)
    , _iSSHKeyName               :: !(Maybe Text)
    , _iEC2InstanceId            :: !(Maybe Text)
    , _iAgentVersion             :: !(Maybe Text)
    , _iRootDeviceVolumeId       :: !(Maybe Text)
    , _iSubnetId                 :: !(Maybe Text)
    , _iInstanceType             :: !(Maybe Text)
    , _iInfrastructureClass      :: !(Maybe Text)
    , _iEBSOptimized             :: !(Maybe Bool)
    , _iSSHHostDsaKeyFingerprint :: !(Maybe Text)
    , _iElasticIP                :: !(Maybe Text)
    , _iOS                       :: !(Maybe Text)
    , _iAvailabilityZone         :: !(Maybe Text)
    , _iLastServiceErrorId       :: !(Maybe Text)
    , _iAutoScalingType          :: !(Maybe AutoScalingType)
    , _iLayerIds                 :: !(Maybe [Text])
    , _iArchitecture             :: !(Maybe Architecture)
    , _iPublicDNS                :: !(Maybe Text)
    , _iPublicIP                 :: !(Maybe Text)
    , _iAMIId                    :: !(Maybe Text)
    , _iReportedOS               :: !(Maybe ReportedOS)
    , _iStackId                  :: !(Maybe Text)
    , _iRegisteredBy             :: !(Maybe Text)
    , _iBlockDeviceMappings      :: !(Maybe [BlockDeviceMapping])
    , _iRootDeviceType           :: !(Maybe RootDeviceType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Instance' smart constructor.
instance' :: Instance
instance' =
    Instance'
    { _iInstanceId = Nothing
    , _iPrivateIP = Nothing
    , _iInstallUpdatesOnBoot = Nothing
    , _iReportedAgentVersion = Nothing
    , _iStatus = Nothing
    , _iPrivateDNS = Nothing
    , _iVirtualizationType = Nothing
    , _iSecurityGroupIds = Nothing
    , _iSSHHostRsaKeyFingerprint = Nothing
    , _iInstanceProfileARN = Nothing
    , _iPlatform = Nothing
    , _iHostname = Nothing
    , _iCreatedAt = Nothing
    , _iSSHKeyName = Nothing
    , _iEC2InstanceId = Nothing
    , _iAgentVersion = Nothing
    , _iRootDeviceVolumeId = Nothing
    , _iSubnetId = Nothing
    , _iInstanceType = Nothing
    , _iInfrastructureClass = Nothing
    , _iEBSOptimized = Nothing
    , _iSSHHostDsaKeyFingerprint = Nothing
    , _iElasticIP = Nothing
    , _iOS = Nothing
    , _iAvailabilityZone = Nothing
    , _iLastServiceErrorId = Nothing
    , _iAutoScalingType = Nothing
    , _iLayerIds = Nothing
    , _iArchitecture = Nothing
    , _iPublicDNS = Nothing
    , _iPublicIP = Nothing
    , _iAMIId = Nothing
    , _iReportedOS = Nothing
    , _iStackId = Nothing
    , _iRegisteredBy = Nothing
    , _iBlockDeviceMappings = Nothing
    , _iRootDeviceType = Nothing
    }

-- | The instance ID.
iInstanceId :: Lens' Instance (Maybe Text)
iInstanceId = lens _iInstanceId (\ s a -> s{_iInstanceId = a});

-- | The instance\'s private IP address.
iPrivateIP :: Lens' Instance (Maybe Text)
iPrivateIP = lens _iPrivateIP (\ s a -> s{_iPrivateIP = a});

-- | Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. If this value is set to
-- @false@, you must then update your instances manually by using
-- CreateDeployment to run the @update_dependencies@ stack command or by
-- manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the
-- instances.
--
-- We strongly recommend using the default value of @true@, to ensure that
-- your instances have the latest security updates.
iInstallUpdatesOnBoot :: Lens' Instance (Maybe Bool)
iInstallUpdatesOnBoot = lens _iInstallUpdatesOnBoot (\ s a -> s{_iInstallUpdatesOnBoot = a});

-- | The instance\'s reported AWS OpsWorks agent version.
iReportedAgentVersion :: Lens' Instance (Maybe Text)
iReportedAgentVersion = lens _iReportedAgentVersion (\ s a -> s{_iReportedAgentVersion = a});

-- | The instance status:
--
-- -   @booting@
-- -   @connection_lost@
-- -   @online@
-- -   @pending@
-- -   @rebooting@
-- -   @requested@
-- -   @running_setup@
-- -   @setup_failed@
-- -   @shutting_down@
-- -   @start_failed@
-- -   @stopped@
-- -   @stopping@
-- -   @terminated@
-- -   @terminating@
iStatus :: Lens' Instance (Maybe Text)
iStatus = lens _iStatus (\ s a -> s{_iStatus = a});

-- | The The instance\'s private DNS name.
iPrivateDNS :: Lens' Instance (Maybe Text)
iPrivateDNS = lens _iPrivateDNS (\ s a -> s{_iPrivateDNS = a});

-- | The instance\'s virtualization type: @paravirtual@ or @hvm@.
iVirtualizationType :: Lens' Instance (Maybe VirtualizationType)
iVirtualizationType = lens _iVirtualizationType (\ s a -> s{_iVirtualizationType = a});

-- | An array containing the instance security group IDs.
iSecurityGroupIds :: Lens' Instance [Text]
iSecurityGroupIds = lens _iSecurityGroupIds (\ s a -> s{_iSecurityGroupIds = a}) . _Default;

-- | The SSH key\'s RSA fingerprint.
iSSHHostRsaKeyFingerprint :: Lens' Instance (Maybe Text)
iSSHHostRsaKeyFingerprint = lens _iSSHHostRsaKeyFingerprint (\ s a -> s{_iSSHHostRsaKeyFingerprint = a});

-- | The ARN of the instance\'s IAM profile. For more information about IAM
-- ARNs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
iInstanceProfileARN :: Lens' Instance (Maybe Text)
iInstanceProfileARN = lens _iInstanceProfileARN (\ s a -> s{_iInstanceProfileARN = a});

-- | The instance\'s platform.
iPlatform :: Lens' Instance (Maybe Text)
iPlatform = lens _iPlatform (\ s a -> s{_iPlatform = a});

-- | The instance host name.
iHostname :: Lens' Instance (Maybe Text)
iHostname = lens _iHostname (\ s a -> s{_iHostname = a});

-- | The time that the instance was created.
iCreatedAt :: Lens' Instance (Maybe Text)
iCreatedAt = lens _iCreatedAt (\ s a -> s{_iCreatedAt = a});

-- | The instance\'s Amazon EC2 key-pair name.
iSSHKeyName :: Lens' Instance (Maybe Text)
iSSHKeyName = lens _iSSHKeyName (\ s a -> s{_iSSHKeyName = a});

-- | The ID of the associated Amazon EC2 instance.
iEC2InstanceId :: Lens' Instance (Maybe Text)
iEC2InstanceId = lens _iEC2InstanceId (\ s a -> s{_iEC2InstanceId = a});

-- | The agent version. This parameter is set to @INHERIT@ if the instance
-- inherits the default stack setting or to a a version number for a fixed
-- agent version.
iAgentVersion :: Lens' Instance (Maybe Text)
iAgentVersion = lens _iAgentVersion (\ s a -> s{_iAgentVersion = a});

-- | The root device volume ID.
iRootDeviceVolumeId :: Lens' Instance (Maybe Text)
iRootDeviceVolumeId = lens _iRootDeviceVolumeId (\ s a -> s{_iRootDeviceVolumeId = a});

-- | The instance\'s subnet ID; applicable only if the stack is running in a
-- VPC.
iSubnetId :: Lens' Instance (Maybe Text)
iSubnetId = lens _iSubnetId (\ s a -> s{_iSubnetId = a});

-- | The instance type, such as @t2.micro@.
iInstanceType :: Lens' Instance (Maybe Text)
iInstanceType = lens _iInstanceType (\ s a -> s{_iInstanceType = a});

-- | For registered instances, the infrastructure class: @ec2@ or
-- @on-premises@.
iInfrastructureClass :: Lens' Instance (Maybe Text)
iInfrastructureClass = lens _iInfrastructureClass (\ s a -> s{_iInfrastructureClass = a});

-- | Whether this is an Amazon EBS-optimized instance.
iEBSOptimized :: Lens' Instance (Maybe Bool)
iEBSOptimized = lens _iEBSOptimized (\ s a -> s{_iEBSOptimized = a});

-- | The SSH key\'s Deep Security Agent (DSA) fingerprint.
iSSHHostDsaKeyFingerprint :: Lens' Instance (Maybe Text)
iSSHHostDsaKeyFingerprint = lens _iSSHHostDsaKeyFingerprint (\ s a -> s{_iSSHHostDsaKeyFingerprint = a});

-- | The instance
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>.
iElasticIP :: Lens' Instance (Maybe Text)
iElasticIP = lens _iElasticIP (\ s a -> s{_iElasticIP = a});

-- | The instance\'s operating system.
iOS :: Lens' Instance (Maybe Text)
iOS = lens _iOS (\ s a -> s{_iOS = a});

-- | The instance Availability Zone. For more information, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
iAvailabilityZone :: Lens' Instance (Maybe Text)
iAvailabilityZone = lens _iAvailabilityZone (\ s a -> s{_iAvailabilityZone = a});

-- | The ID of the last service error. For more information, call
-- DescribeServiceErrors.
iLastServiceErrorId :: Lens' Instance (Maybe Text)
iLastServiceErrorId = lens _iLastServiceErrorId (\ s a -> s{_iLastServiceErrorId = a});

-- | For load-based or time-based instances, the type.
iAutoScalingType :: Lens' Instance (Maybe AutoScalingType)
iAutoScalingType = lens _iAutoScalingType (\ s a -> s{_iAutoScalingType = a});

-- | An array containing the instance layer IDs.
iLayerIds :: Lens' Instance [Text]
iLayerIds = lens _iLayerIds (\ s a -> s{_iLayerIds = a}) . _Default;

-- | The instance architecture: \"i386\" or \"x86_64\".
iArchitecture :: Lens' Instance (Maybe Architecture)
iArchitecture = lens _iArchitecture (\ s a -> s{_iArchitecture = a});

-- | The instance public DNS name.
iPublicDNS :: Lens' Instance (Maybe Text)
iPublicDNS = lens _iPublicDNS (\ s a -> s{_iPublicDNS = a});

-- | The instance public IP address.
iPublicIP :: Lens' Instance (Maybe Text)
iPublicIP = lens _iPublicIP (\ s a -> s{_iPublicIP = a});

-- | A custom AMI ID to be used to create the instance. For more information,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Instances>
iAMIId :: Lens' Instance (Maybe Text)
iAMIId = lens _iAMIId (\ s a -> s{_iAMIId = a});

-- | For registered instances, the reported operating system.
iReportedOS :: Lens' Instance (Maybe ReportedOS)
iReportedOS = lens _iReportedOS (\ s a -> s{_iReportedOS = a});

-- | The stack ID.
iStackId :: Lens' Instance (Maybe Text)
iStackId = lens _iStackId (\ s a -> s{_iStackId = a});

-- | For registered instances, who performed the registration.
iRegisteredBy :: Lens' Instance (Maybe Text)
iRegisteredBy = lens _iRegisteredBy (\ s a -> s{_iRegisteredBy = a});

-- | An array of @BlockDeviceMapping@ objects that specify the instance\'s
-- block device mappings.
iBlockDeviceMappings :: Lens' Instance [BlockDeviceMapping]
iBlockDeviceMappings = lens _iBlockDeviceMappings (\ s a -> s{_iBlockDeviceMappings = a}) . _Default;

-- | The instance\'s root device type. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
iRootDeviceType :: Lens' Instance (Maybe RootDeviceType)
iRootDeviceType = lens _iRootDeviceType (\ s a -> s{_iRootDeviceType = a});

instance FromJSON Instance where
        parseJSON
          = withObject "Instance"
              (\ x ->
                 Instance' <$>
                   (x .:? "InstanceId") <*> (x .:? "PrivateIp") <*>
                     (x .:? "InstallUpdatesOnBoot")
                     <*> (x .:? "ReportedAgentVersion")
                     <*> (x .:? "Status")
                     <*> (x .:? "PrivateDns")
                     <*> (x .:? "VirtualizationType")
                     <*> (x .:? "SecurityGroupIds" .!= mempty)
                     <*> (x .:? "SshHostRsaKeyFingerprint")
                     <*> (x .:? "InstanceProfileArn")
                     <*> (x .:? "Platform")
                     <*> (x .:? "Hostname")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "SshKeyName")
                     <*> (x .:? "Ec2InstanceId")
                     <*> (x .:? "AgentVersion")
                     <*> (x .:? "RootDeviceVolumeId")
                     <*> (x .:? "SubnetId")
                     <*> (x .:? "InstanceType")
                     <*> (x .:? "InfrastructureClass")
                     <*> (x .:? "EbsOptimized")
                     <*> (x .:? "SshHostDsaKeyFingerprint")
                     <*> (x .:? "ElasticIp")
                     <*> (x .:? "Os")
                     <*> (x .:? "AvailabilityZone")
                     <*> (x .:? "LastServiceErrorId")
                     <*> (x .:? "AutoScalingType")
                     <*> (x .:? "LayerIds" .!= mempty)
                     <*> (x .:? "Architecture")
                     <*> (x .:? "PublicDns")
                     <*> (x .:? "PublicIp")
                     <*> (x .:? "AmiId")
                     <*> (x .:? "ReportedOs")
                     <*> (x .:? "StackId")
                     <*> (x .:? "RegisteredBy")
                     <*> (x .:? "BlockDeviceMappings" .!= mempty)
                     <*> (x .:? "RootDeviceType"))

-- | Contains a description of an Amazon EC2 instance from the Amazon EC2
-- metadata service. For more information, see
-- <http://docs.aws.amazon.com/sdkfornet/latest/apidocs/Index.html Instance Metadata and User Data>.
--
-- /See:/ 'instanceIdentity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'iiSignature'
--
-- * 'iiDocument'
data InstanceIdentity = InstanceIdentity'
    { _iiSignature :: !(Maybe Text)
    , _iiDocument  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'InstanceIdentity' smart constructor.
instanceIdentity :: InstanceIdentity
instanceIdentity =
    InstanceIdentity'
    { _iiSignature = Nothing
    , _iiDocument = Nothing
    }

-- | A signature that can be used to verify the document\'s accuracy and
-- authenticity.
iiSignature :: Lens' InstanceIdentity (Maybe Text)
iiSignature = lens _iiSignature (\ s a -> s{_iiSignature = a});

-- | A JSON document that contains the metadata.
iiDocument :: Lens' InstanceIdentity (Maybe Text)
iiDocument = lens _iiDocument (\ s a -> s{_iiDocument = a});

instance ToJSON InstanceIdentity where
        toJSON InstanceIdentity'{..}
          = object
              ["Signature" .= _iiSignature,
               "Document" .= _iiDocument]

-- | Describes how many instances a stack has for each status.
--
-- /See:/ 'instancesCount' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'icTerminating'
--
-- * 'icPending'
--
-- * 'icOnline'
--
-- * 'icUnassigning'
--
-- * 'icRequested'
--
-- * 'icRunningSetup'
--
-- * 'icDeregistering'
--
-- * 'icBooting'
--
-- * 'icStopped'
--
-- * 'icRebooting'
--
-- * 'icAssigning'
--
-- * 'icShuttingDown'
--
-- * 'icSetupFailed'
--
-- * 'icConnectionLost'
--
-- * 'icTerminated'
--
-- * 'icStopping'
--
-- * 'icRegistered'
--
-- * 'icStartFailed'
--
-- * 'icRegistering'
data InstancesCount = InstancesCount'
    { _icTerminating    :: !(Maybe Int)
    , _icPending        :: !(Maybe Int)
    , _icOnline         :: !(Maybe Int)
    , _icUnassigning    :: !(Maybe Int)
    , _icRequested      :: !(Maybe Int)
    , _icRunningSetup   :: !(Maybe Int)
    , _icDeregistering  :: !(Maybe Int)
    , _icBooting        :: !(Maybe Int)
    , _icStopped        :: !(Maybe Int)
    , _icRebooting      :: !(Maybe Int)
    , _icAssigning      :: !(Maybe Int)
    , _icShuttingDown   :: !(Maybe Int)
    , _icSetupFailed    :: !(Maybe Int)
    , _icConnectionLost :: !(Maybe Int)
    , _icTerminated     :: !(Maybe Int)
    , _icStopping       :: !(Maybe Int)
    , _icRegistered     :: !(Maybe Int)
    , _icStartFailed    :: !(Maybe Int)
    , _icRegistering    :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'InstancesCount' smart constructor.
instancesCount :: InstancesCount
instancesCount =
    InstancesCount'
    { _icTerminating = Nothing
    , _icPending = Nothing
    , _icOnline = Nothing
    , _icUnassigning = Nothing
    , _icRequested = Nothing
    , _icRunningSetup = Nothing
    , _icDeregistering = Nothing
    , _icBooting = Nothing
    , _icStopped = Nothing
    , _icRebooting = Nothing
    , _icAssigning = Nothing
    , _icShuttingDown = Nothing
    , _icSetupFailed = Nothing
    , _icConnectionLost = Nothing
    , _icTerminated = Nothing
    , _icStopping = Nothing
    , _icRegistered = Nothing
    , _icStartFailed = Nothing
    , _icRegistering = Nothing
    }

-- | The number of instances with @terminating@ status.
icTerminating :: Lens' InstancesCount (Maybe Int)
icTerminating = lens _icTerminating (\ s a -> s{_icTerminating = a});

-- | The number of instances with @pending@ status.
icPending :: Lens' InstancesCount (Maybe Int)
icPending = lens _icPending (\ s a -> s{_icPending = a});

-- | The number of instances with @online@ status.
icOnline :: Lens' InstancesCount (Maybe Int)
icOnline = lens _icOnline (\ s a -> s{_icOnline = a});

-- | The number of instances in the Unassigning state.
icUnassigning :: Lens' InstancesCount (Maybe Int)
icUnassigning = lens _icUnassigning (\ s a -> s{_icUnassigning = a});

-- | The number of instances with @requested@ status.
icRequested :: Lens' InstancesCount (Maybe Int)
icRequested = lens _icRequested (\ s a -> s{_icRequested = a});

-- | The number of instances with @running_setup@ status.
icRunningSetup :: Lens' InstancesCount (Maybe Int)
icRunningSetup = lens _icRunningSetup (\ s a -> s{_icRunningSetup = a});

-- | The number of instances in the Deregistering state.
icDeregistering :: Lens' InstancesCount (Maybe Int)
icDeregistering = lens _icDeregistering (\ s a -> s{_icDeregistering = a});

-- | The number of instances with @booting@ status.
icBooting :: Lens' InstancesCount (Maybe Int)
icBooting = lens _icBooting (\ s a -> s{_icBooting = a});

-- | The number of instances with @stopped@ status.
icStopped :: Lens' InstancesCount (Maybe Int)
icStopped = lens _icStopped (\ s a -> s{_icStopped = a});

-- | The number of instances with @rebooting@ status.
icRebooting :: Lens' InstancesCount (Maybe Int)
icRebooting = lens _icRebooting (\ s a -> s{_icRebooting = a});

-- | The number of instances in the Assigning state.
icAssigning :: Lens' InstancesCount (Maybe Int)
icAssigning = lens _icAssigning (\ s a -> s{_icAssigning = a});

-- | The number of instances with @shutting_down@ status.
icShuttingDown :: Lens' InstancesCount (Maybe Int)
icShuttingDown = lens _icShuttingDown (\ s a -> s{_icShuttingDown = a});

-- | The number of instances with @setup_failed@ status.
icSetupFailed :: Lens' InstancesCount (Maybe Int)
icSetupFailed = lens _icSetupFailed (\ s a -> s{_icSetupFailed = a});

-- | The number of instances with @connection_lost@ status.
icConnectionLost :: Lens' InstancesCount (Maybe Int)
icConnectionLost = lens _icConnectionLost (\ s a -> s{_icConnectionLost = a});

-- | The number of instances with @terminated@ status.
icTerminated :: Lens' InstancesCount (Maybe Int)
icTerminated = lens _icTerminated (\ s a -> s{_icTerminated = a});

-- | The number of instances with @stopping@ status.
icStopping :: Lens' InstancesCount (Maybe Int)
icStopping = lens _icStopping (\ s a -> s{_icStopping = a});

-- | The number of instances in the Registered state.
icRegistered :: Lens' InstancesCount (Maybe Int)
icRegistered = lens _icRegistered (\ s a -> s{_icRegistered = a});

-- | The number of instances with @start_failed@ status.
icStartFailed :: Lens' InstancesCount (Maybe Int)
icStartFailed = lens _icStartFailed (\ s a -> s{_icStartFailed = a});

-- | The number of instances in the Registering state.
icRegistering :: Lens' InstancesCount (Maybe Int)
icRegistering = lens _icRegistering (\ s a -> s{_icRegistering = a});

instance FromJSON InstancesCount where
        parseJSON
          = withObject "InstancesCount"
              (\ x ->
                 InstancesCount' <$>
                   (x .:? "Terminating") <*> (x .:? "Pending") <*>
                     (x .:? "Online")
                     <*> (x .:? "Unassigning")
                     <*> (x .:? "Requested")
                     <*> (x .:? "RunningSetup")
                     <*> (x .:? "Deregistering")
                     <*> (x .:? "Booting")
                     <*> (x .:? "Stopped")
                     <*> (x .:? "Rebooting")
                     <*> (x .:? "Assigning")
                     <*> (x .:? "ShuttingDown")
                     <*> (x .:? "SetupFailed")
                     <*> (x .:? "ConnectionLost")
                     <*> (x .:? "Terminated")
                     <*> (x .:? "Stopping")
                     <*> (x .:? "Registered")
                     <*> (x .:? "StartFailed")
                     <*> (x .:? "Registering"))

-- | Describes a layer.
--
-- /See:/ 'layer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lCustomInstanceProfileARN'
--
-- * 'lInstallUpdatesOnBoot'
--
-- * 'lCustomSecurityGroupIds'
--
-- * 'lLifecycleEventConfiguration'
--
-- * 'lShortname'
--
-- * 'lCreatedAt'
--
-- * 'lDefaultRecipes'
--
-- * 'lCustomRecipes'
--
-- * 'lVolumeConfigurations'
--
-- * 'lEnableAutoHealing'
--
-- * 'lPackages'
--
-- * 'lName'
--
-- * 'lAttributes'
--
-- * 'lAutoAssignPublicIPs'
--
-- * 'lUseEBSOptimizedInstances'
--
-- * 'lType'
--
-- * 'lStackId'
--
-- * 'lLayerId'
--
-- * 'lDefaultSecurityGroupNames'
--
-- * 'lAutoAssignElasticIPs'
data Layer = Layer'
    { _lCustomInstanceProfileARN    :: !(Maybe Text)
    , _lInstallUpdatesOnBoot        :: !(Maybe Bool)
    , _lCustomSecurityGroupIds      :: !(Maybe [Text])
    , _lLifecycleEventConfiguration :: !(Maybe LifecycleEventConfiguration)
    , _lShortname                   :: !(Maybe Text)
    , _lCreatedAt                   :: !(Maybe Text)
    , _lDefaultRecipes              :: !(Maybe Recipes)
    , _lCustomRecipes               :: !(Maybe Recipes)
    , _lVolumeConfigurations        :: !(Maybe [VolumeConfiguration])
    , _lEnableAutoHealing           :: !(Maybe Bool)
    , _lPackages                    :: !(Maybe [Text])
    , _lName                        :: !(Maybe Text)
    , _lAttributes                  :: !(Maybe (Map LayerAttributesKeys Text))
    , _lAutoAssignPublicIPs         :: !(Maybe Bool)
    , _lUseEBSOptimizedInstances    :: !(Maybe Bool)
    , _lType                        :: !(Maybe LayerType)
    , _lStackId                     :: !(Maybe Text)
    , _lLayerId                     :: !(Maybe Text)
    , _lDefaultSecurityGroupNames   :: !(Maybe [Text])
    , _lAutoAssignElasticIPs        :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Layer' smart constructor.
layer :: Layer
layer =
    Layer'
    { _lCustomInstanceProfileARN = Nothing
    , _lInstallUpdatesOnBoot = Nothing
    , _lCustomSecurityGroupIds = Nothing
    , _lLifecycleEventConfiguration = Nothing
    , _lShortname = Nothing
    , _lCreatedAt = Nothing
    , _lDefaultRecipes = Nothing
    , _lCustomRecipes = Nothing
    , _lVolumeConfigurations = Nothing
    , _lEnableAutoHealing = Nothing
    , _lPackages = Nothing
    , _lName = Nothing
    , _lAttributes = Nothing
    , _lAutoAssignPublicIPs = Nothing
    , _lUseEBSOptimizedInstances = Nothing
    , _lType = Nothing
    , _lStackId = Nothing
    , _lLayerId = Nothing
    , _lDefaultSecurityGroupNames = Nothing
    , _lAutoAssignElasticIPs = Nothing
    }

-- | The ARN of the default IAM profile to be used for the layer\'s EC2
-- instances. For more information about IAM ARNs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
lCustomInstanceProfileARN :: Lens' Layer (Maybe Text)
lCustomInstanceProfileARN = lens _lCustomInstanceProfileARN (\ s a -> s{_lCustomInstanceProfileARN = a});

-- | Whether to install operating system and package updates when the
-- instance boots. The default value is @true@. If this value is set to
-- @false@, you must then update your instances manually by using
-- CreateDeployment to run the @update_dependencies@ stack command or
-- manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the
-- instances.
--
-- We strongly recommend using the default value of @true@, to ensure that
-- your instances have the latest security updates.
lInstallUpdatesOnBoot :: Lens' Layer (Maybe Bool)
lInstallUpdatesOnBoot = lens _lInstallUpdatesOnBoot (\ s a -> s{_lInstallUpdatesOnBoot = a});

-- | An array containing the layer\'s custom security group IDs.
lCustomSecurityGroupIds :: Lens' Layer [Text]
lCustomSecurityGroupIds = lens _lCustomSecurityGroupIds (\ s a -> s{_lCustomSecurityGroupIds = a}) . _Default;

-- | A @LifeCycleEventConfiguration@ object that specifies the Shutdown event
-- configuration.
lLifecycleEventConfiguration :: Lens' Layer (Maybe LifecycleEventConfiguration)
lLifecycleEventConfiguration = lens _lLifecycleEventConfiguration (\ s a -> s{_lLifecycleEventConfiguration = a});

-- | The layer short name.
lShortname :: Lens' Layer (Maybe Text)
lShortname = lens _lShortname (\ s a -> s{_lShortname = a});

-- | Date when the layer was created.
lCreatedAt :: Lens' Layer (Maybe Text)
lCreatedAt = lens _lCreatedAt (\ s a -> s{_lCreatedAt = a});

-- | FIXME: Undocumented member.
lDefaultRecipes :: Lens' Layer (Maybe Recipes)
lDefaultRecipes = lens _lDefaultRecipes (\ s a -> s{_lDefaultRecipes = a});

-- | A @LayerCustomRecipes@ object that specifies the layer\'s custom
-- recipes.
lCustomRecipes :: Lens' Layer (Maybe Recipes)
lCustomRecipes = lens _lCustomRecipes (\ s a -> s{_lCustomRecipes = a});

-- | A @VolumeConfigurations@ object that describes the layer\'s Amazon EBS
-- volumes.
lVolumeConfigurations :: Lens' Layer [VolumeConfiguration]
lVolumeConfigurations = lens _lVolumeConfigurations (\ s a -> s{_lVolumeConfigurations = a}) . _Default;

-- | Whether auto healing is disabled for the layer.
lEnableAutoHealing :: Lens' Layer (Maybe Bool)
lEnableAutoHealing = lens _lEnableAutoHealing (\ s a -> s{_lEnableAutoHealing = a});

-- | An array of @Package@ objects that describe the layer\'s packages.
lPackages :: Lens' Layer [Text]
lPackages = lens _lPackages (\ s a -> s{_lPackages = a}) . _Default;

-- | The layer name.
lName :: Lens' Layer (Maybe Text)
lName = lens _lName (\ s a -> s{_lName = a});

-- | The layer attributes.
--
-- For the @HaproxyStatsPassword@, @MysqlRootPassword@, and
-- @GangliaPassword@ attributes, AWS OpsWorks returns @*****FILTERED*****@
-- instead of the actual value
lAttributes :: Lens' Layer (HashMap LayerAttributesKeys Text)
lAttributes = lens _lAttributes (\ s a -> s{_lAttributes = a}) . _Default . _Map;

-- | For stacks that are running in a VPC, whether to automatically assign a
-- public IP address to the layer\'s instances. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
lAutoAssignPublicIPs :: Lens' Layer (Maybe Bool)
lAutoAssignPublicIPs = lens _lAutoAssignPublicIPs (\ s a -> s{_lAutoAssignPublicIPs = a});

-- | Whether the layer uses Amazon EBS-optimized instances.
lUseEBSOptimizedInstances :: Lens' Layer (Maybe Bool)
lUseEBSOptimizedInstances = lens _lUseEBSOptimizedInstances (\ s a -> s{_lUseEBSOptimizedInstances = a});

-- | The layer type.
lType :: Lens' Layer (Maybe LayerType)
lType = lens _lType (\ s a -> s{_lType = a});

-- | The layer stack ID.
lStackId :: Lens' Layer (Maybe Text)
lStackId = lens _lStackId (\ s a -> s{_lStackId = a});

-- | The layer ID.
lLayerId :: Lens' Layer (Maybe Text)
lLayerId = lens _lLayerId (\ s a -> s{_lLayerId = a});

-- | An array containing the layer\'s security group names.
lDefaultSecurityGroupNames :: Lens' Layer [Text]
lDefaultSecurityGroupNames = lens _lDefaultSecurityGroupNames (\ s a -> s{_lDefaultSecurityGroupNames = a}) . _Default;

-- | Whether to automatically assign an
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address>
-- to the layer\'s instances. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer>.
lAutoAssignElasticIPs :: Lens' Layer (Maybe Bool)
lAutoAssignElasticIPs = lens _lAutoAssignElasticIPs (\ s a -> s{_lAutoAssignElasticIPs = a});

instance FromJSON Layer where
        parseJSON
          = withObject "Layer"
              (\ x ->
                 Layer' <$>
                   (x .:? "CustomInstanceProfileArn") <*>
                     (x .:? "InstallUpdatesOnBoot")
                     <*> (x .:? "CustomSecurityGroupIds" .!= mempty)
                     <*> (x .:? "LifecycleEventConfiguration")
                     <*> (x .:? "Shortname")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "DefaultRecipes")
                     <*> (x .:? "CustomRecipes")
                     <*> (x .:? "VolumeConfigurations" .!= mempty)
                     <*> (x .:? "EnableAutoHealing")
                     <*> (x .:? "Packages" .!= mempty)
                     <*> (x .:? "Name")
                     <*> (x .:? "Attributes" .!= mempty)
                     <*> (x .:? "AutoAssignPublicIps")
                     <*> (x .:? "UseEbsOptimizedInstances")
                     <*> (x .:? "Type")
                     <*> (x .:? "StackId")
                     <*> (x .:? "LayerId")
                     <*> (x .:? "DefaultSecurityGroupNames" .!= mempty)
                     <*> (x .:? "AutoAssignElasticIps"))

-- | Specifies the lifecycle event configuration
--
-- /See:/ 'lifecycleEventConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lecShutdown'
newtype LifecycleEventConfiguration = LifecycleEventConfiguration'
    { _lecShutdown :: Maybe ShutdownEventConfiguration
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'LifecycleEventConfiguration' smart constructor.
lifecycleEventConfiguration :: LifecycleEventConfiguration
lifecycleEventConfiguration =
    LifecycleEventConfiguration'
    { _lecShutdown = Nothing
    }

-- | A @ShutdownEventConfiguration@ object that specifies the Shutdown event
-- configuration.
lecShutdown :: Lens' LifecycleEventConfiguration (Maybe ShutdownEventConfiguration)
lecShutdown = lens _lecShutdown (\ s a -> s{_lecShutdown = a});

instance FromJSON LifecycleEventConfiguration where
        parseJSON
          = withObject "LifecycleEventConfiguration"
              (\ x ->
                 LifecycleEventConfiguration' <$> (x .:? "Shutdown"))

instance ToJSON LifecycleEventConfiguration where
        toJSON LifecycleEventConfiguration'{..}
          = object ["Shutdown" .= _lecShutdown]

-- | Describes a layer\'s load-based auto scaling configuration.
--
-- /See:/ 'loadBasedAutoScalingConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbascUpScaling'
--
-- * 'lbascEnable'
--
-- * 'lbascDownScaling'
--
-- * 'lbascLayerId'
data LoadBasedAutoScalingConfiguration = LoadBasedAutoScalingConfiguration'
    { _lbascUpScaling   :: !(Maybe AutoScalingThresholds)
    , _lbascEnable      :: !(Maybe Bool)
    , _lbascDownScaling :: !(Maybe AutoScalingThresholds)
    , _lbascLayerId     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'LoadBasedAutoScalingConfiguration' smart constructor.
loadBasedAutoScalingConfiguration :: LoadBasedAutoScalingConfiguration
loadBasedAutoScalingConfiguration =
    LoadBasedAutoScalingConfiguration'
    { _lbascUpScaling = Nothing
    , _lbascEnable = Nothing
    , _lbascDownScaling = Nothing
    , _lbascLayerId = Nothing
    }

-- | An @AutoScalingThresholds@ object that describes the upscaling
-- configuration, which defines how and when AWS OpsWorks increases the
-- number of instances.
lbascUpScaling :: Lens' LoadBasedAutoScalingConfiguration (Maybe AutoScalingThresholds)
lbascUpScaling = lens _lbascUpScaling (\ s a -> s{_lbascUpScaling = a});

-- | Whether load-based auto scaling is enabled for the layer.
lbascEnable :: Lens' LoadBasedAutoScalingConfiguration (Maybe Bool)
lbascEnable = lens _lbascEnable (\ s a -> s{_lbascEnable = a});

-- | An @AutoScalingThresholds@ object that describes the downscaling
-- configuration, which defines how and when AWS OpsWorks reduces the
-- number of instances.
lbascDownScaling :: Lens' LoadBasedAutoScalingConfiguration (Maybe AutoScalingThresholds)
lbascDownScaling = lens _lbascDownScaling (\ s a -> s{_lbascDownScaling = a});

-- | The layer ID.
lbascLayerId :: Lens' LoadBasedAutoScalingConfiguration (Maybe Text)
lbascLayerId = lens _lbascLayerId (\ s a -> s{_lbascLayerId = a});

instance FromJSON LoadBasedAutoScalingConfiguration
         where
        parseJSON
          = withObject "LoadBasedAutoScalingConfiguration"
              (\ x ->
                 LoadBasedAutoScalingConfiguration' <$>
                   (x .:? "UpScaling") <*> (x .:? "Enable") <*>
                     (x .:? "DownScaling")
                     <*> (x .:? "LayerId"))

-- | Describes stack or user permissions.
--
-- /See:/ 'permission' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pIAMUserARN'
--
-- * 'pAllowSudo'
--
-- * 'pStackId'
--
-- * 'pLevel'
--
-- * 'pAllowSSH'
data Permission = Permission'
    { _pIAMUserARN :: !(Maybe Text)
    , _pAllowSudo  :: !(Maybe Bool)
    , _pStackId    :: !(Maybe Text)
    , _pLevel      :: !(Maybe Text)
    , _pAllowSSH   :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Permission' smart constructor.
permission :: Permission
permission =
    Permission'
    { _pIAMUserARN = Nothing
    , _pAllowSudo = Nothing
    , _pStackId = Nothing
    , _pLevel = Nothing
    , _pAllowSSH = Nothing
    }

-- | The Amazon Resource Name (ARN) for an AWS Identity and Access Management
-- (IAM) role. For more information about IAM ARNs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
pIAMUserARN :: Lens' Permission (Maybe Text)
pIAMUserARN = lens _pIAMUserARN (\ s a -> s{_pIAMUserARN = a});

-- | Whether the user can use __sudo__.
pAllowSudo :: Lens' Permission (Maybe Bool)
pAllowSudo = lens _pAllowSudo (\ s a -> s{_pAllowSudo = a});

-- | A stack ID.
pStackId :: Lens' Permission (Maybe Text)
pStackId = lens _pStackId (\ s a -> s{_pStackId = a});

-- | The user\'s permission level, which must be the following:
--
-- -   @deny@
-- -   @show@
-- -   @deploy@
-- -   @manage@
-- -   @iam_only@
--
-- For more information on the permissions associated with these levels,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>
pLevel :: Lens' Permission (Maybe Text)
pLevel = lens _pLevel (\ s a -> s{_pLevel = a});

-- | Whether the user can use SSH.
pAllowSSH :: Lens' Permission (Maybe Bool)
pAllowSSH = lens _pAllowSSH (\ s a -> s{_pAllowSSH = a});

instance FromJSON Permission where
        parseJSON
          = withObject "Permission"
              (\ x ->
                 Permission' <$>
                   (x .:? "IamUserArn") <*> (x .:? "AllowSudo") <*>
                     (x .:? "StackId")
                     <*> (x .:? "Level")
                     <*> (x .:? "AllowSsh"))

-- | Describes an instance\'s RAID array.
--
-- /See:/ 'rAIdArray' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'raiaInstanceId'
--
-- * 'raiaSize'
--
-- * 'raiaCreatedAt'
--
-- * 'raiaIOPS'
--
-- * 'raiaRAIdLevel'
--
-- * 'raiaDevice'
--
-- * 'raiaNumberOfDisks'
--
-- * 'raiaName'
--
-- * 'raiaAvailabilityZone'
--
-- * 'raiaRAIdArrayId'
--
-- * 'raiaVolumeType'
--
-- * 'raiaStackId'
--
-- * 'raiaMountPoint'
data RAIdArray = RAIdArray'
    { _raiaInstanceId       :: !(Maybe Text)
    , _raiaSize             :: !(Maybe Int)
    , _raiaCreatedAt        :: !(Maybe Text)
    , _raiaIOPS             :: !(Maybe Int)
    , _raiaRAIdLevel        :: !(Maybe Int)
    , _raiaDevice           :: !(Maybe Text)
    , _raiaNumberOfDisks    :: !(Maybe Int)
    , _raiaName             :: !(Maybe Text)
    , _raiaAvailabilityZone :: !(Maybe Text)
    , _raiaRAIdArrayId      :: !(Maybe Text)
    , _raiaVolumeType       :: !(Maybe Text)
    , _raiaStackId          :: !(Maybe Text)
    , _raiaMountPoint       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RAIdArray' smart constructor.
rAIdArray :: RAIdArray
rAIdArray =
    RAIdArray'
    { _raiaInstanceId = Nothing
    , _raiaSize = Nothing
    , _raiaCreatedAt = Nothing
    , _raiaIOPS = Nothing
    , _raiaRAIdLevel = Nothing
    , _raiaDevice = Nothing
    , _raiaNumberOfDisks = Nothing
    , _raiaName = Nothing
    , _raiaAvailabilityZone = Nothing
    , _raiaRAIdArrayId = Nothing
    , _raiaVolumeType = Nothing
    , _raiaStackId = Nothing
    , _raiaMountPoint = Nothing
    }

-- | The instance ID.
raiaInstanceId :: Lens' RAIdArray (Maybe Text)
raiaInstanceId = lens _raiaInstanceId (\ s a -> s{_raiaInstanceId = a});

-- | The array\'s size.
raiaSize :: Lens' RAIdArray (Maybe Int)
raiaSize = lens _raiaSize (\ s a -> s{_raiaSize = a});

-- | When the RAID array was created.
raiaCreatedAt :: Lens' RAIdArray (Maybe Text)
raiaCreatedAt = lens _raiaCreatedAt (\ s a -> s{_raiaCreatedAt = a});

-- | For PIOPS volumes, the IOPS per disk.
raiaIOPS :: Lens' RAIdArray (Maybe Int)
raiaIOPS = lens _raiaIOPS (\ s a -> s{_raiaIOPS = a});

-- | The <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
raiaRAIdLevel :: Lens' RAIdArray (Maybe Int)
raiaRAIdLevel = lens _raiaRAIdLevel (\ s a -> s{_raiaRAIdLevel = a});

-- | The array\'s Linux device. For example \/dev\/mdadm0.
raiaDevice :: Lens' RAIdArray (Maybe Text)
raiaDevice = lens _raiaDevice (\ s a -> s{_raiaDevice = a});

-- | The number of disks in the array.
raiaNumberOfDisks :: Lens' RAIdArray (Maybe Int)
raiaNumberOfDisks = lens _raiaNumberOfDisks (\ s a -> s{_raiaNumberOfDisks = a});

-- | The array name.
raiaName :: Lens' RAIdArray (Maybe Text)
raiaName = lens _raiaName (\ s a -> s{_raiaName = a});

-- | The array\'s Availability Zone. For more information, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
raiaAvailabilityZone :: Lens' RAIdArray (Maybe Text)
raiaAvailabilityZone = lens _raiaAvailabilityZone (\ s a -> s{_raiaAvailabilityZone = a});

-- | The array ID.
raiaRAIdArrayId :: Lens' RAIdArray (Maybe Text)
raiaRAIdArrayId = lens _raiaRAIdArrayId (\ s a -> s{_raiaRAIdArrayId = a});

-- | The volume type, standard or PIOPS.
raiaVolumeType :: Lens' RAIdArray (Maybe Text)
raiaVolumeType = lens _raiaVolumeType (\ s a -> s{_raiaVolumeType = a});

-- | The stack ID.
raiaStackId :: Lens' RAIdArray (Maybe Text)
raiaStackId = lens _raiaStackId (\ s a -> s{_raiaStackId = a});

-- | The array\'s mount point.
raiaMountPoint :: Lens' RAIdArray (Maybe Text)
raiaMountPoint = lens _raiaMountPoint (\ s a -> s{_raiaMountPoint = a});

instance FromJSON RAIdArray where
        parseJSON
          = withObject "RAIdArray"
              (\ x ->
                 RAIdArray' <$>
                   (x .:? "InstanceId") <*> (x .:? "Size") <*>
                     (x .:? "CreatedAt")
                     <*> (x .:? "Iops")
                     <*> (x .:? "RaidLevel")
                     <*> (x .:? "Device")
                     <*> (x .:? "NumberOfDisks")
                     <*> (x .:? "Name")
                     <*> (x .:? "AvailabilityZone")
                     <*> (x .:? "RaidArrayId")
                     <*> (x .:? "VolumeType")
                     <*> (x .:? "StackId")
                     <*> (x .:? "MountPoint"))

-- | Describes an Amazon RDS instance.
--
-- /See:/ 'rdsDBInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdiDBUser'
--
-- * 'rdiRDSDBInstanceARN'
--
-- * 'rdiMissingOnRDS'
--
-- * 'rdiEngine'
--
-- * 'rdiAddress'
--
-- * 'rdiDBInstanceIdentifier'
--
-- * 'rdiRegion'
--
-- * 'rdiStackId'
--
-- * 'rdiDBPassword'
data RDSDBInstance = RDSDBInstance'
    { _rdiDBUser               :: !(Maybe Text)
    , _rdiRDSDBInstanceARN     :: !(Maybe Text)
    , _rdiMissingOnRDS         :: !(Maybe Bool)
    , _rdiEngine               :: !(Maybe Text)
    , _rdiAddress              :: !(Maybe Text)
    , _rdiDBInstanceIdentifier :: !(Maybe Text)
    , _rdiRegion               :: !(Maybe Text)
    , _rdiStackId              :: !(Maybe Text)
    , _rdiDBPassword           :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RDSDBInstance' smart constructor.
rdsDBInstance :: RDSDBInstance
rdsDBInstance =
    RDSDBInstance'
    { _rdiDBUser = Nothing
    , _rdiRDSDBInstanceARN = Nothing
    , _rdiMissingOnRDS = Nothing
    , _rdiEngine = Nothing
    , _rdiAddress = Nothing
    , _rdiDBInstanceIdentifier = Nothing
    , _rdiRegion = Nothing
    , _rdiStackId = Nothing
    , _rdiDBPassword = Nothing
    }

-- | The master user name.
rdiDBUser :: Lens' RDSDBInstance (Maybe Text)
rdiDBUser = lens _rdiDBUser (\ s a -> s{_rdiDBUser = a});

-- | The instance\'s ARN.
rdiRDSDBInstanceARN :: Lens' RDSDBInstance (Maybe Text)
rdiRDSDBInstanceARN = lens _rdiRDSDBInstanceARN (\ s a -> s{_rdiRDSDBInstanceARN = a});

-- | Set to @true@ if AWS OpsWorks was unable to discover the Amazon RDS
-- instance. AWS OpsWorks attempts to discover the instance only once. If
-- this value is set to @true@, you must deregister the instance and then
-- register it again.
rdiMissingOnRDS :: Lens' RDSDBInstance (Maybe Bool)
rdiMissingOnRDS = lens _rdiMissingOnRDS (\ s a -> s{_rdiMissingOnRDS = a});

-- | The instance\'s database engine.
rdiEngine :: Lens' RDSDBInstance (Maybe Text)
rdiEngine = lens _rdiEngine (\ s a -> s{_rdiEngine = a});

-- | The instance\'s address.
rdiAddress :: Lens' RDSDBInstance (Maybe Text)
rdiAddress = lens _rdiAddress (\ s a -> s{_rdiAddress = a});

-- | The DB instance identifier.
rdiDBInstanceIdentifier :: Lens' RDSDBInstance (Maybe Text)
rdiDBInstanceIdentifier = lens _rdiDBInstanceIdentifier (\ s a -> s{_rdiDBInstanceIdentifier = a});

-- | The instance\'s AWS region.
rdiRegion :: Lens' RDSDBInstance (Maybe Text)
rdiRegion = lens _rdiRegion (\ s a -> s{_rdiRegion = a});

-- | The ID of the stack that the instance is registered with.
rdiStackId :: Lens' RDSDBInstance (Maybe Text)
rdiStackId = lens _rdiStackId (\ s a -> s{_rdiStackId = a});

-- | AWS OpsWorks returns @*****FILTERED*****@ instead of the actual value.
rdiDBPassword :: Lens' RDSDBInstance (Maybe Text)
rdiDBPassword = lens _rdiDBPassword (\ s a -> s{_rdiDBPassword = a});

instance FromJSON RDSDBInstance where
        parseJSON
          = withObject "RDSDBInstance"
              (\ x ->
                 RDSDBInstance' <$>
                   (x .:? "DbUser") <*> (x .:? "RdsDbInstanceArn") <*>
                     (x .:? "MissingOnRds")
                     <*> (x .:? "Engine")
                     <*> (x .:? "Address")
                     <*> (x .:? "DbInstanceIdentifier")
                     <*> (x .:? "Region")
                     <*> (x .:? "StackId")
                     <*> (x .:? "DbPassword"))

-- | AWS OpsWorks supports five lifecycle events: __setup__,
-- __configuration__, __deploy__, __undeploy__, and __shutdown__. For each
-- layer, AWS OpsWorks runs a set of standard recipes for each event. In
-- addition, you can provide custom recipes for any or all layers and
-- events. AWS OpsWorks runs custom event recipes after the standard
-- recipes. @LayerCustomRecipes@ specifies the custom recipes for a
-- particular layer to be run in response to each of the five events.
--
-- To specify a recipe, use the cookbook\'s directory name in the
-- repository followed by two colons and the recipe name, which is the
-- recipe\'s file name without the .rb extension. For example:
-- phpapp2::dbsetup specifies the dbsetup.rb recipe in the repository\'s
-- phpapp2 folder.
--
-- /See:/ 'recipes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rSetup'
--
-- * 'rUndeploy'
--
-- * 'rShutdown'
--
-- * 'rConfigure'
--
-- * 'rDeploy'
data Recipes = Recipes'
    { _rSetup     :: !(Maybe [Text])
    , _rUndeploy  :: !(Maybe [Text])
    , _rShutdown  :: !(Maybe [Text])
    , _rConfigure :: !(Maybe [Text])
    , _rDeploy    :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Recipes' smart constructor.
recipes :: Recipes
recipes =
    Recipes'
    { _rSetup = Nothing
    , _rUndeploy = Nothing
    , _rShutdown = Nothing
    , _rConfigure = Nothing
    , _rDeploy = Nothing
    }

-- | An array of custom recipe names to be run following a @setup@ event.
rSetup :: Lens' Recipes [Text]
rSetup = lens _rSetup (\ s a -> s{_rSetup = a}) . _Default;

-- | An array of custom recipe names to be run following a @undeploy@ event.
rUndeploy :: Lens' Recipes [Text]
rUndeploy = lens _rUndeploy (\ s a -> s{_rUndeploy = a}) . _Default;

-- | An array of custom recipe names to be run following a @shutdown@ event.
rShutdown :: Lens' Recipes [Text]
rShutdown = lens _rShutdown (\ s a -> s{_rShutdown = a}) . _Default;

-- | An array of custom recipe names to be run following a @configure@ event.
rConfigure :: Lens' Recipes [Text]
rConfigure = lens _rConfigure (\ s a -> s{_rConfigure = a}) . _Default;

-- | An array of custom recipe names to be run following a @deploy@ event.
rDeploy :: Lens' Recipes [Text]
rDeploy = lens _rDeploy (\ s a -> s{_rDeploy = a}) . _Default;

instance FromJSON Recipes where
        parseJSON
          = withObject "Recipes"
              (\ x ->
                 Recipes' <$>
                   (x .:? "Setup" .!= mempty) <*>
                     (x .:? "Undeploy" .!= mempty)
                     <*> (x .:? "Shutdown" .!= mempty)
                     <*> (x .:? "Configure" .!= mempty)
                     <*> (x .:? "Deploy" .!= mempty))

instance ToJSON Recipes where
        toJSON Recipes'{..}
          = object
              ["Setup" .= _rSetup, "Undeploy" .= _rUndeploy,
               "Shutdown" .= _rShutdown, "Configure" .= _rConfigure,
               "Deploy" .= _rDeploy]

-- | A registered instance\'s reported operating system.
--
-- /See:/ 'reportedOS' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'roFamily'
--
-- * 'roName'
--
-- * 'roVersion'
data ReportedOS = ReportedOS'
    { _roFamily  :: !(Maybe Text)
    , _roName    :: !(Maybe Text)
    , _roVersion :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReportedOS' smart constructor.
reportedOS :: ReportedOS
reportedOS =
    ReportedOS'
    { _roFamily = Nothing
    , _roName = Nothing
    , _roVersion = Nothing
    }

-- | The operating system family.
roFamily :: Lens' ReportedOS (Maybe Text)
roFamily = lens _roFamily (\ s a -> s{_roFamily = a});

-- | The operating system name.
roName :: Lens' ReportedOS (Maybe Text)
roName = lens _roName (\ s a -> s{_roName = a});

-- | The operating system version.
roVersion :: Lens' ReportedOS (Maybe Text)
roVersion = lens _roVersion (\ s a -> s{_roVersion = a});

instance FromJSON ReportedOS where
        parseJSON
          = withObject "ReportedOS"
              (\ x ->
                 ReportedOS' <$>
                   (x .:? "Family") <*> (x .:? "Name") <*>
                     (x .:? "Version"))

-- | Describes an app\'s SSL configuration.
--
-- /See:/ 'sslConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scChain'
--
-- * 'scCertificate'
--
-- * 'scPrivateKey'
data SSLConfiguration = SSLConfiguration'
    { _scChain       :: !(Maybe Text)
    , _scCertificate :: !Text
    , _scPrivateKey  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SSLConfiguration' smart constructor.
sslConfiguration :: Text -> Text -> SSLConfiguration
sslConfiguration pCertificate pPrivateKey =
    SSLConfiguration'
    { _scChain = Nothing
    , _scCertificate = pCertificate
    , _scPrivateKey = pPrivateKey
    }

-- | Optional. Can be used to specify an intermediate certificate authority
-- key or client authentication.
scChain :: Lens' SSLConfiguration (Maybe Text)
scChain = lens _scChain (\ s a -> s{_scChain = a});

-- | The contents of the certificate\'s domain.crt file.
scCertificate :: Lens' SSLConfiguration Text
scCertificate = lens _scCertificate (\ s a -> s{_scCertificate = a});

-- | The private key; the contents of the certificate\'s domain.kex file.
scPrivateKey :: Lens' SSLConfiguration Text
scPrivateKey = lens _scPrivateKey (\ s a -> s{_scPrivateKey = a});

instance FromJSON SSLConfiguration where
        parseJSON
          = withObject "SSLConfiguration"
              (\ x ->
                 SSLConfiguration' <$>
                   (x .:? "Chain") <*> (x .: "Certificate") <*>
                     (x .: "PrivateKey"))

instance ToJSON SSLConfiguration where
        toJSON SSLConfiguration'{..}
          = object
              ["Chain" .= _scChain,
               "Certificate" .= _scCertificate,
               "PrivateKey" .= _scPrivateKey]

-- | Describes a user\'s SSH information.
--
-- /See:/ 'selfUserProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'supSSHUsername'
--
-- * 'supSSHPublicKey'
--
-- * 'supIAMUserARN'
--
-- * 'supName'
data SelfUserProfile = SelfUserProfile'
    { _supSSHUsername  :: !(Maybe Text)
    , _supSSHPublicKey :: !(Maybe Text)
    , _supIAMUserARN   :: !(Maybe Text)
    , _supName         :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SelfUserProfile' smart constructor.
selfUserProfile :: SelfUserProfile
selfUserProfile =
    SelfUserProfile'
    { _supSSHUsername = Nothing
    , _supSSHPublicKey = Nothing
    , _supIAMUserARN = Nothing
    , _supName = Nothing
    }

-- | The user\'s SSH user name.
supSSHUsername :: Lens' SelfUserProfile (Maybe Text)
supSSHUsername = lens _supSSHUsername (\ s a -> s{_supSSHUsername = a});

-- | The user\'s SSH public key.
supSSHPublicKey :: Lens' SelfUserProfile (Maybe Text)
supSSHPublicKey = lens _supSSHPublicKey (\ s a -> s{_supSSHPublicKey = a});

-- | The user\'s IAM ARN.
supIAMUserARN :: Lens' SelfUserProfile (Maybe Text)
supIAMUserARN = lens _supIAMUserARN (\ s a -> s{_supIAMUserARN = a});

-- | The user\'s name.
supName :: Lens' SelfUserProfile (Maybe Text)
supName = lens _supName (\ s a -> s{_supName = a});

instance FromJSON SelfUserProfile where
        parseJSON
          = withObject "SelfUserProfile"
              (\ x ->
                 SelfUserProfile' <$>
                   (x .:? "SshUsername") <*> (x .:? "SshPublicKey") <*>
                     (x .:? "IamUserArn")
                     <*> (x .:? "Name"))

-- | Describes an AWS OpsWorks service error.
--
-- /See:/ 'serviceError'' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'seInstanceId'
--
-- * 'seCreatedAt'
--
-- * 'seServiceErrorId'
--
-- * 'seType'
--
-- * 'seMessage'
--
-- * 'seStackId'
data ServiceError' = ServiceError''
    { _seInstanceId     :: !(Maybe Text)
    , _seCreatedAt      :: !(Maybe Text)
    , _seServiceErrorId :: !(Maybe Text)
    , _seType           :: !(Maybe Text)
    , _seMessage        :: !(Maybe Text)
    , _seStackId        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ServiceError'' smart constructor.
serviceError' :: ServiceError'
serviceError' =
    ServiceError''
    { _seInstanceId = Nothing
    , _seCreatedAt = Nothing
    , _seServiceErrorId = Nothing
    , _seType = Nothing
    , _seMessage = Nothing
    , _seStackId = Nothing
    }

-- | The instance ID.
seInstanceId :: Lens' ServiceError' (Maybe Text)
seInstanceId = lens _seInstanceId (\ s a -> s{_seInstanceId = a});

-- | When the error occurred.
seCreatedAt :: Lens' ServiceError' (Maybe Text)
seCreatedAt = lens _seCreatedAt (\ s a -> s{_seCreatedAt = a});

-- | The error ID.
seServiceErrorId :: Lens' ServiceError' (Maybe Text)
seServiceErrorId = lens _seServiceErrorId (\ s a -> s{_seServiceErrorId = a});

-- | The error type.
seType :: Lens' ServiceError' (Maybe Text)
seType = lens _seType (\ s a -> s{_seType = a});

-- | A message that describes the error.
seMessage :: Lens' ServiceError' (Maybe Text)
seMessage = lens _seMessage (\ s a -> s{_seMessage = a});

-- | The stack ID.
seStackId :: Lens' ServiceError' (Maybe Text)
seStackId = lens _seStackId (\ s a -> s{_seStackId = a});

instance FromJSON ServiceError' where
        parseJSON
          = withObject "ServiceError'"
              (\ x ->
                 ServiceError'' <$>
                   (x .:? "InstanceId") <*> (x .:? "CreatedAt") <*>
                     (x .:? "ServiceErrorId")
                     <*> (x .:? "Type")
                     <*> (x .:? "Message")
                     <*> (x .:? "StackId"))

-- | The Shutdown event configuration.
--
-- /See:/ 'shutdownEventConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'secExecutionTimeout'
--
-- * 'secDelayUntilElbConnectionsDrained'
data ShutdownEventConfiguration = ShutdownEventConfiguration'
    { _secExecutionTimeout                :: !(Maybe Int)
    , _secDelayUntilElbConnectionsDrained :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ShutdownEventConfiguration' smart constructor.
shutdownEventConfiguration :: ShutdownEventConfiguration
shutdownEventConfiguration =
    ShutdownEventConfiguration'
    { _secExecutionTimeout = Nothing
    , _secDelayUntilElbConnectionsDrained = Nothing
    }

-- | The time, in seconds, that AWS OpsWorks will wait after triggering a
-- Shutdown event before shutting down an instance.
secExecutionTimeout :: Lens' ShutdownEventConfiguration (Maybe Int)
secExecutionTimeout = lens _secExecutionTimeout (\ s a -> s{_secExecutionTimeout = a});

-- | Whether to enable Elastic Load Balancing connection draining. For more
-- information, see
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#conn-drain Connection Draining>
secDelayUntilElbConnectionsDrained :: Lens' ShutdownEventConfiguration (Maybe Bool)
secDelayUntilElbConnectionsDrained = lens _secDelayUntilElbConnectionsDrained (\ s a -> s{_secDelayUntilElbConnectionsDrained = a});

instance FromJSON ShutdownEventConfiguration where
        parseJSON
          = withObject "ShutdownEventConfiguration"
              (\ x ->
                 ShutdownEventConfiguration' <$>
                   (x .:? "ExecutionTimeout") <*>
                     (x .:? "DelayUntilElbConnectionsDrained"))

instance ToJSON ShutdownEventConfiguration where
        toJSON ShutdownEventConfiguration'{..}
          = object
              ["ExecutionTimeout" .= _secExecutionTimeout,
               "DelayUntilElbConnectionsDrained" .=
                 _secDelayUntilElbConnectionsDrained]

-- | Contains the information required to retrieve an app or cookbook from a
-- repository. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Creating Apps>
-- or
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Custom Recipes and Cookbooks>.
--
-- /See:/ 'source' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sURL'
--
-- * 'sUsername'
--
-- * 'sSSHKey'
--
-- * 'sPassword'
--
-- * 'sType'
--
-- * 'sRevision'
data Source = Source'
    { _sURL      :: !(Maybe Text)
    , _sUsername :: !(Maybe Text)
    , _sSSHKey   :: !(Maybe Text)
    , _sPassword :: !(Maybe Text)
    , _sType     :: !(Maybe SourceType)
    , _sRevision :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Source' smart constructor.
source :: Source
source =
    Source'
    { _sURL = Nothing
    , _sUsername = Nothing
    , _sSSHKey = Nothing
    , _sPassword = Nothing
    , _sType = Nothing
    , _sRevision = Nothing
    }

-- | The source URL.
sURL :: Lens' Source (Maybe Text)
sURL = lens _sURL (\ s a -> s{_sURL = a});

-- | This parameter depends on the repository type.
--
-- -   For Amazon S3 bundles, set @Username@ to the appropriate IAM access
--     key ID.
-- -   For HTTP bundles, Git repositories, and Subversion repositories, set
--     @Username@ to the user name.
sUsername :: Lens' Source (Maybe Text)
sUsername = lens _sUsername (\ s a -> s{_sUsername = a});

-- | In requests, the repository\'s SSH key.
--
-- In responses, AWS OpsWorks returns @*****FILTERED*****@ instead of the
-- actual value.
sSSHKey :: Lens' Source (Maybe Text)
sSSHKey = lens _sSSHKey (\ s a -> s{_sSSHKey = a});

-- | When included in a request, the parameter depends on the repository
-- type.
--
-- -   For Amazon S3 bundles, set @Password@ to the appropriate IAM secret
--     access key.
-- -   For HTTP bundles and Subversion repositories, set @Password@ to the
--     password.
--
-- For more information on how to safely handle IAM credentials, see
-- <http://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html >.
--
-- In responses, AWS OpsWorks returns @*****FILTERED*****@ instead of the
-- actual value.
sPassword :: Lens' Source (Maybe Text)
sPassword = lens _sPassword (\ s a -> s{_sPassword = a});

-- | The repository type.
sType :: Lens' Source (Maybe SourceType)
sType = lens _sType (\ s a -> s{_sType = a});

-- | The application\'s version. AWS OpsWorks enables you to easily deploy
-- new versions of an application. One of the simplest approaches is to
-- have branches or revisions in your repository that represent different
-- versions that can potentially be deployed.
sRevision :: Lens' Source (Maybe Text)
sRevision = lens _sRevision (\ s a -> s{_sRevision = a});

instance FromJSON Source where
        parseJSON
          = withObject "Source"
              (\ x ->
                 Source' <$>
                   (x .:? "Url") <*> (x .:? "Username") <*>
                     (x .:? "SshKey")
                     <*> (x .:? "Password")
                     <*> (x .:? "Type")
                     <*> (x .:? "Revision"))

instance ToJSON Source where
        toJSON Source'{..}
          = object
              ["Url" .= _sURL, "Username" .= _sUsername,
               "SshKey" .= _sSSHKey, "Password" .= _sPassword,
               "Type" .= _sType, "Revision" .= _sRevision]

-- | Describes a stack.
--
-- /See:/ 'stack' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sDefaultInstanceProfileARN'
--
-- * 'sServiceRoleARN'
--
-- * 'sARN'
--
-- * 'sDefaultRootDeviceType'
--
-- * 'sCreatedAt'
--
-- * 'sChefConfiguration'
--
-- * 'sVPCId'
--
-- * 'sAgentVersion'
--
-- * 'sDefaultSSHKeyName'
--
-- * 'sCustomJSON'
--
-- * 'sCustomCookbooksSource'
--
-- * 'sDefaultAvailabilityZone'
--
-- * 'sName'
--
-- * 'sUseOpsworksSecurityGroups'
--
-- * 'sDefaultOS'
--
-- * 'sAttributes'
--
-- * 'sUseCustomCookbooks'
--
-- * 'sDefaultSubnetId'
--
-- * 'sRegion'
--
-- * 'sConfigurationManager'
--
-- * 'sStackId'
--
-- * 'sHostnameTheme'
data Stack = Stack'
    { _sDefaultInstanceProfileARN :: !(Maybe Text)
    , _sServiceRoleARN            :: !(Maybe Text)
    , _sARN                       :: !(Maybe Text)
    , _sDefaultRootDeviceType     :: !(Maybe RootDeviceType)
    , _sCreatedAt                 :: !(Maybe Text)
    , _sChefConfiguration         :: !(Maybe ChefConfiguration)
    , _sVPCId                     :: !(Maybe Text)
    , _sAgentVersion              :: !(Maybe Text)
    , _sDefaultSSHKeyName         :: !(Maybe Text)
    , _sCustomJSON                :: !(Maybe Text)
    , _sCustomCookbooksSource     :: !(Maybe Source)
    , _sDefaultAvailabilityZone   :: !(Maybe Text)
    , _sName                      :: !(Maybe Text)
    , _sUseOpsworksSecurityGroups :: !(Maybe Bool)
    , _sDefaultOS                 :: !(Maybe Text)
    , _sAttributes                :: !(Maybe (Map StackAttributesKeys Text))
    , _sUseCustomCookbooks        :: !(Maybe Bool)
    , _sDefaultSubnetId           :: !(Maybe Text)
    , _sRegion                    :: !(Maybe Text)
    , _sConfigurationManager      :: !(Maybe StackConfigurationManager)
    , _sStackId                   :: !(Maybe Text)
    , _sHostnameTheme             :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Stack' smart constructor.
stack :: Stack
stack =
    Stack'
    { _sDefaultInstanceProfileARN = Nothing
    , _sServiceRoleARN = Nothing
    , _sARN = Nothing
    , _sDefaultRootDeviceType = Nothing
    , _sCreatedAt = Nothing
    , _sChefConfiguration = Nothing
    , _sVPCId = Nothing
    , _sAgentVersion = Nothing
    , _sDefaultSSHKeyName = Nothing
    , _sCustomJSON = Nothing
    , _sCustomCookbooksSource = Nothing
    , _sDefaultAvailabilityZone = Nothing
    , _sName = Nothing
    , _sUseOpsworksSecurityGroups = Nothing
    , _sDefaultOS = Nothing
    , _sAttributes = Nothing
    , _sUseCustomCookbooks = Nothing
    , _sDefaultSubnetId = Nothing
    , _sRegion = Nothing
    , _sConfigurationManager = Nothing
    , _sStackId = Nothing
    , _sHostnameTheme = Nothing
    }

-- | The ARN of an IAM profile that is the default profile for all of the
-- stack\'s EC2 instances. For more information about IAM ARNs, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers>.
sDefaultInstanceProfileARN :: Lens' Stack (Maybe Text)
sDefaultInstanceProfileARN = lens _sDefaultInstanceProfileARN (\ s a -> s{_sDefaultInstanceProfileARN = a});

-- | The stack AWS Identity and Access Management (IAM) role.
sServiceRoleARN :: Lens' Stack (Maybe Text)
sServiceRoleARN = lens _sServiceRoleARN (\ s a -> s{_sServiceRoleARN = a});

-- | The stack\'s ARN.
sARN :: Lens' Stack (Maybe Text)
sARN = lens _sARN (\ s a -> s{_sARN = a});

-- | The default root device type. This value is used by default for all
-- instances in the stack, but you can override it when you create an
-- instance. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device>.
sDefaultRootDeviceType :: Lens' Stack (Maybe RootDeviceType)
sDefaultRootDeviceType = lens _sDefaultRootDeviceType (\ s a -> s{_sDefaultRootDeviceType = a});

-- | The date when the stack was created.
sCreatedAt :: Lens' Stack (Maybe Text)
sCreatedAt = lens _sCreatedAt (\ s a -> s{_sCreatedAt = a});

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf
-- and the Berkshelf version. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack>.
sChefConfiguration :: Lens' Stack (Maybe ChefConfiguration)
sChefConfiguration = lens _sChefConfiguration (\ s a -> s{_sChefConfiguration = a});

-- | The VPC ID; applicable only if the stack is running in a VPC.
sVPCId :: Lens' Stack (Maybe Text)
sVPCId = lens _sVPCId (\ s a -> s{_sVPCId = a});

-- | The agent version. This parameter is set to @LATEST@ for auto-update. or
-- a version number for a fixed agent version.
sAgentVersion :: Lens' Stack (Maybe Text)
sAgentVersion = lens _sAgentVersion (\ s a -> s{_sAgentVersion = a});

-- | A default Amazon EC2 key pair for the stack\'s instances. You can
-- override this value when you create or update an instance.
sDefaultSSHKeyName :: Lens' Stack (Maybe Text)
sDefaultSSHKeyName = lens _sDefaultSSHKeyName (\ s a -> s{_sDefaultSSHKeyName = a});

-- | A JSON object that contains user-defined attributes to be added to the
-- stack configuration and deployment attributes. You can use custom JSON
-- to override the corresponding default stack configuration attribute
-- values or to pass data to recipes. The string should be in the following
-- format and must escape characters such as \'\"\':
--
-- @\"{\\\"key1\\\": \\\"value1\\\", \\\"key2\\\": \\\"value2\\\",...}\"@
--
-- For more information on custom JSON, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes>.
sCustomJSON :: Lens' Stack (Maybe Text)
sCustomJSON = lens _sCustomJSON (\ s a -> s{_sCustomJSON = a});

-- | FIXME: Undocumented member.
sCustomCookbooksSource :: Lens' Stack (Maybe Source)
sCustomCookbooksSource = lens _sCustomCookbooksSource (\ s a -> s{_sCustomCookbooksSource = a});

-- | The stack\'s default Availability Zone. For more information, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
sDefaultAvailabilityZone :: Lens' Stack (Maybe Text)
sDefaultAvailabilityZone = lens _sDefaultAvailabilityZone (\ s a -> s{_sDefaultAvailabilityZone = a});

-- | The stack name.
sName :: Lens' Stack (Maybe Text)
sName = lens _sName (\ s a -> s{_sName = a});

-- | Whether the stack automatically associates the AWS OpsWorks built-in
-- security groups with the stack\'s layers.
sUseOpsworksSecurityGroups :: Lens' Stack (Maybe Bool)
sUseOpsworksSecurityGroups = lens _sUseOpsworksSecurityGroups (\ s a -> s{_sUseOpsworksSecurityGroups = a});

-- | The stack\'s default operating system.
sDefaultOS :: Lens' Stack (Maybe Text)
sDefaultOS = lens _sDefaultOS (\ s a -> s{_sDefaultOS = a});

-- | The stack\'s attributes.
sAttributes :: Lens' Stack (HashMap StackAttributesKeys Text)
sAttributes = lens _sAttributes (\ s a -> s{_sAttributes = a}) . _Default . _Map;

-- | Whether the stack uses custom cookbooks.
sUseCustomCookbooks :: Lens' Stack (Maybe Bool)
sUseCustomCookbooks = lens _sUseCustomCookbooks (\ s a -> s{_sUseCustomCookbooks = a});

-- | The default subnet ID; applicable only if the stack is running in a VPC.
sDefaultSubnetId :: Lens' Stack (Maybe Text)
sDefaultSubnetId = lens _sDefaultSubnetId (\ s a -> s{_sDefaultSubnetId = a});

-- | The stack AWS region, such as \"us-east-1\". For more information about
-- AWS regions, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
sRegion :: Lens' Stack (Maybe Text)
sRegion = lens _sRegion (\ s a -> s{_sRegion = a});

-- | The configuration manager.
sConfigurationManager :: Lens' Stack (Maybe StackConfigurationManager)
sConfigurationManager = lens _sConfigurationManager (\ s a -> s{_sConfigurationManager = a});

-- | The stack ID.
sStackId :: Lens' Stack (Maybe Text)
sStackId = lens _sStackId (\ s a -> s{_sStackId = a});

-- | The stack host name theme, with spaces replaced by underscores.
sHostnameTheme :: Lens' Stack (Maybe Text)
sHostnameTheme = lens _sHostnameTheme (\ s a -> s{_sHostnameTheme = a});

instance FromJSON Stack where
        parseJSON
          = withObject "Stack"
              (\ x ->
                 Stack' <$>
                   (x .:? "DefaultInstanceProfileArn") <*>
                     (x .:? "ServiceRoleArn")
                     <*> (x .:? "Arn")
                     <*> (x .:? "DefaultRootDeviceType")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "ChefConfiguration")
                     <*> (x .:? "VpcId")
                     <*> (x .:? "AgentVersion")
                     <*> (x .:? "DefaultSshKeyName")
                     <*> (x .:? "CustomJson")
                     <*> (x .:? "CustomCookbooksSource")
                     <*> (x .:? "DefaultAvailabilityZone")
                     <*> (x .:? "Name")
                     <*> (x .:? "UseOpsworksSecurityGroups")
                     <*> (x .:? "DefaultOs")
                     <*> (x .:? "Attributes" .!= mempty)
                     <*> (x .:? "UseCustomCookbooks")
                     <*> (x .:? "DefaultSubnetId")
                     <*> (x .:? "Region")
                     <*> (x .:? "ConfigurationManager")
                     <*> (x .:? "StackId")
                     <*> (x .:? "HostnameTheme"))

-- | Describes the configuration manager.
--
-- /See:/ 'stackConfigurationManager' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'scmName'
--
-- * 'scmVersion'
data StackConfigurationManager = StackConfigurationManager'
    { _scmName    :: !(Maybe Text)
    , _scmVersion :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StackConfigurationManager' smart constructor.
stackConfigurationManager :: StackConfigurationManager
stackConfigurationManager =
    StackConfigurationManager'
    { _scmName = Nothing
    , _scmVersion = Nothing
    }

-- | The name. This parameter must be set to \"Chef\".
scmName :: Lens' StackConfigurationManager (Maybe Text)
scmName = lens _scmName (\ s a -> s{_scmName = a});

-- | The Chef version. This parameter must be set to 0.9, 11.4, or 11.10. The
-- default value is 11.4.
scmVersion :: Lens' StackConfigurationManager (Maybe Text)
scmVersion = lens _scmVersion (\ s a -> s{_scmVersion = a});

instance FromJSON StackConfigurationManager where
        parseJSON
          = withObject "StackConfigurationManager"
              (\ x ->
                 StackConfigurationManager' <$>
                   (x .:? "Name") <*> (x .:? "Version"))

instance ToJSON StackConfigurationManager where
        toJSON StackConfigurationManager'{..}
          = object
              ["Name" .= _scmName, "Version" .= _scmVersion]

-- | Summarizes the number of layers, instances, and apps in a stack.
--
-- /See:/ 'stackSummary' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssARN'
--
-- * 'ssAppsCount'
--
-- * 'ssName'
--
-- * 'ssStackId'
--
-- * 'ssLayersCount'
--
-- * 'ssInstancesCount'
data StackSummary = StackSummary'
    { _ssARN            :: !(Maybe Text)
    , _ssAppsCount      :: !(Maybe Int)
    , _ssName           :: !(Maybe Text)
    , _ssStackId        :: !(Maybe Text)
    , _ssLayersCount    :: !(Maybe Int)
    , _ssInstancesCount :: !(Maybe InstancesCount)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'StackSummary' smart constructor.
stackSummary :: StackSummary
stackSummary =
    StackSummary'
    { _ssARN = Nothing
    , _ssAppsCount = Nothing
    , _ssName = Nothing
    , _ssStackId = Nothing
    , _ssLayersCount = Nothing
    , _ssInstancesCount = Nothing
    }

-- | The stack\'s ARN.
ssARN :: Lens' StackSummary (Maybe Text)
ssARN = lens _ssARN (\ s a -> s{_ssARN = a});

-- | The number of apps.
ssAppsCount :: Lens' StackSummary (Maybe Int)
ssAppsCount = lens _ssAppsCount (\ s a -> s{_ssAppsCount = a});

-- | The stack name.
ssName :: Lens' StackSummary (Maybe Text)
ssName = lens _ssName (\ s a -> s{_ssName = a});

-- | The stack ID.
ssStackId :: Lens' StackSummary (Maybe Text)
ssStackId = lens _ssStackId (\ s a -> s{_ssStackId = a});

-- | The number of layers.
ssLayersCount :: Lens' StackSummary (Maybe Int)
ssLayersCount = lens _ssLayersCount (\ s a -> s{_ssLayersCount = a});

-- | An @InstancesCount@ object with the number of instances in each status.
ssInstancesCount :: Lens' StackSummary (Maybe InstancesCount)
ssInstancesCount = lens _ssInstancesCount (\ s a -> s{_ssInstancesCount = a});

instance FromJSON StackSummary where
        parseJSON
          = withObject "StackSummary"
              (\ x ->
                 StackSummary' <$>
                   (x .:? "Arn") <*> (x .:? "AppsCount") <*>
                     (x .:? "Name")
                     <*> (x .:? "StackId")
                     <*> (x .:? "LayersCount")
                     <*> (x .:? "InstancesCount"))

-- | Contains the data needed by RDP clients such as the Microsoft Remote
-- Desktop Connection to log in to the instance.
--
-- /See:/ 'temporaryCredential' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tcInstanceId'
--
-- * 'tcUsername'
--
-- * 'tcPassword'
--
-- * 'tcValidForInMinutes'
data TemporaryCredential = TemporaryCredential'
    { _tcInstanceId        :: !(Maybe Text)
    , _tcUsername          :: !(Maybe Text)
    , _tcPassword          :: !(Maybe Text)
    , _tcValidForInMinutes :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TemporaryCredential' smart constructor.
temporaryCredential :: TemporaryCredential
temporaryCredential =
    TemporaryCredential'
    { _tcInstanceId = Nothing
    , _tcUsername = Nothing
    , _tcPassword = Nothing
    , _tcValidForInMinutes = Nothing
    }

-- | The instance\'s AWS OpsWorks ID.
tcInstanceId :: Lens' TemporaryCredential (Maybe Text)
tcInstanceId = lens _tcInstanceId (\ s a -> s{_tcInstanceId = a});

-- | The user name.
tcUsername :: Lens' TemporaryCredential (Maybe Text)
tcUsername = lens _tcUsername (\ s a -> s{_tcUsername = a});

-- | The password.
tcPassword :: Lens' TemporaryCredential (Maybe Text)
tcPassword = lens _tcPassword (\ s a -> s{_tcPassword = a});

-- | The length of time (in minutes) that the grant is valid. When the grant
-- expires, at the end of this period, the user will no longer be able to
-- use the credentials to log in. If they are logged in at the time, they
-- will be automatically logged out.
tcValidForInMinutes :: Lens' TemporaryCredential (Maybe Int)
tcValidForInMinutes = lens _tcValidForInMinutes (\ s a -> s{_tcValidForInMinutes = a});

instance FromJSON TemporaryCredential where
        parseJSON
          = withObject "TemporaryCredential"
              (\ x ->
                 TemporaryCredential' <$>
                   (x .:? "InstanceId") <*> (x .:? "Username") <*>
                     (x .:? "Password")
                     <*> (x .:? "ValidForInMinutes"))

-- | Describes an instance\'s time-based auto scaling configuration.
--
-- /See:/ 'timeBasedAutoScalingConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tbascInstanceId'
--
-- * 'tbascAutoScalingSchedule'
data TimeBasedAutoScalingConfiguration = TimeBasedAutoScalingConfiguration'
    { _tbascInstanceId          :: !(Maybe Text)
    , _tbascAutoScalingSchedule :: !(Maybe WeeklyAutoScalingSchedule)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TimeBasedAutoScalingConfiguration' smart constructor.
timeBasedAutoScalingConfiguration :: TimeBasedAutoScalingConfiguration
timeBasedAutoScalingConfiguration =
    TimeBasedAutoScalingConfiguration'
    { _tbascInstanceId = Nothing
    , _tbascAutoScalingSchedule = Nothing
    }

-- | The instance ID.
tbascInstanceId :: Lens' TimeBasedAutoScalingConfiguration (Maybe Text)
tbascInstanceId = lens _tbascInstanceId (\ s a -> s{_tbascInstanceId = a});

-- | A @WeeklyAutoScalingSchedule@ object with the instance schedule.
tbascAutoScalingSchedule :: Lens' TimeBasedAutoScalingConfiguration (Maybe WeeklyAutoScalingSchedule)
tbascAutoScalingSchedule = lens _tbascAutoScalingSchedule (\ s a -> s{_tbascAutoScalingSchedule = a});

instance FromJSON TimeBasedAutoScalingConfiguration
         where
        parseJSON
          = withObject "TimeBasedAutoScalingConfiguration"
              (\ x ->
                 TimeBasedAutoScalingConfiguration' <$>
                   (x .:? "InstanceId") <*>
                     (x .:? "AutoScalingSchedule"))

-- | Describes a user\'s SSH information.
--
-- /See:/ 'userProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'upSSHUsername'
--
-- * 'upSSHPublicKey'
--
-- * 'upAllowSelfManagement'
--
-- * 'upIAMUserARN'
--
-- * 'upName'
data UserProfile = UserProfile'
    { _upSSHUsername         :: !(Maybe Text)
    , _upSSHPublicKey        :: !(Maybe Text)
    , _upAllowSelfManagement :: !(Maybe Bool)
    , _upIAMUserARN          :: !(Maybe Text)
    , _upName                :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UserProfile' smart constructor.
userProfile :: UserProfile
userProfile =
    UserProfile'
    { _upSSHUsername = Nothing
    , _upSSHPublicKey = Nothing
    , _upAllowSelfManagement = Nothing
    , _upIAMUserARN = Nothing
    , _upName = Nothing
    }

-- | The user\'s SSH user name.
upSSHUsername :: Lens' UserProfile (Maybe Text)
upSSHUsername = lens _upSSHUsername (\ s a -> s{_upSSHUsername = a});

-- | The user\'s SSH public key.
upSSHPublicKey :: Lens' UserProfile (Maybe Text)
upSSHPublicKey = lens _upSSHPublicKey (\ s a -> s{_upSSHPublicKey = a});

-- | Whether users can specify their own SSH public key through the My
-- Settings page. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions>.
upAllowSelfManagement :: Lens' UserProfile (Maybe Bool)
upAllowSelfManagement = lens _upAllowSelfManagement (\ s a -> s{_upAllowSelfManagement = a});

-- | The user\'s IAM ARN.
upIAMUserARN :: Lens' UserProfile (Maybe Text)
upIAMUserARN = lens _upIAMUserARN (\ s a -> s{_upIAMUserARN = a});

-- | The user\'s name.
upName :: Lens' UserProfile (Maybe Text)
upName = lens _upName (\ s a -> s{_upName = a});

instance FromJSON UserProfile where
        parseJSON
          = withObject "UserProfile"
              (\ x ->
                 UserProfile' <$>
                   (x .:? "SshUsername") <*> (x .:? "SshPublicKey") <*>
                     (x .:? "AllowSelfManagement")
                     <*> (x .:? "IamUserArn")
                     <*> (x .:? "Name"))

-- | Describes an instance\'s Amazon EBS volume.
--
-- /See:/ 'volume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vInstanceId'
--
-- * 'vStatus'
--
-- * 'vSize'
--
-- * 'vIOPS'
--
-- * 'vDevice'
--
-- * 'vName'
--
-- * 'vAvailabilityZone'
--
-- * 'vRAIdArrayId'
--
-- * 'vVolumeId'
--
-- * 'vRegion'
--
-- * 'vVolumeType'
--
-- * 'vEC2VolumeId'
--
-- * 'vMountPoint'
data Volume = Volume'
    { _vInstanceId       :: !(Maybe Text)
    , _vStatus           :: !(Maybe Text)
    , _vSize             :: !(Maybe Int)
    , _vIOPS             :: !(Maybe Int)
    , _vDevice           :: !(Maybe Text)
    , _vName             :: !(Maybe Text)
    , _vAvailabilityZone :: !(Maybe Text)
    , _vRAIdArrayId      :: !(Maybe Text)
    , _vVolumeId         :: !(Maybe Text)
    , _vRegion           :: !(Maybe Text)
    , _vVolumeType       :: !(Maybe Text)
    , _vEC2VolumeId      :: !(Maybe Text)
    , _vMountPoint       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Volume' smart constructor.
volume :: Volume
volume =
    Volume'
    { _vInstanceId = Nothing
    , _vStatus = Nothing
    , _vSize = Nothing
    , _vIOPS = Nothing
    , _vDevice = Nothing
    , _vName = Nothing
    , _vAvailabilityZone = Nothing
    , _vRAIdArrayId = Nothing
    , _vVolumeId = Nothing
    , _vRegion = Nothing
    , _vVolumeType = Nothing
    , _vEC2VolumeId = Nothing
    , _vMountPoint = Nothing
    }

-- | The instance ID.
vInstanceId :: Lens' Volume (Maybe Text)
vInstanceId = lens _vInstanceId (\ s a -> s{_vInstanceId = a});

-- | The value returned by
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumes.html DescribeVolumes>.
vStatus :: Lens' Volume (Maybe Text)
vStatus = lens _vStatus (\ s a -> s{_vStatus = a});

-- | The volume size.
vSize :: Lens' Volume (Maybe Int)
vSize = lens _vSize (\ s a -> s{_vSize = a});

-- | For PIOPS volumes, the IOPS per disk.
vIOPS :: Lens' Volume (Maybe Int)
vIOPS = lens _vIOPS (\ s a -> s{_vIOPS = a});

-- | The device name.
vDevice :: Lens' Volume (Maybe Text)
vDevice = lens _vDevice (\ s a -> s{_vDevice = a});

-- | The volume name.
vName :: Lens' Volume (Maybe Text)
vName = lens _vName (\ s a -> s{_vName = a});

-- | The volume Availability Zone. For more information, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
vAvailabilityZone :: Lens' Volume (Maybe Text)
vAvailabilityZone = lens _vAvailabilityZone (\ s a -> s{_vAvailabilityZone = a});

-- | The RAID array ID.
vRAIdArrayId :: Lens' Volume (Maybe Text)
vRAIdArrayId = lens _vRAIdArrayId (\ s a -> s{_vRAIdArrayId = a});

-- | The volume ID.
vVolumeId :: Lens' Volume (Maybe Text)
vVolumeId = lens _vVolumeId (\ s a -> s{_vVolumeId = a});

-- | The AWS region. For more information about AWS regions, see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
vRegion :: Lens' Volume (Maybe Text)
vRegion = lens _vRegion (\ s a -> s{_vRegion = a});

-- | The volume type, standard or PIOPS.
vVolumeType :: Lens' Volume (Maybe Text)
vVolumeType = lens _vVolumeType (\ s a -> s{_vVolumeType = a});

-- | The Amazon EC2 volume ID.
vEC2VolumeId :: Lens' Volume (Maybe Text)
vEC2VolumeId = lens _vEC2VolumeId (\ s a -> s{_vEC2VolumeId = a});

-- | The volume mount point. For example \"\/dev\/sdh\".
vMountPoint :: Lens' Volume (Maybe Text)
vMountPoint = lens _vMountPoint (\ s a -> s{_vMountPoint = a});

instance FromJSON Volume where
        parseJSON
          = withObject "Volume"
              (\ x ->
                 Volume' <$>
                   (x .:? "InstanceId") <*> (x .:? "Status") <*>
                     (x .:? "Size")
                     <*> (x .:? "Iops")
                     <*> (x .:? "Device")
                     <*> (x .:? "Name")
                     <*> (x .:? "AvailabilityZone")
                     <*> (x .:? "RaidArrayId")
                     <*> (x .:? "VolumeId")
                     <*> (x .:? "Region")
                     <*> (x .:? "VolumeType")
                     <*> (x .:? "Ec2VolumeId")
                     <*> (x .:? "MountPoint"))

-- | Describes an Amazon EBS volume configuration.
--
-- /See:/ 'volumeConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'vcIOPS'
--
-- * 'vcRAIdLevel'
--
-- * 'vcVolumeType'
--
-- * 'vcMountPoint'
--
-- * 'vcNumberOfDisks'
--
-- * 'vcSize'
data VolumeConfiguration = VolumeConfiguration'
    { _vcIOPS          :: !(Maybe Int)
    , _vcRAIdLevel     :: !(Maybe Int)
    , _vcVolumeType    :: !(Maybe Text)
    , _vcMountPoint    :: !Text
    , _vcNumberOfDisks :: !Int
    , _vcSize          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'VolumeConfiguration' smart constructor.
volumeConfiguration :: Text -> Int -> Int -> VolumeConfiguration
volumeConfiguration pMountPoint pNumberOfDisks pSize =
    VolumeConfiguration'
    { _vcIOPS = Nothing
    , _vcRAIdLevel = Nothing
    , _vcVolumeType = Nothing
    , _vcMountPoint = pMountPoint
    , _vcNumberOfDisks = pNumberOfDisks
    , _vcSize = pSize
    }

-- | For PIOPS volumes, the IOPS per disk.
vcIOPS :: Lens' VolumeConfiguration (Maybe Int)
vcIOPS = lens _vcIOPS (\ s a -> s{_vcIOPS = a});

-- | The volume
-- <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level>.
vcRAIdLevel :: Lens' VolumeConfiguration (Maybe Int)
vcRAIdLevel = lens _vcRAIdLevel (\ s a -> s{_vcRAIdLevel = a});

-- | The volume type:
--
-- -   @standard@ - Magnetic
-- -   @io1@ - Provisioned IOPS (SSD)
-- -   @gp2@ - General Purpose (SSD)
vcVolumeType :: Lens' VolumeConfiguration (Maybe Text)
vcVolumeType = lens _vcVolumeType (\ s a -> s{_vcVolumeType = a});

-- | The volume mount point. For example \"\/dev\/sdh\".
vcMountPoint :: Lens' VolumeConfiguration Text
vcMountPoint = lens _vcMountPoint (\ s a -> s{_vcMountPoint = a});

-- | The number of disks in the volume.
vcNumberOfDisks :: Lens' VolumeConfiguration Int
vcNumberOfDisks = lens _vcNumberOfDisks (\ s a -> s{_vcNumberOfDisks = a});

-- | The volume size.
vcSize :: Lens' VolumeConfiguration Int
vcSize = lens _vcSize (\ s a -> s{_vcSize = a});

instance FromJSON VolumeConfiguration where
        parseJSON
          = withObject "VolumeConfiguration"
              (\ x ->
                 VolumeConfiguration' <$>
                   (x .:? "Iops") <*> (x .:? "RaidLevel") <*>
                     (x .:? "VolumeType")
                     <*> (x .: "MountPoint")
                     <*> (x .: "NumberOfDisks")
                     <*> (x .: "Size"))

instance ToJSON VolumeConfiguration where
        toJSON VolumeConfiguration'{..}
          = object
              ["Iops" .= _vcIOPS, "RaidLevel" .= _vcRAIdLevel,
               "VolumeType" .= _vcVolumeType,
               "MountPoint" .= _vcMountPoint,
               "NumberOfDisks" .= _vcNumberOfDisks,
               "Size" .= _vcSize]

-- | Describes a time-based instance\'s auto scaling schedule. The schedule
-- consists of a set of key-value pairs.
--
-- -   The key is the time period (a UTC hour) and must be an integer from
--     0 - 23.
-- -   The value indicates whether the instance should be online or offline
--     for the specified period, and must be set to \"on\" or \"off\"
--
-- The default setting for all time periods is off, so you use the
-- following parameters primarily to specify the online periods. You don\'t
-- have to explicitly specify offline periods unless you want to change an
-- online period to an offline period.
--
-- The following example specifies that the instance should be online for
-- four hours, from UTC 1200 - 1600. It will be off for the remainder of
-- the day.
--
-- @ { \"12\":\"on\", \"13\":\"on\", \"14\":\"on\", \"15\":\"on\" } @
--
-- /See:/ 'weeklyAutoScalingSchedule' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wassThursday'
--
-- * 'wassWednesday'
--
-- * 'wassSaturday'
--
-- * 'wassMonday'
--
-- * 'wassFriday'
--
-- * 'wassSunday'
--
-- * 'wassTuesday'
data WeeklyAutoScalingSchedule = WeeklyAutoScalingSchedule'
    { _wassThursday  :: !(Maybe (Map Text Text))
    , _wassWednesday :: !(Maybe (Map Text Text))
    , _wassSaturday  :: !(Maybe (Map Text Text))
    , _wassMonday    :: !(Maybe (Map Text Text))
    , _wassFriday    :: !(Maybe (Map Text Text))
    , _wassSunday    :: !(Maybe (Map Text Text))
    , _wassTuesday   :: !(Maybe (Map Text Text))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'WeeklyAutoScalingSchedule' smart constructor.
weeklyAutoScalingSchedule :: WeeklyAutoScalingSchedule
weeklyAutoScalingSchedule =
    WeeklyAutoScalingSchedule'
    { _wassThursday = Nothing
    , _wassWednesday = Nothing
    , _wassSaturday = Nothing
    , _wassMonday = Nothing
    , _wassFriday = Nothing
    , _wassSunday = Nothing
    , _wassTuesday = Nothing
    }

-- | The schedule for Thursday.
wassThursday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassThursday = lens _wassThursday (\ s a -> s{_wassThursday = a}) . _Default . _Map;

-- | The schedule for Wednesday.
wassWednesday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassWednesday = lens _wassWednesday (\ s a -> s{_wassWednesday = a}) . _Default . _Map;

-- | The schedule for Saturday.
wassSaturday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassSaturday = lens _wassSaturday (\ s a -> s{_wassSaturday = a}) . _Default . _Map;

-- | The schedule for Monday.
wassMonday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassMonday = lens _wassMonday (\ s a -> s{_wassMonday = a}) . _Default . _Map;

-- | The schedule for Friday.
wassFriday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassFriday = lens _wassFriday (\ s a -> s{_wassFriday = a}) . _Default . _Map;

-- | The schedule for Sunday.
wassSunday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassSunday = lens _wassSunday (\ s a -> s{_wassSunday = a}) . _Default . _Map;

-- | The schedule for Tuesday.
wassTuesday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassTuesday = lens _wassTuesday (\ s a -> s{_wassTuesday = a}) . _Default . _Map;

instance FromJSON WeeklyAutoScalingSchedule where
        parseJSON
          = withObject "WeeklyAutoScalingSchedule"
              (\ x ->
                 WeeklyAutoScalingSchedule' <$>
                   (x .:? "Thursday" .!= mempty) <*>
                     (x .:? "Wednesday" .!= mempty)
                     <*> (x .:? "Saturday" .!= mempty)
                     <*> (x .:? "Monday" .!= mempty)
                     <*> (x .:? "Friday" .!= mempty)
                     <*> (x .:? "Sunday" .!= mempty)
                     <*> (x .:? "Tuesday" .!= mempty))

instance ToJSON WeeklyAutoScalingSchedule where
        toJSON WeeklyAutoScalingSchedule'{..}
          = object
              ["Thursday" .= _wassThursday,
               "Wednesday" .= _wassWednesday,
               "Saturday" .= _wassSaturday, "Monday" .= _wassMonday,
               "Friday" .= _wassFriday, "Sunday" .= _wassSunday,
               "Tuesday" .= _wassTuesday]
