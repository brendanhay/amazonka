{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.Product where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types.Sum
import Network.AWS.Prelude

-- | Describes an agent version.
--
--
--
-- /See:/ 'agentVersion' smart constructor.
data AgentVersion = AgentVersion'
  { _avVersion              :: !(Maybe Text)
  , _avConfigurationManager :: !(Maybe StackConfigurationManager)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AgentVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avVersion' - The agent version.
--
-- * 'avConfigurationManager' - The configuration manager.
agentVersion
    :: AgentVersion
agentVersion =
  AgentVersion' {_avVersion = Nothing, _avConfigurationManager = Nothing}


-- | The agent version.
avVersion :: Lens' AgentVersion (Maybe Text)
avVersion = lens _avVersion (\ s a -> s{_avVersion = a})

-- | The configuration manager.
avConfigurationManager :: Lens' AgentVersion (Maybe StackConfigurationManager)
avConfigurationManager = lens _avConfigurationManager (\ s a -> s{_avConfigurationManager = a})

instance FromJSON AgentVersion where
        parseJSON
          = withObject "AgentVersion"
              (\ x ->
                 AgentVersion' <$>
                   (x .:? "Version") <*> (x .:? "ConfigurationManager"))

instance Hashable AgentVersion where

instance NFData AgentVersion where

-- | A description of the app.
--
--
--
-- /See:/ 'app' smart constructor.
data App = App'
  { _appSSLConfiguration :: !(Maybe SSLConfiguration)
  , _appEnvironment      :: !(Maybe [EnvironmentVariable])
  , _appEnableSSL        :: !(Maybe Bool)
  , _appCreatedAt        :: !(Maybe Text)
  , _appShortname        :: !(Maybe Text)
  , _appDataSources      :: !(Maybe [DataSource])
  , _appAppSource        :: !(Maybe Source)
  , _appAppId            :: !(Maybe Text)
  , _appAttributes       :: !(Maybe (Map AppAttributesKeys Text))
  , _appName             :: !(Maybe Text)
  , _appType             :: !(Maybe AppType)
  , _appStackId          :: !(Maybe Text)
  , _appDomains          :: !(Maybe [Text])
  , _appDescription      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'App' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'appSSLConfiguration' - An @SslConfiguration@ object with the SSL configuration.
--
-- * 'appEnvironment' - An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instances. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> .
--
-- * 'appEnableSSL' - Whether to enable SSL for the app.
--
-- * 'appCreatedAt' - When the app was created.
--
-- * 'appShortname' - The app's short name.
--
-- * 'appDataSources' - The app's data sources.
--
-- * 'appAppSource' - A @Source@ object that describes the app repository.
--
-- * 'appAppId' - The app ID.
--
-- * 'appAttributes' - The stack attributes.
--
-- * 'appName' - The app name.
--
-- * 'appType' - The app type.
--
-- * 'appStackId' - The app stack ID.
--
-- * 'appDomains' - The app vhost settings with multiple domains separated by commas. For example: @'www.example.com, example.com'@
--
-- * 'appDescription' - A description of the app.
app
    :: App
app =
  App'
    { _appSSLConfiguration = Nothing
    , _appEnvironment = Nothing
    , _appEnableSSL = Nothing
    , _appCreatedAt = Nothing
    , _appShortname = Nothing
    , _appDataSources = Nothing
    , _appAppSource = Nothing
    , _appAppId = Nothing
    , _appAttributes = Nothing
    , _appName = Nothing
    , _appType = Nothing
    , _appStackId = Nothing
    , _appDomains = Nothing
    , _appDescription = Nothing
    }


-- | An @SslConfiguration@ object with the SSL configuration.
appSSLConfiguration :: Lens' App (Maybe SSLConfiguration)
appSSLConfiguration = lens _appSSLConfiguration (\ s a -> s{_appSSLConfiguration = a})

-- | An array of @EnvironmentVariable@ objects that specify environment variables to be associated with the app. After you deploy the app, these variables are defined on the associated app server instances. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html#workingapps-creating-environment Environment Variables> .
appEnvironment :: Lens' App [EnvironmentVariable]
appEnvironment = lens _appEnvironment (\ s a -> s{_appEnvironment = a}) . _Default . _Coerce

-- | Whether to enable SSL for the app.
appEnableSSL :: Lens' App (Maybe Bool)
appEnableSSL = lens _appEnableSSL (\ s a -> s{_appEnableSSL = a})

-- | When the app was created.
appCreatedAt :: Lens' App (Maybe Text)
appCreatedAt = lens _appCreatedAt (\ s a -> s{_appCreatedAt = a})

-- | The app's short name.
appShortname :: Lens' App (Maybe Text)
appShortname = lens _appShortname (\ s a -> s{_appShortname = a})

-- | The app's data sources.
appDataSources :: Lens' App [DataSource]
appDataSources = lens _appDataSources (\ s a -> s{_appDataSources = a}) . _Default . _Coerce

-- | A @Source@ object that describes the app repository.
appAppSource :: Lens' App (Maybe Source)
appAppSource = lens _appAppSource (\ s a -> s{_appAppSource = a})

-- | The app ID.
appAppId :: Lens' App (Maybe Text)
appAppId = lens _appAppId (\ s a -> s{_appAppId = a})

-- | The stack attributes.
appAttributes :: Lens' App (HashMap AppAttributesKeys Text)
appAttributes = lens _appAttributes (\ s a -> s{_appAttributes = a}) . _Default . _Map

-- | The app name.
appName :: Lens' App (Maybe Text)
appName = lens _appName (\ s a -> s{_appName = a})

-- | The app type.
appType :: Lens' App (Maybe AppType)
appType = lens _appType (\ s a -> s{_appType = a})

-- | The app stack ID.
appStackId :: Lens' App (Maybe Text)
appStackId = lens _appStackId (\ s a -> s{_appStackId = a})

-- | The app vhost settings with multiple domains separated by commas. For example: @'www.example.com, example.com'@
appDomains :: Lens' App [Text]
appDomains = lens _appDomains (\ s a -> s{_appDomains = a}) . _Default . _Coerce

-- | A description of the app.
appDescription :: Lens' App (Maybe Text)
appDescription = lens _appDescription (\ s a -> s{_appDescription = a})

instance FromJSON App where
        parseJSON
          = withObject "App"
              (\ x ->
                 App' <$>
                   (x .:? "SslConfiguration") <*>
                     (x .:? "Environment" .!= mempty)
                     <*> (x .:? "EnableSsl")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "Shortname")
                     <*> (x .:? "DataSources" .!= mempty)
                     <*> (x .:? "AppSource")
                     <*> (x .:? "AppId")
                     <*> (x .:? "Attributes" .!= mempty)
                     <*> (x .:? "Name")
                     <*> (x .:? "Type")
                     <*> (x .:? "StackId")
                     <*> (x .:? "Domains" .!= mempty)
                     <*> (x .:? "Description"))

instance Hashable App where

instance NFData App where

-- | Describes a load-based auto scaling upscaling or downscaling threshold configuration, which specifies when AWS OpsWorks Stacks starts or stops load-based instances.
--
--
--
-- /See:/ 'autoScalingThresholds' smart constructor.
data AutoScalingThresholds = AutoScalingThresholds'
  { _astInstanceCount      :: !(Maybe Int)
  , _astIgnoreMetricsTime  :: !(Maybe Nat)
  , _astLoadThreshold      :: !(Maybe Double)
  , _astThresholdsWaitTime :: !(Maybe Nat)
  , _astAlarms             :: !(Maybe [Text])
  , _astMemoryThreshold    :: !(Maybe Double)
  , _astCPUThreshold       :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AutoScalingThresholds' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'astInstanceCount' - The number of instances to add or remove when the load exceeds a threshold.
--
-- * 'astIgnoreMetricsTime' - The amount of time (in minutes) after a scaling event occurs that AWS OpsWorks Stacks should ignore metrics and suppress additional scaling events. For example, AWS OpsWorks Stacks adds new instances following an upscaling event but the instances won't start reducing the load until they have been booted and configured. There is no point in raising additional scaling events during that operation, which typically takes several minutes. @IgnoreMetricsTime@ allows you to direct AWS OpsWorks Stacks to suppress scaling events long enough to get the new instances online.
--
-- * 'astLoadThreshold' - The load threshold. A value of -1 disables the threshold. For more information about how load is computed, see <http://en.wikipedia.org/wiki/Load_%28computing%29 Load (computing)> .
--
-- * 'astThresholdsWaitTime' - The amount of time, in minutes, that the load must exceed a threshold before more instances are added or removed.
--
-- * 'astAlarms' - Custom Cloudwatch auto scaling alarms, to be used as thresholds. This parameter takes a list of up to five alarm names, which are case sensitive and must be in the same region as the stack.
--
-- * 'astMemoryThreshold' - The memory utilization threshold, as a percent of the available memory. A value of -1 disables the threshold.
--
-- * 'astCPUThreshold' - The CPU utilization threshold, as a percent of the available CPU. A value of -1 disables the threshold.
autoScalingThresholds
    :: AutoScalingThresholds
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


-- | The number of instances to add or remove when the load exceeds a threshold.
astInstanceCount :: Lens' AutoScalingThresholds (Maybe Int)
astInstanceCount = lens _astInstanceCount (\ s a -> s{_astInstanceCount = a})

-- | The amount of time (in minutes) after a scaling event occurs that AWS OpsWorks Stacks should ignore metrics and suppress additional scaling events. For example, AWS OpsWorks Stacks adds new instances following an upscaling event but the instances won't start reducing the load until they have been booted and configured. There is no point in raising additional scaling events during that operation, which typically takes several minutes. @IgnoreMetricsTime@ allows you to direct AWS OpsWorks Stacks to suppress scaling events long enough to get the new instances online.
astIgnoreMetricsTime :: Lens' AutoScalingThresholds (Maybe Natural)
astIgnoreMetricsTime = lens _astIgnoreMetricsTime (\ s a -> s{_astIgnoreMetricsTime = a}) . mapping _Nat

-- | The load threshold. A value of -1 disables the threshold. For more information about how load is computed, see <http://en.wikipedia.org/wiki/Load_%28computing%29 Load (computing)> .
astLoadThreshold :: Lens' AutoScalingThresholds (Maybe Double)
astLoadThreshold = lens _astLoadThreshold (\ s a -> s{_astLoadThreshold = a})

-- | The amount of time, in minutes, that the load must exceed a threshold before more instances are added or removed.
astThresholdsWaitTime :: Lens' AutoScalingThresholds (Maybe Natural)
astThresholdsWaitTime = lens _astThresholdsWaitTime (\ s a -> s{_astThresholdsWaitTime = a}) . mapping _Nat

-- | Custom Cloudwatch auto scaling alarms, to be used as thresholds. This parameter takes a list of up to five alarm names, which are case sensitive and must be in the same region as the stack.
astAlarms :: Lens' AutoScalingThresholds [Text]
astAlarms = lens _astAlarms (\ s a -> s{_astAlarms = a}) . _Default . _Coerce

-- | The memory utilization threshold, as a percent of the available memory. A value of -1 disables the threshold.
astMemoryThreshold :: Lens' AutoScalingThresholds (Maybe Double)
astMemoryThreshold = lens _astMemoryThreshold (\ s a -> s{_astMemoryThreshold = a})

-- | The CPU utilization threshold, as a percent of the available CPU. A value of -1 disables the threshold.
astCPUThreshold :: Lens' AutoScalingThresholds (Maybe Double)
astCPUThreshold = lens _astCPUThreshold (\ s a -> s{_astCPUThreshold = a})

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

instance Hashable AutoScalingThresholds where

instance NFData AutoScalingThresholds where

instance ToJSON AutoScalingThresholds where
        toJSON AutoScalingThresholds'{..}
          = object
              (catMaybes
                 [("InstanceCount" .=) <$> _astInstanceCount,
                  ("IgnoreMetricsTime" .=) <$> _astIgnoreMetricsTime,
                  ("LoadThreshold" .=) <$> _astLoadThreshold,
                  ("ThresholdsWaitTime" .=) <$> _astThresholdsWaitTime,
                  ("Alarms" .=) <$> _astAlarms,
                  ("MemoryThreshold" .=) <$> _astMemoryThreshold,
                  ("CpuThreshold" .=) <$> _astCPUThreshold])

-- | Describes a block device mapping. This data type maps directly to the Amazon EC2 <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_BlockDeviceMapping.html BlockDeviceMapping> data type.
--
--
--
-- /See:/ 'blockDeviceMapping' smart constructor.
data BlockDeviceMapping = BlockDeviceMapping'
  { _bdmVirtualName :: !(Maybe Text)
  , _bdmNoDevice    :: !(Maybe Text)
  , _bdmEBS         :: !(Maybe EBSBlockDevice)
  , _bdmDeviceName  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BlockDeviceMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdmVirtualName' - The virtual device name. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_BlockDeviceMapping.html BlockDeviceMapping> .
--
-- * 'bdmNoDevice' - Suppresses the specified device included in the AMI's block device mapping.
--
-- * 'bdmEBS' - An @EBSBlockDevice@ that defines how to configure an Amazon EBS volume when the instance is launched.
--
-- * 'bdmDeviceName' - The device name that is exposed to the instance, such as @/dev/sdh@ . For the root device, you can use the explicit device name or you can set this parameter to @ROOT_DEVICE@ and AWS OpsWorks Stacks will provide the correct device name.
blockDeviceMapping
    :: BlockDeviceMapping
blockDeviceMapping =
  BlockDeviceMapping'
    { _bdmVirtualName = Nothing
    , _bdmNoDevice = Nothing
    , _bdmEBS = Nothing
    , _bdmDeviceName = Nothing
    }


-- | The virtual device name. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_BlockDeviceMapping.html BlockDeviceMapping> .
bdmVirtualName :: Lens' BlockDeviceMapping (Maybe Text)
bdmVirtualName = lens _bdmVirtualName (\ s a -> s{_bdmVirtualName = a})

-- | Suppresses the specified device included in the AMI's block device mapping.
bdmNoDevice :: Lens' BlockDeviceMapping (Maybe Text)
bdmNoDevice = lens _bdmNoDevice (\ s a -> s{_bdmNoDevice = a})

-- | An @EBSBlockDevice@ that defines how to configure an Amazon EBS volume when the instance is launched.
bdmEBS :: Lens' BlockDeviceMapping (Maybe EBSBlockDevice)
bdmEBS = lens _bdmEBS (\ s a -> s{_bdmEBS = a})

-- | The device name that is exposed to the instance, such as @/dev/sdh@ . For the root device, you can use the explicit device name or you can set this parameter to @ROOT_DEVICE@ and AWS OpsWorks Stacks will provide the correct device name.
bdmDeviceName :: Lens' BlockDeviceMapping (Maybe Text)
bdmDeviceName = lens _bdmDeviceName (\ s a -> s{_bdmDeviceName = a})

instance FromJSON BlockDeviceMapping where
        parseJSON
          = withObject "BlockDeviceMapping"
              (\ x ->
                 BlockDeviceMapping' <$>
                   (x .:? "VirtualName") <*> (x .:? "NoDevice") <*>
                     (x .:? "Ebs")
                     <*> (x .:? "DeviceName"))

instance Hashable BlockDeviceMapping where

instance NFData BlockDeviceMapping where

instance ToJSON BlockDeviceMapping where
        toJSON BlockDeviceMapping'{..}
          = object
              (catMaybes
                 [("VirtualName" .=) <$> _bdmVirtualName,
                  ("NoDevice" .=) <$> _bdmNoDevice,
                  ("Ebs" .=) <$> _bdmEBS,
                  ("DeviceName" .=) <$> _bdmDeviceName])

-- | Describes the Chef configuration.
--
--
--
-- /See:/ 'chefConfiguration' smart constructor.
data ChefConfiguration = ChefConfiguration'
  { _ccBerkshelfVersion :: !(Maybe Text)
  , _ccManageBerkshelf  :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChefConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccBerkshelfVersion' - The Berkshelf version.
--
-- * 'ccManageBerkshelf' - Whether to enable Berkshelf.
chefConfiguration
    :: ChefConfiguration
chefConfiguration =
  ChefConfiguration'
    {_ccBerkshelfVersion = Nothing, _ccManageBerkshelf = Nothing}


-- | The Berkshelf version.
ccBerkshelfVersion :: Lens' ChefConfiguration (Maybe Text)
ccBerkshelfVersion = lens _ccBerkshelfVersion (\ s a -> s{_ccBerkshelfVersion = a})

-- | Whether to enable Berkshelf.
ccManageBerkshelf :: Lens' ChefConfiguration (Maybe Bool)
ccManageBerkshelf = lens _ccManageBerkshelf (\ s a -> s{_ccManageBerkshelf = a})

instance FromJSON ChefConfiguration where
        parseJSON
          = withObject "ChefConfiguration"
              (\ x ->
                 ChefConfiguration' <$>
                   (x .:? "BerkshelfVersion") <*>
                     (x .:? "ManageBerkshelf"))

instance Hashable ChefConfiguration where

instance NFData ChefConfiguration where

instance ToJSON ChefConfiguration where
        toJSON ChefConfiguration'{..}
          = object
              (catMaybes
                 [("BerkshelfVersion" .=) <$> _ccBerkshelfVersion,
                  ("ManageBerkshelf" .=) <$> _ccManageBerkshelf])

-- | Describes the Amazon CloudWatch logs configuration for a layer.
--
--
--
-- /See:/ 'cloudWatchLogsConfiguration' smart constructor.
data CloudWatchLogsConfiguration = CloudWatchLogsConfiguration'
  { _cwlcEnabled    :: !(Maybe Bool)
  , _cwlcLogStreams :: !(Maybe [CloudWatchLogsLogStream])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloudWatchLogsConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwlcEnabled' - Whether CloudWatch Logs is enabled for a layer.
--
-- * 'cwlcLogStreams' - A list of configuration options for CloudWatch Logs.
cloudWatchLogsConfiguration
    :: CloudWatchLogsConfiguration
cloudWatchLogsConfiguration =
  CloudWatchLogsConfiguration'
    {_cwlcEnabled = Nothing, _cwlcLogStreams = Nothing}


-- | Whether CloudWatch Logs is enabled for a layer.
cwlcEnabled :: Lens' CloudWatchLogsConfiguration (Maybe Bool)
cwlcEnabled = lens _cwlcEnabled (\ s a -> s{_cwlcEnabled = a})

-- | A list of configuration options for CloudWatch Logs.
cwlcLogStreams :: Lens' CloudWatchLogsConfiguration [CloudWatchLogsLogStream]
cwlcLogStreams = lens _cwlcLogStreams (\ s a -> s{_cwlcLogStreams = a}) . _Default . _Coerce

instance FromJSON CloudWatchLogsConfiguration where
        parseJSON
          = withObject "CloudWatchLogsConfiguration"
              (\ x ->
                 CloudWatchLogsConfiguration' <$>
                   (x .:? "Enabled") <*>
                     (x .:? "LogStreams" .!= mempty))

instance Hashable CloudWatchLogsConfiguration where

instance NFData CloudWatchLogsConfiguration where

instance ToJSON CloudWatchLogsConfiguration where
        toJSON CloudWatchLogsConfiguration'{..}
          = object
              (catMaybes
                 [("Enabled" .=) <$> _cwlcEnabled,
                  ("LogStreams" .=) <$> _cwlcLogStreams])

-- | Describes the Amazon CloudWatch logs configuration for a layer. For detailed information about members of this data type, see the <http://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/AgentReference.html CloudWatch Logs Agent Reference> .
--
--
--
-- /See:/ 'cloudWatchLogsLogStream' smart constructor.
data CloudWatchLogsLogStream = CloudWatchLogsLogStream'
  { _cwllsBatchCount            :: !(Maybe Int)
  , _cwllsFileFingerprintLines  :: !(Maybe Text)
  , _cwllsBufferDuration        :: !(Maybe Int)
  , _cwllsBatchSize             :: !(Maybe Int)
  , _cwllsLogGroupName          :: !(Maybe Text)
  , _cwllsMultiLineStartPattern :: !(Maybe Text)
  , _cwllsInitialPosition       :: !(Maybe CloudWatchLogsInitialPosition)
  , _cwllsDatetimeFormat        :: !(Maybe Text)
  , _cwllsEncoding              :: !(Maybe CloudWatchLogsEncoding)
  , _cwllsTimeZone              :: !(Maybe CloudWatchLogsTimeZone)
  , _cwllsFile                  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloudWatchLogsLogStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwllsBatchCount' - Specifies the max number of log events in a batch, up to 10000. The default value is 1000.
--
-- * 'cwllsFileFingerprintLines' - Specifies the range of lines for identifying a file. The valid values are one number, or two dash-delimited numbers, such as '1', '2-5'. The default value is '1', meaning the first line is used to calculate the fingerprint. Fingerprint lines are not sent to CloudWatch Logs unless all specified lines are available.
--
-- * 'cwllsBufferDuration' - Specifies the time duration for the batching of log events. The minimum value is 5000ms and default value is 5000ms.
--
-- * 'cwllsBatchSize' - Specifies the maximum size of log events in a batch, in bytes, up to 1048576 bytes. The default value is 32768 bytes. This size is calculated as the sum of all event messages in UTF-8, plus 26 bytes for each log event.
--
-- * 'cwllsLogGroupName' - Specifies the destination log group. A log group is created automatically if it doesn't already exist. Log group names can be between 1 and 512 characters long. Allowed characters include a-z, A-Z, 0-9, '_' (underscore), '-' (hyphen), '/' (forward slash), and '.' (period).
--
-- * 'cwllsMultiLineStartPattern' - Specifies the pattern for identifying the start of a log message.
--
-- * 'cwllsInitialPosition' - Specifies where to start to read data (start_of_file or end_of_file). The default is start_of_file. This setting is only used if there is no state persisted for that log stream.
--
-- * 'cwllsDatetimeFormat' - Specifies how the time stamp is extracted from logs. For more information, see the <http://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/AgentReference.html CloudWatch Logs Agent Reference> .
--
-- * 'cwllsEncoding' - Specifies the encoding of the log file so that the file can be read correctly. The default is @utf_8@ . Encodings supported by Python @codecs.decode()@ can be used here.
--
-- * 'cwllsTimeZone' - Specifies the time zone of log event time stamps.
--
-- * 'cwllsFile' - Specifies log files that you want to push to CloudWatch Logs. @File@ can point to a specific file or multiple files (by using wild card characters such as @/var/log/system.log*@ ). Only the latest file is pushed to CloudWatch Logs, based on file modification time. We recommend that you use wild card characters to specify a series of files of the same type, such as @access_log.2014-06-01-01@ , @access_log.2014-06-01-02@ , and so on by using a pattern like @access_log.*@ . Don't use a wildcard to match multiple file types, such as @access_log_80@ and @access_log_443@ . To specify multiple, different file types, add another log stream entry to the configuration file, so that each log file type is stored in a different log group. Zipped files are not supported.
cloudWatchLogsLogStream
    :: CloudWatchLogsLogStream
cloudWatchLogsLogStream =
  CloudWatchLogsLogStream'
    { _cwllsBatchCount = Nothing
    , _cwllsFileFingerprintLines = Nothing
    , _cwllsBufferDuration = Nothing
    , _cwllsBatchSize = Nothing
    , _cwllsLogGroupName = Nothing
    , _cwllsMultiLineStartPattern = Nothing
    , _cwllsInitialPosition = Nothing
    , _cwllsDatetimeFormat = Nothing
    , _cwllsEncoding = Nothing
    , _cwllsTimeZone = Nothing
    , _cwllsFile = Nothing
    }


-- | Specifies the max number of log events in a batch, up to 10000. The default value is 1000.
cwllsBatchCount :: Lens' CloudWatchLogsLogStream (Maybe Int)
cwllsBatchCount = lens _cwllsBatchCount (\ s a -> s{_cwllsBatchCount = a})

-- | Specifies the range of lines for identifying a file. The valid values are one number, or two dash-delimited numbers, such as '1', '2-5'. The default value is '1', meaning the first line is used to calculate the fingerprint. Fingerprint lines are not sent to CloudWatch Logs unless all specified lines are available.
cwllsFileFingerprintLines :: Lens' CloudWatchLogsLogStream (Maybe Text)
cwllsFileFingerprintLines = lens _cwllsFileFingerprintLines (\ s a -> s{_cwllsFileFingerprintLines = a})

-- | Specifies the time duration for the batching of log events. The minimum value is 5000ms and default value is 5000ms.
cwllsBufferDuration :: Lens' CloudWatchLogsLogStream (Maybe Int)
cwllsBufferDuration = lens _cwllsBufferDuration (\ s a -> s{_cwllsBufferDuration = a})

-- | Specifies the maximum size of log events in a batch, in bytes, up to 1048576 bytes. The default value is 32768 bytes. This size is calculated as the sum of all event messages in UTF-8, plus 26 bytes for each log event.
cwllsBatchSize :: Lens' CloudWatchLogsLogStream (Maybe Int)
cwllsBatchSize = lens _cwllsBatchSize (\ s a -> s{_cwllsBatchSize = a})

-- | Specifies the destination log group. A log group is created automatically if it doesn't already exist. Log group names can be between 1 and 512 characters long. Allowed characters include a-z, A-Z, 0-9, '_' (underscore), '-' (hyphen), '/' (forward slash), and '.' (period).
cwllsLogGroupName :: Lens' CloudWatchLogsLogStream (Maybe Text)
cwllsLogGroupName = lens _cwllsLogGroupName (\ s a -> s{_cwllsLogGroupName = a})

-- | Specifies the pattern for identifying the start of a log message.
cwllsMultiLineStartPattern :: Lens' CloudWatchLogsLogStream (Maybe Text)
cwllsMultiLineStartPattern = lens _cwllsMultiLineStartPattern (\ s a -> s{_cwllsMultiLineStartPattern = a})

-- | Specifies where to start to read data (start_of_file or end_of_file). The default is start_of_file. This setting is only used if there is no state persisted for that log stream.
cwllsInitialPosition :: Lens' CloudWatchLogsLogStream (Maybe CloudWatchLogsInitialPosition)
cwllsInitialPosition = lens _cwllsInitialPosition (\ s a -> s{_cwllsInitialPosition = a})

-- | Specifies how the time stamp is extracted from logs. For more information, see the <http://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/AgentReference.html CloudWatch Logs Agent Reference> .
cwllsDatetimeFormat :: Lens' CloudWatchLogsLogStream (Maybe Text)
cwllsDatetimeFormat = lens _cwllsDatetimeFormat (\ s a -> s{_cwllsDatetimeFormat = a})

-- | Specifies the encoding of the log file so that the file can be read correctly. The default is @utf_8@ . Encodings supported by Python @codecs.decode()@ can be used here.
cwllsEncoding :: Lens' CloudWatchLogsLogStream (Maybe CloudWatchLogsEncoding)
cwllsEncoding = lens _cwllsEncoding (\ s a -> s{_cwllsEncoding = a})

-- | Specifies the time zone of log event time stamps.
cwllsTimeZone :: Lens' CloudWatchLogsLogStream (Maybe CloudWatchLogsTimeZone)
cwllsTimeZone = lens _cwllsTimeZone (\ s a -> s{_cwllsTimeZone = a})

-- | Specifies log files that you want to push to CloudWatch Logs. @File@ can point to a specific file or multiple files (by using wild card characters such as @/var/log/system.log*@ ). Only the latest file is pushed to CloudWatch Logs, based on file modification time. We recommend that you use wild card characters to specify a series of files of the same type, such as @access_log.2014-06-01-01@ , @access_log.2014-06-01-02@ , and so on by using a pattern like @access_log.*@ . Don't use a wildcard to match multiple file types, such as @access_log_80@ and @access_log_443@ . To specify multiple, different file types, add another log stream entry to the configuration file, so that each log file type is stored in a different log group. Zipped files are not supported.
cwllsFile :: Lens' CloudWatchLogsLogStream (Maybe Text)
cwllsFile = lens _cwllsFile (\ s a -> s{_cwllsFile = a})

instance FromJSON CloudWatchLogsLogStream where
        parseJSON
          = withObject "CloudWatchLogsLogStream"
              (\ x ->
                 CloudWatchLogsLogStream' <$>
                   (x .:? "BatchCount") <*>
                     (x .:? "FileFingerprintLines")
                     <*> (x .:? "BufferDuration")
                     <*> (x .:? "BatchSize")
                     <*> (x .:? "LogGroupName")
                     <*> (x .:? "MultiLineStartPattern")
                     <*> (x .:? "InitialPosition")
                     <*> (x .:? "DatetimeFormat")
                     <*> (x .:? "Encoding")
                     <*> (x .:? "TimeZone")
                     <*> (x .:? "File"))

instance Hashable CloudWatchLogsLogStream where

instance NFData CloudWatchLogsLogStream where

instance ToJSON CloudWatchLogsLogStream where
        toJSON CloudWatchLogsLogStream'{..}
          = object
              (catMaybes
                 [("BatchCount" .=) <$> _cwllsBatchCount,
                  ("FileFingerprintLines" .=) <$>
                    _cwllsFileFingerprintLines,
                  ("BufferDuration" .=) <$> _cwllsBufferDuration,
                  ("BatchSize" .=) <$> _cwllsBatchSize,
                  ("LogGroupName" .=) <$> _cwllsLogGroupName,
                  ("MultiLineStartPattern" .=) <$>
                    _cwllsMultiLineStartPattern,
                  ("InitialPosition" .=) <$> _cwllsInitialPosition,
                  ("DatetimeFormat" .=) <$> _cwllsDatetimeFormat,
                  ("Encoding" .=) <$> _cwllsEncoding,
                  ("TimeZone" .=) <$> _cwllsTimeZone,
                  ("File" .=) <$> _cwllsFile])

-- | Describes a command.
--
--
--
-- /See:/ 'command' smart constructor.
data Command = Command'
  { _cDeploymentId   :: !(Maybe Text)
  , _cInstanceId     :: !(Maybe Text)
  , _cStatus         :: !(Maybe Text)
  , _cLogURL         :: !(Maybe Text)
  , _cCreatedAt      :: !(Maybe Text)
  , _cCommandId      :: !(Maybe Text)
  , _cExitCode       :: !(Maybe Int)
  , _cType           :: !(Maybe Text)
  , _cCompletedAt    :: !(Maybe Text)
  , _cAcknowledgedAt :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Command' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cDeploymentId' - The command deployment ID.
--
-- * 'cInstanceId' - The ID of the instance where the command was executed.
--
-- * 'cStatus' - The command status:     * failed     * successful     * skipped     * pending
--
-- * 'cLogURL' - The URL of the command log.
--
-- * 'cCreatedAt' - Date and time when the command was run.
--
-- * 'cCommandId' - The command ID.
--
-- * 'cExitCode' - The command exit code.
--
-- * 'cType' - The command type:     * @configure@      * @deploy@      * @execute_recipes@      * @install_dependencies@      * @restart@      * @rollback@      * @setup@      * @start@      * @stop@      * @undeploy@      * @update_custom_cookbooks@      * @update_dependencies@
--
-- * 'cCompletedAt' - Date when the command completed.
--
-- * 'cAcknowledgedAt' - Date and time when the command was acknowledged.
command
    :: Command
command =
  Command'
    { _cDeploymentId = Nothing
    , _cInstanceId = Nothing
    , _cStatus = Nothing
    , _cLogURL = Nothing
    , _cCreatedAt = Nothing
    , _cCommandId = Nothing
    , _cExitCode = Nothing
    , _cType = Nothing
    , _cCompletedAt = Nothing
    , _cAcknowledgedAt = Nothing
    }


-- | The command deployment ID.
cDeploymentId :: Lens' Command (Maybe Text)
cDeploymentId = lens _cDeploymentId (\ s a -> s{_cDeploymentId = a})

-- | The ID of the instance where the command was executed.
cInstanceId :: Lens' Command (Maybe Text)
cInstanceId = lens _cInstanceId (\ s a -> s{_cInstanceId = a})

-- | The command status:     * failed     * successful     * skipped     * pending
cStatus :: Lens' Command (Maybe Text)
cStatus = lens _cStatus (\ s a -> s{_cStatus = a})

-- | The URL of the command log.
cLogURL :: Lens' Command (Maybe Text)
cLogURL = lens _cLogURL (\ s a -> s{_cLogURL = a})

-- | Date and time when the command was run.
cCreatedAt :: Lens' Command (Maybe Text)
cCreatedAt = lens _cCreatedAt (\ s a -> s{_cCreatedAt = a})

-- | The command ID.
cCommandId :: Lens' Command (Maybe Text)
cCommandId = lens _cCommandId (\ s a -> s{_cCommandId = a})

-- | The command exit code.
cExitCode :: Lens' Command (Maybe Int)
cExitCode = lens _cExitCode (\ s a -> s{_cExitCode = a})

-- | The command type:     * @configure@      * @deploy@      * @execute_recipes@      * @install_dependencies@      * @restart@      * @rollback@      * @setup@      * @start@      * @stop@      * @undeploy@      * @update_custom_cookbooks@      * @update_dependencies@
cType :: Lens' Command (Maybe Text)
cType = lens _cType (\ s a -> s{_cType = a})

-- | Date when the command completed.
cCompletedAt :: Lens' Command (Maybe Text)
cCompletedAt = lens _cCompletedAt (\ s a -> s{_cCompletedAt = a})

-- | Date and time when the command was acknowledged.
cAcknowledgedAt :: Lens' Command (Maybe Text)
cAcknowledgedAt = lens _cAcknowledgedAt (\ s a -> s{_cAcknowledgedAt = a})

instance FromJSON Command where
        parseJSON
          = withObject "Command"
              (\ x ->
                 Command' <$>
                   (x .:? "DeploymentId") <*> (x .:? "InstanceId") <*>
                     (x .:? "Status")
                     <*> (x .:? "LogUrl")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "CommandId")
                     <*> (x .:? "ExitCode")
                     <*> (x .:? "Type")
                     <*> (x .:? "CompletedAt")
                     <*> (x .:? "AcknowledgedAt"))

instance Hashable Command where

instance NFData Command where

-- | Describes an app's data source.
--
--
--
-- /See:/ 'dataSource' smart constructor.
data DataSource = DataSource'
  { _dsARN          :: !(Maybe Text)
  , _dsDatabaseName :: !(Maybe Text)
  , _dsType         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsARN' - The data source's ARN.
--
-- * 'dsDatabaseName' - The database name.
--
-- * 'dsType' - The data source's type, @AutoSelectOpsworksMysqlInstance@ , @OpsworksMysqlInstance@ , @RdsDbInstance@ , or @None@ .
dataSource
    :: DataSource
dataSource =
  DataSource' {_dsARN = Nothing, _dsDatabaseName = Nothing, _dsType = Nothing}


-- | The data source's ARN.
dsARN :: Lens' DataSource (Maybe Text)
dsARN = lens _dsARN (\ s a -> s{_dsARN = a})

-- | The database name.
dsDatabaseName :: Lens' DataSource (Maybe Text)
dsDatabaseName = lens _dsDatabaseName (\ s a -> s{_dsDatabaseName = a})

-- | The data source's type, @AutoSelectOpsworksMysqlInstance@ , @OpsworksMysqlInstance@ , @RdsDbInstance@ , or @None@ .
dsType :: Lens' DataSource (Maybe Text)
dsType = lens _dsType (\ s a -> s{_dsType = a})

instance FromJSON DataSource where
        parseJSON
          = withObject "DataSource"
              (\ x ->
                 DataSource' <$>
                   (x .:? "Arn") <*> (x .:? "DatabaseName") <*>
                     (x .:? "Type"))

instance Hashable DataSource where

instance NFData DataSource where

instance ToJSON DataSource where
        toJSON DataSource'{..}
          = object
              (catMaybes
                 [("Arn" .=) <$> _dsARN,
                  ("DatabaseName" .=) <$> _dsDatabaseName,
                  ("Type" .=) <$> _dsType])

-- | Describes a deployment of a stack or app.
--
--
--
-- /See:/ 'deployment' smart constructor.
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Deployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDeploymentId' - The deployment ID.
--
-- * 'dStatus' - The deployment status:     * running     * successful     * failed
--
-- * 'dCommand' - Undocumented member.
--
-- * 'dCreatedAt' - Date when the deployment was created.
--
-- * 'dCustomJSON' - A string that contains user-defined custom JSON. It can be used to override the corresponding default stack configuration attribute values for stack or to pass data to recipes. The string should be in the following format: @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@  For more information on custom JSON, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
--
-- * 'dIAMUserARN' - The user's IAM ARN.
--
-- * 'dAppId' - The app ID.
--
-- * 'dInstanceIds' - The IDs of the target instances.
--
-- * 'dCompletedAt' - Date when the deployment completed.
--
-- * 'dStackId' - The stack ID.
--
-- * 'dComment' - A user-defined comment.
--
-- * 'dDuration' - The deployment duration.
deployment
    :: Deployment
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
dDeploymentId = lens _dDeploymentId (\ s a -> s{_dDeploymentId = a})

-- | The deployment status:     * running     * successful     * failed
dStatus :: Lens' Deployment (Maybe Text)
dStatus = lens _dStatus (\ s a -> s{_dStatus = a})

-- | Undocumented member.
dCommand :: Lens' Deployment (Maybe DeploymentCommand)
dCommand = lens _dCommand (\ s a -> s{_dCommand = a})

-- | Date when the deployment was created.
dCreatedAt :: Lens' Deployment (Maybe Text)
dCreatedAt = lens _dCreatedAt (\ s a -> s{_dCreatedAt = a})

-- | A string that contains user-defined custom JSON. It can be used to override the corresponding default stack configuration attribute values for stack or to pass data to recipes. The string should be in the following format: @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@  For more information on custom JSON, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
dCustomJSON :: Lens' Deployment (Maybe Text)
dCustomJSON = lens _dCustomJSON (\ s a -> s{_dCustomJSON = a})

-- | The user's IAM ARN.
dIAMUserARN :: Lens' Deployment (Maybe Text)
dIAMUserARN = lens _dIAMUserARN (\ s a -> s{_dIAMUserARN = a})

-- | The app ID.
dAppId :: Lens' Deployment (Maybe Text)
dAppId = lens _dAppId (\ s a -> s{_dAppId = a})

-- | The IDs of the target instances.
dInstanceIds :: Lens' Deployment [Text]
dInstanceIds = lens _dInstanceIds (\ s a -> s{_dInstanceIds = a}) . _Default . _Coerce

-- | Date when the deployment completed.
dCompletedAt :: Lens' Deployment (Maybe Text)
dCompletedAt = lens _dCompletedAt (\ s a -> s{_dCompletedAt = a})

-- | The stack ID.
dStackId :: Lens' Deployment (Maybe Text)
dStackId = lens _dStackId (\ s a -> s{_dStackId = a})

-- | A user-defined comment.
dComment :: Lens' Deployment (Maybe Text)
dComment = lens _dComment (\ s a -> s{_dComment = a})

-- | The deployment duration.
dDuration :: Lens' Deployment (Maybe Int)
dDuration = lens _dDuration (\ s a -> s{_dDuration = a})

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

instance Hashable Deployment where

instance NFData Deployment where

-- | Used to specify a stack or deployment command.
--
--
--
-- /See:/ 'deploymentCommand' smart constructor.
data DeploymentCommand = DeploymentCommand'
  { _dcArgs :: !(Maybe (Map Text [Text]))
  , _dcName :: !DeploymentCommandName
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeploymentCommand' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcArgs' - The arguments of those commands that take arguments. It should be set to a JSON object with the following format: @{"arg_name1" : ["value1", "value2", ...], "arg_name2" : ["value1", "value2", ...], ...}@  The @update_dependencies@ command takes two arguments:     * @upgrade_os_to@ - Specifies the desired Amazon Linux version for instances whose OS you want to upgrade, such as @Amazon Linux 2016.09@ . You must also set the @allow_reboot@ argument to true.     * @allow_reboot@ - Specifies whether to allow AWS OpsWorks Stacks to reboot the instances if necessary, after installing the updates. This argument can be set to either @true@ or @false@ . The default value is @false@ . For example, to upgrade an instance to Amazon Linux 2016.09, set @Args@ to the following. @{ "upgrade_os_to":["Amazon Linux 2016.09"], "allow_reboot":["true"] } @
--
-- * 'dcName' - Specifies the operation. You can specify only one command. For stacks, the following commands are available:     * @execute_recipes@ : Execute one or more recipes. To specify the recipes, set an @Args@ parameter named @recipes@ to the list of recipes to be executed. For example, to execute @phpapp::appsetup@ , set @Args@ to @{"recipes":["phpapp::appsetup"]}@ .     * @install_dependencies@ : Install the stack's dependencies.     * @update_custom_cookbooks@ : Update the stack's custom cookbooks.     * @update_dependencies@ : Update the stack's dependencies. For apps, the following commands are available:     * @deploy@ : Deploy an app. Ruby on Rails apps have an optional @Args@ parameter named @migrate@ . Set @Args@ to {"migrate":["true"]} to migrate the database. The default setting is {"migrate":["false"]}.     * @rollback@ Roll the app back to the previous version. When you update an app, AWS OpsWorks Stacks stores the previous version, up to a maximum of five versions. You can use this command to roll an app back as many as four versions.     * @start@ : Start the app's web or application server.     * @stop@ : Stop the app's web or application server.     * @restart@ : Restart the app's web or application server.     * @undeploy@ : Undeploy the app.
deploymentCommand
    :: DeploymentCommandName -- ^ 'dcName'
    -> DeploymentCommand
deploymentCommand pName_ =
  DeploymentCommand' {_dcArgs = Nothing, _dcName = pName_}


-- | The arguments of those commands that take arguments. It should be set to a JSON object with the following format: @{"arg_name1" : ["value1", "value2", ...], "arg_name2" : ["value1", "value2", ...], ...}@  The @update_dependencies@ command takes two arguments:     * @upgrade_os_to@ - Specifies the desired Amazon Linux version for instances whose OS you want to upgrade, such as @Amazon Linux 2016.09@ . You must also set the @allow_reboot@ argument to true.     * @allow_reboot@ - Specifies whether to allow AWS OpsWorks Stacks to reboot the instances if necessary, after installing the updates. This argument can be set to either @true@ or @false@ . The default value is @false@ . For example, to upgrade an instance to Amazon Linux 2016.09, set @Args@ to the following. @{ "upgrade_os_to":["Amazon Linux 2016.09"], "allow_reboot":["true"] } @
dcArgs :: Lens' DeploymentCommand (HashMap Text [Text])
dcArgs = lens _dcArgs (\ s a -> s{_dcArgs = a}) . _Default . _Map

-- | Specifies the operation. You can specify only one command. For stacks, the following commands are available:     * @execute_recipes@ : Execute one or more recipes. To specify the recipes, set an @Args@ parameter named @recipes@ to the list of recipes to be executed. For example, to execute @phpapp::appsetup@ , set @Args@ to @{"recipes":["phpapp::appsetup"]}@ .     * @install_dependencies@ : Install the stack's dependencies.     * @update_custom_cookbooks@ : Update the stack's custom cookbooks.     * @update_dependencies@ : Update the stack's dependencies. For apps, the following commands are available:     * @deploy@ : Deploy an app. Ruby on Rails apps have an optional @Args@ parameter named @migrate@ . Set @Args@ to {"migrate":["true"]} to migrate the database. The default setting is {"migrate":["false"]}.     * @rollback@ Roll the app back to the previous version. When you update an app, AWS OpsWorks Stacks stores the previous version, up to a maximum of five versions. You can use this command to roll an app back as many as four versions.     * @start@ : Start the app's web or application server.     * @stop@ : Stop the app's web or application server.     * @restart@ : Restart the app's web or application server.     * @undeploy@ : Undeploy the app.
dcName :: Lens' DeploymentCommand DeploymentCommandName
dcName = lens _dcName (\ s a -> s{_dcName = a})

instance FromJSON DeploymentCommand where
        parseJSON
          = withObject "DeploymentCommand"
              (\ x ->
                 DeploymentCommand' <$>
                   (x .:? "Args" .!= mempty) <*> (x .: "Name"))

instance Hashable DeploymentCommand where

instance NFData DeploymentCommand where

instance ToJSON DeploymentCommand where
        toJSON DeploymentCommand'{..}
          = object
              (catMaybes
                 [("Args" .=) <$> _dcArgs, Just ("Name" .= _dcName)])

-- | Describes an Amazon EBS volume. This data type maps directly to the Amazon EC2 <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice> data type.
--
--
--
-- /See:/ 'ebsBlockDevice' smart constructor.
data EBSBlockDevice = EBSBlockDevice'
  { _ebdDeleteOnTermination :: !(Maybe Bool)
  , _ebdVolumeSize          :: !(Maybe Int)
  , _ebdIOPS                :: !(Maybe Int)
  , _ebdVolumeType          :: !(Maybe VolumeType)
  , _ebdSnapshotId          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EBSBlockDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebdDeleteOnTermination' - Whether the volume is deleted on instance termination.
--
-- * 'ebdVolumeSize' - The volume size, in GiB. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice> .
--
-- * 'ebdIOPS' - The number of I/O operations per second (IOPS) that the volume supports. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice> .
--
-- * 'ebdVolumeType' - The volume type. @gp2@ for General Purpose (SSD) volumes, @io1@ for Provisioned IOPS (SSD) volumes, @st1@ for Throughput Optimized hard disk drives (HDD), @sc1@ for Cold HDD,and @standard@ for Magnetic volumes. If you specify the @io1@ volume type, you must also specify a value for the @Iops@ attribute. The maximum ratio of provisioned IOPS to requested volume size (in GiB) is 50:1. AWS uses the default volume size (in GiB) specified in the AMI attributes to set IOPS to 50 x (volume size).
--
-- * 'ebdSnapshotId' - The snapshot ID.
ebsBlockDevice
    :: EBSBlockDevice
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
ebdDeleteOnTermination = lens _ebdDeleteOnTermination (\ s a -> s{_ebdDeleteOnTermination = a})

-- | The volume size, in GiB. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice> .
ebdVolumeSize :: Lens' EBSBlockDevice (Maybe Int)
ebdVolumeSize = lens _ebdVolumeSize (\ s a -> s{_ebdVolumeSize = a})

-- | The number of I/O operations per second (IOPS) that the volume supports. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html EbsBlockDevice> .
ebdIOPS :: Lens' EBSBlockDevice (Maybe Int)
ebdIOPS = lens _ebdIOPS (\ s a -> s{_ebdIOPS = a})

-- | The volume type. @gp2@ for General Purpose (SSD) volumes, @io1@ for Provisioned IOPS (SSD) volumes, @st1@ for Throughput Optimized hard disk drives (HDD), @sc1@ for Cold HDD,and @standard@ for Magnetic volumes. If you specify the @io1@ volume type, you must also specify a value for the @Iops@ attribute. The maximum ratio of provisioned IOPS to requested volume size (in GiB) is 50:1. AWS uses the default volume size (in GiB) specified in the AMI attributes to set IOPS to 50 x (volume size).
ebdVolumeType :: Lens' EBSBlockDevice (Maybe VolumeType)
ebdVolumeType = lens _ebdVolumeType (\ s a -> s{_ebdVolumeType = a})

-- | The snapshot ID.
ebdSnapshotId :: Lens' EBSBlockDevice (Maybe Text)
ebdSnapshotId = lens _ebdSnapshotId (\ s a -> s{_ebdSnapshotId = a})

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

instance Hashable EBSBlockDevice where

instance NFData EBSBlockDevice where

instance ToJSON EBSBlockDevice where
        toJSON EBSBlockDevice'{..}
          = object
              (catMaybes
                 [("DeleteOnTermination" .=) <$>
                    _ebdDeleteOnTermination,
                  ("VolumeSize" .=) <$> _ebdVolumeSize,
                  ("Iops" .=) <$> _ebdIOPS,
                  ("VolumeType" .=) <$> _ebdVolumeType,
                  ("SnapshotId" .=) <$> _ebdSnapshotId])

-- | Describes a registered Amazon ECS cluster.
--
--
--
-- /See:/ 'ecsCluster' smart constructor.
data EcsCluster = EcsCluster'
  { _ecEcsClusterARN  :: !(Maybe Text)
  , _ecEcsClusterName :: !(Maybe Text)
  , _ecRegisteredAt   :: !(Maybe Text)
  , _ecStackId        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EcsCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecEcsClusterARN' - The cluster's ARN.
--
-- * 'ecEcsClusterName' - The cluster name.
--
-- * 'ecRegisteredAt' - The time and date that the cluster was registered with the stack.
--
-- * 'ecStackId' - The stack ID.
ecsCluster
    :: EcsCluster
ecsCluster =
  EcsCluster'
    { _ecEcsClusterARN = Nothing
    , _ecEcsClusterName = Nothing
    , _ecRegisteredAt = Nothing
    , _ecStackId = Nothing
    }


-- | The cluster's ARN.
ecEcsClusterARN :: Lens' EcsCluster (Maybe Text)
ecEcsClusterARN = lens _ecEcsClusterARN (\ s a -> s{_ecEcsClusterARN = a})

-- | The cluster name.
ecEcsClusterName :: Lens' EcsCluster (Maybe Text)
ecEcsClusterName = lens _ecEcsClusterName (\ s a -> s{_ecEcsClusterName = a})

-- | The time and date that the cluster was registered with the stack.
ecRegisteredAt :: Lens' EcsCluster (Maybe Text)
ecRegisteredAt = lens _ecRegisteredAt (\ s a -> s{_ecRegisteredAt = a})

-- | The stack ID.
ecStackId :: Lens' EcsCluster (Maybe Text)
ecStackId = lens _ecStackId (\ s a -> s{_ecStackId = a})

instance FromJSON EcsCluster where
        parseJSON
          = withObject "EcsCluster"
              (\ x ->
                 EcsCluster' <$>
                   (x .:? "EcsClusterArn") <*> (x .:? "EcsClusterName")
                     <*> (x .:? "RegisteredAt")
                     <*> (x .:? "StackId"))

instance Hashable EcsCluster where

instance NFData EcsCluster where

-- | Describes an Elastic IP address.
--
--
--
-- /See:/ 'elasticIP' smart constructor.
data ElasticIP = ElasticIP'
  { _eiInstanceId :: !(Maybe Text)
  , _eiDomain     :: !(Maybe Text)
  , _eiIP         :: !(Maybe Text)
  , _eiName       :: !(Maybe Text)
  , _eiRegion     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ElasticIP' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiInstanceId' - The ID of the instance that the address is attached to.
--
-- * 'eiDomain' - The domain.
--
-- * 'eiIP' - The IP address.
--
-- * 'eiName' - The name.
--
-- * 'eiRegion' - The AWS region. For more information, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
elasticIP
    :: ElasticIP
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
eiInstanceId = lens _eiInstanceId (\ s a -> s{_eiInstanceId = a})

-- | The domain.
eiDomain :: Lens' ElasticIP (Maybe Text)
eiDomain = lens _eiDomain (\ s a -> s{_eiDomain = a})

-- | The IP address.
eiIP :: Lens' ElasticIP (Maybe Text)
eiIP = lens _eiIP (\ s a -> s{_eiIP = a})

-- | The name.
eiName :: Lens' ElasticIP (Maybe Text)
eiName = lens _eiName (\ s a -> s{_eiName = a})

-- | The AWS region. For more information, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
eiRegion :: Lens' ElasticIP (Maybe Text)
eiRegion = lens _eiRegion (\ s a -> s{_eiRegion = a})

instance FromJSON ElasticIP where
        parseJSON
          = withObject "ElasticIP"
              (\ x ->
                 ElasticIP' <$>
                   (x .:? "InstanceId") <*> (x .:? "Domain") <*>
                     (x .:? "Ip")
                     <*> (x .:? "Name")
                     <*> (x .:? "Region"))

instance Hashable ElasticIP where

instance NFData ElasticIP where

-- | Describes an Elastic Load Balancing instance.
--
--
--
-- /See:/ 'elasticLoadBalancer' smart constructor.
data ElasticLoadBalancer = ElasticLoadBalancer'
  { _elbSubnetIds               :: !(Maybe [Text])
  , _elbVPCId                   :: !(Maybe Text)
  , _elbAvailabilityZones       :: !(Maybe [Text])
  , _elbRegion                  :: !(Maybe Text)
  , _elbElasticLoadBalancerName :: !(Maybe Text)
  , _elbStackId                 :: !(Maybe Text)
  , _elbEC2InstanceIds          :: !(Maybe [Text])
  , _elbLayerId                 :: !(Maybe Text)
  , _elbDNSName                 :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ElasticLoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'elbSubnetIds' - A list of subnet IDs, if the stack is running in a VPC.
--
-- * 'elbVPCId' - The VPC ID.
--
-- * 'elbAvailabilityZones' - A list of Availability Zones.
--
-- * 'elbRegion' - The instance's AWS region.
--
-- * 'elbElasticLoadBalancerName' - The Elastic Load Balancing instance's name.
--
-- * 'elbStackId' - The ID of the stack that the instance is associated with.
--
-- * 'elbEC2InstanceIds' - A list of the EC2 instances that the Elastic Load Balancing instance is managing traffic for.
--
-- * 'elbLayerId' - The ID of the layer that the instance is attached to.
--
-- * 'elbDNSName' - The instance's public DNS name.
elasticLoadBalancer
    :: ElasticLoadBalancer
elasticLoadBalancer =
  ElasticLoadBalancer'
    { _elbSubnetIds = Nothing
    , _elbVPCId = Nothing
    , _elbAvailabilityZones = Nothing
    , _elbRegion = Nothing
    , _elbElasticLoadBalancerName = Nothing
    , _elbStackId = Nothing
    , _elbEC2InstanceIds = Nothing
    , _elbLayerId = Nothing
    , _elbDNSName = Nothing
    }


-- | A list of subnet IDs, if the stack is running in a VPC.
elbSubnetIds :: Lens' ElasticLoadBalancer [Text]
elbSubnetIds = lens _elbSubnetIds (\ s a -> s{_elbSubnetIds = a}) . _Default . _Coerce

-- | The VPC ID.
elbVPCId :: Lens' ElasticLoadBalancer (Maybe Text)
elbVPCId = lens _elbVPCId (\ s a -> s{_elbVPCId = a})

-- | A list of Availability Zones.
elbAvailabilityZones :: Lens' ElasticLoadBalancer [Text]
elbAvailabilityZones = lens _elbAvailabilityZones (\ s a -> s{_elbAvailabilityZones = a}) . _Default . _Coerce

-- | The instance's AWS region.
elbRegion :: Lens' ElasticLoadBalancer (Maybe Text)
elbRegion = lens _elbRegion (\ s a -> s{_elbRegion = a})

-- | The Elastic Load Balancing instance's name.
elbElasticLoadBalancerName :: Lens' ElasticLoadBalancer (Maybe Text)
elbElasticLoadBalancerName = lens _elbElasticLoadBalancerName (\ s a -> s{_elbElasticLoadBalancerName = a})

-- | The ID of the stack that the instance is associated with.
elbStackId :: Lens' ElasticLoadBalancer (Maybe Text)
elbStackId = lens _elbStackId (\ s a -> s{_elbStackId = a})

-- | A list of the EC2 instances that the Elastic Load Balancing instance is managing traffic for.
elbEC2InstanceIds :: Lens' ElasticLoadBalancer [Text]
elbEC2InstanceIds = lens _elbEC2InstanceIds (\ s a -> s{_elbEC2InstanceIds = a}) . _Default . _Coerce

-- | The ID of the layer that the instance is attached to.
elbLayerId :: Lens' ElasticLoadBalancer (Maybe Text)
elbLayerId = lens _elbLayerId (\ s a -> s{_elbLayerId = a})

-- | The instance's public DNS name.
elbDNSName :: Lens' ElasticLoadBalancer (Maybe Text)
elbDNSName = lens _elbDNSName (\ s a -> s{_elbDNSName = a})

instance FromJSON ElasticLoadBalancer where
        parseJSON
          = withObject "ElasticLoadBalancer"
              (\ x ->
                 ElasticLoadBalancer' <$>
                   (x .:? "SubnetIds" .!= mempty) <*> (x .:? "VpcId")
                     <*> (x .:? "AvailabilityZones" .!= mempty)
                     <*> (x .:? "Region")
                     <*> (x .:? "ElasticLoadBalancerName")
                     <*> (x .:? "StackId")
                     <*> (x .:? "Ec2InstanceIds" .!= mempty)
                     <*> (x .:? "LayerId")
                     <*> (x .:? "DnsName"))

instance Hashable ElasticLoadBalancer where

instance NFData ElasticLoadBalancer where

-- | Represents an app's environment variable.
--
--
--
-- /See:/ 'environmentVariable' smart constructor.
data EnvironmentVariable = EnvironmentVariable'
  { _evSecure :: !(Maybe Bool)
  , _evKey    :: !Text
  , _evValue  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnvironmentVariable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'evSecure' - (Optional) Whether the variable's value will be returned by the 'DescribeApps' action. To conceal an environment variable's value, set @Secure@ to @true@ . @DescribeApps@ then returns @*****FILTERED*****@ instead of the actual value. The default value for @Secure@ is @false@ .
--
-- * 'evKey' - (Required) The environment variable's name, which can consist of up to 64 characters and must be specified. The name can contain upper- and lowercase letters, numbers, and underscores (_), but it must start with a letter or underscore.
--
-- * 'evValue' - (Optional) The environment variable's value, which can be left empty. If you specify a value, it can contain up to 256 characters, which must all be printable.
environmentVariable
    :: Text -- ^ 'evKey'
    -> Text -- ^ 'evValue'
    -> EnvironmentVariable
environmentVariable pKey_ pValue_ =
  EnvironmentVariable' {_evSecure = Nothing, _evKey = pKey_, _evValue = pValue_}


-- | (Optional) Whether the variable's value will be returned by the 'DescribeApps' action. To conceal an environment variable's value, set @Secure@ to @true@ . @DescribeApps@ then returns @*****FILTERED*****@ instead of the actual value. The default value for @Secure@ is @false@ .
evSecure :: Lens' EnvironmentVariable (Maybe Bool)
evSecure = lens _evSecure (\ s a -> s{_evSecure = a})

-- | (Required) The environment variable's name, which can consist of up to 64 characters and must be specified. The name can contain upper- and lowercase letters, numbers, and underscores (_), but it must start with a letter or underscore.
evKey :: Lens' EnvironmentVariable Text
evKey = lens _evKey (\ s a -> s{_evKey = a})

-- | (Optional) The environment variable's value, which can be left empty. If you specify a value, it can contain up to 256 characters, which must all be printable.
evValue :: Lens' EnvironmentVariable Text
evValue = lens _evValue (\ s a -> s{_evValue = a})

instance FromJSON EnvironmentVariable where
        parseJSON
          = withObject "EnvironmentVariable"
              (\ x ->
                 EnvironmentVariable' <$>
                   (x .:? "Secure") <*> (x .: "Key") <*> (x .: "Value"))

instance Hashable EnvironmentVariable where

instance NFData EnvironmentVariable where

instance ToJSON EnvironmentVariable where
        toJSON EnvironmentVariable'{..}
          = object
              (catMaybes
                 [("Secure" .=) <$> _evSecure, Just ("Key" .= _evKey),
                  Just ("Value" .= _evValue)])

-- | Describes an instance.
--
--
--
-- /See:/ 'instance'' smart constructor.
data Instance = Instance'
  { _iPrivateDNS               :: !(Maybe Text)
  , _iReportedAgentVersion     :: !(Maybe Text)
  , _iInstanceId               :: !(Maybe Text)
  , _iStatus                   :: !(Maybe Text)
  , _iPrivateIP                :: !(Maybe Text)
  , _iInstallUpdatesOnBoot     :: !(Maybe Bool)
  , _iVirtualizationType       :: !(Maybe VirtualizationType)
  , _iInstanceProfileARN       :: !(Maybe Text)
  , _iPlatform                 :: !(Maybe Text)
  , _iHostname                 :: !(Maybe Text)
  , _iSSHHostRsaKeyFingerprint :: !(Maybe Text)
  , _iSecurityGroupIds         :: !(Maybe [Text])
  , _iEcsClusterARN            :: !(Maybe Text)
  , _iARN                      :: !(Maybe Text)
  , _iCreatedAt                :: !(Maybe Text)
  , _iEC2InstanceId            :: !(Maybe Text)
  , _iSSHKeyName               :: !(Maybe Text)
  , _iAgentVersion             :: !(Maybe Text)
  , _iRootDeviceVolumeId       :: !(Maybe Text)
  , _iSubnetId                 :: !(Maybe Text)
  , _iInfrastructureClass      :: !(Maybe Text)
  , _iSSHHostDsaKeyFingerprint :: !(Maybe Text)
  , _iInstanceType             :: !(Maybe Text)
  , _iEBSOptimized             :: !(Maybe Bool)
  , _iElasticIP                :: !(Maybe Text)
  , _iOS                       :: !(Maybe Text)
  , _iAvailabilityZone         :: !(Maybe Text)
  , _iLastServiceErrorId       :: !(Maybe Text)
  , _iTenancy                  :: !(Maybe Text)
  , _iAutoScalingType          :: !(Maybe AutoScalingType)
  , _iLayerIds                 :: !(Maybe [Text])
  , _iArchitecture             :: !(Maybe Architecture)
  , _iPublicDNS                :: !(Maybe Text)
  , _iAMIId                    :: !(Maybe Text)
  , _iPublicIP                 :: !(Maybe Text)
  , _iReportedOS               :: !(Maybe ReportedOS)
  , _iRegisteredBy             :: !(Maybe Text)
  , _iStackId                  :: !(Maybe Text)
  , _iRootDeviceType           :: !(Maybe RootDeviceType)
  , _iEcsContainerInstanceARN  :: !(Maybe Text)
  , _iBlockDeviceMappings      :: !(Maybe [BlockDeviceMapping])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iPrivateDNS' - The instance's private DNS name.
--
-- * 'iReportedAgentVersion' - The instance's reported AWS OpsWorks Stacks agent version.
--
-- * 'iInstanceId' - The instance ID.
--
-- * 'iStatus' - The instance status:     * @booting@      * @connection_lost@      * @online@      * @pending@      * @rebooting@      * @requested@      * @running_setup@      * @setup_failed@      * @shutting_down@      * @start_failed@      * @stop_failed@      * @stopped@      * @stopping@      * @terminated@      * @terminating@
--
-- * 'iPrivateIP' - The instance's private IP address.
--
-- * 'iInstallUpdatesOnBoot' - Whether to install operating system and package updates when the instance boots. The default value is @true@ . If this value is set to @false@ , you must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- * 'iVirtualizationType' - The instance's virtualization type: @paravirtual@ or @hvm@ .
--
-- * 'iInstanceProfileARN' - The ARN of the instance's IAM profile. For more information about IAM ARNs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- * 'iPlatform' - The instance's platform.
--
-- * 'iHostname' - The instance host name.
--
-- * 'iSSHHostRsaKeyFingerprint' - The SSH key's RSA fingerprint.
--
-- * 'iSecurityGroupIds' - An array containing the instance security group IDs.
--
-- * 'iEcsClusterARN' - For container instances, the Amazon ECS cluster's ARN.
--
-- * 'iARN' - Undocumented member.
--
-- * 'iCreatedAt' - The time that the instance was created.
--
-- * 'iEC2InstanceId' - The ID of the associated Amazon EC2 instance.
--
-- * 'iSSHKeyName' - The instance's Amazon EC2 key-pair name.
--
-- * 'iAgentVersion' - The agent version. This parameter is set to @INHERIT@ if the instance inherits the default stack setting or to a a version number for a fixed agent version.
--
-- * 'iRootDeviceVolumeId' - The root device volume ID.
--
-- * 'iSubnetId' - The instance's subnet ID; applicable only if the stack is running in a VPC.
--
-- * 'iInfrastructureClass' - For registered instances, the infrastructure class: @ec2@ or @on-premises@ .
--
-- * 'iSSHHostDsaKeyFingerprint' - The SSH key's Deep Security Agent (DSA) fingerprint.
--
-- * 'iInstanceType' - The instance type, such as @t2.micro@ .
--
-- * 'iEBSOptimized' - Whether this is an Amazon EBS-optimized instance.
--
-- * 'iElasticIP' - The instance <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address > .
--
-- * 'iOS' - The instance's operating system.
--
-- * 'iAvailabilityZone' - The instance Availability Zone. For more information, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- * 'iLastServiceErrorId' - The ID of the last service error. For more information, call 'DescribeServiceErrors' .
--
-- * 'iTenancy' - The instance's tenancy option, such as @dedicated@ or @host@ .
--
-- * 'iAutoScalingType' - For load-based or time-based instances, the type.
--
-- * 'iLayerIds' - An array containing the instance layer IDs.
--
-- * 'iArchitecture' - The instance architecture: "i386" or "x86_64".
--
-- * 'iPublicDNS' - The instance public DNS name.
--
-- * 'iAMIId' - A custom AMI ID to be used to create the instance. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Instances>
--
-- * 'iPublicIP' - The instance public IP address.
--
-- * 'iReportedOS' - For registered instances, the reported operating system.
--
-- * 'iRegisteredBy' - For registered instances, who performed the registration.
--
-- * 'iStackId' - The stack ID.
--
-- * 'iRootDeviceType' - The instance's root device type. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
--
-- * 'iEcsContainerInstanceARN' - For container instances, the instance's ARN.
--
-- * 'iBlockDeviceMappings' - An array of @BlockDeviceMapping@ objects that specify the instance's block device mappings.
instance'
    :: Instance
instance' =
  Instance'
    { _iPrivateDNS = Nothing
    , _iReportedAgentVersion = Nothing
    , _iInstanceId = Nothing
    , _iStatus = Nothing
    , _iPrivateIP = Nothing
    , _iInstallUpdatesOnBoot = Nothing
    , _iVirtualizationType = Nothing
    , _iInstanceProfileARN = Nothing
    , _iPlatform = Nothing
    , _iHostname = Nothing
    , _iSSHHostRsaKeyFingerprint = Nothing
    , _iSecurityGroupIds = Nothing
    , _iEcsClusterARN = Nothing
    , _iARN = Nothing
    , _iCreatedAt = Nothing
    , _iEC2InstanceId = Nothing
    , _iSSHKeyName = Nothing
    , _iAgentVersion = Nothing
    , _iRootDeviceVolumeId = Nothing
    , _iSubnetId = Nothing
    , _iInfrastructureClass = Nothing
    , _iSSHHostDsaKeyFingerprint = Nothing
    , _iInstanceType = Nothing
    , _iEBSOptimized = Nothing
    , _iElasticIP = Nothing
    , _iOS = Nothing
    , _iAvailabilityZone = Nothing
    , _iLastServiceErrorId = Nothing
    , _iTenancy = Nothing
    , _iAutoScalingType = Nothing
    , _iLayerIds = Nothing
    , _iArchitecture = Nothing
    , _iPublicDNS = Nothing
    , _iAMIId = Nothing
    , _iPublicIP = Nothing
    , _iReportedOS = Nothing
    , _iRegisteredBy = Nothing
    , _iStackId = Nothing
    , _iRootDeviceType = Nothing
    , _iEcsContainerInstanceARN = Nothing
    , _iBlockDeviceMappings = Nothing
    }


-- | The instance's private DNS name.
iPrivateDNS :: Lens' Instance (Maybe Text)
iPrivateDNS = lens _iPrivateDNS (\ s a -> s{_iPrivateDNS = a})

-- | The instance's reported AWS OpsWorks Stacks agent version.
iReportedAgentVersion :: Lens' Instance (Maybe Text)
iReportedAgentVersion = lens _iReportedAgentVersion (\ s a -> s{_iReportedAgentVersion = a})

-- | The instance ID.
iInstanceId :: Lens' Instance (Maybe Text)
iInstanceId = lens _iInstanceId (\ s a -> s{_iInstanceId = a})

-- | The instance status:     * @booting@      * @connection_lost@      * @online@      * @pending@      * @rebooting@      * @requested@      * @running_setup@      * @setup_failed@      * @shutting_down@      * @start_failed@      * @stop_failed@      * @stopped@      * @stopping@      * @terminated@      * @terminating@
iStatus :: Lens' Instance (Maybe Text)
iStatus = lens _iStatus (\ s a -> s{_iStatus = a})

-- | The instance's private IP address.
iPrivateIP :: Lens' Instance (Maybe Text)
iPrivateIP = lens _iPrivateIP (\ s a -> s{_iPrivateIP = a})

-- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . If this value is set to @false@ , you must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or by manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
iInstallUpdatesOnBoot :: Lens' Instance (Maybe Bool)
iInstallUpdatesOnBoot = lens _iInstallUpdatesOnBoot (\ s a -> s{_iInstallUpdatesOnBoot = a})

-- | The instance's virtualization type: @paravirtual@ or @hvm@ .
iVirtualizationType :: Lens' Instance (Maybe VirtualizationType)
iVirtualizationType = lens _iVirtualizationType (\ s a -> s{_iVirtualizationType = a})

-- | The ARN of the instance's IAM profile. For more information about IAM ARNs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
iInstanceProfileARN :: Lens' Instance (Maybe Text)
iInstanceProfileARN = lens _iInstanceProfileARN (\ s a -> s{_iInstanceProfileARN = a})

-- | The instance's platform.
iPlatform :: Lens' Instance (Maybe Text)
iPlatform = lens _iPlatform (\ s a -> s{_iPlatform = a})

-- | The instance host name.
iHostname :: Lens' Instance (Maybe Text)
iHostname = lens _iHostname (\ s a -> s{_iHostname = a})

-- | The SSH key's RSA fingerprint.
iSSHHostRsaKeyFingerprint :: Lens' Instance (Maybe Text)
iSSHHostRsaKeyFingerprint = lens _iSSHHostRsaKeyFingerprint (\ s a -> s{_iSSHHostRsaKeyFingerprint = a})

-- | An array containing the instance security group IDs.
iSecurityGroupIds :: Lens' Instance [Text]
iSecurityGroupIds = lens _iSecurityGroupIds (\ s a -> s{_iSecurityGroupIds = a}) . _Default . _Coerce

-- | For container instances, the Amazon ECS cluster's ARN.
iEcsClusterARN :: Lens' Instance (Maybe Text)
iEcsClusterARN = lens _iEcsClusterARN (\ s a -> s{_iEcsClusterARN = a})

-- | Undocumented member.
iARN :: Lens' Instance (Maybe Text)
iARN = lens _iARN (\ s a -> s{_iARN = a})

-- | The time that the instance was created.
iCreatedAt :: Lens' Instance (Maybe Text)
iCreatedAt = lens _iCreatedAt (\ s a -> s{_iCreatedAt = a})

-- | The ID of the associated Amazon EC2 instance.
iEC2InstanceId :: Lens' Instance (Maybe Text)
iEC2InstanceId = lens _iEC2InstanceId (\ s a -> s{_iEC2InstanceId = a})

-- | The instance's Amazon EC2 key-pair name.
iSSHKeyName :: Lens' Instance (Maybe Text)
iSSHKeyName = lens _iSSHKeyName (\ s a -> s{_iSSHKeyName = a})

-- | The agent version. This parameter is set to @INHERIT@ if the instance inherits the default stack setting or to a a version number for a fixed agent version.
iAgentVersion :: Lens' Instance (Maybe Text)
iAgentVersion = lens _iAgentVersion (\ s a -> s{_iAgentVersion = a})

-- | The root device volume ID.
iRootDeviceVolumeId :: Lens' Instance (Maybe Text)
iRootDeviceVolumeId = lens _iRootDeviceVolumeId (\ s a -> s{_iRootDeviceVolumeId = a})

-- | The instance's subnet ID; applicable only if the stack is running in a VPC.
iSubnetId :: Lens' Instance (Maybe Text)
iSubnetId = lens _iSubnetId (\ s a -> s{_iSubnetId = a})

-- | For registered instances, the infrastructure class: @ec2@ or @on-premises@ .
iInfrastructureClass :: Lens' Instance (Maybe Text)
iInfrastructureClass = lens _iInfrastructureClass (\ s a -> s{_iInfrastructureClass = a})

-- | The SSH key's Deep Security Agent (DSA) fingerprint.
iSSHHostDsaKeyFingerprint :: Lens' Instance (Maybe Text)
iSSHHostDsaKeyFingerprint = lens _iSSHHostDsaKeyFingerprint (\ s a -> s{_iSSHHostDsaKeyFingerprint = a})

-- | The instance type, such as @t2.micro@ .
iInstanceType :: Lens' Instance (Maybe Text)
iInstanceType = lens _iInstanceType (\ s a -> s{_iInstanceType = a})

-- | Whether this is an Amazon EBS-optimized instance.
iEBSOptimized :: Lens' Instance (Maybe Bool)
iEBSOptimized = lens _iEBSOptimized (\ s a -> s{_iEBSOptimized = a})

-- | The instance <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address > .
iElasticIP :: Lens' Instance (Maybe Text)
iElasticIP = lens _iElasticIP (\ s a -> s{_iElasticIP = a})

-- | The instance's operating system.
iOS :: Lens' Instance (Maybe Text)
iOS = lens _iOS (\ s a -> s{_iOS = a})

-- | The instance Availability Zone. For more information, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
iAvailabilityZone :: Lens' Instance (Maybe Text)
iAvailabilityZone = lens _iAvailabilityZone (\ s a -> s{_iAvailabilityZone = a})

-- | The ID of the last service error. For more information, call 'DescribeServiceErrors' .
iLastServiceErrorId :: Lens' Instance (Maybe Text)
iLastServiceErrorId = lens _iLastServiceErrorId (\ s a -> s{_iLastServiceErrorId = a})

-- | The instance's tenancy option, such as @dedicated@ or @host@ .
iTenancy :: Lens' Instance (Maybe Text)
iTenancy = lens _iTenancy (\ s a -> s{_iTenancy = a})

-- | For load-based or time-based instances, the type.
iAutoScalingType :: Lens' Instance (Maybe AutoScalingType)
iAutoScalingType = lens _iAutoScalingType (\ s a -> s{_iAutoScalingType = a})

-- | An array containing the instance layer IDs.
iLayerIds :: Lens' Instance [Text]
iLayerIds = lens _iLayerIds (\ s a -> s{_iLayerIds = a}) . _Default . _Coerce

-- | The instance architecture: "i386" or "x86_64".
iArchitecture :: Lens' Instance (Maybe Architecture)
iArchitecture = lens _iArchitecture (\ s a -> s{_iArchitecture = a})

-- | The instance public DNS name.
iPublicDNS :: Lens' Instance (Maybe Text)
iPublicDNS = lens _iPublicDNS (\ s a -> s{_iPublicDNS = a})

-- | A custom AMI ID to be used to create the instance. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-custom-ami.html Instances>
iAMIId :: Lens' Instance (Maybe Text)
iAMIId = lens _iAMIId (\ s a -> s{_iAMIId = a})

-- | The instance public IP address.
iPublicIP :: Lens' Instance (Maybe Text)
iPublicIP = lens _iPublicIP (\ s a -> s{_iPublicIP = a})

-- | For registered instances, the reported operating system.
iReportedOS :: Lens' Instance (Maybe ReportedOS)
iReportedOS = lens _iReportedOS (\ s a -> s{_iReportedOS = a})

-- | For registered instances, who performed the registration.
iRegisteredBy :: Lens' Instance (Maybe Text)
iRegisteredBy = lens _iRegisteredBy (\ s a -> s{_iRegisteredBy = a})

-- | The stack ID.
iStackId :: Lens' Instance (Maybe Text)
iStackId = lens _iStackId (\ s a -> s{_iStackId = a})

-- | The instance's root device type. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
iRootDeviceType :: Lens' Instance (Maybe RootDeviceType)
iRootDeviceType = lens _iRootDeviceType (\ s a -> s{_iRootDeviceType = a})

-- | For container instances, the instance's ARN.
iEcsContainerInstanceARN :: Lens' Instance (Maybe Text)
iEcsContainerInstanceARN = lens _iEcsContainerInstanceARN (\ s a -> s{_iEcsContainerInstanceARN = a})

-- | An array of @BlockDeviceMapping@ objects that specify the instance's block device mappings.
iBlockDeviceMappings :: Lens' Instance [BlockDeviceMapping]
iBlockDeviceMappings = lens _iBlockDeviceMappings (\ s a -> s{_iBlockDeviceMappings = a}) . _Default . _Coerce

instance FromJSON Instance where
        parseJSON
          = withObject "Instance"
              (\ x ->
                 Instance' <$>
                   (x .:? "PrivateDns") <*>
                     (x .:? "ReportedAgentVersion")
                     <*> (x .:? "InstanceId")
                     <*> (x .:? "Status")
                     <*> (x .:? "PrivateIp")
                     <*> (x .:? "InstallUpdatesOnBoot")
                     <*> (x .:? "VirtualizationType")
                     <*> (x .:? "InstanceProfileArn")
                     <*> (x .:? "Platform")
                     <*> (x .:? "Hostname")
                     <*> (x .:? "SshHostRsaKeyFingerprint")
                     <*> (x .:? "SecurityGroupIds" .!= mempty)
                     <*> (x .:? "EcsClusterArn")
                     <*> (x .:? "Arn")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "Ec2InstanceId")
                     <*> (x .:? "SshKeyName")
                     <*> (x .:? "AgentVersion")
                     <*> (x .:? "RootDeviceVolumeId")
                     <*> (x .:? "SubnetId")
                     <*> (x .:? "InfrastructureClass")
                     <*> (x .:? "SshHostDsaKeyFingerprint")
                     <*> (x .:? "InstanceType")
                     <*> (x .:? "EbsOptimized")
                     <*> (x .:? "ElasticIp")
                     <*> (x .:? "Os")
                     <*> (x .:? "AvailabilityZone")
                     <*> (x .:? "LastServiceErrorId")
                     <*> (x .:? "Tenancy")
                     <*> (x .:? "AutoScalingType")
                     <*> (x .:? "LayerIds" .!= mempty)
                     <*> (x .:? "Architecture")
                     <*> (x .:? "PublicDns")
                     <*> (x .:? "AmiId")
                     <*> (x .:? "PublicIp")
                     <*> (x .:? "ReportedOs")
                     <*> (x .:? "RegisteredBy")
                     <*> (x .:? "StackId")
                     <*> (x .:? "RootDeviceType")
                     <*> (x .:? "EcsContainerInstanceArn")
                     <*> (x .:? "BlockDeviceMappings" .!= mempty))

instance Hashable Instance where

instance NFData Instance where

-- | Contains a description of an Amazon EC2 instance from the Amazon EC2 metadata service. For more information, see <http://docs.aws.amazon.com/sdkfornet/latest/apidocs/Index.html Instance Metadata and User Data> .
--
--
--
-- /See:/ 'instanceIdentity' smart constructor.
data InstanceIdentity = InstanceIdentity'
  { _iiSignature :: !(Maybe Text)
  , _iiDocument  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceIdentity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiSignature' - A signature that can be used to verify the document's accuracy and authenticity.
--
-- * 'iiDocument' - A JSON document that contains the metadata.
instanceIdentity
    :: InstanceIdentity
instanceIdentity =
  InstanceIdentity' {_iiSignature = Nothing, _iiDocument = Nothing}


-- | A signature that can be used to verify the document's accuracy and authenticity.
iiSignature :: Lens' InstanceIdentity (Maybe Text)
iiSignature = lens _iiSignature (\ s a -> s{_iiSignature = a})

-- | A JSON document that contains the metadata.
iiDocument :: Lens' InstanceIdentity (Maybe Text)
iiDocument = lens _iiDocument (\ s a -> s{_iiDocument = a})

instance Hashable InstanceIdentity where

instance NFData InstanceIdentity where

instance ToJSON InstanceIdentity where
        toJSON InstanceIdentity'{..}
          = object
              (catMaybes
                 [("Signature" .=) <$> _iiSignature,
                  ("Document" .=) <$> _iiDocument])

-- | Describes how many instances a stack has for each status.
--
--
--
-- /See:/ 'instancesCount' smart constructor.
data InstancesCount = InstancesCount'
  { _icTerminating    :: !(Maybe Int)
  , _icPending        :: !(Maybe Int)
  , _icOnline         :: !(Maybe Int)
  , _icUnassigning    :: !(Maybe Int)
  , _icDeregistering  :: !(Maybe Int)
  , _icRunningSetup   :: !(Maybe Int)
  , _icRequested      :: !(Maybe Int)
  , _icStopFailed     :: !(Maybe Int)
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstancesCount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icTerminating' - The number of instances with @terminating@ status.
--
-- * 'icPending' - The number of instances with @pending@ status.
--
-- * 'icOnline' - The number of instances with @online@ status.
--
-- * 'icUnassigning' - The number of instances in the Unassigning state.
--
-- * 'icDeregistering' - The number of instances in the Deregistering state.
--
-- * 'icRunningSetup' - The number of instances with @running_setup@ status.
--
-- * 'icRequested' - The number of instances with @requested@ status.
--
-- * 'icStopFailed' - Undocumented member.
--
-- * 'icBooting' - The number of instances with @booting@ status.
--
-- * 'icStopped' - The number of instances with @stopped@ status.
--
-- * 'icRebooting' - The number of instances with @rebooting@ status.
--
-- * 'icAssigning' - The number of instances in the Assigning state.
--
-- * 'icShuttingDown' - The number of instances with @shutting_down@ status.
--
-- * 'icSetupFailed' - The number of instances with @setup_failed@ status.
--
-- * 'icConnectionLost' - The number of instances with @connection_lost@ status.
--
-- * 'icTerminated' - The number of instances with @terminated@ status.
--
-- * 'icStopping' - The number of instances with @stopping@ status.
--
-- * 'icRegistered' - The number of instances in the Registered state.
--
-- * 'icStartFailed' - The number of instances with @start_failed@ status.
--
-- * 'icRegistering' - The number of instances in the Registering state.
instancesCount
    :: InstancesCount
instancesCount =
  InstancesCount'
    { _icTerminating = Nothing
    , _icPending = Nothing
    , _icOnline = Nothing
    , _icUnassigning = Nothing
    , _icDeregistering = Nothing
    , _icRunningSetup = Nothing
    , _icRequested = Nothing
    , _icStopFailed = Nothing
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
icTerminating = lens _icTerminating (\ s a -> s{_icTerminating = a})

-- | The number of instances with @pending@ status.
icPending :: Lens' InstancesCount (Maybe Int)
icPending = lens _icPending (\ s a -> s{_icPending = a})

-- | The number of instances with @online@ status.
icOnline :: Lens' InstancesCount (Maybe Int)
icOnline = lens _icOnline (\ s a -> s{_icOnline = a})

-- | The number of instances in the Unassigning state.
icUnassigning :: Lens' InstancesCount (Maybe Int)
icUnassigning = lens _icUnassigning (\ s a -> s{_icUnassigning = a})

-- | The number of instances in the Deregistering state.
icDeregistering :: Lens' InstancesCount (Maybe Int)
icDeregistering = lens _icDeregistering (\ s a -> s{_icDeregistering = a})

-- | The number of instances with @running_setup@ status.
icRunningSetup :: Lens' InstancesCount (Maybe Int)
icRunningSetup = lens _icRunningSetup (\ s a -> s{_icRunningSetup = a})

-- | The number of instances with @requested@ status.
icRequested :: Lens' InstancesCount (Maybe Int)
icRequested = lens _icRequested (\ s a -> s{_icRequested = a})

-- | Undocumented member.
icStopFailed :: Lens' InstancesCount (Maybe Int)
icStopFailed = lens _icStopFailed (\ s a -> s{_icStopFailed = a})

-- | The number of instances with @booting@ status.
icBooting :: Lens' InstancesCount (Maybe Int)
icBooting = lens _icBooting (\ s a -> s{_icBooting = a})

-- | The number of instances with @stopped@ status.
icStopped :: Lens' InstancesCount (Maybe Int)
icStopped = lens _icStopped (\ s a -> s{_icStopped = a})

-- | The number of instances with @rebooting@ status.
icRebooting :: Lens' InstancesCount (Maybe Int)
icRebooting = lens _icRebooting (\ s a -> s{_icRebooting = a})

-- | The number of instances in the Assigning state.
icAssigning :: Lens' InstancesCount (Maybe Int)
icAssigning = lens _icAssigning (\ s a -> s{_icAssigning = a})

-- | The number of instances with @shutting_down@ status.
icShuttingDown :: Lens' InstancesCount (Maybe Int)
icShuttingDown = lens _icShuttingDown (\ s a -> s{_icShuttingDown = a})

-- | The number of instances with @setup_failed@ status.
icSetupFailed :: Lens' InstancesCount (Maybe Int)
icSetupFailed = lens _icSetupFailed (\ s a -> s{_icSetupFailed = a})

-- | The number of instances with @connection_lost@ status.
icConnectionLost :: Lens' InstancesCount (Maybe Int)
icConnectionLost = lens _icConnectionLost (\ s a -> s{_icConnectionLost = a})

-- | The number of instances with @terminated@ status.
icTerminated :: Lens' InstancesCount (Maybe Int)
icTerminated = lens _icTerminated (\ s a -> s{_icTerminated = a})

-- | The number of instances with @stopping@ status.
icStopping :: Lens' InstancesCount (Maybe Int)
icStopping = lens _icStopping (\ s a -> s{_icStopping = a})

-- | The number of instances in the Registered state.
icRegistered :: Lens' InstancesCount (Maybe Int)
icRegistered = lens _icRegistered (\ s a -> s{_icRegistered = a})

-- | The number of instances with @start_failed@ status.
icStartFailed :: Lens' InstancesCount (Maybe Int)
icStartFailed = lens _icStartFailed (\ s a -> s{_icStartFailed = a})

-- | The number of instances in the Registering state.
icRegistering :: Lens' InstancesCount (Maybe Int)
icRegistering = lens _icRegistering (\ s a -> s{_icRegistering = a})

instance FromJSON InstancesCount where
        parseJSON
          = withObject "InstancesCount"
              (\ x ->
                 InstancesCount' <$>
                   (x .:? "Terminating") <*> (x .:? "Pending") <*>
                     (x .:? "Online")
                     <*> (x .:? "Unassigning")
                     <*> (x .:? "Deregistering")
                     <*> (x .:? "RunningSetup")
                     <*> (x .:? "Requested")
                     <*> (x .:? "StopFailed")
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

instance Hashable InstancesCount where

instance NFData InstancesCount where

-- | Describes a layer.
--
--
--
-- /See:/ 'layer' smart constructor.
data Layer = Layer'
  { _lCustomInstanceProfileARN :: !(Maybe Text)
  , _lCustomSecurityGroupIds :: !(Maybe [Text])
  , _lInstallUpdatesOnBoot :: !(Maybe Bool)
  , _lCloudWatchLogsConfiguration :: !(Maybe CloudWatchLogsConfiguration)
  , _lLifecycleEventConfiguration :: !(Maybe LifecycleEventConfiguration)
  , _lARN :: !(Maybe Text)
  , _lCreatedAt :: !(Maybe Text)
  , _lShortname :: !(Maybe Text)
  , _lDefaultRecipes :: !(Maybe Recipes)
  , _lCustomRecipes :: !(Maybe Recipes)
  , _lCustomJSON :: !(Maybe Text)
  , _lVolumeConfigurations :: !(Maybe [VolumeConfiguration])
  , _lEnableAutoHealing :: !(Maybe Bool)
  , _lPackages :: !(Maybe [Text])
  , _lAttributes :: !(Maybe (Map LayerAttributesKeys (Maybe Text)))
  , _lName :: !(Maybe Text)
  , _lAutoAssignPublicIPs :: !(Maybe Bool)
  , _lType :: !(Maybe LayerType)
  , _lUseEBSOptimizedInstances :: !(Maybe Bool)
  , _lStackId :: !(Maybe Text)
  , _lLayerId :: !(Maybe Text)
  , _lDefaultSecurityGroupNames :: !(Maybe [Text])
  , _lAutoAssignElasticIPs :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Layer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lCustomInstanceProfileARN' - The ARN of the default IAM profile to be used for the layer's EC2 instances. For more information about IAM ARNs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- * 'lCustomSecurityGroupIds' - An array containing the layer's custom security group IDs.
--
-- * 'lInstallUpdatesOnBoot' - Whether to install operating system and package updates when the instance boots. The default value is @true@ . If this value is set to @false@ , you must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
--
-- * 'lCloudWatchLogsConfiguration' - The Amazon CloudWatch Logs configuration settings for the layer.
--
-- * 'lLifecycleEventConfiguration' - A @LifeCycleEventConfiguration@ object that specifies the Shutdown event configuration.
--
-- * 'lARN' - Undocumented member.
--
-- * 'lCreatedAt' - Date when the layer was created.
--
-- * 'lShortname' - The layer short name.
--
-- * 'lDefaultRecipes' - Undocumented member.
--
-- * 'lCustomRecipes' - A @LayerCustomRecipes@ object that specifies the layer's custom recipes.
--
-- * 'lCustomJSON' - A JSON formatted string containing the layer's custom stack configuration and deployment attributes.
--
-- * 'lVolumeConfigurations' - A @VolumeConfigurations@ object that describes the layer's Amazon EBS volumes.
--
-- * 'lEnableAutoHealing' - Whether auto healing is disabled for the layer.
--
-- * 'lPackages' - An array of @Package@ objects that describe the layer's packages.
--
-- * 'lAttributes' - The layer attributes. For the @HaproxyStatsPassword@ , @MysqlRootPassword@ , and @GangliaPassword@ attributes, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value For an ECS Cluster layer, AWS OpsWorks Stacks the @EcsClusterArn@ attribute is set to the cluster's ARN.
--
-- * 'lName' - The layer name.
--
-- * 'lAutoAssignPublicIPs' - For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer's instances. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
--
-- * 'lType' - The layer type.
--
-- * 'lUseEBSOptimizedInstances' - Whether the layer uses Amazon EBS-optimized instances.
--
-- * 'lStackId' - The layer stack ID.
--
-- * 'lLayerId' - The layer ID.
--
-- * 'lDefaultSecurityGroupNames' - An array containing the layer's security group names.
--
-- * 'lAutoAssignElasticIPs' - Whether to automatically assign an <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's instances. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
layer
    :: Layer
layer =
  Layer'
    { _lCustomInstanceProfileARN = Nothing
    , _lCustomSecurityGroupIds = Nothing
    , _lInstallUpdatesOnBoot = Nothing
    , _lCloudWatchLogsConfiguration = Nothing
    , _lLifecycleEventConfiguration = Nothing
    , _lARN = Nothing
    , _lCreatedAt = Nothing
    , _lShortname = Nothing
    , _lDefaultRecipes = Nothing
    , _lCustomRecipes = Nothing
    , _lCustomJSON = Nothing
    , _lVolumeConfigurations = Nothing
    , _lEnableAutoHealing = Nothing
    , _lPackages = Nothing
    , _lAttributes = Nothing
    , _lName = Nothing
    , _lAutoAssignPublicIPs = Nothing
    , _lType = Nothing
    , _lUseEBSOptimizedInstances = Nothing
    , _lStackId = Nothing
    , _lLayerId = Nothing
    , _lDefaultSecurityGroupNames = Nothing
    , _lAutoAssignElasticIPs = Nothing
    }


-- | The ARN of the default IAM profile to be used for the layer's EC2 instances. For more information about IAM ARNs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
lCustomInstanceProfileARN :: Lens' Layer (Maybe Text)
lCustomInstanceProfileARN = lens _lCustomInstanceProfileARN (\ s a -> s{_lCustomInstanceProfileARN = a})

-- | An array containing the layer's custom security group IDs.
lCustomSecurityGroupIds :: Lens' Layer [Text]
lCustomSecurityGroupIds = lens _lCustomSecurityGroupIds (\ s a -> s{_lCustomSecurityGroupIds = a}) . _Default . _Coerce

-- | Whether to install operating system and package updates when the instance boots. The default value is @true@ . If this value is set to @false@ , you must then update your instances manually by using 'CreateDeployment' to run the @update_dependencies@ stack command or manually running @yum@ (Amazon Linux) or @apt-get@ (Ubuntu) on the instances.
lInstallUpdatesOnBoot :: Lens' Layer (Maybe Bool)
lInstallUpdatesOnBoot = lens _lInstallUpdatesOnBoot (\ s a -> s{_lInstallUpdatesOnBoot = a})

-- | The Amazon CloudWatch Logs configuration settings for the layer.
lCloudWatchLogsConfiguration :: Lens' Layer (Maybe CloudWatchLogsConfiguration)
lCloudWatchLogsConfiguration = lens _lCloudWatchLogsConfiguration (\ s a -> s{_lCloudWatchLogsConfiguration = a})

-- | A @LifeCycleEventConfiguration@ object that specifies the Shutdown event configuration.
lLifecycleEventConfiguration :: Lens' Layer (Maybe LifecycleEventConfiguration)
lLifecycleEventConfiguration = lens _lLifecycleEventConfiguration (\ s a -> s{_lLifecycleEventConfiguration = a})

-- | Undocumented member.
lARN :: Lens' Layer (Maybe Text)
lARN = lens _lARN (\ s a -> s{_lARN = a})

-- | Date when the layer was created.
lCreatedAt :: Lens' Layer (Maybe Text)
lCreatedAt = lens _lCreatedAt (\ s a -> s{_lCreatedAt = a})

-- | The layer short name.
lShortname :: Lens' Layer (Maybe Text)
lShortname = lens _lShortname (\ s a -> s{_lShortname = a})

-- | Undocumented member.
lDefaultRecipes :: Lens' Layer (Maybe Recipes)
lDefaultRecipes = lens _lDefaultRecipes (\ s a -> s{_lDefaultRecipes = a})

-- | A @LayerCustomRecipes@ object that specifies the layer's custom recipes.
lCustomRecipes :: Lens' Layer (Maybe Recipes)
lCustomRecipes = lens _lCustomRecipes (\ s a -> s{_lCustomRecipes = a})

-- | A JSON formatted string containing the layer's custom stack configuration and deployment attributes.
lCustomJSON :: Lens' Layer (Maybe Text)
lCustomJSON = lens _lCustomJSON (\ s a -> s{_lCustomJSON = a})

-- | A @VolumeConfigurations@ object that describes the layer's Amazon EBS volumes.
lVolumeConfigurations :: Lens' Layer [VolumeConfiguration]
lVolumeConfigurations = lens _lVolumeConfigurations (\ s a -> s{_lVolumeConfigurations = a}) . _Default . _Coerce

-- | Whether auto healing is disabled for the layer.
lEnableAutoHealing :: Lens' Layer (Maybe Bool)
lEnableAutoHealing = lens _lEnableAutoHealing (\ s a -> s{_lEnableAutoHealing = a})

-- | An array of @Package@ objects that describe the layer's packages.
lPackages :: Lens' Layer [Text]
lPackages = lens _lPackages (\ s a -> s{_lPackages = a}) . _Default . _Coerce

-- | The layer attributes. For the @HaproxyStatsPassword@ , @MysqlRootPassword@ , and @GangliaPassword@ attributes, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value For an ECS Cluster layer, AWS OpsWorks Stacks the @EcsClusterArn@ attribute is set to the cluster's ARN.
lAttributes :: Lens' Layer (HashMap LayerAttributesKeys (Maybe Text))
lAttributes = lens _lAttributes (\ s a -> s{_lAttributes = a}) . _Default . _Map

-- | The layer name.
lName :: Lens' Layer (Maybe Text)
lName = lens _lName (\ s a -> s{_lName = a})

-- | For stacks that are running in a VPC, whether to automatically assign a public IP address to the layer's instances. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
lAutoAssignPublicIPs :: Lens' Layer (Maybe Bool)
lAutoAssignPublicIPs = lens _lAutoAssignPublicIPs (\ s a -> s{_lAutoAssignPublicIPs = a})

-- | The layer type.
lType :: Lens' Layer (Maybe LayerType)
lType = lens _lType (\ s a -> s{_lType = a})

-- | Whether the layer uses Amazon EBS-optimized instances.
lUseEBSOptimizedInstances :: Lens' Layer (Maybe Bool)
lUseEBSOptimizedInstances = lens _lUseEBSOptimizedInstances (\ s a -> s{_lUseEBSOptimizedInstances = a})

-- | The layer stack ID.
lStackId :: Lens' Layer (Maybe Text)
lStackId = lens _lStackId (\ s a -> s{_lStackId = a})

-- | The layer ID.
lLayerId :: Lens' Layer (Maybe Text)
lLayerId = lens _lLayerId (\ s a -> s{_lLayerId = a})

-- | An array containing the layer's security group names.
lDefaultSecurityGroupNames :: Lens' Layer [Text]
lDefaultSecurityGroupNames = lens _lDefaultSecurityGroupNames (\ s a -> s{_lDefaultSecurityGroupNames = a}) . _Default . _Coerce

-- | Whether to automatically assign an <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP address> to the layer's instances. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-edit.html How to Edit a Layer> .
lAutoAssignElasticIPs :: Lens' Layer (Maybe Bool)
lAutoAssignElasticIPs = lens _lAutoAssignElasticIPs (\ s a -> s{_lAutoAssignElasticIPs = a})

instance FromJSON Layer where
        parseJSON
          = withObject "Layer"
              (\ x ->
                 Layer' <$>
                   (x .:? "CustomInstanceProfileArn") <*>
                     (x .:? "CustomSecurityGroupIds" .!= mempty)
                     <*> (x .:? "InstallUpdatesOnBoot")
                     <*> (x .:? "CloudWatchLogsConfiguration")
                     <*> (x .:? "LifecycleEventConfiguration")
                     <*> (x .:? "Arn")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "Shortname")
                     <*> (x .:? "DefaultRecipes")
                     <*> (x .:? "CustomRecipes")
                     <*> (x .:? "CustomJson")
                     <*> (x .:? "VolumeConfigurations" .!= mempty)
                     <*> (x .:? "EnableAutoHealing")
                     <*> (x .:? "Packages" .!= mempty)
                     <*> (x .:? "Attributes" .!= mempty)
                     <*> (x .:? "Name")
                     <*> (x .:? "AutoAssignPublicIps")
                     <*> (x .:? "Type")
                     <*> (x .:? "UseEbsOptimizedInstances")
                     <*> (x .:? "StackId")
                     <*> (x .:? "LayerId")
                     <*> (x .:? "DefaultSecurityGroupNames" .!= mempty)
                     <*> (x .:? "AutoAssignElasticIps"))

instance Hashable Layer where

instance NFData Layer where

-- | Specifies the lifecycle event configuration
--
--
--
-- /See:/ 'lifecycleEventConfiguration' smart constructor.
newtype LifecycleEventConfiguration = LifecycleEventConfiguration'
  { _lecShutdown :: Maybe ShutdownEventConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LifecycleEventConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lecShutdown' - A @ShutdownEventConfiguration@ object that specifies the Shutdown event configuration.
lifecycleEventConfiguration
    :: LifecycleEventConfiguration
lifecycleEventConfiguration =
  LifecycleEventConfiguration' {_lecShutdown = Nothing}


-- | A @ShutdownEventConfiguration@ object that specifies the Shutdown event configuration.
lecShutdown :: Lens' LifecycleEventConfiguration (Maybe ShutdownEventConfiguration)
lecShutdown = lens _lecShutdown (\ s a -> s{_lecShutdown = a})

instance FromJSON LifecycleEventConfiguration where
        parseJSON
          = withObject "LifecycleEventConfiguration"
              (\ x ->
                 LifecycleEventConfiguration' <$> (x .:? "Shutdown"))

instance Hashable LifecycleEventConfiguration where

instance NFData LifecycleEventConfiguration where

instance ToJSON LifecycleEventConfiguration where
        toJSON LifecycleEventConfiguration'{..}
          = object
              (catMaybes [("Shutdown" .=) <$> _lecShutdown])

-- | Describes a layer's load-based auto scaling configuration.
--
--
--
-- /See:/ 'loadBasedAutoScalingConfiguration' smart constructor.
data LoadBasedAutoScalingConfiguration = LoadBasedAutoScalingConfiguration'
  { _lbascUpScaling   :: !(Maybe AutoScalingThresholds)
  , _lbascEnable      :: !(Maybe Bool)
  , _lbascDownScaling :: !(Maybe AutoScalingThresholds)
  , _lbascLayerId     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadBasedAutoScalingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbascUpScaling' - An @AutoScalingThresholds@ object that describes the upscaling configuration, which defines how and when AWS OpsWorks Stacks increases the number of instances.
--
-- * 'lbascEnable' - Whether load-based auto scaling is enabled for the layer.
--
-- * 'lbascDownScaling' - An @AutoScalingThresholds@ object that describes the downscaling configuration, which defines how and when AWS OpsWorks Stacks reduces the number of instances.
--
-- * 'lbascLayerId' - The layer ID.
loadBasedAutoScalingConfiguration
    :: LoadBasedAutoScalingConfiguration
loadBasedAutoScalingConfiguration =
  LoadBasedAutoScalingConfiguration'
    { _lbascUpScaling = Nothing
    , _lbascEnable = Nothing
    , _lbascDownScaling = Nothing
    , _lbascLayerId = Nothing
    }


-- | An @AutoScalingThresholds@ object that describes the upscaling configuration, which defines how and when AWS OpsWorks Stacks increases the number of instances.
lbascUpScaling :: Lens' LoadBasedAutoScalingConfiguration (Maybe AutoScalingThresholds)
lbascUpScaling = lens _lbascUpScaling (\ s a -> s{_lbascUpScaling = a})

-- | Whether load-based auto scaling is enabled for the layer.
lbascEnable :: Lens' LoadBasedAutoScalingConfiguration (Maybe Bool)
lbascEnable = lens _lbascEnable (\ s a -> s{_lbascEnable = a})

-- | An @AutoScalingThresholds@ object that describes the downscaling configuration, which defines how and when AWS OpsWorks Stacks reduces the number of instances.
lbascDownScaling :: Lens' LoadBasedAutoScalingConfiguration (Maybe AutoScalingThresholds)
lbascDownScaling = lens _lbascDownScaling (\ s a -> s{_lbascDownScaling = a})

-- | The layer ID.
lbascLayerId :: Lens' LoadBasedAutoScalingConfiguration (Maybe Text)
lbascLayerId = lens _lbascLayerId (\ s a -> s{_lbascLayerId = a})

instance FromJSON LoadBasedAutoScalingConfiguration
         where
        parseJSON
          = withObject "LoadBasedAutoScalingConfiguration"
              (\ x ->
                 LoadBasedAutoScalingConfiguration' <$>
                   (x .:? "UpScaling") <*> (x .:? "Enable") <*>
                     (x .:? "DownScaling")
                     <*> (x .:? "LayerId"))

instance Hashable LoadBasedAutoScalingConfiguration
         where

instance NFData LoadBasedAutoScalingConfiguration
         where

-- | Describes supported operating systems in AWS OpsWorks Stacks.
--
--
--
-- /See:/ 'operatingSystem' smart constructor.
data OperatingSystem = OperatingSystem'
  { _osReportedVersion       :: !(Maybe Text)
  , _osSupported             :: !(Maybe Bool)
  , _osName                  :: !(Maybe Text)
  , _osId                    :: !(Maybe Text)
  , _osConfigurationManagers :: !(Maybe [OperatingSystemConfigurationManager])
  , _osType                  :: !(Maybe Text)
  , _osReportedName          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OperatingSystem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osReportedVersion' - The version of the operating system, including the release and edition, if applicable.
--
-- * 'osSupported' - Indicates that an operating system is not supported for new instances.
--
-- * 'osName' - The name of the operating system, such as @Amazon Linux 2017.09@ .
--
-- * 'osId' - The ID of a supported operating system, such as @Amazon Linux 2017.09@ .
--
-- * 'osConfigurationManagers' - Supported configuration manager name and versions for an AWS OpsWorks Stacks operating system.
--
-- * 'osType' - The type of a supported operating system, either @Linux@ or @Windows@ .
--
-- * 'osReportedName' - A short name for the operating system manufacturer.
operatingSystem
    :: OperatingSystem
operatingSystem =
  OperatingSystem'
    { _osReportedVersion = Nothing
    , _osSupported = Nothing
    , _osName = Nothing
    , _osId = Nothing
    , _osConfigurationManagers = Nothing
    , _osType = Nothing
    , _osReportedName = Nothing
    }


-- | The version of the operating system, including the release and edition, if applicable.
osReportedVersion :: Lens' OperatingSystem (Maybe Text)
osReportedVersion = lens _osReportedVersion (\ s a -> s{_osReportedVersion = a})

-- | Indicates that an operating system is not supported for new instances.
osSupported :: Lens' OperatingSystem (Maybe Bool)
osSupported = lens _osSupported (\ s a -> s{_osSupported = a})

-- | The name of the operating system, such as @Amazon Linux 2017.09@ .
osName :: Lens' OperatingSystem (Maybe Text)
osName = lens _osName (\ s a -> s{_osName = a})

-- | The ID of a supported operating system, such as @Amazon Linux 2017.09@ .
osId :: Lens' OperatingSystem (Maybe Text)
osId = lens _osId (\ s a -> s{_osId = a})

-- | Supported configuration manager name and versions for an AWS OpsWorks Stacks operating system.
osConfigurationManagers :: Lens' OperatingSystem [OperatingSystemConfigurationManager]
osConfigurationManagers = lens _osConfigurationManagers (\ s a -> s{_osConfigurationManagers = a}) . _Default . _Coerce

-- | The type of a supported operating system, either @Linux@ or @Windows@ .
osType :: Lens' OperatingSystem (Maybe Text)
osType = lens _osType (\ s a -> s{_osType = a})

-- | A short name for the operating system manufacturer.
osReportedName :: Lens' OperatingSystem (Maybe Text)
osReportedName = lens _osReportedName (\ s a -> s{_osReportedName = a})

instance FromJSON OperatingSystem where
        parseJSON
          = withObject "OperatingSystem"
              (\ x ->
                 OperatingSystem' <$>
                   (x .:? "ReportedVersion") <*> (x .:? "Supported") <*>
                     (x .:? "Name")
                     <*> (x .:? "Id")
                     <*> (x .:? "ConfigurationManagers" .!= mempty)
                     <*> (x .:? "Type")
                     <*> (x .:? "ReportedName"))

instance Hashable OperatingSystem where

instance NFData OperatingSystem where

-- | A block that contains information about the configuration manager (Chef) and the versions of the configuration manager that are supported for an operating system.
--
--
--
-- /See:/ 'operatingSystemConfigurationManager' smart constructor.
data OperatingSystemConfigurationManager = OperatingSystemConfigurationManager'
  { _oscmName    :: !(Maybe Text)
  , _oscmVersion :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OperatingSystemConfigurationManager' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oscmName' - The name of the configuration manager, which is Chef.
--
-- * 'oscmVersion' - The versions of the configuration manager that are supported by an operating system.
operatingSystemConfigurationManager
    :: OperatingSystemConfigurationManager
operatingSystemConfigurationManager =
  OperatingSystemConfigurationManager'
    {_oscmName = Nothing, _oscmVersion = Nothing}


-- | The name of the configuration manager, which is Chef.
oscmName :: Lens' OperatingSystemConfigurationManager (Maybe Text)
oscmName = lens _oscmName (\ s a -> s{_oscmName = a})

-- | The versions of the configuration manager that are supported by an operating system.
oscmVersion :: Lens' OperatingSystemConfigurationManager (Maybe Text)
oscmVersion = lens _oscmVersion (\ s a -> s{_oscmVersion = a})

instance FromJSON OperatingSystemConfigurationManager
         where
        parseJSON
          = withObject "OperatingSystemConfigurationManager"
              (\ x ->
                 OperatingSystemConfigurationManager' <$>
                   (x .:? "Name") <*> (x .:? "Version"))

instance Hashable OperatingSystemConfigurationManager
         where

instance NFData OperatingSystemConfigurationManager
         where

-- | Describes stack or user permissions.
--
--
--
-- /See:/ 'permission' smart constructor.
data Permission = Permission'
  { _pIAMUserARN :: !(Maybe Text)
  , _pAllowSudo  :: !(Maybe Bool)
  , _pStackId    :: !(Maybe Text)
  , _pLevel      :: !(Maybe Text)
  , _pAllowSSH   :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Permission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pIAMUserARN' - The Amazon Resource Name (ARN) for an AWS Identity and Access Management (IAM) role. For more information about IAM ARNs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- * 'pAllowSudo' - Whether the user can use __sudo__ .
--
-- * 'pStackId' - A stack ID.
--
-- * 'pLevel' - The user's permission level, which must be the following:     * @deny@      * @show@      * @deploy@      * @manage@      * @iam_only@  For more information on the permissions associated with these levels, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>
--
-- * 'pAllowSSH' - Whether the user can use SSH.
permission
    :: Permission
permission =
  Permission'
    { _pIAMUserARN = Nothing
    , _pAllowSudo = Nothing
    , _pStackId = Nothing
    , _pLevel = Nothing
    , _pAllowSSH = Nothing
    }


-- | The Amazon Resource Name (ARN) for an AWS Identity and Access Management (IAM) role. For more information about IAM ARNs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
pIAMUserARN :: Lens' Permission (Maybe Text)
pIAMUserARN = lens _pIAMUserARN (\ s a -> s{_pIAMUserARN = a})

-- | Whether the user can use __sudo__ .
pAllowSudo :: Lens' Permission (Maybe Bool)
pAllowSudo = lens _pAllowSudo (\ s a -> s{_pAllowSudo = a})

-- | A stack ID.
pStackId :: Lens' Permission (Maybe Text)
pStackId = lens _pStackId (\ s a -> s{_pStackId = a})

-- | The user's permission level, which must be the following:     * @deny@      * @show@      * @deploy@      * @manage@      * @iam_only@  For more information on the permissions associated with these levels, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>
pLevel :: Lens' Permission (Maybe Text)
pLevel = lens _pLevel (\ s a -> s{_pLevel = a})

-- | Whether the user can use SSH.
pAllowSSH :: Lens' Permission (Maybe Bool)
pAllowSSH = lens _pAllowSSH (\ s a -> s{_pAllowSSH = a})

instance FromJSON Permission where
        parseJSON
          = withObject "Permission"
              (\ x ->
                 Permission' <$>
                   (x .:? "IamUserArn") <*> (x .:? "AllowSudo") <*>
                     (x .:? "StackId")
                     <*> (x .:? "Level")
                     <*> (x .:? "AllowSsh"))

instance Hashable Permission where

instance NFData Permission where

-- | Describes an instance's RAID array.
--
--
--
-- /See:/ 'rAIdArray' smart constructor.
data RAIdArray = RAIdArray'
  { _raiaInstanceId       :: !(Maybe Text)
  , _raiaSize             :: !(Maybe Int)
  , _raiaIOPS             :: !(Maybe Int)
  , _raiaCreatedAt        :: !(Maybe Text)
  , _raiaRAIdLevel        :: !(Maybe Int)
  , _raiaDevice           :: !(Maybe Text)
  , _raiaNumberOfDisks    :: !(Maybe Int)
  , _raiaAvailabilityZone :: !(Maybe Text)
  , _raiaName             :: !(Maybe Text)
  , _raiaRAIdArrayId      :: !(Maybe Text)
  , _raiaVolumeType       :: !(Maybe Text)
  , _raiaStackId          :: !(Maybe Text)
  , _raiaMountPoint       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RAIdArray' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raiaInstanceId' - The instance ID.
--
-- * 'raiaSize' - The array's size.
--
-- * 'raiaIOPS' - For PIOPS volumes, the IOPS per disk.
--
-- * 'raiaCreatedAt' - When the RAID array was created.
--
-- * 'raiaRAIdLevel' - The <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level> .
--
-- * 'raiaDevice' - The array's Linux device. For example /dev/mdadm0.
--
-- * 'raiaNumberOfDisks' - The number of disks in the array.
--
-- * 'raiaAvailabilityZone' - The array's Availability Zone. For more information, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- * 'raiaName' - The array name.
--
-- * 'raiaRAIdArrayId' - The array ID.
--
-- * 'raiaVolumeType' - The volume type, standard or PIOPS.
--
-- * 'raiaStackId' - The stack ID.
--
-- * 'raiaMountPoint' - The array's mount point.
rAIdArray
    :: RAIdArray
rAIdArray =
  RAIdArray'
    { _raiaInstanceId = Nothing
    , _raiaSize = Nothing
    , _raiaIOPS = Nothing
    , _raiaCreatedAt = Nothing
    , _raiaRAIdLevel = Nothing
    , _raiaDevice = Nothing
    , _raiaNumberOfDisks = Nothing
    , _raiaAvailabilityZone = Nothing
    , _raiaName = Nothing
    , _raiaRAIdArrayId = Nothing
    , _raiaVolumeType = Nothing
    , _raiaStackId = Nothing
    , _raiaMountPoint = Nothing
    }


-- | The instance ID.
raiaInstanceId :: Lens' RAIdArray (Maybe Text)
raiaInstanceId = lens _raiaInstanceId (\ s a -> s{_raiaInstanceId = a})

-- | The array's size.
raiaSize :: Lens' RAIdArray (Maybe Int)
raiaSize = lens _raiaSize (\ s a -> s{_raiaSize = a})

-- | For PIOPS volumes, the IOPS per disk.
raiaIOPS :: Lens' RAIdArray (Maybe Int)
raiaIOPS = lens _raiaIOPS (\ s a -> s{_raiaIOPS = a})

-- | When the RAID array was created.
raiaCreatedAt :: Lens' RAIdArray (Maybe Text)
raiaCreatedAt = lens _raiaCreatedAt (\ s a -> s{_raiaCreatedAt = a})

-- | The <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level> .
raiaRAIdLevel :: Lens' RAIdArray (Maybe Int)
raiaRAIdLevel = lens _raiaRAIdLevel (\ s a -> s{_raiaRAIdLevel = a})

-- | The array's Linux device. For example /dev/mdadm0.
raiaDevice :: Lens' RAIdArray (Maybe Text)
raiaDevice = lens _raiaDevice (\ s a -> s{_raiaDevice = a})

-- | The number of disks in the array.
raiaNumberOfDisks :: Lens' RAIdArray (Maybe Int)
raiaNumberOfDisks = lens _raiaNumberOfDisks (\ s a -> s{_raiaNumberOfDisks = a})

-- | The array's Availability Zone. For more information, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
raiaAvailabilityZone :: Lens' RAIdArray (Maybe Text)
raiaAvailabilityZone = lens _raiaAvailabilityZone (\ s a -> s{_raiaAvailabilityZone = a})

-- | The array name.
raiaName :: Lens' RAIdArray (Maybe Text)
raiaName = lens _raiaName (\ s a -> s{_raiaName = a})

-- | The array ID.
raiaRAIdArrayId :: Lens' RAIdArray (Maybe Text)
raiaRAIdArrayId = lens _raiaRAIdArrayId (\ s a -> s{_raiaRAIdArrayId = a})

-- | The volume type, standard or PIOPS.
raiaVolumeType :: Lens' RAIdArray (Maybe Text)
raiaVolumeType = lens _raiaVolumeType (\ s a -> s{_raiaVolumeType = a})

-- | The stack ID.
raiaStackId :: Lens' RAIdArray (Maybe Text)
raiaStackId = lens _raiaStackId (\ s a -> s{_raiaStackId = a})

-- | The array's mount point.
raiaMountPoint :: Lens' RAIdArray (Maybe Text)
raiaMountPoint = lens _raiaMountPoint (\ s a -> s{_raiaMountPoint = a})

instance FromJSON RAIdArray where
        parseJSON
          = withObject "RAIdArray"
              (\ x ->
                 RAIdArray' <$>
                   (x .:? "InstanceId") <*> (x .:? "Size") <*>
                     (x .:? "Iops")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "RaidLevel")
                     <*> (x .:? "Device")
                     <*> (x .:? "NumberOfDisks")
                     <*> (x .:? "AvailabilityZone")
                     <*> (x .:? "Name")
                     <*> (x .:? "RaidArrayId")
                     <*> (x .:? "VolumeType")
                     <*> (x .:? "StackId")
                     <*> (x .:? "MountPoint"))

instance Hashable RAIdArray where

instance NFData RAIdArray where

-- | Describes an Amazon RDS instance.
--
--
--
-- /See:/ 'rdsDBInstance' smart constructor.
data RDSDBInstance = RDSDBInstance'
  { _rdiRDSDBInstanceARN     :: !(Maybe Text)
  , _rdiDBUser               :: !(Maybe Text)
  , _rdiMissingOnRDS         :: !(Maybe Bool)
  , _rdiEngine               :: !(Maybe Text)
  , _rdiAddress              :: !(Maybe Text)
  , _rdiDBInstanceIdentifier :: !(Maybe Text)
  , _rdiRegion               :: !(Maybe Text)
  , _rdiStackId              :: !(Maybe Text)
  , _rdiDBPassword           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RDSDBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdiRDSDBInstanceARN' - The instance's ARN.
--
-- * 'rdiDBUser' - The master user name.
--
-- * 'rdiMissingOnRDS' - Set to @true@ if AWS OpsWorks Stacks is unable to discover the Amazon RDS instance. AWS OpsWorks Stacks attempts to discover the instance only once. If this value is set to @true@ , you must deregister the instance, and then register it again.
--
-- * 'rdiEngine' - The instance's database engine.
--
-- * 'rdiAddress' - The instance's address.
--
-- * 'rdiDBInstanceIdentifier' - The DB instance identifier.
--
-- * 'rdiRegion' - The instance's AWS region.
--
-- * 'rdiStackId' - The ID of the stack with which the instance is registered.
--
-- * 'rdiDBPassword' - AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
rdsDBInstance
    :: RDSDBInstance
rdsDBInstance =
  RDSDBInstance'
    { _rdiRDSDBInstanceARN = Nothing
    , _rdiDBUser = Nothing
    , _rdiMissingOnRDS = Nothing
    , _rdiEngine = Nothing
    , _rdiAddress = Nothing
    , _rdiDBInstanceIdentifier = Nothing
    , _rdiRegion = Nothing
    , _rdiStackId = Nothing
    , _rdiDBPassword = Nothing
    }


-- | The instance's ARN.
rdiRDSDBInstanceARN :: Lens' RDSDBInstance (Maybe Text)
rdiRDSDBInstanceARN = lens _rdiRDSDBInstanceARN (\ s a -> s{_rdiRDSDBInstanceARN = a})

-- | The master user name.
rdiDBUser :: Lens' RDSDBInstance (Maybe Text)
rdiDBUser = lens _rdiDBUser (\ s a -> s{_rdiDBUser = a})

-- | Set to @true@ if AWS OpsWorks Stacks is unable to discover the Amazon RDS instance. AWS OpsWorks Stacks attempts to discover the instance only once. If this value is set to @true@ , you must deregister the instance, and then register it again.
rdiMissingOnRDS :: Lens' RDSDBInstance (Maybe Bool)
rdiMissingOnRDS = lens _rdiMissingOnRDS (\ s a -> s{_rdiMissingOnRDS = a})

-- | The instance's database engine.
rdiEngine :: Lens' RDSDBInstance (Maybe Text)
rdiEngine = lens _rdiEngine (\ s a -> s{_rdiEngine = a})

-- | The instance's address.
rdiAddress :: Lens' RDSDBInstance (Maybe Text)
rdiAddress = lens _rdiAddress (\ s a -> s{_rdiAddress = a})

-- | The DB instance identifier.
rdiDBInstanceIdentifier :: Lens' RDSDBInstance (Maybe Text)
rdiDBInstanceIdentifier = lens _rdiDBInstanceIdentifier (\ s a -> s{_rdiDBInstanceIdentifier = a})

-- | The instance's AWS region.
rdiRegion :: Lens' RDSDBInstance (Maybe Text)
rdiRegion = lens _rdiRegion (\ s a -> s{_rdiRegion = a})

-- | The ID of the stack with which the instance is registered.
rdiStackId :: Lens' RDSDBInstance (Maybe Text)
rdiStackId = lens _rdiStackId (\ s a -> s{_rdiStackId = a})

-- | AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
rdiDBPassword :: Lens' RDSDBInstance (Maybe Text)
rdiDBPassword = lens _rdiDBPassword (\ s a -> s{_rdiDBPassword = a})

instance FromJSON RDSDBInstance where
        parseJSON
          = withObject "RDSDBInstance"
              (\ x ->
                 RDSDBInstance' <$>
                   (x .:? "RdsDbInstanceArn") <*> (x .:? "DbUser") <*>
                     (x .:? "MissingOnRds")
                     <*> (x .:? "Engine")
                     <*> (x .:? "Address")
                     <*> (x .:? "DbInstanceIdentifier")
                     <*> (x .:? "Region")
                     <*> (x .:? "StackId")
                     <*> (x .:? "DbPassword"))

instance Hashable RDSDBInstance where

instance NFData RDSDBInstance where

-- | AWS OpsWorks Stacks supports five lifecycle events: __setup__ , __configuration__ , __deploy__ , __undeploy__ , and __shutdown__ . For each layer, AWS OpsWorks Stacks runs a set of standard recipes for each event. In addition, you can provide custom recipes for any or all layers and events. AWS OpsWorks Stacks runs custom event recipes after the standard recipes. @LayerCustomRecipes@ specifies the custom recipes for a particular layer to be run in response to each of the five events.
--
--
-- To specify a recipe, use the cookbook's directory name in the repository followed by two colons and the recipe name, which is the recipe's file name without the .rb extension. For example: phpapp2::dbsetup specifies the dbsetup.rb recipe in the repository's phpapp2 folder.
--
--
-- /See:/ 'recipes' smart constructor.
data Recipes = Recipes'
  { _rSetup     :: !(Maybe [Text])
  , _rShutdown  :: !(Maybe [Text])
  , _rUndeploy  :: !(Maybe [Text])
  , _rConfigure :: !(Maybe [Text])
  , _rDeploy    :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Recipes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rSetup' - An array of custom recipe names to be run following a @setup@ event.
--
-- * 'rShutdown' - An array of custom recipe names to be run following a @shutdown@ event.
--
-- * 'rUndeploy' - An array of custom recipe names to be run following a @undeploy@ event.
--
-- * 'rConfigure' - An array of custom recipe names to be run following a @configure@ event.
--
-- * 'rDeploy' - An array of custom recipe names to be run following a @deploy@ event.
recipes
    :: Recipes
recipes =
  Recipes'
    { _rSetup = Nothing
    , _rShutdown = Nothing
    , _rUndeploy = Nothing
    , _rConfigure = Nothing
    , _rDeploy = Nothing
    }


-- | An array of custom recipe names to be run following a @setup@ event.
rSetup :: Lens' Recipes [Text]
rSetup = lens _rSetup (\ s a -> s{_rSetup = a}) . _Default . _Coerce

-- | An array of custom recipe names to be run following a @shutdown@ event.
rShutdown :: Lens' Recipes [Text]
rShutdown = lens _rShutdown (\ s a -> s{_rShutdown = a}) . _Default . _Coerce

-- | An array of custom recipe names to be run following a @undeploy@ event.
rUndeploy :: Lens' Recipes [Text]
rUndeploy = lens _rUndeploy (\ s a -> s{_rUndeploy = a}) . _Default . _Coerce

-- | An array of custom recipe names to be run following a @configure@ event.
rConfigure :: Lens' Recipes [Text]
rConfigure = lens _rConfigure (\ s a -> s{_rConfigure = a}) . _Default . _Coerce

-- | An array of custom recipe names to be run following a @deploy@ event.
rDeploy :: Lens' Recipes [Text]
rDeploy = lens _rDeploy (\ s a -> s{_rDeploy = a}) . _Default . _Coerce

instance FromJSON Recipes where
        parseJSON
          = withObject "Recipes"
              (\ x ->
                 Recipes' <$>
                   (x .:? "Setup" .!= mempty) <*>
                     (x .:? "Shutdown" .!= mempty)
                     <*> (x .:? "Undeploy" .!= mempty)
                     <*> (x .:? "Configure" .!= mempty)
                     <*> (x .:? "Deploy" .!= mempty))

instance Hashable Recipes where

instance NFData Recipes where

instance ToJSON Recipes where
        toJSON Recipes'{..}
          = object
              (catMaybes
                 [("Setup" .=) <$> _rSetup,
                  ("Shutdown" .=) <$> _rShutdown,
                  ("Undeploy" .=) <$> _rUndeploy,
                  ("Configure" .=) <$> _rConfigure,
                  ("Deploy" .=) <$> _rDeploy])

-- | A registered instance's reported operating system.
--
--
--
-- /See:/ 'reportedOS' smart constructor.
data ReportedOS = ReportedOS'
  { _roFamily  :: !(Maybe Text)
  , _roName    :: !(Maybe Text)
  , _roVersion :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReportedOS' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'roFamily' - The operating system family.
--
-- * 'roName' - The operating system name.
--
-- * 'roVersion' - The operating system version.
reportedOS
    :: ReportedOS
reportedOS =
  ReportedOS' {_roFamily = Nothing, _roName = Nothing, _roVersion = Nothing}


-- | The operating system family.
roFamily :: Lens' ReportedOS (Maybe Text)
roFamily = lens _roFamily (\ s a -> s{_roFamily = a})

-- | The operating system name.
roName :: Lens' ReportedOS (Maybe Text)
roName = lens _roName (\ s a -> s{_roName = a})

-- | The operating system version.
roVersion :: Lens' ReportedOS (Maybe Text)
roVersion = lens _roVersion (\ s a -> s{_roVersion = a})

instance FromJSON ReportedOS where
        parseJSON
          = withObject "ReportedOS"
              (\ x ->
                 ReportedOS' <$>
                   (x .:? "Family") <*> (x .:? "Name") <*>
                     (x .:? "Version"))

instance Hashable ReportedOS where

instance NFData ReportedOS where

-- | Describes an app's SSL configuration.
--
--
--
-- /See:/ 'sslConfiguration' smart constructor.
data SSLConfiguration = SSLConfiguration'
  { _scPrivateKey  :: !(Maybe Text)
  , _scCertificate :: !(Maybe Text)
  , _scChain       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SSLConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scPrivateKey' - The private key; the contents of the certificate's domain.kex file.
--
-- * 'scCertificate' - The contents of the certificate's domain.crt file.
--
-- * 'scChain' - Optional. Can be used to specify an intermediate certificate authority key or client authentication.
sslConfiguration
    :: SSLConfiguration
sslConfiguration =
  SSLConfiguration'
    {_scPrivateKey = Nothing, _scCertificate = Nothing, _scChain = Nothing}


-- | The private key; the contents of the certificate's domain.kex file.
scPrivateKey :: Lens' SSLConfiguration (Maybe Text)
scPrivateKey = lens _scPrivateKey (\ s a -> s{_scPrivateKey = a})

-- | The contents of the certificate's domain.crt file.
scCertificate :: Lens' SSLConfiguration (Maybe Text)
scCertificate = lens _scCertificate (\ s a -> s{_scCertificate = a})

-- | Optional. Can be used to specify an intermediate certificate authority key or client authentication.
scChain :: Lens' SSLConfiguration (Maybe Text)
scChain = lens _scChain (\ s a -> s{_scChain = a})

instance FromJSON SSLConfiguration where
        parseJSON
          = withObject "SSLConfiguration"
              (\ x ->
                 SSLConfiguration' <$>
                   (x .:? "PrivateKey") <*> (x .:? "Certificate") <*>
                     (x .:? "Chain"))

instance Hashable SSLConfiguration where

instance NFData SSLConfiguration where

instance ToJSON SSLConfiguration where
        toJSON SSLConfiguration'{..}
          = object
              (catMaybes
                 [("PrivateKey" .=) <$> _scPrivateKey,
                  ("Certificate" .=) <$> _scCertificate,
                  ("Chain" .=) <$> _scChain])

-- | Describes a user's SSH information.
--
--
--
-- /See:/ 'selfUserProfile' smart constructor.
data SelfUserProfile = SelfUserProfile'
  { _supSSHPublicKey :: !(Maybe Text)
  , _supSSHUsername  :: !(Maybe Text)
  , _supIAMUserARN   :: !(Maybe Text)
  , _supName         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SelfUserProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'supSSHPublicKey' - The user's SSH public key.
--
-- * 'supSSHUsername' - The user's SSH user name.
--
-- * 'supIAMUserARN' - The user's IAM ARN.
--
-- * 'supName' - The user's name.
selfUserProfile
    :: SelfUserProfile
selfUserProfile =
  SelfUserProfile'
    { _supSSHPublicKey = Nothing
    , _supSSHUsername = Nothing
    , _supIAMUserARN = Nothing
    , _supName = Nothing
    }


-- | The user's SSH public key.
supSSHPublicKey :: Lens' SelfUserProfile (Maybe Text)
supSSHPublicKey = lens _supSSHPublicKey (\ s a -> s{_supSSHPublicKey = a})

-- | The user's SSH user name.
supSSHUsername :: Lens' SelfUserProfile (Maybe Text)
supSSHUsername = lens _supSSHUsername (\ s a -> s{_supSSHUsername = a})

-- | The user's IAM ARN.
supIAMUserARN :: Lens' SelfUserProfile (Maybe Text)
supIAMUserARN = lens _supIAMUserARN (\ s a -> s{_supIAMUserARN = a})

-- | The user's name.
supName :: Lens' SelfUserProfile (Maybe Text)
supName = lens _supName (\ s a -> s{_supName = a})

instance FromJSON SelfUserProfile where
        parseJSON
          = withObject "SelfUserProfile"
              (\ x ->
                 SelfUserProfile' <$>
                   (x .:? "SshPublicKey") <*> (x .:? "SshUsername") <*>
                     (x .:? "IamUserArn")
                     <*> (x .:? "Name"))

instance Hashable SelfUserProfile where

instance NFData SelfUserProfile where

-- | Describes an AWS OpsWorks Stacks service error.
--
--
--
-- /See:/ 'serviceError'' smart constructor.
data ServiceError' = ServiceError''
  { _seInstanceId     :: !(Maybe Text)
  , _seCreatedAt      :: !(Maybe Text)
  , _seServiceErrorId :: !(Maybe Text)
  , _seType           :: !(Maybe Text)
  , _seStackId        :: !(Maybe Text)
  , _seMessage        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ServiceError'' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seInstanceId' - The instance ID.
--
-- * 'seCreatedAt' - When the error occurred.
--
-- * 'seServiceErrorId' - The error ID.
--
-- * 'seType' - The error type.
--
-- * 'seStackId' - The stack ID.
--
-- * 'seMessage' - A message that describes the error.
serviceError'
    :: ServiceError'
serviceError' =
  ServiceError''
    { _seInstanceId = Nothing
    , _seCreatedAt = Nothing
    , _seServiceErrorId = Nothing
    , _seType = Nothing
    , _seStackId = Nothing
    , _seMessage = Nothing
    }


-- | The instance ID.
seInstanceId :: Lens' ServiceError' (Maybe Text)
seInstanceId = lens _seInstanceId (\ s a -> s{_seInstanceId = a})

-- | When the error occurred.
seCreatedAt :: Lens' ServiceError' (Maybe Text)
seCreatedAt = lens _seCreatedAt (\ s a -> s{_seCreatedAt = a})

-- | The error ID.
seServiceErrorId :: Lens' ServiceError' (Maybe Text)
seServiceErrorId = lens _seServiceErrorId (\ s a -> s{_seServiceErrorId = a})

-- | The error type.
seType :: Lens' ServiceError' (Maybe Text)
seType = lens _seType (\ s a -> s{_seType = a})

-- | The stack ID.
seStackId :: Lens' ServiceError' (Maybe Text)
seStackId = lens _seStackId (\ s a -> s{_seStackId = a})

-- | A message that describes the error.
seMessage :: Lens' ServiceError' (Maybe Text)
seMessage = lens _seMessage (\ s a -> s{_seMessage = a})

instance FromJSON ServiceError' where
        parseJSON
          = withObject "ServiceError'"
              (\ x ->
                 ServiceError'' <$>
                   (x .:? "InstanceId") <*> (x .:? "CreatedAt") <*>
                     (x .:? "ServiceErrorId")
                     <*> (x .:? "Type")
                     <*> (x .:? "StackId")
                     <*> (x .:? "Message"))

instance Hashable ServiceError' where

instance NFData ServiceError' where

-- | The Shutdown event configuration.
--
--
--
-- /See:/ 'shutdownEventConfiguration' smart constructor.
data ShutdownEventConfiguration = ShutdownEventConfiguration'
  { _secExecutionTimeout                :: !(Maybe Int)
  , _secDelayUntilElbConnectionsDrained :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ShutdownEventConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'secExecutionTimeout' - The time, in seconds, that AWS OpsWorks Stacks will wait after triggering a Shutdown event before shutting down an instance.
--
-- * 'secDelayUntilElbConnectionsDrained' - Whether to enable Elastic Load Balancing connection draining. For more information, see <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#conn-drain Connection Draining>
shutdownEventConfiguration
    :: ShutdownEventConfiguration
shutdownEventConfiguration =
  ShutdownEventConfiguration'
    { _secExecutionTimeout = Nothing
    , _secDelayUntilElbConnectionsDrained = Nothing
    }


-- | The time, in seconds, that AWS OpsWorks Stacks will wait after triggering a Shutdown event before shutting down an instance.
secExecutionTimeout :: Lens' ShutdownEventConfiguration (Maybe Int)
secExecutionTimeout = lens _secExecutionTimeout (\ s a -> s{_secExecutionTimeout = a})

-- | Whether to enable Elastic Load Balancing connection draining. For more information, see <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#conn-drain Connection Draining>
secDelayUntilElbConnectionsDrained :: Lens' ShutdownEventConfiguration (Maybe Bool)
secDelayUntilElbConnectionsDrained = lens _secDelayUntilElbConnectionsDrained (\ s a -> s{_secDelayUntilElbConnectionsDrained = a})

instance FromJSON ShutdownEventConfiguration where
        parseJSON
          = withObject "ShutdownEventConfiguration"
              (\ x ->
                 ShutdownEventConfiguration' <$>
                   (x .:? "ExecutionTimeout") <*>
                     (x .:? "DelayUntilElbConnectionsDrained"))

instance Hashable ShutdownEventConfiguration where

instance NFData ShutdownEventConfiguration where

instance ToJSON ShutdownEventConfiguration where
        toJSON ShutdownEventConfiguration'{..}
          = object
              (catMaybes
                 [("ExecutionTimeout" .=) <$> _secExecutionTimeout,
                  ("DelayUntilElbConnectionsDrained" .=) <$>
                    _secDelayUntilElbConnectionsDrained])

-- | Contains the information required to retrieve an app or cookbook from a repository. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html Creating Apps> or <http://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html Custom Recipes and Cookbooks> .
--
--
--
-- /See:/ 'source' smart constructor.
data Source = Source'
  { _sURL      :: !(Maybe Text)
  , _sUsername :: !(Maybe Text)
  , _sSSHKey   :: !(Maybe Text)
  , _sPassword :: !(Maybe Text)
  , _sType     :: !(Maybe SourceType)
  , _sRevision :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Source' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sURL' - The source URL. The following is an example of an Amazon S3 source URL: @https://s3.amazonaws.com/opsworks-demo-bucket/opsworks_cookbook_demo.tar.gz@ .
--
-- * 'sUsername' - This parameter depends on the repository type.     * For Amazon S3 bundles, set @Username@ to the appropriate IAM access key ID.     * For HTTP bundles, Git repositories, and Subversion repositories, set @Username@ to the user name.
--
-- * 'sSSHKey' - In requests, the repository's SSH key. In responses, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
--
-- * 'sPassword' - When included in a request, the parameter depends on the repository type.     * For Amazon S3 bundles, set @Password@ to the appropriate IAM secret access key.     * For HTTP bundles and Subversion repositories, set @Password@ to the password. For more information on how to safely handle IAM credentials, see <http://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html http://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html> . In responses, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
--
-- * 'sType' - The repository type.
--
-- * 'sRevision' - The application's version. AWS OpsWorks Stacks enables you to easily deploy new versions of an application. One of the simplest approaches is to have branches or revisions in your repository that represent different versions that can potentially be deployed.
source
    :: Source
source =
  Source'
    { _sURL = Nothing
    , _sUsername = Nothing
    , _sSSHKey = Nothing
    , _sPassword = Nothing
    , _sType = Nothing
    , _sRevision = Nothing
    }


-- | The source URL. The following is an example of an Amazon S3 source URL: @https://s3.amazonaws.com/opsworks-demo-bucket/opsworks_cookbook_demo.tar.gz@ .
sURL :: Lens' Source (Maybe Text)
sURL = lens _sURL (\ s a -> s{_sURL = a})

-- | This parameter depends on the repository type.     * For Amazon S3 bundles, set @Username@ to the appropriate IAM access key ID.     * For HTTP bundles, Git repositories, and Subversion repositories, set @Username@ to the user name.
sUsername :: Lens' Source (Maybe Text)
sUsername = lens _sUsername (\ s a -> s{_sUsername = a})

-- | In requests, the repository's SSH key. In responses, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
sSSHKey :: Lens' Source (Maybe Text)
sSSHKey = lens _sSSHKey (\ s a -> s{_sSSHKey = a})

-- | When included in a request, the parameter depends on the repository type.     * For Amazon S3 bundles, set @Password@ to the appropriate IAM secret access key.     * For HTTP bundles and Subversion repositories, set @Password@ to the password. For more information on how to safely handle IAM credentials, see <http://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html http://docs.aws.amazon.com/general/latest/gr/aws-access-keys-best-practices.html> . In responses, AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
sPassword :: Lens' Source (Maybe Text)
sPassword = lens _sPassword (\ s a -> s{_sPassword = a})

-- | The repository type.
sType :: Lens' Source (Maybe SourceType)
sType = lens _sType (\ s a -> s{_sType = a})

-- | The application's version. AWS OpsWorks Stacks enables you to easily deploy new versions of an application. One of the simplest approaches is to have branches or revisions in your repository that represent different versions that can potentially be deployed.
sRevision :: Lens' Source (Maybe Text)
sRevision = lens _sRevision (\ s a -> s{_sRevision = a})

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

instance Hashable Source where

instance NFData Source where

instance ToJSON Source where
        toJSON Source'{..}
          = object
              (catMaybes
                 [("Url" .=) <$> _sURL,
                  ("Username" .=) <$> _sUsername,
                  ("SshKey" .=) <$> _sSSHKey,
                  ("Password" .=) <$> _sPassword,
                  ("Type" .=) <$> _sType,
                  ("Revision" .=) <$> _sRevision])

-- | Describes a stack.
--
--
--
-- /See:/ 'stack' smart constructor.
data Stack = Stack'
  { _sDefaultInstanceProfileARN :: !(Maybe Text)
  , _sServiceRoleARN :: !(Maybe Text)
  , _sDefaultRootDeviceType :: !(Maybe RootDeviceType)
  , _sARN :: !(Maybe Text)
  , _sCreatedAt :: !(Maybe Text)
  , _sVPCId :: !(Maybe Text)
  , _sChefConfiguration :: !(Maybe ChefConfiguration)
  , _sAgentVersion :: !(Maybe Text)
  , _sDefaultSSHKeyName :: !(Maybe Text)
  , _sCustomJSON :: !(Maybe Text)
  , _sCustomCookbooksSource :: !(Maybe Source)
  , _sDefaultAvailabilityZone :: !(Maybe Text)
  , _sAttributes :: !(Maybe (Map StackAttributesKeys (Maybe Text)))
  , _sName :: !(Maybe Text)
  , _sDefaultOS :: !(Maybe Text)
  , _sUseOpsworksSecurityGroups :: !(Maybe Bool)
  , _sUseCustomCookbooks :: !(Maybe Bool)
  , _sDefaultSubnetId :: !(Maybe Text)
  , _sRegion :: !(Maybe Text)
  , _sConfigurationManager :: !(Maybe StackConfigurationManager)
  , _sStackId :: !(Maybe Text)
  , _sHostnameTheme :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Stack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sDefaultInstanceProfileARN' - The ARN of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
--
-- * 'sServiceRoleARN' - The stack AWS Identity and Access Management (IAM) role.
--
-- * 'sDefaultRootDeviceType' - The default root device type. This value is used by default for all instances in the stack, but you can override it when you create an instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
--
-- * 'sARN' - The stack's ARN.
--
-- * 'sCreatedAt' - The date when the stack was created.
--
-- * 'sVPCId' - The VPC ID; applicable only if the stack is running in a VPC.
--
-- * 'sChefConfiguration' - A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
--
-- * 'sAgentVersion' - The agent version. This parameter is set to @LATEST@ for auto-update. or a version number for a fixed agent version.
--
-- * 'sDefaultSSHKeyName' - A default Amazon EC2 key pair for the stack's instances. You can override this value when you create or update an instance.
--
-- * 'sCustomJSON' - A JSON object that contains user-defined attributes to be added to the stack configuration and deployment attributes. You can use custom JSON to override the corresponding default stack configuration attribute values or to pass data to recipes. The string should be in the following format: @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@  For more information on custom JSON, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
--
-- * 'sCustomCookbooksSource' - Undocumented member.
--
-- * 'sDefaultAvailabilityZone' - The stack's default Availability Zone. For more information, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- * 'sAttributes' - The stack's attributes.
--
-- * 'sName' - The stack name.
--
-- * 'sDefaultOS' - The stack's default operating system.
--
-- * 'sUseOpsworksSecurityGroups' - Whether the stack automatically associates the AWS OpsWorks Stacks built-in security groups with the stack's layers.
--
-- * 'sUseCustomCookbooks' - Whether the stack uses custom cookbooks.
--
-- * 'sDefaultSubnetId' - The default subnet ID; applicable only if the stack is running in a VPC.
--
-- * 'sRegion' - The stack AWS region, such as "ap-northeast-2". For more information about AWS regions, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- * 'sConfigurationManager' - The configuration manager.
--
-- * 'sStackId' - The stack ID.
--
-- * 'sHostnameTheme' - The stack host name theme, with spaces replaced by underscores.
stack
    :: Stack
stack =
  Stack'
    { _sDefaultInstanceProfileARN = Nothing
    , _sServiceRoleARN = Nothing
    , _sDefaultRootDeviceType = Nothing
    , _sARN = Nothing
    , _sCreatedAt = Nothing
    , _sVPCId = Nothing
    , _sChefConfiguration = Nothing
    , _sAgentVersion = Nothing
    , _sDefaultSSHKeyName = Nothing
    , _sCustomJSON = Nothing
    , _sCustomCookbooksSource = Nothing
    , _sDefaultAvailabilityZone = Nothing
    , _sAttributes = Nothing
    , _sName = Nothing
    , _sDefaultOS = Nothing
    , _sUseOpsworksSecurityGroups = Nothing
    , _sUseCustomCookbooks = Nothing
    , _sDefaultSubnetId = Nothing
    , _sRegion = Nothing
    , _sConfigurationManager = Nothing
    , _sStackId = Nothing
    , _sHostnameTheme = Nothing
    }


-- | The ARN of an IAM profile that is the default profile for all of the stack's EC2 instances. For more information about IAM ARNs, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html Using Identifiers> .
sDefaultInstanceProfileARN :: Lens' Stack (Maybe Text)
sDefaultInstanceProfileARN = lens _sDefaultInstanceProfileARN (\ s a -> s{_sDefaultInstanceProfileARN = a})

-- | The stack AWS Identity and Access Management (IAM) role.
sServiceRoleARN :: Lens' Stack (Maybe Text)
sServiceRoleARN = lens _sServiceRoleARN (\ s a -> s{_sServiceRoleARN = a})

-- | The default root device type. This value is used by default for all instances in the stack, but you can override it when you create an instance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ComponentsAMIs.html#storage-for-the-root-device Storage for the Root Device> .
sDefaultRootDeviceType :: Lens' Stack (Maybe RootDeviceType)
sDefaultRootDeviceType = lens _sDefaultRootDeviceType (\ s a -> s{_sDefaultRootDeviceType = a})

-- | The stack's ARN.
sARN :: Lens' Stack (Maybe Text)
sARN = lens _sARN (\ s a -> s{_sARN = a})

-- | The date when the stack was created.
sCreatedAt :: Lens' Stack (Maybe Text)
sCreatedAt = lens _sCreatedAt (\ s a -> s{_sCreatedAt = a})

-- | The VPC ID; applicable only if the stack is running in a VPC.
sVPCId :: Lens' Stack (Maybe Text)
sVPCId = lens _sVPCId (\ s a -> s{_sVPCId = a})

-- | A @ChefConfiguration@ object that specifies whether to enable Berkshelf and the Berkshelf version. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-creating.html Create a New Stack> .
sChefConfiguration :: Lens' Stack (Maybe ChefConfiguration)
sChefConfiguration = lens _sChefConfiguration (\ s a -> s{_sChefConfiguration = a})

-- | The agent version. This parameter is set to @LATEST@ for auto-update. or a version number for a fixed agent version.
sAgentVersion :: Lens' Stack (Maybe Text)
sAgentVersion = lens _sAgentVersion (\ s a -> s{_sAgentVersion = a})

-- | A default Amazon EC2 key pair for the stack's instances. You can override this value when you create or update an instance.
sDefaultSSHKeyName :: Lens' Stack (Maybe Text)
sDefaultSSHKeyName = lens _sDefaultSSHKeyName (\ s a -> s{_sDefaultSSHKeyName = a})

-- | A JSON object that contains user-defined attributes to be added to the stack configuration and deployment attributes. You can use custom JSON to override the corresponding default stack configuration attribute values or to pass data to recipes. The string should be in the following format: @"{\"key1\": \"value1\", \"key2\": \"value2\",...}"@  For more information on custom JSON, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-json.html Use Custom JSON to Modify the Stack Configuration Attributes> .
sCustomJSON :: Lens' Stack (Maybe Text)
sCustomJSON = lens _sCustomJSON (\ s a -> s{_sCustomJSON = a})

-- | Undocumented member.
sCustomCookbooksSource :: Lens' Stack (Maybe Source)
sCustomCookbooksSource = lens _sCustomCookbooksSource (\ s a -> s{_sCustomCookbooksSource = a})

-- | The stack's default Availability Zone. For more information, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
sDefaultAvailabilityZone :: Lens' Stack (Maybe Text)
sDefaultAvailabilityZone = lens _sDefaultAvailabilityZone (\ s a -> s{_sDefaultAvailabilityZone = a})

-- | The stack's attributes.
sAttributes :: Lens' Stack (HashMap StackAttributesKeys (Maybe Text))
sAttributes = lens _sAttributes (\ s a -> s{_sAttributes = a}) . _Default . _Map

-- | The stack name.
sName :: Lens' Stack (Maybe Text)
sName = lens _sName (\ s a -> s{_sName = a})

-- | The stack's default operating system.
sDefaultOS :: Lens' Stack (Maybe Text)
sDefaultOS = lens _sDefaultOS (\ s a -> s{_sDefaultOS = a})

-- | Whether the stack automatically associates the AWS OpsWorks Stacks built-in security groups with the stack's layers.
sUseOpsworksSecurityGroups :: Lens' Stack (Maybe Bool)
sUseOpsworksSecurityGroups = lens _sUseOpsworksSecurityGroups (\ s a -> s{_sUseOpsworksSecurityGroups = a})

-- | Whether the stack uses custom cookbooks.
sUseCustomCookbooks :: Lens' Stack (Maybe Bool)
sUseCustomCookbooks = lens _sUseCustomCookbooks (\ s a -> s{_sUseCustomCookbooks = a})

-- | The default subnet ID; applicable only if the stack is running in a VPC.
sDefaultSubnetId :: Lens' Stack (Maybe Text)
sDefaultSubnetId = lens _sDefaultSubnetId (\ s a -> s{_sDefaultSubnetId = a})

-- | The stack AWS region, such as "ap-northeast-2". For more information about AWS regions, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
sRegion :: Lens' Stack (Maybe Text)
sRegion = lens _sRegion (\ s a -> s{_sRegion = a})

-- | The configuration manager.
sConfigurationManager :: Lens' Stack (Maybe StackConfigurationManager)
sConfigurationManager = lens _sConfigurationManager (\ s a -> s{_sConfigurationManager = a})

-- | The stack ID.
sStackId :: Lens' Stack (Maybe Text)
sStackId = lens _sStackId (\ s a -> s{_sStackId = a})

-- | The stack host name theme, with spaces replaced by underscores.
sHostnameTheme :: Lens' Stack (Maybe Text)
sHostnameTheme = lens _sHostnameTheme (\ s a -> s{_sHostnameTheme = a})

instance FromJSON Stack where
        parseJSON
          = withObject "Stack"
              (\ x ->
                 Stack' <$>
                   (x .:? "DefaultInstanceProfileArn") <*>
                     (x .:? "ServiceRoleArn")
                     <*> (x .:? "DefaultRootDeviceType")
                     <*> (x .:? "Arn")
                     <*> (x .:? "CreatedAt")
                     <*> (x .:? "VpcId")
                     <*> (x .:? "ChefConfiguration")
                     <*> (x .:? "AgentVersion")
                     <*> (x .:? "DefaultSshKeyName")
                     <*> (x .:? "CustomJson")
                     <*> (x .:? "CustomCookbooksSource")
                     <*> (x .:? "DefaultAvailabilityZone")
                     <*> (x .:? "Attributes" .!= mempty)
                     <*> (x .:? "Name")
                     <*> (x .:? "DefaultOs")
                     <*> (x .:? "UseOpsworksSecurityGroups")
                     <*> (x .:? "UseCustomCookbooks")
                     <*> (x .:? "DefaultSubnetId")
                     <*> (x .:? "Region")
                     <*> (x .:? "ConfigurationManager")
                     <*> (x .:? "StackId")
                     <*> (x .:? "HostnameTheme"))

instance Hashable Stack where

instance NFData Stack where

-- | Describes the configuration manager.
--
--
--
-- /See:/ 'stackConfigurationManager' smart constructor.
data StackConfigurationManager = StackConfigurationManager'
  { _scmName    :: !(Maybe Text)
  , _scmVersion :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackConfigurationManager' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scmName' - The name. This parameter must be set to "Chef".
--
-- * 'scmVersion' - The Chef version. This parameter must be set to 12, 11.10, or 11.4 for Linux stacks, and to 12.2 for Windows stacks. The default value for Linux stacks is 11.4.
stackConfigurationManager
    :: StackConfigurationManager
stackConfigurationManager =
  StackConfigurationManager' {_scmName = Nothing, _scmVersion = Nothing}


-- | The name. This parameter must be set to "Chef".
scmName :: Lens' StackConfigurationManager (Maybe Text)
scmName = lens _scmName (\ s a -> s{_scmName = a})

-- | The Chef version. This parameter must be set to 12, 11.10, or 11.4 for Linux stacks, and to 12.2 for Windows stacks. The default value for Linux stacks is 11.4.
scmVersion :: Lens' StackConfigurationManager (Maybe Text)
scmVersion = lens _scmVersion (\ s a -> s{_scmVersion = a})

instance FromJSON StackConfigurationManager where
        parseJSON
          = withObject "StackConfigurationManager"
              (\ x ->
                 StackConfigurationManager' <$>
                   (x .:? "Name") <*> (x .:? "Version"))

instance Hashable StackConfigurationManager where

instance NFData StackConfigurationManager where

instance ToJSON StackConfigurationManager where
        toJSON StackConfigurationManager'{..}
          = object
              (catMaybes
                 [("Name" .=) <$> _scmName,
                  ("Version" .=) <$> _scmVersion])

-- | Summarizes the number of layers, instances, and apps in a stack.
--
--
--
-- /See:/ 'stackSummary' smart constructor.
data StackSummary = StackSummary'
  { _ssARN            :: !(Maybe Text)
  , _ssAppsCount      :: !(Maybe Int)
  , _ssName           :: !(Maybe Text)
  , _ssStackId        :: !(Maybe Text)
  , _ssLayersCount    :: !(Maybe Int)
  , _ssInstancesCount :: !(Maybe InstancesCount)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StackSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssARN' - The stack's ARN.
--
-- * 'ssAppsCount' - The number of apps.
--
-- * 'ssName' - The stack name.
--
-- * 'ssStackId' - The stack ID.
--
-- * 'ssLayersCount' - The number of layers.
--
-- * 'ssInstancesCount' - An @InstancesCount@ object with the number of instances in each status.
stackSummary
    :: StackSummary
stackSummary =
  StackSummary'
    { _ssARN = Nothing
    , _ssAppsCount = Nothing
    , _ssName = Nothing
    , _ssStackId = Nothing
    , _ssLayersCount = Nothing
    , _ssInstancesCount = Nothing
    }


-- | The stack's ARN.
ssARN :: Lens' StackSummary (Maybe Text)
ssARN = lens _ssARN (\ s a -> s{_ssARN = a})

-- | The number of apps.
ssAppsCount :: Lens' StackSummary (Maybe Int)
ssAppsCount = lens _ssAppsCount (\ s a -> s{_ssAppsCount = a})

-- | The stack name.
ssName :: Lens' StackSummary (Maybe Text)
ssName = lens _ssName (\ s a -> s{_ssName = a})

-- | The stack ID.
ssStackId :: Lens' StackSummary (Maybe Text)
ssStackId = lens _ssStackId (\ s a -> s{_ssStackId = a})

-- | The number of layers.
ssLayersCount :: Lens' StackSummary (Maybe Int)
ssLayersCount = lens _ssLayersCount (\ s a -> s{_ssLayersCount = a})

-- | An @InstancesCount@ object with the number of instances in each status.
ssInstancesCount :: Lens' StackSummary (Maybe InstancesCount)
ssInstancesCount = lens _ssInstancesCount (\ s a -> s{_ssInstancesCount = a})

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

instance Hashable StackSummary where

instance NFData StackSummary where

-- | Contains the data needed by RDP clients such as the Microsoft Remote Desktop Connection to log in to the instance.
--
--
--
-- /See:/ 'temporaryCredential' smart constructor.
data TemporaryCredential = TemporaryCredential'
  { _tcInstanceId        :: !(Maybe Text)
  , _tcUsername          :: !(Maybe Text)
  , _tcPassword          :: !(Maybe Text)
  , _tcValidForInMinutes :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TemporaryCredential' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcInstanceId' - The instance's AWS OpsWorks Stacks ID.
--
-- * 'tcUsername' - The user name.
--
-- * 'tcPassword' - The password.
--
-- * 'tcValidForInMinutes' - The length of time (in minutes) that the grant is valid. When the grant expires, at the end of this period, the user will no longer be able to use the credentials to log in. If they are logged in at the time, they will be automatically logged out.
temporaryCredential
    :: TemporaryCredential
temporaryCredential =
  TemporaryCredential'
    { _tcInstanceId = Nothing
    , _tcUsername = Nothing
    , _tcPassword = Nothing
    , _tcValidForInMinutes = Nothing
    }


-- | The instance's AWS OpsWorks Stacks ID.
tcInstanceId :: Lens' TemporaryCredential (Maybe Text)
tcInstanceId = lens _tcInstanceId (\ s a -> s{_tcInstanceId = a})

-- | The user name.
tcUsername :: Lens' TemporaryCredential (Maybe Text)
tcUsername = lens _tcUsername (\ s a -> s{_tcUsername = a})

-- | The password.
tcPassword :: Lens' TemporaryCredential (Maybe Text)
tcPassword = lens _tcPassword (\ s a -> s{_tcPassword = a})

-- | The length of time (in minutes) that the grant is valid. When the grant expires, at the end of this period, the user will no longer be able to use the credentials to log in. If they are logged in at the time, they will be automatically logged out.
tcValidForInMinutes :: Lens' TemporaryCredential (Maybe Int)
tcValidForInMinutes = lens _tcValidForInMinutes (\ s a -> s{_tcValidForInMinutes = a})

instance FromJSON TemporaryCredential where
        parseJSON
          = withObject "TemporaryCredential"
              (\ x ->
                 TemporaryCredential' <$>
                   (x .:? "InstanceId") <*> (x .:? "Username") <*>
                     (x .:? "Password")
                     <*> (x .:? "ValidForInMinutes"))

instance Hashable TemporaryCredential where

instance NFData TemporaryCredential where

-- | Describes an instance's time-based auto scaling configuration.
--
--
--
-- /See:/ 'timeBasedAutoScalingConfiguration' smart constructor.
data TimeBasedAutoScalingConfiguration = TimeBasedAutoScalingConfiguration'
  { _tbascInstanceId          :: !(Maybe Text)
  , _tbascAutoScalingSchedule :: !(Maybe WeeklyAutoScalingSchedule)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TimeBasedAutoScalingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tbascInstanceId' - The instance ID.
--
-- * 'tbascAutoScalingSchedule' - A @WeeklyAutoScalingSchedule@ object with the instance schedule.
timeBasedAutoScalingConfiguration
    :: TimeBasedAutoScalingConfiguration
timeBasedAutoScalingConfiguration =
  TimeBasedAutoScalingConfiguration'
    {_tbascInstanceId = Nothing, _tbascAutoScalingSchedule = Nothing}


-- | The instance ID.
tbascInstanceId :: Lens' TimeBasedAutoScalingConfiguration (Maybe Text)
tbascInstanceId = lens _tbascInstanceId (\ s a -> s{_tbascInstanceId = a})

-- | A @WeeklyAutoScalingSchedule@ object with the instance schedule.
tbascAutoScalingSchedule :: Lens' TimeBasedAutoScalingConfiguration (Maybe WeeklyAutoScalingSchedule)
tbascAutoScalingSchedule = lens _tbascAutoScalingSchedule (\ s a -> s{_tbascAutoScalingSchedule = a})

instance FromJSON TimeBasedAutoScalingConfiguration
         where
        parseJSON
          = withObject "TimeBasedAutoScalingConfiguration"
              (\ x ->
                 TimeBasedAutoScalingConfiguration' <$>
                   (x .:? "InstanceId") <*>
                     (x .:? "AutoScalingSchedule"))

instance Hashable TimeBasedAutoScalingConfiguration
         where

instance NFData TimeBasedAutoScalingConfiguration
         where

-- | Describes a user's SSH information.
--
--
--
-- /See:/ 'userProfile' smart constructor.
data UserProfile = UserProfile'
  { _upAllowSelfManagement :: !(Maybe Bool)
  , _upSSHPublicKey        :: !(Maybe Text)
  , _upSSHUsername         :: !(Maybe Text)
  , _upIAMUserARN          :: !(Maybe Text)
  , _upName                :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upAllowSelfManagement' - Whether users can specify their own SSH public key through the My Settings page. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions> .
--
-- * 'upSSHPublicKey' - The user's SSH public key.
--
-- * 'upSSHUsername' - The user's SSH user name.
--
-- * 'upIAMUserARN' - The user's IAM ARN.
--
-- * 'upName' - The user's name.
userProfile
    :: UserProfile
userProfile =
  UserProfile'
    { _upAllowSelfManagement = Nothing
    , _upSSHPublicKey = Nothing
    , _upSSHUsername = Nothing
    , _upIAMUserARN = Nothing
    , _upName = Nothing
    }


-- | Whether users can specify their own SSH public key through the My Settings page. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions> .
upAllowSelfManagement :: Lens' UserProfile (Maybe Bool)
upAllowSelfManagement = lens _upAllowSelfManagement (\ s a -> s{_upAllowSelfManagement = a})

-- | The user's SSH public key.
upSSHPublicKey :: Lens' UserProfile (Maybe Text)
upSSHPublicKey = lens _upSSHPublicKey (\ s a -> s{_upSSHPublicKey = a})

-- | The user's SSH user name.
upSSHUsername :: Lens' UserProfile (Maybe Text)
upSSHUsername = lens _upSSHUsername (\ s a -> s{_upSSHUsername = a})

-- | The user's IAM ARN.
upIAMUserARN :: Lens' UserProfile (Maybe Text)
upIAMUserARN = lens _upIAMUserARN (\ s a -> s{_upIAMUserARN = a})

-- | The user's name.
upName :: Lens' UserProfile (Maybe Text)
upName = lens _upName (\ s a -> s{_upName = a})

instance FromJSON UserProfile where
        parseJSON
          = withObject "UserProfile"
              (\ x ->
                 UserProfile' <$>
                   (x .:? "AllowSelfManagement") <*>
                     (x .:? "SshPublicKey")
                     <*> (x .:? "SshUsername")
                     <*> (x .:? "IamUserArn")
                     <*> (x .:? "Name"))

instance Hashable UserProfile where

instance NFData UserProfile where

-- | Describes an instance's Amazon EBS volume.
--
--
--
-- /See:/ 'volume' smart constructor.
data Volume = Volume'
  { _vInstanceId       :: !(Maybe Text)
  , _vStatus           :: !(Maybe Text)
  , _vSize             :: !(Maybe Int)
  , _vIOPS             :: !(Maybe Int)
  , _vDevice           :: !(Maybe Text)
  , _vEncrypted        :: !(Maybe Bool)
  , _vAvailabilityZone :: !(Maybe Text)
  , _vName             :: !(Maybe Text)
  , _vRAIdArrayId      :: !(Maybe Text)
  , _vVolumeId         :: !(Maybe Text)
  , _vRegion           :: !(Maybe Text)
  , _vVolumeType       :: !(Maybe Text)
  , _vEC2VolumeId      :: !(Maybe Text)
  , _vMountPoint       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Volume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vInstanceId' - The instance ID.
--
-- * 'vStatus' - The value returned by <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumes.html DescribeVolumes> .
--
-- * 'vSize' - The volume size.
--
-- * 'vIOPS' - For PIOPS volumes, the IOPS per disk.
--
-- * 'vDevice' - The device name.
--
-- * 'vEncrypted' - Undocumented member.
--
-- * 'vAvailabilityZone' - The volume Availability Zone. For more information, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- * 'vName' - The volume name.
--
-- * 'vRAIdArrayId' - The RAID array ID.
--
-- * 'vVolumeId' - The volume ID.
--
-- * 'vRegion' - The AWS region. For more information about AWS regions, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- * 'vVolumeType' - The volume type, standard or PIOPS.
--
-- * 'vEC2VolumeId' - The Amazon EC2 volume ID.
--
-- * 'vMountPoint' - The volume mount point. For example, "/mnt/disk1".
volume
    :: Volume
volume =
  Volume'
    { _vInstanceId = Nothing
    , _vStatus = Nothing
    , _vSize = Nothing
    , _vIOPS = Nothing
    , _vDevice = Nothing
    , _vEncrypted = Nothing
    , _vAvailabilityZone = Nothing
    , _vName = Nothing
    , _vRAIdArrayId = Nothing
    , _vVolumeId = Nothing
    , _vRegion = Nothing
    , _vVolumeType = Nothing
    , _vEC2VolumeId = Nothing
    , _vMountPoint = Nothing
    }


-- | The instance ID.
vInstanceId :: Lens' Volume (Maybe Text)
vInstanceId = lens _vInstanceId (\ s a -> s{_vInstanceId = a})

-- | The value returned by <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVolumes.html DescribeVolumes> .
vStatus :: Lens' Volume (Maybe Text)
vStatus = lens _vStatus (\ s a -> s{_vStatus = a})

-- | The volume size.
vSize :: Lens' Volume (Maybe Int)
vSize = lens _vSize (\ s a -> s{_vSize = a})

-- | For PIOPS volumes, the IOPS per disk.
vIOPS :: Lens' Volume (Maybe Int)
vIOPS = lens _vIOPS (\ s a -> s{_vIOPS = a})

-- | The device name.
vDevice :: Lens' Volume (Maybe Text)
vDevice = lens _vDevice (\ s a -> s{_vDevice = a})

-- | Undocumented member.
vEncrypted :: Lens' Volume (Maybe Bool)
vEncrypted = lens _vEncrypted (\ s a -> s{_vEncrypted = a})

-- | The volume Availability Zone. For more information, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
vAvailabilityZone :: Lens' Volume (Maybe Text)
vAvailabilityZone = lens _vAvailabilityZone (\ s a -> s{_vAvailabilityZone = a})

-- | The volume name.
vName :: Lens' Volume (Maybe Text)
vName = lens _vName (\ s a -> s{_vName = a})

-- | The RAID array ID.
vRAIdArrayId :: Lens' Volume (Maybe Text)
vRAIdArrayId = lens _vRAIdArrayId (\ s a -> s{_vRAIdArrayId = a})

-- | The volume ID.
vVolumeId :: Lens' Volume (Maybe Text)
vVolumeId = lens _vVolumeId (\ s a -> s{_vVolumeId = a})

-- | The AWS region. For more information about AWS regions, see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
vRegion :: Lens' Volume (Maybe Text)
vRegion = lens _vRegion (\ s a -> s{_vRegion = a})

-- | The volume type, standard or PIOPS.
vVolumeType :: Lens' Volume (Maybe Text)
vVolumeType = lens _vVolumeType (\ s a -> s{_vVolumeType = a})

-- | The Amazon EC2 volume ID.
vEC2VolumeId :: Lens' Volume (Maybe Text)
vEC2VolumeId = lens _vEC2VolumeId (\ s a -> s{_vEC2VolumeId = a})

-- | The volume mount point. For example, "/mnt/disk1".
vMountPoint :: Lens' Volume (Maybe Text)
vMountPoint = lens _vMountPoint (\ s a -> s{_vMountPoint = a})

instance FromJSON Volume where
        parseJSON
          = withObject "Volume"
              (\ x ->
                 Volume' <$>
                   (x .:? "InstanceId") <*> (x .:? "Status") <*>
                     (x .:? "Size")
                     <*> (x .:? "Iops")
                     <*> (x .:? "Device")
                     <*> (x .:? "Encrypted")
                     <*> (x .:? "AvailabilityZone")
                     <*> (x .:? "Name")
                     <*> (x .:? "RaidArrayId")
                     <*> (x .:? "VolumeId")
                     <*> (x .:? "Region")
                     <*> (x .:? "VolumeType")
                     <*> (x .:? "Ec2VolumeId")
                     <*> (x .:? "MountPoint"))

instance Hashable Volume where

instance NFData Volume where

-- | Describes an Amazon EBS volume configuration.
--
--
--
-- /See:/ 'volumeConfiguration' smart constructor.
data VolumeConfiguration = VolumeConfiguration'
  { _vcIOPS          :: !(Maybe Int)
  , _vcRAIdLevel     :: !(Maybe Int)
  , _vcEncrypted     :: !(Maybe Bool)
  , _vcVolumeType    :: !(Maybe Text)
  , _vcMountPoint    :: !Text
  , _vcNumberOfDisks :: !Int
  , _vcSize          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VolumeConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcIOPS' - For PIOPS volumes, the IOPS per disk.
--
-- * 'vcRAIdLevel' - The volume <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level> .
--
-- * 'vcEncrypted' - Specifies whether an Amazon EBS volume is encrypted. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> .
--
-- * 'vcVolumeType' - The volume type. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> .     * @standard@ - Magnetic     * @io1@ - Provisioned IOPS (SSD)     * @gp2@ - General Purpose (SSD)     * @st1@ - Throughput Optimized hard disk drive (HDD)     * @sc1@ - Cold HDD
--
-- * 'vcMountPoint' - The volume mount point. For example "/dev/sdh".
--
-- * 'vcNumberOfDisks' - The number of disks in the volume.
--
-- * 'vcSize' - The volume size.
volumeConfiguration
    :: Text -- ^ 'vcMountPoint'
    -> Int -- ^ 'vcNumberOfDisks'
    -> Int -- ^ 'vcSize'
    -> VolumeConfiguration
volumeConfiguration pMountPoint_ pNumberOfDisks_ pSize_ =
  VolumeConfiguration'
    { _vcIOPS = Nothing
    , _vcRAIdLevel = Nothing
    , _vcEncrypted = Nothing
    , _vcVolumeType = Nothing
    , _vcMountPoint = pMountPoint_
    , _vcNumberOfDisks = pNumberOfDisks_
    , _vcSize = pSize_
    }


-- | For PIOPS volumes, the IOPS per disk.
vcIOPS :: Lens' VolumeConfiguration (Maybe Int)
vcIOPS = lens _vcIOPS (\ s a -> s{_vcIOPS = a})

-- | The volume <http://en.wikipedia.org/wiki/Standard_RAID_levels RAID level> .
vcRAIdLevel :: Lens' VolumeConfiguration (Maybe Int)
vcRAIdLevel = lens _vcRAIdLevel (\ s a -> s{_vcRAIdLevel = a})

-- | Specifies whether an Amazon EBS volume is encrypted. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> .
vcEncrypted :: Lens' VolumeConfiguration (Maybe Bool)
vcEncrypted = lens _vcEncrypted (\ s a -> s{_vcEncrypted = a})

-- | The volume type. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> .     * @standard@ - Magnetic     * @io1@ - Provisioned IOPS (SSD)     * @gp2@ - General Purpose (SSD)     * @st1@ - Throughput Optimized hard disk drive (HDD)     * @sc1@ - Cold HDD
vcVolumeType :: Lens' VolumeConfiguration (Maybe Text)
vcVolumeType = lens _vcVolumeType (\ s a -> s{_vcVolumeType = a})

-- | The volume mount point. For example "/dev/sdh".
vcMountPoint :: Lens' VolumeConfiguration Text
vcMountPoint = lens _vcMountPoint (\ s a -> s{_vcMountPoint = a})

-- | The number of disks in the volume.
vcNumberOfDisks :: Lens' VolumeConfiguration Int
vcNumberOfDisks = lens _vcNumberOfDisks (\ s a -> s{_vcNumberOfDisks = a})

-- | The volume size.
vcSize :: Lens' VolumeConfiguration Int
vcSize = lens _vcSize (\ s a -> s{_vcSize = a})

instance FromJSON VolumeConfiguration where
        parseJSON
          = withObject "VolumeConfiguration"
              (\ x ->
                 VolumeConfiguration' <$>
                   (x .:? "Iops") <*> (x .:? "RaidLevel") <*>
                     (x .:? "Encrypted")
                     <*> (x .:? "VolumeType")
                     <*> (x .: "MountPoint")
                     <*> (x .: "NumberOfDisks")
                     <*> (x .: "Size"))

instance Hashable VolumeConfiguration where

instance NFData VolumeConfiguration where

instance ToJSON VolumeConfiguration where
        toJSON VolumeConfiguration'{..}
          = object
              (catMaybes
                 [("Iops" .=) <$> _vcIOPS,
                  ("RaidLevel" .=) <$> _vcRAIdLevel,
                  ("Encrypted" .=) <$> _vcEncrypted,
                  ("VolumeType" .=) <$> _vcVolumeType,
                  Just ("MountPoint" .= _vcMountPoint),
                  Just ("NumberOfDisks" .= _vcNumberOfDisks),
                  Just ("Size" .= _vcSize)])

-- | Describes a time-based instance's auto scaling schedule. The schedule consists of a set of key-value pairs.
--
--
--     * The key is the time period (a UTC hour) and must be an integer from 0 - 23.
--
--     * The value indicates whether the instance should be online or offline for the specified period, and must be set to "on" or "off"
--
--
--
-- The default setting for all time periods is off, so you use the following parameters primarily to specify the online periods. You don't have to explicitly specify offline periods unless you want to change an online period to an offline period.
--
-- The following example specifies that the instance should be online for four hours, from UTC 1200 - 1600. It will be off for the remainder of the day.
--
-- @{ "12":"on", "13":"on", "14":"on", "15":"on" } @
--
--
-- /See:/ 'weeklyAutoScalingSchedule' smart constructor.
data WeeklyAutoScalingSchedule = WeeklyAutoScalingSchedule'
  { _wassThursday  :: !(Maybe (Map Text Text))
  , _wassWednesday :: !(Maybe (Map Text Text))
  , _wassSaturday  :: !(Maybe (Map Text Text))
  , _wassMonday    :: !(Maybe (Map Text Text))
  , _wassFriday    :: !(Maybe (Map Text Text))
  , _wassSunday    :: !(Maybe (Map Text Text))
  , _wassTuesday   :: !(Maybe (Map Text Text))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'WeeklyAutoScalingSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wassThursday' - The schedule for Thursday.
--
-- * 'wassWednesday' - The schedule for Wednesday.
--
-- * 'wassSaturday' - The schedule for Saturday.
--
-- * 'wassMonday' - The schedule for Monday.
--
-- * 'wassFriday' - The schedule for Friday.
--
-- * 'wassSunday' - The schedule for Sunday.
--
-- * 'wassTuesday' - The schedule for Tuesday.
weeklyAutoScalingSchedule
    :: WeeklyAutoScalingSchedule
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
wassThursday = lens _wassThursday (\ s a -> s{_wassThursday = a}) . _Default . _Map

-- | The schedule for Wednesday.
wassWednesday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassWednesday = lens _wassWednesday (\ s a -> s{_wassWednesday = a}) . _Default . _Map

-- | The schedule for Saturday.
wassSaturday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassSaturday = lens _wassSaturday (\ s a -> s{_wassSaturday = a}) . _Default . _Map

-- | The schedule for Monday.
wassMonday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassMonday = lens _wassMonday (\ s a -> s{_wassMonday = a}) . _Default . _Map

-- | The schedule for Friday.
wassFriday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassFriday = lens _wassFriday (\ s a -> s{_wassFriday = a}) . _Default . _Map

-- | The schedule for Sunday.
wassSunday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassSunday = lens _wassSunday (\ s a -> s{_wassSunday = a}) . _Default . _Map

-- | The schedule for Tuesday.
wassTuesday :: Lens' WeeklyAutoScalingSchedule (HashMap Text Text)
wassTuesday = lens _wassTuesday (\ s a -> s{_wassTuesday = a}) . _Default . _Map

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

instance Hashable WeeklyAutoScalingSchedule where

instance NFData WeeklyAutoScalingSchedule where

instance ToJSON WeeklyAutoScalingSchedule where
        toJSON WeeklyAutoScalingSchedule'{..}
          = object
              (catMaybes
                 [("Thursday" .=) <$> _wassThursday,
                  ("Wednesday" .=) <$> _wassWednesday,
                  ("Saturday" .=) <$> _wassSaturday,
                  ("Monday" .=) <$> _wassMonday,
                  ("Friday" .=) <$> _wassFriday,
                  ("Sunday" .=) <$> _wassSunday,
                  ("Tuesday" .=) <$> _wassTuesday])
