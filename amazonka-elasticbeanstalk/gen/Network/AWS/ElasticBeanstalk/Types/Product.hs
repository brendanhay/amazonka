{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.Product where

import Network.AWS.ElasticBeanstalk.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the properties of an application.
--
--
--
-- /See:/ 'applicationDescription' smart constructor.
data ApplicationDescription = ApplicationDescription'
  { _adApplicationARN          :: !(Maybe Text)
  , _adVersions                :: !(Maybe [Text])
  , _adDateUpdated             :: !(Maybe ISO8601)
  , _adDateCreated             :: !(Maybe ISO8601)
  , _adApplicationName         :: !(Maybe Text)
  , _adConfigurationTemplates  :: !(Maybe [Text])
  , _adResourceLifecycleConfig :: !(Maybe ApplicationResourceLifecycleConfig)
  , _adDescription             :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adApplicationARN' - The Amazon Resource Name (ARN) of the application.
--
-- * 'adVersions' - The names of the versions for this application.
--
-- * 'adDateUpdated' - The date when the application was last modified.
--
-- * 'adDateCreated' - The date when the application was created.
--
-- * 'adApplicationName' - The name of the application.
--
-- * 'adConfigurationTemplates' - The names of the configuration templates associated with this application.
--
-- * 'adResourceLifecycleConfig' - The lifecycle settings for the application.
--
-- * 'adDescription' - User-defined description of the application.
applicationDescription
    :: ApplicationDescription
applicationDescription =
  ApplicationDescription'
    { _adApplicationARN = Nothing
    , _adVersions = Nothing
    , _adDateUpdated = Nothing
    , _adDateCreated = Nothing
    , _adApplicationName = Nothing
    , _adConfigurationTemplates = Nothing
    , _adResourceLifecycleConfig = Nothing
    , _adDescription = Nothing
    }


-- | The Amazon Resource Name (ARN) of the application.
adApplicationARN :: Lens' ApplicationDescription (Maybe Text)
adApplicationARN = lens _adApplicationARN (\ s a -> s{_adApplicationARN = a})

-- | The names of the versions for this application.
adVersions :: Lens' ApplicationDescription [Text]
adVersions = lens _adVersions (\ s a -> s{_adVersions = a}) . _Default . _Coerce

-- | The date when the application was last modified.
adDateUpdated :: Lens' ApplicationDescription (Maybe UTCTime)
adDateUpdated = lens _adDateUpdated (\ s a -> s{_adDateUpdated = a}) . mapping _Time

-- | The date when the application was created.
adDateCreated :: Lens' ApplicationDescription (Maybe UTCTime)
adDateCreated = lens _adDateCreated (\ s a -> s{_adDateCreated = a}) . mapping _Time

-- | The name of the application.
adApplicationName :: Lens' ApplicationDescription (Maybe Text)
adApplicationName = lens _adApplicationName (\ s a -> s{_adApplicationName = a})

-- | The names of the configuration templates associated with this application.
adConfigurationTemplates :: Lens' ApplicationDescription [Text]
adConfigurationTemplates = lens _adConfigurationTemplates (\ s a -> s{_adConfigurationTemplates = a}) . _Default . _Coerce

-- | The lifecycle settings for the application.
adResourceLifecycleConfig :: Lens' ApplicationDescription (Maybe ApplicationResourceLifecycleConfig)
adResourceLifecycleConfig = lens _adResourceLifecycleConfig (\ s a -> s{_adResourceLifecycleConfig = a})

-- | User-defined description of the application.
adDescription :: Lens' ApplicationDescription (Maybe Text)
adDescription = lens _adDescription (\ s a -> s{_adDescription = a})

instance FromXML ApplicationDescription where
        parseXML x
          = ApplicationDescription' <$>
              (x .@? "ApplicationArn") <*>
                (x .@? "Versions" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "DateUpdated")
                <*> (x .@? "DateCreated")
                <*> (x .@? "ApplicationName")
                <*>
                (x .@? "ConfigurationTemplates" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "ResourceLifecycleConfig")
                <*> (x .@? "Description")

instance Hashable ApplicationDescription where

instance NFData ApplicationDescription where

-- | Result message containing a single description of an application.
--
--
--
-- /See:/ 'applicationDescriptionMessage' smart constructor.
newtype ApplicationDescriptionMessage = ApplicationDescriptionMessage'
  { _admApplication :: Maybe ApplicationDescription
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationDescriptionMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'admApplication' - The 'ApplicationDescription' of the application.
applicationDescriptionMessage
    :: ApplicationDescriptionMessage
applicationDescriptionMessage =
  ApplicationDescriptionMessage' {_admApplication = Nothing}


-- | The 'ApplicationDescription' of the application.
admApplication :: Lens' ApplicationDescriptionMessage (Maybe ApplicationDescription)
admApplication = lens _admApplication (\ s a -> s{_admApplication = a})

instance FromXML ApplicationDescriptionMessage where
        parseXML x
          = ApplicationDescriptionMessage' <$>
              (x .@? "Application")

instance Hashable ApplicationDescriptionMessage where

instance NFData ApplicationDescriptionMessage where

-- | Application request metrics for an AWS Elastic Beanstalk environment.
--
--
--
-- /See:/ 'applicationMetrics' smart constructor.
data ApplicationMetrics = ApplicationMetrics'
  { _amRequestCount :: !(Maybe Int)
  , _amLatency      :: !(Maybe Latency)
  , _amStatusCodes  :: !(Maybe StatusCodes)
  , _amDuration     :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationMetrics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amRequestCount' - Average number of requests handled by the web server per second over the last 10 seconds.
--
-- * 'amLatency' - Represents the average latency for the slowest X percent of requests over the last 10 seconds. Latencies are in seconds with one millisecond resolution.
--
-- * 'amStatusCodes' - Represents the percentage of requests over the last 10 seconds that resulted in each type of status code response.
--
-- * 'amDuration' - The amount of time that the metrics cover (usually 10 seconds). For example, you might have 5 requests (@request_count@ ) within the most recent time slice of 10 seconds (@duration@ ).
applicationMetrics
    :: ApplicationMetrics
applicationMetrics =
  ApplicationMetrics'
    { _amRequestCount = Nothing
    , _amLatency = Nothing
    , _amStatusCodes = Nothing
    , _amDuration = Nothing
    }


-- | Average number of requests handled by the web server per second over the last 10 seconds.
amRequestCount :: Lens' ApplicationMetrics (Maybe Int)
amRequestCount = lens _amRequestCount (\ s a -> s{_amRequestCount = a})

-- | Represents the average latency for the slowest X percent of requests over the last 10 seconds. Latencies are in seconds with one millisecond resolution.
amLatency :: Lens' ApplicationMetrics (Maybe Latency)
amLatency = lens _amLatency (\ s a -> s{_amLatency = a})

-- | Represents the percentage of requests over the last 10 seconds that resulted in each type of status code response.
amStatusCodes :: Lens' ApplicationMetrics (Maybe StatusCodes)
amStatusCodes = lens _amStatusCodes (\ s a -> s{_amStatusCodes = a})

-- | The amount of time that the metrics cover (usually 10 seconds). For example, you might have 5 requests (@request_count@ ) within the most recent time slice of 10 seconds (@duration@ ).
amDuration :: Lens' ApplicationMetrics (Maybe Int)
amDuration = lens _amDuration (\ s a -> s{_amDuration = a})

instance FromXML ApplicationMetrics where
        parseXML x
          = ApplicationMetrics' <$>
              (x .@? "RequestCount") <*> (x .@? "Latency") <*>
                (x .@? "StatusCodes")
                <*> (x .@? "Duration")

instance Hashable ApplicationMetrics where

instance NFData ApplicationMetrics where

-- | The resource lifecycle configuration for an application. Defines lifecycle settings for resources that belong to the application, and the service role that Elastic Beanstalk assumes in order to apply lifecycle settings. The version lifecycle configuration defines lifecycle settings for application versions.
--
--
--
-- /See:/ 'applicationResourceLifecycleConfig' smart constructor.
data ApplicationResourceLifecycleConfig = ApplicationResourceLifecycleConfig'
  { _arlcVersionLifecycleConfig :: !(Maybe ApplicationVersionLifecycleConfig)
  , _arlcServiceRole            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationResourceLifecycleConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arlcVersionLifecycleConfig' - The application version lifecycle configuration.
--
-- * 'arlcServiceRole' - The ARN of an IAM service role that Elastic Beanstalk has permission to assume.
applicationResourceLifecycleConfig
    :: ApplicationResourceLifecycleConfig
applicationResourceLifecycleConfig =
  ApplicationResourceLifecycleConfig'
    {_arlcVersionLifecycleConfig = Nothing, _arlcServiceRole = Nothing}


-- | The application version lifecycle configuration.
arlcVersionLifecycleConfig :: Lens' ApplicationResourceLifecycleConfig (Maybe ApplicationVersionLifecycleConfig)
arlcVersionLifecycleConfig = lens _arlcVersionLifecycleConfig (\ s a -> s{_arlcVersionLifecycleConfig = a})

-- | The ARN of an IAM service role that Elastic Beanstalk has permission to assume.
arlcServiceRole :: Lens' ApplicationResourceLifecycleConfig (Maybe Text)
arlcServiceRole = lens _arlcServiceRole (\ s a -> s{_arlcServiceRole = a})

instance FromXML ApplicationResourceLifecycleConfig
         where
        parseXML x
          = ApplicationResourceLifecycleConfig' <$>
              (x .@? "VersionLifecycleConfig") <*>
                (x .@? "ServiceRole")

instance Hashable ApplicationResourceLifecycleConfig
         where

instance NFData ApplicationResourceLifecycleConfig
         where

instance ToQuery ApplicationResourceLifecycleConfig
         where
        toQuery ApplicationResourceLifecycleConfig'{..}
          = mconcat
              ["VersionLifecycleConfig" =:
                 _arlcVersionLifecycleConfig,
               "ServiceRole" =: _arlcServiceRole]

-- | Describes the properties of an application version.
--
--
--
-- /See:/ 'applicationVersionDescription' smart constructor.
data ApplicationVersionDescription = ApplicationVersionDescription'
  { _avdStatus                 :: !(Maybe ApplicationVersionStatus)
  , _avdSourceBundle           :: !(Maybe S3Location)
  , _avdDateUpdated            :: !(Maybe ISO8601)
  , _avdDateCreated            :: !(Maybe ISO8601)
  , _avdVersionLabel           :: !(Maybe Text)
  , _avdSourceBuildInformation :: !(Maybe SourceBuildInformation)
  , _avdApplicationName        :: !(Maybe Text)
  , _avdApplicationVersionARN  :: !(Maybe Text)
  , _avdBuildARN               :: !(Maybe Text)
  , _avdDescription            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationVersionDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avdStatus' - The processing status of the application version.
--
-- * 'avdSourceBundle' - The storage location of the application version's source bundle in Amazon S3.
--
-- * 'avdDateUpdated' - The last modified date of the application version.
--
-- * 'avdDateCreated' - The creation date of the application version.
--
-- * 'avdVersionLabel' - A unique identifier for the application version.
--
-- * 'avdSourceBuildInformation' - If the version's source code was retrieved from AWS CodeCommit, the location of the source code for the application version.
--
-- * 'avdApplicationName' - The name of the application to which the application version belongs.
--
-- * 'avdApplicationVersionARN' - The Amazon Resource Name (ARN) of the application version.
--
-- * 'avdBuildARN' - Reference to the artifact from the AWS CodeBuild build.
--
-- * 'avdDescription' - The description of the application version.
applicationVersionDescription
    :: ApplicationVersionDescription
applicationVersionDescription =
  ApplicationVersionDescription'
    { _avdStatus = Nothing
    , _avdSourceBundle = Nothing
    , _avdDateUpdated = Nothing
    , _avdDateCreated = Nothing
    , _avdVersionLabel = Nothing
    , _avdSourceBuildInformation = Nothing
    , _avdApplicationName = Nothing
    , _avdApplicationVersionARN = Nothing
    , _avdBuildARN = Nothing
    , _avdDescription = Nothing
    }


-- | The processing status of the application version.
avdStatus :: Lens' ApplicationVersionDescription (Maybe ApplicationVersionStatus)
avdStatus = lens _avdStatus (\ s a -> s{_avdStatus = a})

-- | The storage location of the application version's source bundle in Amazon S3.
avdSourceBundle :: Lens' ApplicationVersionDescription (Maybe S3Location)
avdSourceBundle = lens _avdSourceBundle (\ s a -> s{_avdSourceBundle = a})

-- | The last modified date of the application version.
avdDateUpdated :: Lens' ApplicationVersionDescription (Maybe UTCTime)
avdDateUpdated = lens _avdDateUpdated (\ s a -> s{_avdDateUpdated = a}) . mapping _Time

-- | The creation date of the application version.
avdDateCreated :: Lens' ApplicationVersionDescription (Maybe UTCTime)
avdDateCreated = lens _avdDateCreated (\ s a -> s{_avdDateCreated = a}) . mapping _Time

-- | A unique identifier for the application version.
avdVersionLabel :: Lens' ApplicationVersionDescription (Maybe Text)
avdVersionLabel = lens _avdVersionLabel (\ s a -> s{_avdVersionLabel = a})

-- | If the version's source code was retrieved from AWS CodeCommit, the location of the source code for the application version.
avdSourceBuildInformation :: Lens' ApplicationVersionDescription (Maybe SourceBuildInformation)
avdSourceBuildInformation = lens _avdSourceBuildInformation (\ s a -> s{_avdSourceBuildInformation = a})

-- | The name of the application to which the application version belongs.
avdApplicationName :: Lens' ApplicationVersionDescription (Maybe Text)
avdApplicationName = lens _avdApplicationName (\ s a -> s{_avdApplicationName = a})

-- | The Amazon Resource Name (ARN) of the application version.
avdApplicationVersionARN :: Lens' ApplicationVersionDescription (Maybe Text)
avdApplicationVersionARN = lens _avdApplicationVersionARN (\ s a -> s{_avdApplicationVersionARN = a})

-- | Reference to the artifact from the AWS CodeBuild build.
avdBuildARN :: Lens' ApplicationVersionDescription (Maybe Text)
avdBuildARN = lens _avdBuildARN (\ s a -> s{_avdBuildARN = a})

-- | The description of the application version.
avdDescription :: Lens' ApplicationVersionDescription (Maybe Text)
avdDescription = lens _avdDescription (\ s a -> s{_avdDescription = a})

instance FromXML ApplicationVersionDescription where
        parseXML x
          = ApplicationVersionDescription' <$>
              (x .@? "Status") <*> (x .@? "SourceBundle") <*>
                (x .@? "DateUpdated")
                <*> (x .@? "DateCreated")
                <*> (x .@? "VersionLabel")
                <*> (x .@? "SourceBuildInformation")
                <*> (x .@? "ApplicationName")
                <*> (x .@? "ApplicationVersionArn")
                <*> (x .@? "BuildArn")
                <*> (x .@? "Description")

instance Hashable ApplicationVersionDescription where

instance NFData ApplicationVersionDescription where

-- | Result message wrapping a single description of an application version.
--
--
--
-- /See:/ 'applicationVersionDescriptionMessage' smart constructor.
newtype ApplicationVersionDescriptionMessage = ApplicationVersionDescriptionMessage'
  { _avdmApplicationVersion :: Maybe ApplicationVersionDescription
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationVersionDescriptionMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avdmApplicationVersion' - The 'ApplicationVersionDescription' of the application version.
applicationVersionDescriptionMessage
    :: ApplicationVersionDescriptionMessage
applicationVersionDescriptionMessage =
  ApplicationVersionDescriptionMessage' {_avdmApplicationVersion = Nothing}


-- | The 'ApplicationVersionDescription' of the application version.
avdmApplicationVersion :: Lens' ApplicationVersionDescriptionMessage (Maybe ApplicationVersionDescription)
avdmApplicationVersion = lens _avdmApplicationVersion (\ s a -> s{_avdmApplicationVersion = a})

instance FromXML ApplicationVersionDescriptionMessage
         where
        parseXML x
          = ApplicationVersionDescriptionMessage' <$>
              (x .@? "ApplicationVersion")

instance Hashable
           ApplicationVersionDescriptionMessage
         where

instance NFData ApplicationVersionDescriptionMessage
         where

-- | The application version lifecycle settings for an application. Defines the rules that Elastic Beanstalk applies to an application's versions in order to avoid hitting the per-region limit for application versions.
--
--
-- When Elastic Beanstalk deletes an application version from its database, you can no longer deploy that version to an environment. The source bundle remains in S3 unless you configure the rule to delete it.
--
--
-- /See:/ 'applicationVersionLifecycleConfig' smart constructor.
data ApplicationVersionLifecycleConfig = ApplicationVersionLifecycleConfig'
  { _avlcMaxAgeRule   :: !(Maybe MaxAgeRule)
  , _avlcMaxCountRule :: !(Maybe MaxCountRule)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ApplicationVersionLifecycleConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avlcMaxAgeRule' - Specify a max age rule to restrict the length of time that application versions are retained for an application.
--
-- * 'avlcMaxCountRule' - Specify a max count rule to restrict the number of application versions that are retained for an application.
applicationVersionLifecycleConfig
    :: ApplicationVersionLifecycleConfig
applicationVersionLifecycleConfig =
  ApplicationVersionLifecycleConfig'
    {_avlcMaxAgeRule = Nothing, _avlcMaxCountRule = Nothing}


-- | Specify a max age rule to restrict the length of time that application versions are retained for an application.
avlcMaxAgeRule :: Lens' ApplicationVersionLifecycleConfig (Maybe MaxAgeRule)
avlcMaxAgeRule = lens _avlcMaxAgeRule (\ s a -> s{_avlcMaxAgeRule = a})

-- | Specify a max count rule to restrict the number of application versions that are retained for an application.
avlcMaxCountRule :: Lens' ApplicationVersionLifecycleConfig (Maybe MaxCountRule)
avlcMaxCountRule = lens _avlcMaxCountRule (\ s a -> s{_avlcMaxCountRule = a})

instance FromXML ApplicationVersionLifecycleConfig
         where
        parseXML x
          = ApplicationVersionLifecycleConfig' <$>
              (x .@? "MaxAgeRule") <*> (x .@? "MaxCountRule")

instance Hashable ApplicationVersionLifecycleConfig
         where

instance NFData ApplicationVersionLifecycleConfig
         where

instance ToQuery ApplicationVersionLifecycleConfig
         where
        toQuery ApplicationVersionLifecycleConfig'{..}
          = mconcat
              ["MaxAgeRule" =: _avlcMaxAgeRule,
               "MaxCountRule" =: _avlcMaxCountRule]

-- | Describes an Auto Scaling launch configuration.
--
--
--
-- /See:/ 'autoScalingGroup' smart constructor.
newtype AutoScalingGroup = AutoScalingGroup'
  { _asgName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AutoScalingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asgName' - The name of the @AutoScalingGroup@ .
autoScalingGroup
    :: AutoScalingGroup
autoScalingGroup = AutoScalingGroup' {_asgName = Nothing}


-- | The name of the @AutoScalingGroup@ .
asgName :: Lens' AutoScalingGroup (Maybe Text)
asgName = lens _asgName (\ s a -> s{_asgName = a})

instance FromXML AutoScalingGroup where
        parseXML x = AutoScalingGroup' <$> (x .@? "Name")

instance Hashable AutoScalingGroup where

instance NFData AutoScalingGroup where

-- | Settings for an AWS CodeBuild build.
--
--
--
-- /See:/ 'buildConfiguration' smart constructor.
data BuildConfiguration = BuildConfiguration'
  { _bcArtifactName         :: !(Maybe Text)
  , _bcComputeType          :: !(Maybe ComputeType)
  , _bcTimeoutInMinutes     :: !(Maybe Int)
  , _bcCodeBuildServiceRole :: !Text
  , _bcImage                :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BuildConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bcArtifactName' - The name of the artifact of the CodeBuild build. If provided, Elastic Beanstalk stores the build artifact in the S3 location /S3-bucket/ /resources//application-name/ /codebuild/codebuild-/version-label/ -/artifact-name/ .zip. If not provided, Elastic Beanstalk stores the build artifact in the S3 location /S3-bucket/ /resources//application-name/ /codebuild/codebuild-/version-label/ .zip.
--
-- * 'bcComputeType' - Information about the compute resources the build project will use.     * @BUILD_GENERAL1_SMALL: Use up to 3 GB memory and 2 vCPUs for builds@      * @BUILD_GENERAL1_MEDIUM: Use up to 7 GB memory and 4 vCPUs for builds@      * @BUILD_GENERAL1_LARGE: Use up to 15 GB memory and 8 vCPUs for builds@
--
-- * 'bcTimeoutInMinutes' - How long in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait until timing out any related build that does not get marked as completed. The default is 60 minutes.
--
-- * 'bcCodeBuildServiceRole' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
--
-- * 'bcImage' - The ID of the Docker image to use for this build project.
buildConfiguration
    :: Text -- ^ 'bcCodeBuildServiceRole'
    -> Text -- ^ 'bcImage'
    -> BuildConfiguration
buildConfiguration pCodeBuildServiceRole_ pImage_ =
  BuildConfiguration'
    { _bcArtifactName = Nothing
    , _bcComputeType = Nothing
    , _bcTimeoutInMinutes = Nothing
    , _bcCodeBuildServiceRole = pCodeBuildServiceRole_
    , _bcImage = pImage_
    }


-- | The name of the artifact of the CodeBuild build. If provided, Elastic Beanstalk stores the build artifact in the S3 location /S3-bucket/ /resources//application-name/ /codebuild/codebuild-/version-label/ -/artifact-name/ .zip. If not provided, Elastic Beanstalk stores the build artifact in the S3 location /S3-bucket/ /resources//application-name/ /codebuild/codebuild-/version-label/ .zip.
bcArtifactName :: Lens' BuildConfiguration (Maybe Text)
bcArtifactName = lens _bcArtifactName (\ s a -> s{_bcArtifactName = a})

-- | Information about the compute resources the build project will use.     * @BUILD_GENERAL1_SMALL: Use up to 3 GB memory and 2 vCPUs for builds@      * @BUILD_GENERAL1_MEDIUM: Use up to 7 GB memory and 4 vCPUs for builds@      * @BUILD_GENERAL1_LARGE: Use up to 15 GB memory and 8 vCPUs for builds@
bcComputeType :: Lens' BuildConfiguration (Maybe ComputeType)
bcComputeType = lens _bcComputeType (\ s a -> s{_bcComputeType = a})

-- | How long in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait until timing out any related build that does not get marked as completed. The default is 60 minutes.
bcTimeoutInMinutes :: Lens' BuildConfiguration (Maybe Int)
bcTimeoutInMinutes = lens _bcTimeoutInMinutes (\ s a -> s{_bcTimeoutInMinutes = a})

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
bcCodeBuildServiceRole :: Lens' BuildConfiguration Text
bcCodeBuildServiceRole = lens _bcCodeBuildServiceRole (\ s a -> s{_bcCodeBuildServiceRole = a})

-- | The ID of the Docker image to use for this build project.
bcImage :: Lens' BuildConfiguration Text
bcImage = lens _bcImage (\ s a -> s{_bcImage = a})

instance Hashable BuildConfiguration where

instance NFData BuildConfiguration where

instance ToQuery BuildConfiguration where
        toQuery BuildConfiguration'{..}
          = mconcat
              ["ArtifactName" =: _bcArtifactName,
               "ComputeType" =: _bcComputeType,
               "TimeoutInMinutes" =: _bcTimeoutInMinutes,
               "CodeBuildServiceRole" =: _bcCodeBuildServiceRole,
               "Image" =: _bcImage]

-- | The builder used to build the custom platform.
--
--
--
-- /See:/ 'builder' smart constructor.
newtype Builder = Builder'
  { _bARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Builder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bARN' - The ARN of the builder.
builder
    :: Builder
builder = Builder' {_bARN = Nothing}


-- | The ARN of the builder.
bARN :: Lens' Builder (Maybe Text)
bARN = lens _bARN (\ s a -> s{_bARN = a})

instance FromXML Builder where
        parseXML x = Builder' <$> (x .@? "ARN")

instance Hashable Builder where

instance NFData Builder where

-- | CPU utilization metrics for an instance.
--
--
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CPUUtilization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cuSoftIRQ' - Percentage of time that the CPU has spent in the @SoftIRQ@ state over the last 10 seconds.
--
-- * 'cuIdle' - Percentage of time that the CPU has spent in the @Idle@ state over the last 10 seconds.
--
-- * 'cuIRQ' - Percentage of time that the CPU has spent in the @IRQ@ state over the last 10 seconds.
--
-- * 'cuSystem' - Percentage of time that the CPU has spent in the @System@ state over the last 10 seconds.
--
-- * 'cuUser' - Percentage of time that the CPU has spent in the @User@ state over the last 10 seconds.
--
-- * 'cuIOWait' - Percentage of time that the CPU has spent in the @I/O Wait@ state over the last 10 seconds.
--
-- * 'cuNice' - Percentage of time that the CPU has spent in the @Nice@ state over the last 10 seconds.
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


-- | Percentage of time that the CPU has spent in the @SoftIRQ@ state over the last 10 seconds.
cuSoftIRQ :: Lens' CPUUtilization (Maybe Double)
cuSoftIRQ = lens _cuSoftIRQ (\ s a -> s{_cuSoftIRQ = a})

-- | Percentage of time that the CPU has spent in the @Idle@ state over the last 10 seconds.
cuIdle :: Lens' CPUUtilization (Maybe Double)
cuIdle = lens _cuIdle (\ s a -> s{_cuIdle = a})

-- | Percentage of time that the CPU has spent in the @IRQ@ state over the last 10 seconds.
cuIRQ :: Lens' CPUUtilization (Maybe Double)
cuIRQ = lens _cuIRQ (\ s a -> s{_cuIRQ = a})

-- | Percentage of time that the CPU has spent in the @System@ state over the last 10 seconds.
cuSystem :: Lens' CPUUtilization (Maybe Double)
cuSystem = lens _cuSystem (\ s a -> s{_cuSystem = a})

-- | Percentage of time that the CPU has spent in the @User@ state over the last 10 seconds.
cuUser :: Lens' CPUUtilization (Maybe Double)
cuUser = lens _cuUser (\ s a -> s{_cuUser = a})

-- | Percentage of time that the CPU has spent in the @I/O Wait@ state over the last 10 seconds.
cuIOWait :: Lens' CPUUtilization (Maybe Double)
cuIOWait = lens _cuIOWait (\ s a -> s{_cuIOWait = a})

-- | Percentage of time that the CPU has spent in the @Nice@ state over the last 10 seconds.
cuNice :: Lens' CPUUtilization (Maybe Double)
cuNice = lens _cuNice (\ s a -> s{_cuNice = a})

instance FromXML CPUUtilization where
        parseXML x
          = CPUUtilization' <$>
              (x .@? "SoftIRQ") <*> (x .@? "Idle") <*>
                (x .@? "IRQ")
                <*> (x .@? "System")
                <*> (x .@? "User")
                <*> (x .@? "IOWait")
                <*> (x .@? "Nice")

instance Hashable CPUUtilization where

instance NFData CPUUtilization where

-- | Describes the possible values for a configuration option.
--
--
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfigurationOptionDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'codMaxValue' - If specified, the configuration option must be a numeric value less than this value.
--
-- * 'codRegex' - If specified, the configuration option must be a string value that satisfies this regular expression.
--
-- * 'codMaxLength' - If specified, the configuration option must be a string value no longer than this value.
--
-- * 'codUserDefined' - An indication of whether the user defined this configuration option:     * @true@ : This configuration option was defined by the user. It is a valid choice for specifying if this as an @Option to Remove@ when updating configuration settings.      * @false@ : This configuration was not defined by the user. Constraint: You can remove only @UserDefined@ options from a configuration.  Valid Values: @true@ | @false@
--
-- * 'codNamespace' - A unique namespace identifying the option's associated AWS resource.
--
-- * 'codValueOptions' - If specified, values for the configuration option are selected from this list.
--
-- * 'codName' - The name of the configuration option.
--
-- * 'codChangeSeverity' - An indication of which action is required if the value for this configuration option changes:     * @NoInterruption@ : There is no interruption to the environment or application availability.     * @RestartEnvironment@ : The environment is entirely restarted, all AWS resources are deleted and recreated, and the environment is unavailable during the process.     * @RestartApplicationServer@ : The environment is available the entire time. However, a short application outage occurs when the application servers on the running Amazon EC2 instances are restarted.
--
-- * 'codDefaultValue' - The default value for this configuration option.
--
-- * 'codValueType' - An indication of which type of values this option has and whether it is allowable to select one or more than one of the possible values:     * @Scalar@ : Values for this option are a single selection from the possible values, or an unformatted string, or numeric value governed by the @MIN/MAX/Regex@ constraints.     * @List@ : Values for this option are multiple selections from the possible values.     * @Boolean@ : Values for this option are either @true@ or @false@ .     * @Json@ : Values for this option are a JSON representation of a @ConfigDocument@ .
--
-- * 'codMinValue' - If specified, the configuration option must be a numeric value greater than this value.
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


-- | If specified, the configuration option must be a numeric value less than this value.
codMaxValue :: Lens' ConfigurationOptionDescription (Maybe Int)
codMaxValue = lens _codMaxValue (\ s a -> s{_codMaxValue = a})

-- | If specified, the configuration option must be a string value that satisfies this regular expression.
codRegex :: Lens' ConfigurationOptionDescription (Maybe OptionRestrictionRegex)
codRegex = lens _codRegex (\ s a -> s{_codRegex = a})

-- | If specified, the configuration option must be a string value no longer than this value.
codMaxLength :: Lens' ConfigurationOptionDescription (Maybe Int)
codMaxLength = lens _codMaxLength (\ s a -> s{_codMaxLength = a})

-- | An indication of whether the user defined this configuration option:     * @true@ : This configuration option was defined by the user. It is a valid choice for specifying if this as an @Option to Remove@ when updating configuration settings.      * @false@ : This configuration was not defined by the user. Constraint: You can remove only @UserDefined@ options from a configuration.  Valid Values: @true@ | @false@
codUserDefined :: Lens' ConfigurationOptionDescription (Maybe Bool)
codUserDefined = lens _codUserDefined (\ s a -> s{_codUserDefined = a})

-- | A unique namespace identifying the option's associated AWS resource.
codNamespace :: Lens' ConfigurationOptionDescription (Maybe Text)
codNamespace = lens _codNamespace (\ s a -> s{_codNamespace = a})

-- | If specified, values for the configuration option are selected from this list.
codValueOptions :: Lens' ConfigurationOptionDescription [Text]
codValueOptions = lens _codValueOptions (\ s a -> s{_codValueOptions = a}) . _Default . _Coerce

-- | The name of the configuration option.
codName :: Lens' ConfigurationOptionDescription (Maybe Text)
codName = lens _codName (\ s a -> s{_codName = a})

-- | An indication of which action is required if the value for this configuration option changes:     * @NoInterruption@ : There is no interruption to the environment or application availability.     * @RestartEnvironment@ : The environment is entirely restarted, all AWS resources are deleted and recreated, and the environment is unavailable during the process.     * @RestartApplicationServer@ : The environment is available the entire time. However, a short application outage occurs when the application servers on the running Amazon EC2 instances are restarted.
codChangeSeverity :: Lens' ConfigurationOptionDescription (Maybe Text)
codChangeSeverity = lens _codChangeSeverity (\ s a -> s{_codChangeSeverity = a})

-- | The default value for this configuration option.
codDefaultValue :: Lens' ConfigurationOptionDescription (Maybe Text)
codDefaultValue = lens _codDefaultValue (\ s a -> s{_codDefaultValue = a})

-- | An indication of which type of values this option has and whether it is allowable to select one or more than one of the possible values:     * @Scalar@ : Values for this option are a single selection from the possible values, or an unformatted string, or numeric value governed by the @MIN/MAX/Regex@ constraints.     * @List@ : Values for this option are multiple selections from the possible values.     * @Boolean@ : Values for this option are either @true@ or @false@ .     * @Json@ : Values for this option are a JSON representation of a @ConfigDocument@ .
codValueType :: Lens' ConfigurationOptionDescription (Maybe ConfigurationOptionValueType)
codValueType = lens _codValueType (\ s a -> s{_codValueType = a})

-- | If specified, the configuration option must be a numeric value greater than this value.
codMinValue :: Lens' ConfigurationOptionDescription (Maybe Int)
codMinValue = lens _codMinValue (\ s a -> s{_codMinValue = a})

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
         where

instance NFData ConfigurationOptionDescription where

-- | A specification identifying an individual configuration option along with its current value. For a list of possible option values, go to <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/command-options.html Option Values> in the /AWS Elastic Beanstalk Developer Guide/ .
--
--
--
-- /See:/ 'configurationOptionSetting' smart constructor.
data ConfigurationOptionSetting = ConfigurationOptionSetting'
  { _cosOptionName   :: !(Maybe Text)
  , _cosResourceName :: !(Maybe Text)
  , _cosNamespace    :: !(Maybe Text)
  , _cosValue        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfigurationOptionSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cosOptionName' - The name of the configuration option.
--
-- * 'cosResourceName' - A unique resource name for a time-based scaling configuration option.
--
-- * 'cosNamespace' - A unique namespace identifying the option's associated AWS resource.
--
-- * 'cosValue' - The current value for the configuration option.
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
cosOptionName = lens _cosOptionName (\ s a -> s{_cosOptionName = a})

-- | A unique resource name for a time-based scaling configuration option.
cosResourceName :: Lens' ConfigurationOptionSetting (Maybe Text)
cosResourceName = lens _cosResourceName (\ s a -> s{_cosResourceName = a})

-- | A unique namespace identifying the option's associated AWS resource.
cosNamespace :: Lens' ConfigurationOptionSetting (Maybe Text)
cosNamespace = lens _cosNamespace (\ s a -> s{_cosNamespace = a})

-- | The current value for the configuration option.
cosValue :: Lens' ConfigurationOptionSetting (Maybe Text)
cosValue = lens _cosValue (\ s a -> s{_cosValue = a})

instance FromXML ConfigurationOptionSetting where
        parseXML x
          = ConfigurationOptionSetting' <$>
              (x .@? "OptionName") <*> (x .@? "ResourceName") <*>
                (x .@? "Namespace")
                <*> (x .@? "Value")

instance Hashable ConfigurationOptionSetting where

instance NFData ConfigurationOptionSetting where

instance ToQuery ConfigurationOptionSetting where
        toQuery ConfigurationOptionSetting'{..}
          = mconcat
              ["OptionName" =: _cosOptionName,
               "ResourceName" =: _cosResourceName,
               "Namespace" =: _cosNamespace, "Value" =: _cosValue]

-- | Describes the settings for a configuration set.
--
--
--
-- /See:/ 'configurationSettingsDescription' smart constructor.
data ConfigurationSettingsDescription = ConfigurationSettingsDescription'
  { _csdTemplateName      :: !(Maybe Text)
  , _csdOptionSettings    :: !(Maybe [ConfigurationOptionSetting])
  , _csdDateUpdated       :: !(Maybe ISO8601)
  , _csdDateCreated       :: !(Maybe ISO8601)
  , _csdPlatformARN       :: !(Maybe Text)
  , _csdEnvironmentName   :: !(Maybe Text)
  , _csdApplicationName   :: !(Maybe Text)
  , _csdDeploymentStatus  :: !(Maybe ConfigurationDeploymentStatus)
  , _csdSolutionStackName :: !(Maybe Text)
  , _csdDescription       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfigurationSettingsDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdTemplateName' - If not @null@ , the name of the configuration template for this configuration set.
--
-- * 'csdOptionSettings' - A list of the configuration options and their values in this configuration set.
--
-- * 'csdDateUpdated' - The date (in UTC time) when this configuration set was last modified.
--
-- * 'csdDateCreated' - The date (in UTC time) when this configuration set was created.
--
-- * 'csdPlatformARN' - The ARN of the platform.
--
-- * 'csdEnvironmentName' - If not @null@ , the name of the environment for this configuration set.
--
-- * 'csdApplicationName' - The name of the application associated with this configuration set.
--
-- * 'csdDeploymentStatus' - If this configuration set is associated with an environment, the @DeploymentStatus@ parameter indicates the deployment status of this configuration set:      * @null@ : This configuration is not associated with a running environment.     * @pending@ : This is a draft configuration that is not deployed to the associated environment but is in the process of deploying.     * @deployed@ : This is the configuration that is currently deployed to the associated running environment.     * @failed@ : This is a draft configuration that failed to successfully deploy.
--
-- * 'csdSolutionStackName' - The name of the solution stack this configuration set uses.
--
-- * 'csdDescription' - Describes this configuration set.
configurationSettingsDescription
    :: ConfigurationSettingsDescription
configurationSettingsDescription =
  ConfigurationSettingsDescription'
    { _csdTemplateName = Nothing
    , _csdOptionSettings = Nothing
    , _csdDateUpdated = Nothing
    , _csdDateCreated = Nothing
    , _csdPlatformARN = Nothing
    , _csdEnvironmentName = Nothing
    , _csdApplicationName = Nothing
    , _csdDeploymentStatus = Nothing
    , _csdSolutionStackName = Nothing
    , _csdDescription = Nothing
    }


-- | If not @null@ , the name of the configuration template for this configuration set.
csdTemplateName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdTemplateName = lens _csdTemplateName (\ s a -> s{_csdTemplateName = a})

-- | A list of the configuration options and their values in this configuration set.
csdOptionSettings :: Lens' ConfigurationSettingsDescription [ConfigurationOptionSetting]
csdOptionSettings = lens _csdOptionSettings (\ s a -> s{_csdOptionSettings = a}) . _Default . _Coerce

-- | The date (in UTC time) when this configuration set was last modified.
csdDateUpdated :: Lens' ConfigurationSettingsDescription (Maybe UTCTime)
csdDateUpdated = lens _csdDateUpdated (\ s a -> s{_csdDateUpdated = a}) . mapping _Time

-- | The date (in UTC time) when this configuration set was created.
csdDateCreated :: Lens' ConfigurationSettingsDescription (Maybe UTCTime)
csdDateCreated = lens _csdDateCreated (\ s a -> s{_csdDateCreated = a}) . mapping _Time

-- | The ARN of the platform.
csdPlatformARN :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdPlatformARN = lens _csdPlatformARN (\ s a -> s{_csdPlatformARN = a})

-- | If not @null@ , the name of the environment for this configuration set.
csdEnvironmentName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdEnvironmentName = lens _csdEnvironmentName (\ s a -> s{_csdEnvironmentName = a})

-- | The name of the application associated with this configuration set.
csdApplicationName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdApplicationName = lens _csdApplicationName (\ s a -> s{_csdApplicationName = a})

-- | If this configuration set is associated with an environment, the @DeploymentStatus@ parameter indicates the deployment status of this configuration set:      * @null@ : This configuration is not associated with a running environment.     * @pending@ : This is a draft configuration that is not deployed to the associated environment but is in the process of deploying.     * @deployed@ : This is the configuration that is currently deployed to the associated running environment.     * @failed@ : This is a draft configuration that failed to successfully deploy.
csdDeploymentStatus :: Lens' ConfigurationSettingsDescription (Maybe ConfigurationDeploymentStatus)
csdDeploymentStatus = lens _csdDeploymentStatus (\ s a -> s{_csdDeploymentStatus = a})

-- | The name of the solution stack this configuration set uses.
csdSolutionStackName :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdSolutionStackName = lens _csdSolutionStackName (\ s a -> s{_csdSolutionStackName = a})

-- | Describes this configuration set.
csdDescription :: Lens' ConfigurationSettingsDescription (Maybe Text)
csdDescription = lens _csdDescription (\ s a -> s{_csdDescription = a})

instance FromXML ConfigurationSettingsDescription
         where
        parseXML x
          = ConfigurationSettingsDescription' <$>
              (x .@? "TemplateName") <*>
                (x .@? "OptionSettings" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "DateUpdated")
                <*> (x .@? "DateCreated")
                <*> (x .@? "PlatformArn")
                <*> (x .@? "EnvironmentName")
                <*> (x .@? "ApplicationName")
                <*> (x .@? "DeploymentStatus")
                <*> (x .@? "SolutionStackName")
                <*> (x .@? "Description")

instance Hashable ConfigurationSettingsDescription
         where

instance NFData ConfigurationSettingsDescription
         where

-- | A custom AMI available to platforms.
--
--
--
-- /See:/ 'customAMI' smart constructor.
data CustomAMI = CustomAMI'
  { _caVirtualizationType :: !(Maybe Text)
  , _caImageId            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CustomAMI' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caVirtualizationType' - The type of virtualization used to create the custom AMI.
--
-- * 'caImageId' - THe ID of the image used to create the custom AMI.
customAMI
    :: CustomAMI
customAMI = CustomAMI' {_caVirtualizationType = Nothing, _caImageId = Nothing}


-- | The type of virtualization used to create the custom AMI.
caVirtualizationType :: Lens' CustomAMI (Maybe Text)
caVirtualizationType = lens _caVirtualizationType (\ s a -> s{_caVirtualizationType = a})

-- | THe ID of the image used to create the custom AMI.
caImageId :: Lens' CustomAMI (Maybe Text)
caImageId = lens _caImageId (\ s a -> s{_caImageId = a})

instance FromXML CustomAMI where
        parseXML x
          = CustomAMI' <$>
              (x .@? "VirtualizationType") <*> (x .@? "ImageId")

instance Hashable CustomAMI where

instance NFData CustomAMI where

-- | Information about an application version deployment.
--
--
--
-- /See:/ 'deployment' smart constructor.
data Deployment = Deployment'
  { _dDeploymentId   :: !(Maybe Integer)
  , _dStatus         :: !(Maybe Text)
  , _dDeploymentTime :: !(Maybe ISO8601)
  , _dVersionLabel   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Deployment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDeploymentId' - The ID of the deployment. This number increases by one each time that you deploy source code or change instance configuration settings.
--
-- * 'dStatus' - The status of the deployment:     * @In Progress@ : The deployment is in progress.     * @Deployed@ : The deployment succeeded.     * @Failed@ : The deployment failed.
--
-- * 'dDeploymentTime' - For in-progress deployments, the time that the deployment started. For completed deployments, the time that the deployment ended.
--
-- * 'dVersionLabel' - The version label of the application version in the deployment.
deployment
    :: Deployment
deployment =
  Deployment'
    { _dDeploymentId = Nothing
    , _dStatus = Nothing
    , _dDeploymentTime = Nothing
    , _dVersionLabel = Nothing
    }


-- | The ID of the deployment. This number increases by one each time that you deploy source code or change instance configuration settings.
dDeploymentId :: Lens' Deployment (Maybe Integer)
dDeploymentId = lens _dDeploymentId (\ s a -> s{_dDeploymentId = a})

-- | The status of the deployment:     * @In Progress@ : The deployment is in progress.     * @Deployed@ : The deployment succeeded.     * @Failed@ : The deployment failed.
dStatus :: Lens' Deployment (Maybe Text)
dStatus = lens _dStatus (\ s a -> s{_dStatus = a})

-- | For in-progress deployments, the time that the deployment started. For completed deployments, the time that the deployment ended.
dDeploymentTime :: Lens' Deployment (Maybe UTCTime)
dDeploymentTime = lens _dDeploymentTime (\ s a -> s{_dDeploymentTime = a}) . mapping _Time

-- | The version label of the application version in the deployment.
dVersionLabel :: Lens' Deployment (Maybe Text)
dVersionLabel = lens _dVersionLabel (\ s a -> s{_dVersionLabel = a})

instance FromXML Deployment where
        parseXML x
          = Deployment' <$>
              (x .@? "DeploymentId") <*> (x .@? "Status") <*>
                (x .@? "DeploymentTime")
                <*> (x .@? "VersionLabel")

instance Hashable Deployment where

instance NFData Deployment where

-- | Describes the properties of an environment.
--
--
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
  , _ePlatformARN                  :: !(Maybe Text)
  , _eTier                         :: !(Maybe EnvironmentTier)
  , _eEnvironmentName              :: !(Maybe Text)
  , _eApplicationName              :: !(Maybe Text)
  , _eEnvironmentARN               :: !(Maybe Text)
  , _eSolutionStackName            :: !(Maybe Text)
  , _eEnvironmentId                :: !(Maybe Text)
  , _eHealthStatus                 :: !(Maybe EnvironmentHealthStatus)
  , _eEnvironmentLinks             :: !(Maybe [EnvironmentLink])
  , _eDescription                  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnvironmentDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eStatus' - The current operational status of the environment:     * @Launching@ : Environment is in the process of initial deployment.     * @Updating@ : Environment is in the process of updating its configuration settings or application version.     * @Ready@ : Environment is available to have an action performed on it, such as update or terminate.     * @Terminating@ : Environment is in the shut-down process.     * @Terminated@ : Environment is not running.
--
-- * 'eCNAME' - The URL to the CNAME for this environment.
--
-- * 'eTemplateName' - The name of the configuration template used to originally launch this environment.
--
-- * 'eAbortableOperationInProgress' - Indicates if there is an in-progress environment configuration update or application version deployment that you can cancel. @true:@ There is an update in progress.  @false:@ There are no updates currently in progress.
--
-- * 'eEndpointURL' - For load-balanced, autoscaling environments, the URL to the LoadBalancer. For single-instance environments, the IP address of the instance.
--
-- * 'eResources' - The description of the AWS resources used by this environment.
--
-- * 'eDateUpdated' - The last modified date for this environment.
--
-- * 'eDateCreated' - The creation date for this environment.
--
-- * 'eHealth' - Describes the health status of the environment. AWS Elastic Beanstalk indicates the failure levels for a running environment:     * @Red@ : Indicates the environment is not responsive. Occurs when three or more consecutive failures occur for an environment.     * @Yellow@ : Indicates that something is wrong. Occurs when two consecutive failures occur for an environment.     * @Green@ : Indicates the environment is healthy and fully functional.     * @Grey@ : Default health for a new environment. The environment is not fully launched and health checks have not started or health checks are suspended during an @UpdateEnvironment@ or @RestartEnvironement@ request. Default: @Grey@
--
-- * 'eVersionLabel' - The application version deployed in this environment.
--
-- * 'ePlatformARN' - The ARN of the platform.
--
-- * 'eTier' - Describes the current tier of this environment.
--
-- * 'eEnvironmentName' - The name of this environment.
--
-- * 'eApplicationName' - The name of the application associated with this environment.
--
-- * 'eEnvironmentARN' - The environment's Amazon Resource Name (ARN), which can be used in other API requests that require an ARN.
--
-- * 'eSolutionStackName' - The name of the @SolutionStack@ deployed with this environment.
--
-- * 'eEnvironmentId' - The ID of this environment.
--
-- * 'eHealthStatus' - Returns the health status of the application running in your environment. For more information, see <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
--
-- * 'eEnvironmentLinks' - A list of links to other environments in the same group.
--
-- * 'eDescription' - Describes this environment.
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
    , _ePlatformARN = Nothing
    , _eTier = Nothing
    , _eEnvironmentName = Nothing
    , _eApplicationName = Nothing
    , _eEnvironmentARN = Nothing
    , _eSolutionStackName = Nothing
    , _eEnvironmentId = Nothing
    , _eHealthStatus = Nothing
    , _eEnvironmentLinks = Nothing
    , _eDescription = Nothing
    }


-- | The current operational status of the environment:     * @Launching@ : Environment is in the process of initial deployment.     * @Updating@ : Environment is in the process of updating its configuration settings or application version.     * @Ready@ : Environment is available to have an action performed on it, such as update or terminate.     * @Terminating@ : Environment is in the shut-down process.     * @Terminated@ : Environment is not running.
eStatus :: Lens' EnvironmentDescription (Maybe EnvironmentStatus)
eStatus = lens _eStatus (\ s a -> s{_eStatus = a})

-- | The URL to the CNAME for this environment.
eCNAME :: Lens' EnvironmentDescription (Maybe Text)
eCNAME = lens _eCNAME (\ s a -> s{_eCNAME = a})

-- | The name of the configuration template used to originally launch this environment.
eTemplateName :: Lens' EnvironmentDescription (Maybe Text)
eTemplateName = lens _eTemplateName (\ s a -> s{_eTemplateName = a})

-- | Indicates if there is an in-progress environment configuration update or application version deployment that you can cancel. @true:@ There is an update in progress.  @false:@ There are no updates currently in progress.
eAbortableOperationInProgress :: Lens' EnvironmentDescription (Maybe Bool)
eAbortableOperationInProgress = lens _eAbortableOperationInProgress (\ s a -> s{_eAbortableOperationInProgress = a})

-- | For load-balanced, autoscaling environments, the URL to the LoadBalancer. For single-instance environments, the IP address of the instance.
eEndpointURL :: Lens' EnvironmentDescription (Maybe Text)
eEndpointURL = lens _eEndpointURL (\ s a -> s{_eEndpointURL = a})

-- | The description of the AWS resources used by this environment.
eResources :: Lens' EnvironmentDescription (Maybe EnvironmentResourcesDescription)
eResources = lens _eResources (\ s a -> s{_eResources = a})

-- | The last modified date for this environment.
eDateUpdated :: Lens' EnvironmentDescription (Maybe UTCTime)
eDateUpdated = lens _eDateUpdated (\ s a -> s{_eDateUpdated = a}) . mapping _Time

-- | The creation date for this environment.
eDateCreated :: Lens' EnvironmentDescription (Maybe UTCTime)
eDateCreated = lens _eDateCreated (\ s a -> s{_eDateCreated = a}) . mapping _Time

-- | Describes the health status of the environment. AWS Elastic Beanstalk indicates the failure levels for a running environment:     * @Red@ : Indicates the environment is not responsive. Occurs when three or more consecutive failures occur for an environment.     * @Yellow@ : Indicates that something is wrong. Occurs when two consecutive failures occur for an environment.     * @Green@ : Indicates the environment is healthy and fully functional.     * @Grey@ : Default health for a new environment. The environment is not fully launched and health checks have not started or health checks are suspended during an @UpdateEnvironment@ or @RestartEnvironement@ request. Default: @Grey@
eHealth :: Lens' EnvironmentDescription (Maybe EnvironmentHealth)
eHealth = lens _eHealth (\ s a -> s{_eHealth = a})

-- | The application version deployed in this environment.
eVersionLabel :: Lens' EnvironmentDescription (Maybe Text)
eVersionLabel = lens _eVersionLabel (\ s a -> s{_eVersionLabel = a})

-- | The ARN of the platform.
ePlatformARN :: Lens' EnvironmentDescription (Maybe Text)
ePlatformARN = lens _ePlatformARN (\ s a -> s{_ePlatformARN = a})

-- | Describes the current tier of this environment.
eTier :: Lens' EnvironmentDescription (Maybe EnvironmentTier)
eTier = lens _eTier (\ s a -> s{_eTier = a})

-- | The name of this environment.
eEnvironmentName :: Lens' EnvironmentDescription (Maybe Text)
eEnvironmentName = lens _eEnvironmentName (\ s a -> s{_eEnvironmentName = a})

-- | The name of the application associated with this environment.
eApplicationName :: Lens' EnvironmentDescription (Maybe Text)
eApplicationName = lens _eApplicationName (\ s a -> s{_eApplicationName = a})

-- | The environment's Amazon Resource Name (ARN), which can be used in other API requests that require an ARN.
eEnvironmentARN :: Lens' EnvironmentDescription (Maybe Text)
eEnvironmentARN = lens _eEnvironmentARN (\ s a -> s{_eEnvironmentARN = a})

-- | The name of the @SolutionStack@ deployed with this environment.
eSolutionStackName :: Lens' EnvironmentDescription (Maybe Text)
eSolutionStackName = lens _eSolutionStackName (\ s a -> s{_eSolutionStackName = a})

-- | The ID of this environment.
eEnvironmentId :: Lens' EnvironmentDescription (Maybe Text)
eEnvironmentId = lens _eEnvironmentId (\ s a -> s{_eEnvironmentId = a})

-- | Returns the health status of the application running in your environment. For more information, see <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
eHealthStatus :: Lens' EnvironmentDescription (Maybe EnvironmentHealthStatus)
eHealthStatus = lens _eHealthStatus (\ s a -> s{_eHealthStatus = a})

-- | A list of links to other environments in the same group.
eEnvironmentLinks :: Lens' EnvironmentDescription [EnvironmentLink]
eEnvironmentLinks = lens _eEnvironmentLinks (\ s a -> s{_eEnvironmentLinks = a}) . _Default . _Coerce

-- | Describes this environment.
eDescription :: Lens' EnvironmentDescription (Maybe Text)
eDescription = lens _eDescription (\ s a -> s{_eDescription = a})

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
                <*> (x .@? "PlatformArn")
                <*> (x .@? "Tier")
                <*> (x .@? "EnvironmentName")
                <*> (x .@? "ApplicationName")
                <*> (x .@? "EnvironmentArn")
                <*> (x .@? "SolutionStackName")
                <*> (x .@? "EnvironmentId")
                <*> (x .@? "HealthStatus")
                <*>
                (x .@? "EnvironmentLinks" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "Description")

instance Hashable EnvironmentDescription where

instance NFData EnvironmentDescription where

-- | Result message containing a list of environment descriptions.
--
--
--
-- /See:/ 'environmentDescriptionsMessage' smart constructor.
data EnvironmentDescriptionsMessage = EnvironmentDescriptionsMessage'
  { _edmNextToken    :: !(Maybe Text)
  , _edmEnvironments :: !(Maybe [EnvironmentDescription])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnvironmentDescriptionsMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edmNextToken' - In a paginated request, the token that you can pass in a subsequent request to get the next response page.
--
-- * 'edmEnvironments' - Returns an 'EnvironmentDescription' list.
environmentDescriptionsMessage
    :: EnvironmentDescriptionsMessage
environmentDescriptionsMessage =
  EnvironmentDescriptionsMessage'
    {_edmNextToken = Nothing, _edmEnvironments = Nothing}


-- | In a paginated request, the token that you can pass in a subsequent request to get the next response page.
edmNextToken :: Lens' EnvironmentDescriptionsMessage (Maybe Text)
edmNextToken = lens _edmNextToken (\ s a -> s{_edmNextToken = a})

-- | Returns an 'EnvironmentDescription' list.
edmEnvironments :: Lens' EnvironmentDescriptionsMessage [EnvironmentDescription]
edmEnvironments = lens _edmEnvironments (\ s a -> s{_edmEnvironments = a}) . _Default . _Coerce

instance FromXML EnvironmentDescriptionsMessage where
        parseXML x
          = EnvironmentDescriptionsMessage' <$>
              (x .@? "NextToken") <*>
                (x .@? "Environments" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable EnvironmentDescriptionsMessage
         where

instance NFData EnvironmentDescriptionsMessage where

-- | The information retrieved from the Amazon EC2 instances.
--
--
--
-- /See:/ 'environmentInfoDescription' smart constructor.
data EnvironmentInfoDescription = EnvironmentInfoDescription'
  { _eidSampleTimestamp :: !(Maybe ISO8601)
  , _eidEC2InstanceId   :: !(Maybe Text)
  , _eidInfoType        :: !(Maybe EnvironmentInfoType)
  , _eidMessage         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnvironmentInfoDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eidSampleTimestamp' - The time stamp when this information was retrieved.
--
-- * 'eidEC2InstanceId' - The Amazon EC2 Instance ID for this information.
--
-- * 'eidInfoType' - The type of information retrieved.
--
-- * 'eidMessage' - The retrieved information.
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
eidSampleTimestamp = lens _eidSampleTimestamp (\ s a -> s{_eidSampleTimestamp = a}) . mapping _Time

-- | The Amazon EC2 Instance ID for this information.
eidEC2InstanceId :: Lens' EnvironmentInfoDescription (Maybe Text)
eidEC2InstanceId = lens _eidEC2InstanceId (\ s a -> s{_eidEC2InstanceId = a})

-- | The type of information retrieved.
eidInfoType :: Lens' EnvironmentInfoDescription (Maybe EnvironmentInfoType)
eidInfoType = lens _eidInfoType (\ s a -> s{_eidInfoType = a})

-- | The retrieved information.
eidMessage :: Lens' EnvironmentInfoDescription (Maybe Text)
eidMessage = lens _eidMessage (\ s a -> s{_eidMessage = a})

instance FromXML EnvironmentInfoDescription where
        parseXML x
          = EnvironmentInfoDescription' <$>
              (x .@? "SampleTimestamp") <*> (x .@? "Ec2InstanceId")
                <*> (x .@? "InfoType")
                <*> (x .@? "Message")

instance Hashable EnvironmentInfoDescription where

instance NFData EnvironmentInfoDescription where

-- | A link to another environment, defined in the environment's manifest. Links provide connection information in system properties that can be used to connect to another environment in the same group. See <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)> for details.
--
--
--
-- /See:/ 'environmentLink' smart constructor.
data EnvironmentLink = EnvironmentLink'
  { _elLinkName        :: !(Maybe Text)
  , _elEnvironmentName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnvironmentLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'elLinkName' - The name of the link.
--
-- * 'elEnvironmentName' - The name of the linked environment (the dependency).
environmentLink
    :: EnvironmentLink
environmentLink =
  EnvironmentLink' {_elLinkName = Nothing, _elEnvironmentName = Nothing}


-- | The name of the link.
elLinkName :: Lens' EnvironmentLink (Maybe Text)
elLinkName = lens _elLinkName (\ s a -> s{_elLinkName = a})

-- | The name of the linked environment (the dependency).
elEnvironmentName :: Lens' EnvironmentLink (Maybe Text)
elEnvironmentName = lens _elEnvironmentName (\ s a -> s{_elEnvironmentName = a})

instance FromXML EnvironmentLink where
        parseXML x
          = EnvironmentLink' <$>
              (x .@? "LinkName") <*> (x .@? "EnvironmentName")

instance Hashable EnvironmentLink where

instance NFData EnvironmentLink where

-- | Describes the AWS resources in use by this environment. This data is live.
--
--
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnvironmentResourceDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erdQueues' - The queues used by this environment.
--
-- * 'erdTriggers' - The @AutoScaling@ triggers in use by this environment.
--
-- * 'erdLoadBalancers' - The LoadBalancers in use by this environment.
--
-- * 'erdEnvironmentName' - The name of the environment.
--
-- * 'erdInstances' - The Amazon EC2 instances used by this environment.
--
-- * 'erdLaunchConfigurations' - The Auto Scaling launch configurations in use by this environment.
--
-- * 'erdAutoScalingGroups' - The @AutoScalingGroups@ used by this environment.
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
erdQueues = lens _erdQueues (\ s a -> s{_erdQueues = a}) . _Default . _Coerce

-- | The @AutoScaling@ triggers in use by this environment.
erdTriggers :: Lens' EnvironmentResourceDescription [Trigger]
erdTriggers = lens _erdTriggers (\ s a -> s{_erdTriggers = a}) . _Default . _Coerce

-- | The LoadBalancers in use by this environment.
erdLoadBalancers :: Lens' EnvironmentResourceDescription [LoadBalancer]
erdLoadBalancers = lens _erdLoadBalancers (\ s a -> s{_erdLoadBalancers = a}) . _Default . _Coerce

-- | The name of the environment.
erdEnvironmentName :: Lens' EnvironmentResourceDescription (Maybe Text)
erdEnvironmentName = lens _erdEnvironmentName (\ s a -> s{_erdEnvironmentName = a})

-- | The Amazon EC2 instances used by this environment.
erdInstances :: Lens' EnvironmentResourceDescription [Instance]
erdInstances = lens _erdInstances (\ s a -> s{_erdInstances = a}) . _Default . _Coerce

-- | The Auto Scaling launch configurations in use by this environment.
erdLaunchConfigurations :: Lens' EnvironmentResourceDescription [LaunchConfiguration]
erdLaunchConfigurations = lens _erdLaunchConfigurations (\ s a -> s{_erdLaunchConfigurations = a}) . _Default . _Coerce

-- | The @AutoScalingGroups@ used by this environment.
erdAutoScalingGroups :: Lens' EnvironmentResourceDescription [AutoScalingGroup]
erdAutoScalingGroups = lens _erdAutoScalingGroups (\ s a -> s{_erdAutoScalingGroups = a}) . _Default . _Coerce

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
         where

instance NFData EnvironmentResourceDescription where

-- | Describes the AWS resources in use by this environment. This data is not live data.
--
--
--
-- /See:/ 'environmentResourcesDescription' smart constructor.
newtype EnvironmentResourcesDescription = EnvironmentResourcesDescription'
  { _erdLoadBalancer :: Maybe LoadBalancerDescription
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnvironmentResourcesDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erdLoadBalancer' - Describes the LoadBalancer.
environmentResourcesDescription
    :: EnvironmentResourcesDescription
environmentResourcesDescription =
  EnvironmentResourcesDescription' {_erdLoadBalancer = Nothing}


-- | Describes the LoadBalancer.
erdLoadBalancer :: Lens' EnvironmentResourcesDescription (Maybe LoadBalancerDescription)
erdLoadBalancer = lens _erdLoadBalancer (\ s a -> s{_erdLoadBalancer = a})

instance FromXML EnvironmentResourcesDescription
         where
        parseXML x
          = EnvironmentResourcesDescription' <$>
              (x .@? "LoadBalancer")

instance Hashable EnvironmentResourcesDescription
         where

instance NFData EnvironmentResourcesDescription where

-- | Describes the properties of an environment tier
--
--
--
-- /See:/ 'environmentTier' smart constructor.
data EnvironmentTier = EnvironmentTier'
  { _etName    :: !(Maybe Text)
  , _etVersion :: !(Maybe Text)
  , _etType    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnvironmentTier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etName' - The name of this environment tier.
--
-- * 'etVersion' - The version of this environment tier.
--
-- * 'etType' - The type of this environment tier.
environmentTier
    :: EnvironmentTier
environmentTier =
  EnvironmentTier' {_etName = Nothing, _etVersion = Nothing, _etType = Nothing}


-- | The name of this environment tier.
etName :: Lens' EnvironmentTier (Maybe Text)
etName = lens _etName (\ s a -> s{_etName = a})

-- | The version of this environment tier.
etVersion :: Lens' EnvironmentTier (Maybe Text)
etVersion = lens _etVersion (\ s a -> s{_etVersion = a})

-- | The type of this environment tier.
etType :: Lens' EnvironmentTier (Maybe Text)
etType = lens _etType (\ s a -> s{_etType = a})

instance FromXML EnvironmentTier where
        parseXML x
          = EnvironmentTier' <$>
              (x .@? "Name") <*> (x .@? "Version") <*>
                (x .@? "Type")

instance Hashable EnvironmentTier where

instance NFData EnvironmentTier where

instance ToQuery EnvironmentTier where
        toQuery EnvironmentTier'{..}
          = mconcat
              ["Name" =: _etName, "Version" =: _etVersion,
               "Type" =: _etType]

-- | Describes an event.
--
--
--
-- /See:/ 'eventDescription' smart constructor.
data EventDescription = EventDescription'
  { _edRequestId       :: !(Maybe Text)
  , _edTemplateName    :: !(Maybe Text)
  , _edSeverity        :: !(Maybe EventSeverity)
  , _edVersionLabel    :: !(Maybe Text)
  , _edPlatformARN     :: !(Maybe Text)
  , _edEnvironmentName :: !(Maybe Text)
  , _edApplicationName :: !(Maybe Text)
  , _edEventDate       :: !(Maybe ISO8601)
  , _edMessage         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EventDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edRequestId' - The web service request ID for the activity of this event.
--
-- * 'edTemplateName' - The name of the configuration associated with this event.
--
-- * 'edSeverity' - The severity level of this event.
--
-- * 'edVersionLabel' - The release label for the application version associated with this event.
--
-- * 'edPlatformARN' - The ARN of the platform.
--
-- * 'edEnvironmentName' - The name of the environment associated with this event.
--
-- * 'edApplicationName' - The application associated with the event.
--
-- * 'edEventDate' - The date when the event occurred.
--
-- * 'edMessage' - The event message.
eventDescription
    :: EventDescription
eventDescription =
  EventDescription'
    { _edRequestId = Nothing
    , _edTemplateName = Nothing
    , _edSeverity = Nothing
    , _edVersionLabel = Nothing
    , _edPlatformARN = Nothing
    , _edEnvironmentName = Nothing
    , _edApplicationName = Nothing
    , _edEventDate = Nothing
    , _edMessage = Nothing
    }


-- | The web service request ID for the activity of this event.
edRequestId :: Lens' EventDescription (Maybe Text)
edRequestId = lens _edRequestId (\ s a -> s{_edRequestId = a})

-- | The name of the configuration associated with this event.
edTemplateName :: Lens' EventDescription (Maybe Text)
edTemplateName = lens _edTemplateName (\ s a -> s{_edTemplateName = a})

-- | The severity level of this event.
edSeverity :: Lens' EventDescription (Maybe EventSeverity)
edSeverity = lens _edSeverity (\ s a -> s{_edSeverity = a})

-- | The release label for the application version associated with this event.
edVersionLabel :: Lens' EventDescription (Maybe Text)
edVersionLabel = lens _edVersionLabel (\ s a -> s{_edVersionLabel = a})

-- | The ARN of the platform.
edPlatformARN :: Lens' EventDescription (Maybe Text)
edPlatformARN = lens _edPlatformARN (\ s a -> s{_edPlatformARN = a})

-- | The name of the environment associated with this event.
edEnvironmentName :: Lens' EventDescription (Maybe Text)
edEnvironmentName = lens _edEnvironmentName (\ s a -> s{_edEnvironmentName = a})

-- | The application associated with the event.
edApplicationName :: Lens' EventDescription (Maybe Text)
edApplicationName = lens _edApplicationName (\ s a -> s{_edApplicationName = a})

-- | The date when the event occurred.
edEventDate :: Lens' EventDescription (Maybe UTCTime)
edEventDate = lens _edEventDate (\ s a -> s{_edEventDate = a}) . mapping _Time

-- | The event message.
edMessage :: Lens' EventDescription (Maybe Text)
edMessage = lens _edMessage (\ s a -> s{_edMessage = a})

instance FromXML EventDescription where
        parseXML x
          = EventDescription' <$>
              (x .@? "RequestId") <*> (x .@? "TemplateName") <*>
                (x .@? "Severity")
                <*> (x .@? "VersionLabel")
                <*> (x .@? "PlatformArn")
                <*> (x .@? "EnvironmentName")
                <*> (x .@? "ApplicationName")
                <*> (x .@? "EventDate")
                <*> (x .@? "Message")

instance Hashable EventDescription where

instance NFData EventDescription where

-- | The description of an Amazon EC2 instance.
--
--
--
-- /See:/ 'instance'' smart constructor.
newtype Instance = Instance'
  { _iId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iId' - The ID of the Amazon EC2 instance.
instance'
    :: Instance
instance' = Instance' {_iId = Nothing}


-- | The ID of the Amazon EC2 instance.
iId :: Lens' Instance (Maybe Text)
iId = lens _iId (\ s a -> s{_iId = a})

instance FromXML Instance where
        parseXML x = Instance' <$> (x .@? "Id")

instance Hashable Instance where

instance NFData Instance where

-- | Represents summary information about the health of an instance. For more information, see <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
--
--
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstanceHealthSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ihsOK' - __Green.__ An instance is passing health checks and the health agent is not reporting any problems.
--
-- * 'ihsPending' - __Grey.__ An operation is in progress on an instance within the command timeout.
--
-- * 'ihsSevere' - __Red.__ The health agent is reporting a very high number of request failures or other issues for an instance or environment.
--
-- * 'ihsUnknown' - __Grey.__ AWS Elastic Beanstalk and the health agent are reporting an insufficient amount of data on an instance.
--
-- * 'ihsNoData' - __Grey.__ AWS Elastic Beanstalk and the health agent are reporting no data on an instance.
--
-- * 'ihsWarning' - __Yellow.__ The health agent is reporting a moderate number of request failures or other issues for an instance or environment.
--
-- * 'ihsDegraded' - __Red.__ The health agent is reporting a high number of request failures or other issues for an instance or environment.
--
-- * 'ihsInfo' - __Green.__ An operation is in progress on an instance.
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


-- | __Green.__ An instance is passing health checks and the health agent is not reporting any problems.
ihsOK :: Lens' InstanceHealthSummary (Maybe Int)
ihsOK = lens _ihsOK (\ s a -> s{_ihsOK = a})

-- | __Grey.__ An operation is in progress on an instance within the command timeout.
ihsPending :: Lens' InstanceHealthSummary (Maybe Int)
ihsPending = lens _ihsPending (\ s a -> s{_ihsPending = a})

-- | __Red.__ The health agent is reporting a very high number of request failures or other issues for an instance or environment.
ihsSevere :: Lens' InstanceHealthSummary (Maybe Int)
ihsSevere = lens _ihsSevere (\ s a -> s{_ihsSevere = a})

-- | __Grey.__ AWS Elastic Beanstalk and the health agent are reporting an insufficient amount of data on an instance.
ihsUnknown :: Lens' InstanceHealthSummary (Maybe Int)
ihsUnknown = lens _ihsUnknown (\ s a -> s{_ihsUnknown = a})

-- | __Grey.__ AWS Elastic Beanstalk and the health agent are reporting no data on an instance.
ihsNoData :: Lens' InstanceHealthSummary (Maybe Int)
ihsNoData = lens _ihsNoData (\ s a -> s{_ihsNoData = a})

-- | __Yellow.__ The health agent is reporting a moderate number of request failures or other issues for an instance or environment.
ihsWarning :: Lens' InstanceHealthSummary (Maybe Int)
ihsWarning = lens _ihsWarning (\ s a -> s{_ihsWarning = a})

-- | __Red.__ The health agent is reporting a high number of request failures or other issues for an instance or environment.
ihsDegraded :: Lens' InstanceHealthSummary (Maybe Int)
ihsDegraded = lens _ihsDegraded (\ s a -> s{_ihsDegraded = a})

-- | __Green.__ An operation is in progress on an instance.
ihsInfo :: Lens' InstanceHealthSummary (Maybe Int)
ihsInfo = lens _ihsInfo (\ s a -> s{_ihsInfo = a})

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

instance Hashable InstanceHealthSummary where

instance NFData InstanceHealthSummary where

-- | Represents the average latency for the slowest X percent of requests over the last 10 seconds.
--
--
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Latency' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lP75' - The average latency for the slowest 25 percent of requests over the last 10 seconds.
--
-- * 'lP50' - The average latency for the slowest 50 percent of requests over the last 10 seconds.
--
-- * 'lP85' - The average latency for the slowest 15 percent of requests over the last 10 seconds.
--
-- * 'lP999' - The average latency for the slowest 0.1 percent of requests over the last 10 seconds.
--
-- * 'lP90' - The average latency for the slowest 10 percent of requests over the last 10 seconds.
--
-- * 'lP95' - The average latency for the slowest 5 percent of requests over the last 10 seconds.
--
-- * 'lP99' - The average latency for the slowest 1 percent of requests over the last 10 seconds.
--
-- * 'lP10' - The average latency for the slowest 90 percent of requests over the last 10 seconds.
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


-- | The average latency for the slowest 25 percent of requests over the last 10 seconds.
lP75 :: Lens' Latency (Maybe Double)
lP75 = lens _lP75 (\ s a -> s{_lP75 = a})

-- | The average latency for the slowest 50 percent of requests over the last 10 seconds.
lP50 :: Lens' Latency (Maybe Double)
lP50 = lens _lP50 (\ s a -> s{_lP50 = a})

-- | The average latency for the slowest 15 percent of requests over the last 10 seconds.
lP85 :: Lens' Latency (Maybe Double)
lP85 = lens _lP85 (\ s a -> s{_lP85 = a})

-- | The average latency for the slowest 0.1 percent of requests over the last 10 seconds.
lP999 :: Lens' Latency (Maybe Double)
lP999 = lens _lP999 (\ s a -> s{_lP999 = a})

-- | The average latency for the slowest 10 percent of requests over the last 10 seconds.
lP90 :: Lens' Latency (Maybe Double)
lP90 = lens _lP90 (\ s a -> s{_lP90 = a})

-- | The average latency for the slowest 5 percent of requests over the last 10 seconds.
lP95 :: Lens' Latency (Maybe Double)
lP95 = lens _lP95 (\ s a -> s{_lP95 = a})

-- | The average latency for the slowest 1 percent of requests over the last 10 seconds.
lP99 :: Lens' Latency (Maybe Double)
lP99 = lens _lP99 (\ s a -> s{_lP99 = a})

-- | The average latency for the slowest 90 percent of requests over the last 10 seconds.
lP10 :: Lens' Latency (Maybe Double)
lP10 = lens _lP10 (\ s a -> s{_lP10 = a})

instance FromXML Latency where
        parseXML x
          = Latency' <$>
              (x .@? "P75") <*> (x .@? "P50") <*> (x .@? "P85") <*>
                (x .@? "P999")
                <*> (x .@? "P90")
                <*> (x .@? "P95")
                <*> (x .@? "P99")
                <*> (x .@? "P10")

instance Hashable Latency where

instance NFData Latency where

-- | Describes an Auto Scaling launch configuration.
--
--
--
-- /See:/ 'launchConfiguration' smart constructor.
newtype LaunchConfiguration = LaunchConfiguration'
  { _lcName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcName' - The name of the launch configuration.
launchConfiguration
    :: LaunchConfiguration
launchConfiguration = LaunchConfiguration' {_lcName = Nothing}


-- | The name of the launch configuration.
lcName :: Lens' LaunchConfiguration (Maybe Text)
lcName = lens _lcName (\ s a -> s{_lcName = a})

instance FromXML LaunchConfiguration where
        parseXML x = LaunchConfiguration' <$> (x .@? "Name")

instance Hashable LaunchConfiguration where

instance NFData LaunchConfiguration where

-- | Describes the properties of a Listener for the LoadBalancer.
--
--
--
-- /See:/ 'listener' smart constructor.
data Listener = Listener'
  { _lProtocol :: !(Maybe Text)
  , _lPort     :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Listener' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lProtocol' - The protocol that is used by the Listener.
--
-- * 'lPort' - The port that is used by the Listener.
listener
    :: Listener
listener = Listener' {_lProtocol = Nothing, _lPort = Nothing}


-- | The protocol that is used by the Listener.
lProtocol :: Lens' Listener (Maybe Text)
lProtocol = lens _lProtocol (\ s a -> s{_lProtocol = a})

-- | The port that is used by the Listener.
lPort :: Lens' Listener (Maybe Int)
lPort = lens _lPort (\ s a -> s{_lPort = a})

instance FromXML Listener where
        parseXML x
          = Listener' <$> (x .@? "Protocol") <*> (x .@? "Port")

instance Hashable Listener where

instance NFData Listener where

-- | Describes a LoadBalancer.
--
--
--
-- /See:/ 'loadBalancer' smart constructor.
newtype LoadBalancer = LoadBalancer'
  { _lbName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadBalancer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbName' - The name of the LoadBalancer.
loadBalancer
    :: LoadBalancer
loadBalancer = LoadBalancer' {_lbName = Nothing}


-- | The name of the LoadBalancer.
lbName :: Lens' LoadBalancer (Maybe Text)
lbName = lens _lbName (\ s a -> s{_lbName = a})

instance FromXML LoadBalancer where
        parseXML x = LoadBalancer' <$> (x .@? "Name")

instance Hashable LoadBalancer where

instance NFData LoadBalancer where

-- | Describes the details of a LoadBalancer.
--
--
--
-- /See:/ 'loadBalancerDescription' smart constructor.
data LoadBalancerDescription = LoadBalancerDescription'
  { _lbdLoadBalancerName :: !(Maybe Text)
  , _lbdDomain           :: !(Maybe Text)
  , _lbdListeners        :: !(Maybe [Listener])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LoadBalancerDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbdLoadBalancerName' - The name of the LoadBalancer.
--
-- * 'lbdDomain' - The domain name of the LoadBalancer.
--
-- * 'lbdListeners' - A list of Listeners used by the LoadBalancer.
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
lbdLoadBalancerName = lens _lbdLoadBalancerName (\ s a -> s{_lbdLoadBalancerName = a})

-- | The domain name of the LoadBalancer.
lbdDomain :: Lens' LoadBalancerDescription (Maybe Text)
lbdDomain = lens _lbdDomain (\ s a -> s{_lbdDomain = a})

-- | A list of Listeners used by the LoadBalancer.
lbdListeners :: Lens' LoadBalancerDescription [Listener]
lbdListeners = lens _lbdListeners (\ s a -> s{_lbdListeners = a}) . _Default . _Coerce

instance FromXML LoadBalancerDescription where
        parseXML x
          = LoadBalancerDescription' <$>
              (x .@? "LoadBalancerName") <*> (x .@? "Domain") <*>
                (x .@? "Listeners" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable LoadBalancerDescription where

instance NFData LoadBalancerDescription where

-- | The record of an upcoming or in-progress managed action.
--
--
--
-- /See:/ 'managedAction' smart constructor.
data ManagedAction = ManagedAction'
  { _maStatus            :: !(Maybe ActionStatus)
  , _maActionId          :: !(Maybe Text)
  , _maWindowStartTime   :: !(Maybe ISO8601)
  , _maActionDescription :: !(Maybe Text)
  , _maActionType        :: !(Maybe ActionType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ManagedAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'maStatus' - The status of the managed action. If the action is @Scheduled@ , you can apply it immediately with 'ApplyEnvironmentManagedAction' .
--
-- * 'maActionId' - A unique identifier for the managed action.
--
-- * 'maWindowStartTime' - The start time of the maintenance window in which the managed action will execute.
--
-- * 'maActionDescription' - A description of the managed action.
--
-- * 'maActionType' - The type of managed action.
managedAction
    :: ManagedAction
managedAction =
  ManagedAction'
    { _maStatus = Nothing
    , _maActionId = Nothing
    , _maWindowStartTime = Nothing
    , _maActionDescription = Nothing
    , _maActionType = Nothing
    }


-- | The status of the managed action. If the action is @Scheduled@ , you can apply it immediately with 'ApplyEnvironmentManagedAction' .
maStatus :: Lens' ManagedAction (Maybe ActionStatus)
maStatus = lens _maStatus (\ s a -> s{_maStatus = a})

-- | A unique identifier for the managed action.
maActionId :: Lens' ManagedAction (Maybe Text)
maActionId = lens _maActionId (\ s a -> s{_maActionId = a})

-- | The start time of the maintenance window in which the managed action will execute.
maWindowStartTime :: Lens' ManagedAction (Maybe UTCTime)
maWindowStartTime = lens _maWindowStartTime (\ s a -> s{_maWindowStartTime = a}) . mapping _Time

-- | A description of the managed action.
maActionDescription :: Lens' ManagedAction (Maybe Text)
maActionDescription = lens _maActionDescription (\ s a -> s{_maActionDescription = a})

-- | The type of managed action.
maActionType :: Lens' ManagedAction (Maybe ActionType)
maActionType = lens _maActionType (\ s a -> s{_maActionType = a})

instance FromXML ManagedAction where
        parseXML x
          = ManagedAction' <$>
              (x .@? "Status") <*> (x .@? "ActionId") <*>
                (x .@? "WindowStartTime")
                <*> (x .@? "ActionDescription")
                <*> (x .@? "ActionType")

instance Hashable ManagedAction where

instance NFData ManagedAction where

-- | The record of a completed or failed managed action.
--
--
--
-- /See:/ 'managedActionHistoryItem' smart constructor.
data ManagedActionHistoryItem = ManagedActionHistoryItem'
  { _mahiStatus             :: !(Maybe ActionHistoryStatus)
  , _mahiFailureType        :: !(Maybe FailureType)
  , _mahiActionId           :: !(Maybe Text)
  , _mahiFailureDescription :: !(Maybe Text)
  , _mahiFinishedTime       :: !(Maybe ISO8601)
  , _mahiActionDescription  :: !(Maybe Text)
  , _mahiExecutedTime       :: !(Maybe ISO8601)
  , _mahiActionType         :: !(Maybe ActionType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ManagedActionHistoryItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mahiStatus' - The status of the action.
--
-- * 'mahiFailureType' - If the action failed, the type of failure.
--
-- * 'mahiActionId' - A unique identifier for the managed action.
--
-- * 'mahiFailureDescription' - If the action failed, a description of the failure.
--
-- * 'mahiFinishedTime' - The date and time that the action finished executing.
--
-- * 'mahiActionDescription' - A description of the managed action.
--
-- * 'mahiExecutedTime' - The date and time that the action started executing.
--
-- * 'mahiActionType' - The type of the managed action.
managedActionHistoryItem
    :: ManagedActionHistoryItem
managedActionHistoryItem =
  ManagedActionHistoryItem'
    { _mahiStatus = Nothing
    , _mahiFailureType = Nothing
    , _mahiActionId = Nothing
    , _mahiFailureDescription = Nothing
    , _mahiFinishedTime = Nothing
    , _mahiActionDescription = Nothing
    , _mahiExecutedTime = Nothing
    , _mahiActionType = Nothing
    }


-- | The status of the action.
mahiStatus :: Lens' ManagedActionHistoryItem (Maybe ActionHistoryStatus)
mahiStatus = lens _mahiStatus (\ s a -> s{_mahiStatus = a})

-- | If the action failed, the type of failure.
mahiFailureType :: Lens' ManagedActionHistoryItem (Maybe FailureType)
mahiFailureType = lens _mahiFailureType (\ s a -> s{_mahiFailureType = a})

-- | A unique identifier for the managed action.
mahiActionId :: Lens' ManagedActionHistoryItem (Maybe Text)
mahiActionId = lens _mahiActionId (\ s a -> s{_mahiActionId = a})

-- | If the action failed, a description of the failure.
mahiFailureDescription :: Lens' ManagedActionHistoryItem (Maybe Text)
mahiFailureDescription = lens _mahiFailureDescription (\ s a -> s{_mahiFailureDescription = a})

-- | The date and time that the action finished executing.
mahiFinishedTime :: Lens' ManagedActionHistoryItem (Maybe UTCTime)
mahiFinishedTime = lens _mahiFinishedTime (\ s a -> s{_mahiFinishedTime = a}) . mapping _Time

-- | A description of the managed action.
mahiActionDescription :: Lens' ManagedActionHistoryItem (Maybe Text)
mahiActionDescription = lens _mahiActionDescription (\ s a -> s{_mahiActionDescription = a})

-- | The date and time that the action started executing.
mahiExecutedTime :: Lens' ManagedActionHistoryItem (Maybe UTCTime)
mahiExecutedTime = lens _mahiExecutedTime (\ s a -> s{_mahiExecutedTime = a}) . mapping _Time

-- | The type of the managed action.
mahiActionType :: Lens' ManagedActionHistoryItem (Maybe ActionType)
mahiActionType = lens _mahiActionType (\ s a -> s{_mahiActionType = a})

instance FromXML ManagedActionHistoryItem where
        parseXML x
          = ManagedActionHistoryItem' <$>
              (x .@? "Status") <*> (x .@? "FailureType") <*>
                (x .@? "ActionId")
                <*> (x .@? "FailureDescription")
                <*> (x .@? "FinishedTime")
                <*> (x .@? "ActionDescription")
                <*> (x .@? "ExecutedTime")
                <*> (x .@? "ActionType")

instance Hashable ManagedActionHistoryItem where

instance NFData ManagedActionHistoryItem where

-- | A lifecycle rule that deletes application versions after the specified number of days.
--
--
--
-- /See:/ 'maxAgeRule' smart constructor.
data MaxAgeRule = MaxAgeRule'
  { _marDeleteSourceFromS3 :: !(Maybe Bool)
  , _marMaxAgeInDays       :: !(Maybe Int)
  , _marEnabled            :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MaxAgeRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'marDeleteSourceFromS3' - Set to @true@ to delete a version's source bundle from Amazon S3 when Elastic Beanstalk deletes the application version.
--
-- * 'marMaxAgeInDays' - Specify the number of days to retain an application versions.
--
-- * 'marEnabled' - Specify @true@ to apply the rule, or @false@ to disable it.
maxAgeRule
    :: Bool -- ^ 'marEnabled'
    -> MaxAgeRule
maxAgeRule pEnabled_ =
  MaxAgeRule'
    { _marDeleteSourceFromS3 = Nothing
    , _marMaxAgeInDays = Nothing
    , _marEnabled = pEnabled_
    }


-- | Set to @true@ to delete a version's source bundle from Amazon S3 when Elastic Beanstalk deletes the application version.
marDeleteSourceFromS3 :: Lens' MaxAgeRule (Maybe Bool)
marDeleteSourceFromS3 = lens _marDeleteSourceFromS3 (\ s a -> s{_marDeleteSourceFromS3 = a})

-- | Specify the number of days to retain an application versions.
marMaxAgeInDays :: Lens' MaxAgeRule (Maybe Int)
marMaxAgeInDays = lens _marMaxAgeInDays (\ s a -> s{_marMaxAgeInDays = a})

-- | Specify @true@ to apply the rule, or @false@ to disable it.
marEnabled :: Lens' MaxAgeRule Bool
marEnabled = lens _marEnabled (\ s a -> s{_marEnabled = a})

instance FromXML MaxAgeRule where
        parseXML x
          = MaxAgeRule' <$>
              (x .@? "DeleteSourceFromS3") <*>
                (x .@? "MaxAgeInDays")
                <*> (x .@ "Enabled")

instance Hashable MaxAgeRule where

instance NFData MaxAgeRule where

instance ToQuery MaxAgeRule where
        toQuery MaxAgeRule'{..}
          = mconcat
              ["DeleteSourceFromS3" =: _marDeleteSourceFromS3,
               "MaxAgeInDays" =: _marMaxAgeInDays,
               "Enabled" =: _marEnabled]

-- | A lifecycle rule that deletes the oldest application version when the maximum count is exceeded.
--
--
--
-- /See:/ 'maxCountRule' smart constructor.
data MaxCountRule = MaxCountRule'
  { _mcrMaxCount           :: !(Maybe Int)
  , _mcrDeleteSourceFromS3 :: !(Maybe Bool)
  , _mcrEnabled            :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MaxCountRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcrMaxCount' - Specify the maximum number of application versions to retain.
--
-- * 'mcrDeleteSourceFromS3' - Set to @true@ to delete a version's source bundle from Amazon S3 when Elastic Beanstalk deletes the application version.
--
-- * 'mcrEnabled' - Specify @true@ to apply the rule, or @false@ to disable it.
maxCountRule
    :: Bool -- ^ 'mcrEnabled'
    -> MaxCountRule
maxCountRule pEnabled_ =
  MaxCountRule'
    { _mcrMaxCount = Nothing
    , _mcrDeleteSourceFromS3 = Nothing
    , _mcrEnabled = pEnabled_
    }


-- | Specify the maximum number of application versions to retain.
mcrMaxCount :: Lens' MaxCountRule (Maybe Int)
mcrMaxCount = lens _mcrMaxCount (\ s a -> s{_mcrMaxCount = a})

-- | Set to @true@ to delete a version's source bundle from Amazon S3 when Elastic Beanstalk deletes the application version.
mcrDeleteSourceFromS3 :: Lens' MaxCountRule (Maybe Bool)
mcrDeleteSourceFromS3 = lens _mcrDeleteSourceFromS3 (\ s a -> s{_mcrDeleteSourceFromS3 = a})

-- | Specify @true@ to apply the rule, or @false@ to disable it.
mcrEnabled :: Lens' MaxCountRule Bool
mcrEnabled = lens _mcrEnabled (\ s a -> s{_mcrEnabled = a})

instance FromXML MaxCountRule where
        parseXML x
          = MaxCountRule' <$>
              (x .@? "MaxCount") <*> (x .@? "DeleteSourceFromS3")
                <*> (x .@ "Enabled")

instance Hashable MaxCountRule where

instance NFData MaxCountRule where

instance ToQuery MaxCountRule where
        toQuery MaxCountRule'{..}
          = mconcat
              ["MaxCount" =: _mcrMaxCount,
               "DeleteSourceFromS3" =: _mcrDeleteSourceFromS3,
               "Enabled" =: _mcrEnabled]

-- | A regular expression representing a restriction on a string configuration option value.
--
--
--
-- /See:/ 'optionRestrictionRegex' smart constructor.
data OptionRestrictionRegex = OptionRestrictionRegex'
  { _orrPattern :: !(Maybe Text)
  , _orrLabel   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OptionRestrictionRegex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orrPattern' - The regular expression pattern that a string configuration option value with this restriction must match.
--
-- * 'orrLabel' - A unique name representing this regular expression.
optionRestrictionRegex
    :: OptionRestrictionRegex
optionRestrictionRegex =
  OptionRestrictionRegex' {_orrPattern = Nothing, _orrLabel = Nothing}


-- | The regular expression pattern that a string configuration option value with this restriction must match.
orrPattern :: Lens' OptionRestrictionRegex (Maybe Text)
orrPattern = lens _orrPattern (\ s a -> s{_orrPattern = a})

-- | A unique name representing this regular expression.
orrLabel :: Lens' OptionRestrictionRegex (Maybe Text)
orrLabel = lens _orrLabel (\ s a -> s{_orrLabel = a})

instance FromXML OptionRestrictionRegex where
        parseXML x
          = OptionRestrictionRegex' <$>
              (x .@? "Pattern") <*> (x .@? "Label")

instance Hashable OptionRestrictionRegex where

instance NFData OptionRestrictionRegex where

-- | A specification identifying an individual configuration option.
--
--
--
-- /See:/ 'optionSpecification' smart constructor.
data OptionSpecification = OptionSpecification'
  { _osOptionName   :: !(Maybe Text)
  , _osResourceName :: !(Maybe Text)
  , _osNamespace    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OptionSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osOptionName' - The name of the configuration option.
--
-- * 'osResourceName' - A unique resource name for a time-based scaling configuration option.
--
-- * 'osNamespace' - A unique namespace identifying the option's associated AWS resource.
optionSpecification
    :: OptionSpecification
optionSpecification =
  OptionSpecification'
    {_osOptionName = Nothing, _osResourceName = Nothing, _osNamespace = Nothing}


-- | The name of the configuration option.
osOptionName :: Lens' OptionSpecification (Maybe Text)
osOptionName = lens _osOptionName (\ s a -> s{_osOptionName = a})

-- | A unique resource name for a time-based scaling configuration option.
osResourceName :: Lens' OptionSpecification (Maybe Text)
osResourceName = lens _osResourceName (\ s a -> s{_osResourceName = a})

-- | A unique namespace identifying the option's associated AWS resource.
osNamespace :: Lens' OptionSpecification (Maybe Text)
osNamespace = lens _osNamespace (\ s a -> s{_osNamespace = a})

instance Hashable OptionSpecification where

instance NFData OptionSpecification where

instance ToQuery OptionSpecification where
        toQuery OptionSpecification'{..}
          = mconcat
              ["OptionName" =: _osOptionName,
               "ResourceName" =: _osResourceName,
               "Namespace" =: _osNamespace]

-- | Detailed information about a platform.
--
--
--
-- /See:/ 'platformDescription' smart constructor.
data PlatformDescription = PlatformDescription'
  { _pdSupportedAddonList     :: !(Maybe [Text])
  , _pdPlatformCategory       :: !(Maybe Text)
  , _pdPlatformVersion        :: !(Maybe Text)
  , _pdPlatformStatus         :: !(Maybe PlatformStatus)
  , _pdMaintainer             :: !(Maybe Text)
  , _pdPlatformOwner          :: !(Maybe Text)
  , _pdDateUpdated            :: !(Maybe ISO8601)
  , _pdCustomAMIList          :: !(Maybe [CustomAMI])
  , _pdDateCreated            :: !(Maybe ISO8601)
  , _pdOperatingSystemName    :: !(Maybe Text)
  , _pdFrameworks             :: !(Maybe [PlatformFramework])
  , _pdPlatformARN            :: !(Maybe Text)
  , _pdOperatingSystemVersion :: !(Maybe Text)
  , _pdProgrammingLanguages   :: !(Maybe [PlatformProgrammingLanguage])
  , _pdSolutionStackName      :: !(Maybe Text)
  , _pdPlatformName           :: !(Maybe Text)
  , _pdDescription            :: !(Maybe Text)
  , _pdSupportedTierList      :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PlatformDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdSupportedAddonList' - The additions supported by the platform.
--
-- * 'pdPlatformCategory' - The category of the platform.
--
-- * 'pdPlatformVersion' - The version of the platform.
--
-- * 'pdPlatformStatus' - The status of the platform.
--
-- * 'pdMaintainer' - Information about the maintainer of the platform.
--
-- * 'pdPlatformOwner' - The AWS account ID of the person who created the platform.
--
-- * 'pdDateUpdated' - The date when the platform was last updated.
--
-- * 'pdCustomAMIList' - The custom AMIs supported by the platform.
--
-- * 'pdDateCreated' - The date when the platform was created.
--
-- * 'pdOperatingSystemName' - The operating system used by the platform.
--
-- * 'pdFrameworks' - The frameworks supported by the platform.
--
-- * 'pdPlatformARN' - The ARN of the platform.
--
-- * 'pdOperatingSystemVersion' - The version of the operating system used by the platform.
--
-- * 'pdProgrammingLanguages' - The programming languages supported by the platform.
--
-- * 'pdSolutionStackName' - The name of the solution stack used by the platform.
--
-- * 'pdPlatformName' - The name of the platform.
--
-- * 'pdDescription' - The description of the platform.
--
-- * 'pdSupportedTierList' - The tiers supported by the platform.
platformDescription
    :: PlatformDescription
platformDescription =
  PlatformDescription'
    { _pdSupportedAddonList = Nothing
    , _pdPlatformCategory = Nothing
    , _pdPlatformVersion = Nothing
    , _pdPlatformStatus = Nothing
    , _pdMaintainer = Nothing
    , _pdPlatformOwner = Nothing
    , _pdDateUpdated = Nothing
    , _pdCustomAMIList = Nothing
    , _pdDateCreated = Nothing
    , _pdOperatingSystemName = Nothing
    , _pdFrameworks = Nothing
    , _pdPlatformARN = Nothing
    , _pdOperatingSystemVersion = Nothing
    , _pdProgrammingLanguages = Nothing
    , _pdSolutionStackName = Nothing
    , _pdPlatformName = Nothing
    , _pdDescription = Nothing
    , _pdSupportedTierList = Nothing
    }


-- | The additions supported by the platform.
pdSupportedAddonList :: Lens' PlatformDescription [Text]
pdSupportedAddonList = lens _pdSupportedAddonList (\ s a -> s{_pdSupportedAddonList = a}) . _Default . _Coerce

-- | The category of the platform.
pdPlatformCategory :: Lens' PlatformDescription (Maybe Text)
pdPlatformCategory = lens _pdPlatformCategory (\ s a -> s{_pdPlatformCategory = a})

-- | The version of the platform.
pdPlatformVersion :: Lens' PlatformDescription (Maybe Text)
pdPlatformVersion = lens _pdPlatformVersion (\ s a -> s{_pdPlatformVersion = a})

-- | The status of the platform.
pdPlatformStatus :: Lens' PlatformDescription (Maybe PlatformStatus)
pdPlatformStatus = lens _pdPlatformStatus (\ s a -> s{_pdPlatformStatus = a})

-- | Information about the maintainer of the platform.
pdMaintainer :: Lens' PlatformDescription (Maybe Text)
pdMaintainer = lens _pdMaintainer (\ s a -> s{_pdMaintainer = a})

-- | The AWS account ID of the person who created the platform.
pdPlatformOwner :: Lens' PlatformDescription (Maybe Text)
pdPlatformOwner = lens _pdPlatformOwner (\ s a -> s{_pdPlatformOwner = a})

-- | The date when the platform was last updated.
pdDateUpdated :: Lens' PlatformDescription (Maybe UTCTime)
pdDateUpdated = lens _pdDateUpdated (\ s a -> s{_pdDateUpdated = a}) . mapping _Time

-- | The custom AMIs supported by the platform.
pdCustomAMIList :: Lens' PlatformDescription [CustomAMI]
pdCustomAMIList = lens _pdCustomAMIList (\ s a -> s{_pdCustomAMIList = a}) . _Default . _Coerce

-- | The date when the platform was created.
pdDateCreated :: Lens' PlatformDescription (Maybe UTCTime)
pdDateCreated = lens _pdDateCreated (\ s a -> s{_pdDateCreated = a}) . mapping _Time

-- | The operating system used by the platform.
pdOperatingSystemName :: Lens' PlatformDescription (Maybe Text)
pdOperatingSystemName = lens _pdOperatingSystemName (\ s a -> s{_pdOperatingSystemName = a})

-- | The frameworks supported by the platform.
pdFrameworks :: Lens' PlatformDescription [PlatformFramework]
pdFrameworks = lens _pdFrameworks (\ s a -> s{_pdFrameworks = a}) . _Default . _Coerce

-- | The ARN of the platform.
pdPlatformARN :: Lens' PlatformDescription (Maybe Text)
pdPlatformARN = lens _pdPlatformARN (\ s a -> s{_pdPlatformARN = a})

-- | The version of the operating system used by the platform.
pdOperatingSystemVersion :: Lens' PlatformDescription (Maybe Text)
pdOperatingSystemVersion = lens _pdOperatingSystemVersion (\ s a -> s{_pdOperatingSystemVersion = a})

-- | The programming languages supported by the platform.
pdProgrammingLanguages :: Lens' PlatformDescription [PlatformProgrammingLanguage]
pdProgrammingLanguages = lens _pdProgrammingLanguages (\ s a -> s{_pdProgrammingLanguages = a}) . _Default . _Coerce

-- | The name of the solution stack used by the platform.
pdSolutionStackName :: Lens' PlatformDescription (Maybe Text)
pdSolutionStackName = lens _pdSolutionStackName (\ s a -> s{_pdSolutionStackName = a})

-- | The name of the platform.
pdPlatformName :: Lens' PlatformDescription (Maybe Text)
pdPlatformName = lens _pdPlatformName (\ s a -> s{_pdPlatformName = a})

-- | The description of the platform.
pdDescription :: Lens' PlatformDescription (Maybe Text)
pdDescription = lens _pdDescription (\ s a -> s{_pdDescription = a})

-- | The tiers supported by the platform.
pdSupportedTierList :: Lens' PlatformDescription [Text]
pdSupportedTierList = lens _pdSupportedTierList (\ s a -> s{_pdSupportedTierList = a}) . _Default . _Coerce

instance FromXML PlatformDescription where
        parseXML x
          = PlatformDescription' <$>
              (x .@? "SupportedAddonList" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "PlatformCategory")
                <*> (x .@? "PlatformVersion")
                <*> (x .@? "PlatformStatus")
                <*> (x .@? "Maintainer")
                <*> (x .@? "PlatformOwner")
                <*> (x .@? "DateUpdated")
                <*>
                (x .@? "CustomAmiList" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "DateCreated")
                <*> (x .@? "OperatingSystemName")
                <*>
                (x .@? "Frameworks" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "PlatformArn")
                <*> (x .@? "OperatingSystemVersion")
                <*>
                (x .@? "ProgrammingLanguages" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "SolutionStackName")
                <*> (x .@? "PlatformName")
                <*> (x .@? "Description")
                <*>
                (x .@? "SupportedTierList" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable PlatformDescription where

instance NFData PlatformDescription where

-- | Specify criteria to restrict the results when listing custom platforms.
--
--
-- The filter is evaluated as the expression:
--
-- @Type@ @Operator@ @Values[i]@
--
--
-- /See:/ 'platformFilter' smart constructor.
data PlatformFilter = PlatformFilter'
  { _pfValues   :: !(Maybe [Text])
  , _pfOperator :: !(Maybe Text)
  , _pfType     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PlatformFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfValues' - The list of values applied to the custom platform attribute.
--
-- * 'pfOperator' - The operator to apply to the @Type@ with each of the @Values@ . Valid Values: @=@ (equal to) | @!=@ (not equal to) | @<@ (less than) | @<=@ (less than or equal to) | @>@ (greater than) | @>=@ (greater than or equal to) | @contains@ | @begins_with@ | @ends_with@
--
-- * 'pfType' - The custom platform attribute to which the filter values are applied. Valid Values: @PlatformName@ | @PlatformVersion@ | @PlatformStatus@ | @PlatformOwner@
platformFilter
    :: PlatformFilter
platformFilter =
  PlatformFilter'
    {_pfValues = Nothing, _pfOperator = Nothing, _pfType = Nothing}


-- | The list of values applied to the custom platform attribute.
pfValues :: Lens' PlatformFilter [Text]
pfValues = lens _pfValues (\ s a -> s{_pfValues = a}) . _Default . _Coerce

-- | The operator to apply to the @Type@ with each of the @Values@ . Valid Values: @=@ (equal to) | @!=@ (not equal to) | @<@ (less than) | @<=@ (less than or equal to) | @>@ (greater than) | @>=@ (greater than or equal to) | @contains@ | @begins_with@ | @ends_with@
pfOperator :: Lens' PlatformFilter (Maybe Text)
pfOperator = lens _pfOperator (\ s a -> s{_pfOperator = a})

-- | The custom platform attribute to which the filter values are applied. Valid Values: @PlatformName@ | @PlatformVersion@ | @PlatformStatus@ | @PlatformOwner@
pfType :: Lens' PlatformFilter (Maybe Text)
pfType = lens _pfType (\ s a -> s{_pfType = a})

instance Hashable PlatformFilter where

instance NFData PlatformFilter where

instance ToQuery PlatformFilter where
        toQuery PlatformFilter'{..}
          = mconcat
              ["Values" =:
                 toQuery (toQueryList "member" <$> _pfValues),
               "Operator" =: _pfOperator, "Type" =: _pfType]

-- | A framework supported by the custom platform.
--
--
--
-- /See:/ 'platformFramework' smart constructor.
data PlatformFramework = PlatformFramework'
  { _pfName    :: !(Maybe Text)
  , _pfVersion :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PlatformFramework' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfName' - The name of the framework.
--
-- * 'pfVersion' - The version of the framework.
platformFramework
    :: PlatformFramework
platformFramework = PlatformFramework' {_pfName = Nothing, _pfVersion = Nothing}


-- | The name of the framework.
pfName :: Lens' PlatformFramework (Maybe Text)
pfName = lens _pfName (\ s a -> s{_pfName = a})

-- | The version of the framework.
pfVersion :: Lens' PlatformFramework (Maybe Text)
pfVersion = lens _pfVersion (\ s a -> s{_pfVersion = a})

instance FromXML PlatformFramework where
        parseXML x
          = PlatformFramework' <$>
              (x .@? "Name") <*> (x .@? "Version")

instance Hashable PlatformFramework where

instance NFData PlatformFramework where

-- | A programming language supported by the platform.
--
--
--
-- /See:/ 'platformProgrammingLanguage' smart constructor.
data PlatformProgrammingLanguage = PlatformProgrammingLanguage'
  { _pplName    :: !(Maybe Text)
  , _pplVersion :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PlatformProgrammingLanguage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pplName' - The name of the programming language.
--
-- * 'pplVersion' - The version of the programming language.
platformProgrammingLanguage
    :: PlatformProgrammingLanguage
platformProgrammingLanguage =
  PlatformProgrammingLanguage' {_pplName = Nothing, _pplVersion = Nothing}


-- | The name of the programming language.
pplName :: Lens' PlatformProgrammingLanguage (Maybe Text)
pplName = lens _pplName (\ s a -> s{_pplName = a})

-- | The version of the programming language.
pplVersion :: Lens' PlatformProgrammingLanguage (Maybe Text)
pplVersion = lens _pplVersion (\ s a -> s{_pplVersion = a})

instance FromXML PlatformProgrammingLanguage where
        parseXML x
          = PlatformProgrammingLanguage' <$>
              (x .@? "Name") <*> (x .@? "Version")

instance Hashable PlatformProgrammingLanguage where

instance NFData PlatformProgrammingLanguage where

-- | Detailed information about a platform.
--
--
--
-- /See:/ 'platformSummary' smart constructor.
data PlatformSummary = PlatformSummary'
  { _psSupportedAddonList     :: !(Maybe [Text])
  , _psPlatformCategory       :: !(Maybe Text)
  , _psPlatformStatus         :: !(Maybe PlatformStatus)
  , _psPlatformOwner          :: !(Maybe Text)
  , _psOperatingSystemName    :: !(Maybe Text)
  , _psPlatformARN            :: !(Maybe Text)
  , _psOperatingSystemVersion :: !(Maybe Text)
  , _psSupportedTierList      :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PlatformSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psSupportedAddonList' - The additions associated with the platform.
--
-- * 'psPlatformCategory' - The category of platform.
--
-- * 'psPlatformStatus' - The status of the platform. You can create an environment from the platform once it is ready.
--
-- * 'psPlatformOwner' - The AWS account ID of the person who created the platform.
--
-- * 'psOperatingSystemName' - The operating system used by the platform.
--
-- * 'psPlatformARN' - The ARN of the platform.
--
-- * 'psOperatingSystemVersion' - The version of the operating system used by the platform.
--
-- * 'psSupportedTierList' - The tiers in which the platform runs.
platformSummary
    :: PlatformSummary
platformSummary =
  PlatformSummary'
    { _psSupportedAddonList = Nothing
    , _psPlatformCategory = Nothing
    , _psPlatformStatus = Nothing
    , _psPlatformOwner = Nothing
    , _psOperatingSystemName = Nothing
    , _psPlatformARN = Nothing
    , _psOperatingSystemVersion = Nothing
    , _psSupportedTierList = Nothing
    }


-- | The additions associated with the platform.
psSupportedAddonList :: Lens' PlatformSummary [Text]
psSupportedAddonList = lens _psSupportedAddonList (\ s a -> s{_psSupportedAddonList = a}) . _Default . _Coerce

-- | The category of platform.
psPlatformCategory :: Lens' PlatformSummary (Maybe Text)
psPlatformCategory = lens _psPlatformCategory (\ s a -> s{_psPlatformCategory = a})

-- | The status of the platform. You can create an environment from the platform once it is ready.
psPlatformStatus :: Lens' PlatformSummary (Maybe PlatformStatus)
psPlatformStatus = lens _psPlatformStatus (\ s a -> s{_psPlatformStatus = a})

-- | The AWS account ID of the person who created the platform.
psPlatformOwner :: Lens' PlatformSummary (Maybe Text)
psPlatformOwner = lens _psPlatformOwner (\ s a -> s{_psPlatformOwner = a})

-- | The operating system used by the platform.
psOperatingSystemName :: Lens' PlatformSummary (Maybe Text)
psOperatingSystemName = lens _psOperatingSystemName (\ s a -> s{_psOperatingSystemName = a})

-- | The ARN of the platform.
psPlatformARN :: Lens' PlatformSummary (Maybe Text)
psPlatformARN = lens _psPlatformARN (\ s a -> s{_psPlatformARN = a})

-- | The version of the operating system used by the platform.
psOperatingSystemVersion :: Lens' PlatformSummary (Maybe Text)
psOperatingSystemVersion = lens _psOperatingSystemVersion (\ s a -> s{_psOperatingSystemVersion = a})

-- | The tiers in which the platform runs.
psSupportedTierList :: Lens' PlatformSummary [Text]
psSupportedTierList = lens _psSupportedTierList (\ s a -> s{_psSupportedTierList = a}) . _Default . _Coerce

instance FromXML PlatformSummary where
        parseXML x
          = PlatformSummary' <$>
              (x .@? "SupportedAddonList" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "PlatformCategory")
                <*> (x .@? "PlatformStatus")
                <*> (x .@? "PlatformOwner")
                <*> (x .@? "OperatingSystemName")
                <*> (x .@? "PlatformArn")
                <*> (x .@? "OperatingSystemVersion")
                <*>
                (x .@? "SupportedTierList" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable PlatformSummary where

instance NFData PlatformSummary where

-- | Describes a queue.
--
--
--
-- /See:/ 'queue' smart constructor.
data Queue = Queue'
  { _qURL  :: !(Maybe Text)
  , _qName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Queue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qURL' - The URL of the queue.
--
-- * 'qName' - The name of the queue.
queue
    :: Queue
queue = Queue' {_qURL = Nothing, _qName = Nothing}


-- | The URL of the queue.
qURL :: Lens' Queue (Maybe Text)
qURL = lens _qURL (\ s a -> s{_qURL = a})

-- | The name of the queue.
qName :: Lens' Queue (Maybe Text)
qName = lens _qName (\ s a -> s{_qName = a})

instance FromXML Queue where
        parseXML x
          = Queue' <$> (x .@? "URL") <*> (x .@? "Name")

instance Hashable Queue where

instance NFData Queue where

-- | The AWS Elastic Beanstalk quota information for a single resource type in an AWS account. It reflects the resource's limits for this account.
--
--
--
-- /See:/ 'resourceQuota' smart constructor.
newtype ResourceQuota = ResourceQuota'
  { _rqMaximum :: Maybe Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceQuota' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rqMaximum' - The maximum number of instances of this Elastic Beanstalk resource type that an AWS account can use.
resourceQuota
    :: ResourceQuota
resourceQuota = ResourceQuota' {_rqMaximum = Nothing}


-- | The maximum number of instances of this Elastic Beanstalk resource type that an AWS account can use.
rqMaximum :: Lens' ResourceQuota (Maybe Int)
rqMaximum = lens _rqMaximum (\ s a -> s{_rqMaximum = a})

instance FromXML ResourceQuota where
        parseXML x = ResourceQuota' <$> (x .@? "Maximum")

instance Hashable ResourceQuota where

instance NFData ResourceQuota where

-- | A set of per-resource AWS Elastic Beanstalk quotas associated with an AWS account. They reflect Elastic Beanstalk resource limits for this account.
--
--
--
-- /See:/ 'resourceQuotas' smart constructor.
data ResourceQuotas = ResourceQuotas'
  { _rqApplicationQuota           :: !(Maybe ResourceQuota)
  , _rqCustomPlatformQuota        :: !(Maybe ResourceQuota)
  , _rqApplicationVersionQuota    :: !(Maybe ResourceQuota)
  , _rqEnvironmentQuota           :: !(Maybe ResourceQuota)
  , _rqConfigurationTemplateQuota :: !(Maybe ResourceQuota)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceQuotas' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rqApplicationQuota' - The quota for applications in the AWS account.
--
-- * 'rqCustomPlatformQuota' - The quota for custom platforms in the AWS account.
--
-- * 'rqApplicationVersionQuota' - The quota for application versions in the AWS account.
--
-- * 'rqEnvironmentQuota' - The quota for environments in the AWS account.
--
-- * 'rqConfigurationTemplateQuota' - The quota for configuration templates in the AWS account.
resourceQuotas
    :: ResourceQuotas
resourceQuotas =
  ResourceQuotas'
    { _rqApplicationQuota = Nothing
    , _rqCustomPlatformQuota = Nothing
    , _rqApplicationVersionQuota = Nothing
    , _rqEnvironmentQuota = Nothing
    , _rqConfigurationTemplateQuota = Nothing
    }


-- | The quota for applications in the AWS account.
rqApplicationQuota :: Lens' ResourceQuotas (Maybe ResourceQuota)
rqApplicationQuota = lens _rqApplicationQuota (\ s a -> s{_rqApplicationQuota = a})

-- | The quota for custom platforms in the AWS account.
rqCustomPlatformQuota :: Lens' ResourceQuotas (Maybe ResourceQuota)
rqCustomPlatformQuota = lens _rqCustomPlatformQuota (\ s a -> s{_rqCustomPlatformQuota = a})

-- | The quota for application versions in the AWS account.
rqApplicationVersionQuota :: Lens' ResourceQuotas (Maybe ResourceQuota)
rqApplicationVersionQuota = lens _rqApplicationVersionQuota (\ s a -> s{_rqApplicationVersionQuota = a})

-- | The quota for environments in the AWS account.
rqEnvironmentQuota :: Lens' ResourceQuotas (Maybe ResourceQuota)
rqEnvironmentQuota = lens _rqEnvironmentQuota (\ s a -> s{_rqEnvironmentQuota = a})

-- | The quota for configuration templates in the AWS account.
rqConfigurationTemplateQuota :: Lens' ResourceQuotas (Maybe ResourceQuota)
rqConfigurationTemplateQuota = lens _rqConfigurationTemplateQuota (\ s a -> s{_rqConfigurationTemplateQuota = a})

instance FromXML ResourceQuotas where
        parseXML x
          = ResourceQuotas' <$>
              (x .@? "ApplicationQuota") <*>
                (x .@? "CustomPlatformQuota")
                <*> (x .@? "ApplicationVersionQuota")
                <*> (x .@? "EnvironmentQuota")
                <*> (x .@? "ConfigurationTemplateQuota")

instance Hashable ResourceQuotas where

instance NFData ResourceQuotas where

-- | The bucket and key of an item stored in Amazon S3.
--
--
--
-- /See:/ 's3Location' smart constructor.
data S3Location = S3Location'
  { _slS3Key    :: !(Maybe Text)
  , _slS3Bucket :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slS3Key' - The Amazon S3 key where the data is located.
--
-- * 'slS3Bucket' - The Amazon S3 bucket where the data is located.
s3Location
    :: S3Location
s3Location = S3Location' {_slS3Key = Nothing, _slS3Bucket = Nothing}


-- | The Amazon S3 key where the data is located.
slS3Key :: Lens' S3Location (Maybe Text)
slS3Key = lens _slS3Key (\ s a -> s{_slS3Key = a})

-- | The Amazon S3 bucket where the data is located.
slS3Bucket :: Lens' S3Location (Maybe Text)
slS3Bucket = lens _slS3Bucket (\ s a -> s{_slS3Bucket = a})

instance FromXML S3Location where
        parseXML x
          = S3Location' <$>
              (x .@? "S3Key") <*> (x .@? "S3Bucket")

instance Hashable S3Location where

instance NFData S3Location where

instance ToQuery S3Location where
        toQuery S3Location'{..}
          = mconcat
              ["S3Key" =: _slS3Key, "S3Bucket" =: _slS3Bucket]

-- | Detailed health information about an Amazon EC2 instance in your Elastic Beanstalk environment.
--
--
--
-- /See:/ 'singleInstanceHealth' smart constructor.
data SingleInstanceHealth = SingleInstanceHealth'
  { _sihInstanceId         :: !(Maybe Text)
  , _sihCauses             :: !(Maybe [Text])
  , _sihSystem             :: !(Maybe SystemStatus)
  , _sihApplicationMetrics :: !(Maybe ApplicationMetrics)
  , _sihColor              :: !(Maybe Text)
  , _sihInstanceType       :: !(Maybe Text)
  , _sihAvailabilityZone   :: !(Maybe Text)
  , _sihHealthStatus       :: !(Maybe Text)
  , _sihDeployment         :: !(Maybe Deployment)
  , _sihLaunchedAt         :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SingleInstanceHealth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sihInstanceId' - The ID of the Amazon EC2 instance.
--
-- * 'sihCauses' - Represents the causes, which provide more information about the current health status.
--
-- * 'sihSystem' - Operating system metrics from the instance.
--
-- * 'sihApplicationMetrics' - Request metrics from your application.
--
-- * 'sihColor' - Represents the color indicator that gives you information about the health of the EC2 instance. For more information, see <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
--
-- * 'sihInstanceType' - The instance's type.
--
-- * 'sihAvailabilityZone' - The availability zone in which the instance runs.
--
-- * 'sihHealthStatus' - Returns the health status of the specified instance. For more information, see <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
--
-- * 'sihDeployment' - Information about the most recent deployment to an instance.
--
-- * 'sihLaunchedAt' - The time at which the EC2 instance was launched.
singleInstanceHealth
    :: SingleInstanceHealth
singleInstanceHealth =
  SingleInstanceHealth'
    { _sihInstanceId = Nothing
    , _sihCauses = Nothing
    , _sihSystem = Nothing
    , _sihApplicationMetrics = Nothing
    , _sihColor = Nothing
    , _sihInstanceType = Nothing
    , _sihAvailabilityZone = Nothing
    , _sihHealthStatus = Nothing
    , _sihDeployment = Nothing
    , _sihLaunchedAt = Nothing
    }


-- | The ID of the Amazon EC2 instance.
sihInstanceId :: Lens' SingleInstanceHealth (Maybe Text)
sihInstanceId = lens _sihInstanceId (\ s a -> s{_sihInstanceId = a})

-- | Represents the causes, which provide more information about the current health status.
sihCauses :: Lens' SingleInstanceHealth [Text]
sihCauses = lens _sihCauses (\ s a -> s{_sihCauses = a}) . _Default . _Coerce

-- | Operating system metrics from the instance.
sihSystem :: Lens' SingleInstanceHealth (Maybe SystemStatus)
sihSystem = lens _sihSystem (\ s a -> s{_sihSystem = a})

-- | Request metrics from your application.
sihApplicationMetrics :: Lens' SingleInstanceHealth (Maybe ApplicationMetrics)
sihApplicationMetrics = lens _sihApplicationMetrics (\ s a -> s{_sihApplicationMetrics = a})

-- | Represents the color indicator that gives you information about the health of the EC2 instance. For more information, see <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
sihColor :: Lens' SingleInstanceHealth (Maybe Text)
sihColor = lens _sihColor (\ s a -> s{_sihColor = a})

-- | The instance's type.
sihInstanceType :: Lens' SingleInstanceHealth (Maybe Text)
sihInstanceType = lens _sihInstanceType (\ s a -> s{_sihInstanceType = a})

-- | The availability zone in which the instance runs.
sihAvailabilityZone :: Lens' SingleInstanceHealth (Maybe Text)
sihAvailabilityZone = lens _sihAvailabilityZone (\ s a -> s{_sihAvailabilityZone = a})

-- | Returns the health status of the specified instance. For more information, see <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html Health Colors and Statuses> .
sihHealthStatus :: Lens' SingleInstanceHealth (Maybe Text)
sihHealthStatus = lens _sihHealthStatus (\ s a -> s{_sihHealthStatus = a})

-- | Information about the most recent deployment to an instance.
sihDeployment :: Lens' SingleInstanceHealth (Maybe Deployment)
sihDeployment = lens _sihDeployment (\ s a -> s{_sihDeployment = a})

-- | The time at which the EC2 instance was launched.
sihLaunchedAt :: Lens' SingleInstanceHealth (Maybe UTCTime)
sihLaunchedAt = lens _sihLaunchedAt (\ s a -> s{_sihLaunchedAt = a}) . mapping _Time

instance FromXML SingleInstanceHealth where
        parseXML x
          = SingleInstanceHealth' <$>
              (x .@? "InstanceId") <*>
                (x .@? "Causes" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "System")
                <*> (x .@? "ApplicationMetrics")
                <*> (x .@? "Color")
                <*> (x .@? "InstanceType")
                <*> (x .@? "AvailabilityZone")
                <*> (x .@? "HealthStatus")
                <*> (x .@? "Deployment")
                <*> (x .@? "LaunchedAt")

instance Hashable SingleInstanceHealth where

instance NFData SingleInstanceHealth where

-- | Describes the solution stack.
--
--
--
-- /See:/ 'solutionStackDescription' smart constructor.
data SolutionStackDescription = SolutionStackDescription'
  { _ssdPermittedFileTypes :: !(Maybe [Text])
  , _ssdSolutionStackName  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SolutionStackDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssdPermittedFileTypes' - The permitted file types allowed for a solution stack.
--
-- * 'ssdSolutionStackName' - The name of the solution stack.
solutionStackDescription
    :: SolutionStackDescription
solutionStackDescription =
  SolutionStackDescription'
    {_ssdPermittedFileTypes = Nothing, _ssdSolutionStackName = Nothing}


-- | The permitted file types allowed for a solution stack.
ssdPermittedFileTypes :: Lens' SolutionStackDescription [Text]
ssdPermittedFileTypes = lens _ssdPermittedFileTypes (\ s a -> s{_ssdPermittedFileTypes = a}) . _Default . _Coerce

-- | The name of the solution stack.
ssdSolutionStackName :: Lens' SolutionStackDescription (Maybe Text)
ssdSolutionStackName = lens _ssdSolutionStackName (\ s a -> s{_ssdSolutionStackName = a})

instance FromXML SolutionStackDescription where
        parseXML x
          = SolutionStackDescription' <$>
              (x .@? "PermittedFileTypes" .!@ mempty >>=
                 may (parseXMLList "member"))
                <*> (x .@? "SolutionStackName")

instance Hashable SolutionStackDescription where

instance NFData SolutionStackDescription where

-- | Location of the source code for an application version.
--
--
--
-- /See:/ 'sourceBuildInformation' smart constructor.
data SourceBuildInformation = SourceBuildInformation'
  { _sbiSourceType       :: !SourceType
  , _sbiSourceRepository :: !SourceRepository
  , _sbiSourceLocation   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SourceBuildInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbiSourceType' - The type of repository.     * @Git@      * @Zip@
--
-- * 'sbiSourceRepository' - Location where the repository is stored.     * @CodeCommit@      * @S3@
--
-- * 'sbiSourceLocation' - The location of the source code, as a formatted string, depending on the value of @SourceRepository@      * For @CodeCommit@ , the format is the repository name and commit ID, separated by a forward slash. For example, @my-git-repo/265cfa0cf6af46153527f55d6503ec030551f57a@ .     * For @S3@ , the format is the S3 bucket name and object key, separated by a forward slash. For example, @my-s3-bucket/Folders/my-source-file@ .
sourceBuildInformation
    :: SourceType -- ^ 'sbiSourceType'
    -> SourceRepository -- ^ 'sbiSourceRepository'
    -> Text -- ^ 'sbiSourceLocation'
    -> SourceBuildInformation
sourceBuildInformation pSourceType_ pSourceRepository_ pSourceLocation_ =
  SourceBuildInformation'
    { _sbiSourceType = pSourceType_
    , _sbiSourceRepository = pSourceRepository_
    , _sbiSourceLocation = pSourceLocation_
    }


-- | The type of repository.     * @Git@      * @Zip@
sbiSourceType :: Lens' SourceBuildInformation SourceType
sbiSourceType = lens _sbiSourceType (\ s a -> s{_sbiSourceType = a})

-- | Location where the repository is stored.     * @CodeCommit@      * @S3@
sbiSourceRepository :: Lens' SourceBuildInformation SourceRepository
sbiSourceRepository = lens _sbiSourceRepository (\ s a -> s{_sbiSourceRepository = a})

-- | The location of the source code, as a formatted string, depending on the value of @SourceRepository@      * For @CodeCommit@ , the format is the repository name and commit ID, separated by a forward slash. For example, @my-git-repo/265cfa0cf6af46153527f55d6503ec030551f57a@ .     * For @S3@ , the format is the S3 bucket name and object key, separated by a forward slash. For example, @my-s3-bucket/Folders/my-source-file@ .
sbiSourceLocation :: Lens' SourceBuildInformation Text
sbiSourceLocation = lens _sbiSourceLocation (\ s a -> s{_sbiSourceLocation = a})

instance FromXML SourceBuildInformation where
        parseXML x
          = SourceBuildInformation' <$>
              (x .@ "SourceType") <*> (x .@ "SourceRepository") <*>
                (x .@ "SourceLocation")

instance Hashable SourceBuildInformation where

instance NFData SourceBuildInformation where

instance ToQuery SourceBuildInformation where
        toQuery SourceBuildInformation'{..}
          = mconcat
              ["SourceType" =: _sbiSourceType,
               "SourceRepository" =: _sbiSourceRepository,
               "SourceLocation" =: _sbiSourceLocation]

-- | A specification for an environment configuration
--
--
--
-- /See:/ 'sourceConfiguration' smart constructor.
data SourceConfiguration = SourceConfiguration'
  { _scTemplateName    :: !(Maybe Text)
  , _scApplicationName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SourceConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scTemplateName' - The name of the configuration template.
--
-- * 'scApplicationName' - The name of the application associated with the configuration.
sourceConfiguration
    :: SourceConfiguration
sourceConfiguration =
  SourceConfiguration' {_scTemplateName = Nothing, _scApplicationName = Nothing}


-- | The name of the configuration template.
scTemplateName :: Lens' SourceConfiguration (Maybe Text)
scTemplateName = lens _scTemplateName (\ s a -> s{_scTemplateName = a})

-- | The name of the application associated with the configuration.
scApplicationName :: Lens' SourceConfiguration (Maybe Text)
scApplicationName = lens _scApplicationName (\ s a -> s{_scApplicationName = a})

instance Hashable SourceConfiguration where

instance NFData SourceConfiguration where

instance ToQuery SourceConfiguration where
        toQuery SourceConfiguration'{..}
          = mconcat
              ["TemplateName" =: _scTemplateName,
               "ApplicationName" =: _scApplicationName]

-- | Represents the percentage of requests over the last 10 seconds that resulted in each type of status code response. For more information, see <http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html Status Code Definitions> .
--
--
--
-- /See:/ 'statusCodes' smart constructor.
data StatusCodes = StatusCodes'
  { _scStatus2xx :: !(Maybe Int)
  , _scStatus3xx :: !(Maybe Int)
  , _scStatus4xx :: !(Maybe Int)
  , _scStatus5xx :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StatusCodes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scStatus2xx' - The percentage of requests over the last 10 seconds that resulted in a 2xx (200, 201, etc.) status code.
--
-- * 'scStatus3xx' - The percentage of requests over the last 10 seconds that resulted in a 3xx (300, 301, etc.) status code.
--
-- * 'scStatus4xx' - The percentage of requests over the last 10 seconds that resulted in a 4xx (400, 401, etc.) status code.
--
-- * 'scStatus5xx' - The percentage of requests over the last 10 seconds that resulted in a 5xx (500, 501, etc.) status code.
statusCodes
    :: StatusCodes
statusCodes =
  StatusCodes'
    { _scStatus2xx = Nothing
    , _scStatus3xx = Nothing
    , _scStatus4xx = Nothing
    , _scStatus5xx = Nothing
    }


-- | The percentage of requests over the last 10 seconds that resulted in a 2xx (200, 201, etc.) status code.
scStatus2xx :: Lens' StatusCodes (Maybe Int)
scStatus2xx = lens _scStatus2xx (\ s a -> s{_scStatus2xx = a})

-- | The percentage of requests over the last 10 seconds that resulted in a 3xx (300, 301, etc.) status code.
scStatus3xx :: Lens' StatusCodes (Maybe Int)
scStatus3xx = lens _scStatus3xx (\ s a -> s{_scStatus3xx = a})

-- | The percentage of requests over the last 10 seconds that resulted in a 4xx (400, 401, etc.) status code.
scStatus4xx :: Lens' StatusCodes (Maybe Int)
scStatus4xx = lens _scStatus4xx (\ s a -> s{_scStatus4xx = a})

-- | The percentage of requests over the last 10 seconds that resulted in a 5xx (500, 501, etc.) status code.
scStatus5xx :: Lens' StatusCodes (Maybe Int)
scStatus5xx = lens _scStatus5xx (\ s a -> s{_scStatus5xx = a})

instance FromXML StatusCodes where
        parseXML x
          = StatusCodes' <$>
              (x .@? "Status2xx") <*> (x .@? "Status3xx") <*>
                (x .@? "Status4xx")
                <*> (x .@? "Status5xx")

instance Hashable StatusCodes where

instance NFData StatusCodes where

-- | CPU utilization and load average metrics for an Amazon EC2 instance.
--
--
--
-- /See:/ 'systemStatus' smart constructor.
data SystemStatus = SystemStatus'
  { _ssCPUUtilization :: !(Maybe CPUUtilization)
  , _ssLoadAverage    :: !(Maybe [Double])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SystemStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssCPUUtilization' - CPU utilization metrics for the instance.
--
-- * 'ssLoadAverage' - Load average in the last 1-minute, 5-minute, and 15-minute periods. For more information, see <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-metrics.html#health-enhanced-metrics-os Operating System Metrics> .
systemStatus
    :: SystemStatus
systemStatus =
  SystemStatus' {_ssCPUUtilization = Nothing, _ssLoadAverage = Nothing}


-- | CPU utilization metrics for the instance.
ssCPUUtilization :: Lens' SystemStatus (Maybe CPUUtilization)
ssCPUUtilization = lens _ssCPUUtilization (\ s a -> s{_ssCPUUtilization = a})

-- | Load average in the last 1-minute, 5-minute, and 15-minute periods. For more information, see <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-metrics.html#health-enhanced-metrics-os Operating System Metrics> .
ssLoadAverage :: Lens' SystemStatus [Double]
ssLoadAverage = lens _ssLoadAverage (\ s a -> s{_ssLoadAverage = a}) . _Default . _Coerce

instance FromXML SystemStatus where
        parseXML x
          = SystemStatus' <$>
              (x .@? "CPUUtilization") <*>
                (x .@? "LoadAverage" .!@ mempty >>=
                   may (parseXMLList "member"))

instance Hashable SystemStatus where

instance NFData SystemStatus where

-- | Describes a tag applied to a resource in an environment.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - The value of the tag.
--
-- * 'tagKey' - The key of the tag.
tag
    :: Tag
tag = Tag' {_tagValue = Nothing, _tagKey = Nothing}


-- | The value of the tag.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | The key of the tag.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromXML Tag where
        parseXML x
          = Tag' <$> (x .@? "Value") <*> (x .@? "Key")

instance Hashable Tag where

instance NFData Tag where

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat ["Value" =: _tagValue, "Key" =: _tagKey]

-- | Describes a trigger.
--
--
--
-- /See:/ 'trigger' smart constructor.
newtype Trigger = Trigger'
  { _tName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Trigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tName' - The name of the trigger.
trigger
    :: Trigger
trigger = Trigger' {_tName = Nothing}


-- | The name of the trigger.
tName :: Lens' Trigger (Maybe Text)
tName = lens _tName (\ s a -> s{_tName = a})

instance FromXML Trigger where
        parseXML x = Trigger' <$> (x .@? "Name")

instance Hashable Trigger where

instance NFData Trigger where

-- | An error or warning for a desired configuration option value.
--
--
--
-- /See:/ 'validationMessage' smart constructor.
data ValidationMessage = ValidationMessage'
  { _vmOptionName :: !(Maybe Text)
  , _vmSeverity   :: !(Maybe ValidationSeverity)
  , _vmNamespace  :: !(Maybe Text)
  , _vmMessage    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ValidationMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vmOptionName' - The name of the option.
--
-- * 'vmSeverity' - An indication of the severity of this message:     * @error@ : This message indicates that this is not a valid setting for an option.     * @warning@ : This message is providing information you should take into account.
--
-- * 'vmNamespace' - The namespace to which the option belongs.
--
-- * 'vmMessage' - A message describing the error or warning.
validationMessage
    :: ValidationMessage
validationMessage =
  ValidationMessage'
    { _vmOptionName = Nothing
    , _vmSeverity = Nothing
    , _vmNamespace = Nothing
    , _vmMessage = Nothing
    }


-- | The name of the option.
vmOptionName :: Lens' ValidationMessage (Maybe Text)
vmOptionName = lens _vmOptionName (\ s a -> s{_vmOptionName = a})

-- | An indication of the severity of this message:     * @error@ : This message indicates that this is not a valid setting for an option.     * @warning@ : This message is providing information you should take into account.
vmSeverity :: Lens' ValidationMessage (Maybe ValidationSeverity)
vmSeverity = lens _vmSeverity (\ s a -> s{_vmSeverity = a})

-- | The namespace to which the option belongs.
vmNamespace :: Lens' ValidationMessage (Maybe Text)
vmNamespace = lens _vmNamespace (\ s a -> s{_vmNamespace = a})

-- | A message describing the error or warning.
vmMessage :: Lens' ValidationMessage (Maybe Text)
vmMessage = lens _vmMessage (\ s a -> s{_vmMessage = a})

instance FromXML ValidationMessage where
        parseXML x
          = ValidationMessage' <$>
              (x .@? "OptionName") <*> (x .@? "Severity") <*>
                (x .@? "Namespace")
                <*> (x .@? "Message")

instance Hashable ValidationMessage where

instance NFData ValidationMessage where
