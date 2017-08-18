{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.Product where

import           Network.AWS.CodeBuild.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | Information about a build.
--
--
--
-- /See:/ 'build' smart constructor.
data Build = Build'
    { _bPhases           :: !(Maybe [BuildPhase])
    , _bBuildComplete    :: !(Maybe Bool)
    , _bArn              :: !(Maybe Text)
    , _bStartTime        :: !(Maybe POSIX)
    , _bArtifacts        :: !(Maybe BuildArtifacts)
    , _bEnvironment      :: !(Maybe ProjectEnvironment)
    , _bInitiator        :: !(Maybe Text)
    , _bCurrentPhase     :: !(Maybe Text)
    , _bSourceVersion    :: !(Maybe Text)
    , _bLogs             :: !(Maybe LogsLocation)
    , _bEndTime          :: !(Maybe POSIX)
    , _bProjectName      :: !(Maybe Text)
    , _bBuildStatus      :: !(Maybe StatusType)
    , _bSource           :: !(Maybe ProjectSource)
    , _bId               :: !(Maybe Text)
    , _bTimeoutInMinutes :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Build' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bPhases' - Information about all previous build phases that are completed and information about any current build phase that is not yet complete.
--
-- * 'bBuildComplete' - Whether the build has finished. True if completed; otherwise, false.
--
-- * 'bArn' - The Amazon Resource Name (ARN) of the build.
--
-- * 'bStartTime' - When the build process started, expressed in Unix time format.
--
-- * 'bArtifacts' - Information about the output artifacts for the build.
--
-- * 'bEnvironment' - Information about the build environment for this build.
--
-- * 'bInitiator' - The entity that started the build. Valid values include:     * If AWS CodePipeline started the build, the pipeline's name (for example, @codepipeline/my-demo-pipeline@ ).     * If an AWS Identity and Access Management (IAM) user started the build, the user's name (for example @MyUserName@ ).     * If the Jenkins plugin for AWS CodeBuild started the build, the string @CodeBuild-Jenkins-Plugin@ .
--
-- * 'bCurrentPhase' - The current build phase.
--
-- * 'bSourceVersion' - Any version identifier for the version of the source code to be built.
--
-- * 'bLogs' - Information about the build's logs in Amazon CloudWatch Logs.
--
-- * 'bEndTime' - When the build process ended, expressed in Unix time format.
--
-- * 'bProjectName' - The name of the build project.
--
-- * 'bBuildStatus' - The current status of the build. Valid values include:     * @FAILED@ : The build failed.     * @FAULT@ : The build faulted.     * @IN_PROGRESS@ : The build is still in progress.     * @STOPPED@ : The build stopped.     * @SUCCEEDED@ : The build succeeded.     * @TIMED_OUT@ : The build timed out.
--
-- * 'bSource' - Information about the source code to be built.
--
-- * 'bId' - The unique ID for the build.
--
-- * 'bTimeoutInMinutes' - How long, in minutes, for AWS CodeBuild to wait before timing out this build if it does not get marked as completed.
build
    :: Build
build =
    Build'
    { _bPhases = Nothing
    , _bBuildComplete = Nothing
    , _bArn = Nothing
    , _bStartTime = Nothing
    , _bArtifacts = Nothing
    , _bEnvironment = Nothing
    , _bInitiator = Nothing
    , _bCurrentPhase = Nothing
    , _bSourceVersion = Nothing
    , _bLogs = Nothing
    , _bEndTime = Nothing
    , _bProjectName = Nothing
    , _bBuildStatus = Nothing
    , _bSource = Nothing
    , _bId = Nothing
    , _bTimeoutInMinutes = Nothing
    }

-- | Information about all previous build phases that are completed and information about any current build phase that is not yet complete.
bPhases :: Lens' Build [BuildPhase]
bPhases = lens _bPhases (\ s a -> s{_bPhases = a}) . _Default . _Coerce;

-- | Whether the build has finished. True if completed; otherwise, false.
bBuildComplete :: Lens' Build (Maybe Bool)
bBuildComplete = lens _bBuildComplete (\ s a -> s{_bBuildComplete = a});

-- | The Amazon Resource Name (ARN) of the build.
bArn :: Lens' Build (Maybe Text)
bArn = lens _bArn (\ s a -> s{_bArn = a});

-- | When the build process started, expressed in Unix time format.
bStartTime :: Lens' Build (Maybe UTCTime)
bStartTime = lens _bStartTime (\ s a -> s{_bStartTime = a}) . mapping _Time;

-- | Information about the output artifacts for the build.
bArtifacts :: Lens' Build (Maybe BuildArtifacts)
bArtifacts = lens _bArtifacts (\ s a -> s{_bArtifacts = a});

-- | Information about the build environment for this build.
bEnvironment :: Lens' Build (Maybe ProjectEnvironment)
bEnvironment = lens _bEnvironment (\ s a -> s{_bEnvironment = a});

-- | The entity that started the build. Valid values include:     * If AWS CodePipeline started the build, the pipeline's name (for example, @codepipeline/my-demo-pipeline@ ).     * If an AWS Identity and Access Management (IAM) user started the build, the user's name (for example @MyUserName@ ).     * If the Jenkins plugin for AWS CodeBuild started the build, the string @CodeBuild-Jenkins-Plugin@ .
bInitiator :: Lens' Build (Maybe Text)
bInitiator = lens _bInitiator (\ s a -> s{_bInitiator = a});

-- | The current build phase.
bCurrentPhase :: Lens' Build (Maybe Text)
bCurrentPhase = lens _bCurrentPhase (\ s a -> s{_bCurrentPhase = a});

-- | Any version identifier for the version of the source code to be built.
bSourceVersion :: Lens' Build (Maybe Text)
bSourceVersion = lens _bSourceVersion (\ s a -> s{_bSourceVersion = a});

-- | Information about the build's logs in Amazon CloudWatch Logs.
bLogs :: Lens' Build (Maybe LogsLocation)
bLogs = lens _bLogs (\ s a -> s{_bLogs = a});

-- | When the build process ended, expressed in Unix time format.
bEndTime :: Lens' Build (Maybe UTCTime)
bEndTime = lens _bEndTime (\ s a -> s{_bEndTime = a}) . mapping _Time;

-- | The name of the build project.
bProjectName :: Lens' Build (Maybe Text)
bProjectName = lens _bProjectName (\ s a -> s{_bProjectName = a});

-- | The current status of the build. Valid values include:     * @FAILED@ : The build failed.     * @FAULT@ : The build faulted.     * @IN_PROGRESS@ : The build is still in progress.     * @STOPPED@ : The build stopped.     * @SUCCEEDED@ : The build succeeded.     * @TIMED_OUT@ : The build timed out.
bBuildStatus :: Lens' Build (Maybe StatusType)
bBuildStatus = lens _bBuildStatus (\ s a -> s{_bBuildStatus = a});

-- | Information about the source code to be built.
bSource :: Lens' Build (Maybe ProjectSource)
bSource = lens _bSource (\ s a -> s{_bSource = a});

-- | The unique ID for the build.
bId :: Lens' Build (Maybe Text)
bId = lens _bId (\ s a -> s{_bId = a});

-- | How long, in minutes, for AWS CodeBuild to wait before timing out this build if it does not get marked as completed.
bTimeoutInMinutes :: Lens' Build (Maybe Int)
bTimeoutInMinutes = lens _bTimeoutInMinutes (\ s a -> s{_bTimeoutInMinutes = a});

instance FromJSON Build where
        parseJSON
          = withObject "Build"
              (\ x ->
                 Build' <$>
                   (x .:? "phases" .!= mempty) <*>
                     (x .:? "buildComplete")
                     <*> (x .:? "arn")
                     <*> (x .:? "startTime")
                     <*> (x .:? "artifacts")
                     <*> (x .:? "environment")
                     <*> (x .:? "initiator")
                     <*> (x .:? "currentPhase")
                     <*> (x .:? "sourceVersion")
                     <*> (x .:? "logs")
                     <*> (x .:? "endTime")
                     <*> (x .:? "projectName")
                     <*> (x .:? "buildStatus")
                     <*> (x .:? "source")
                     <*> (x .:? "id")
                     <*> (x .:? "timeoutInMinutes"))

instance Hashable Build

instance NFData Build

-- | Information about build output artifacts.
--
--
--
-- /See:/ 'buildArtifacts' smart constructor.
data BuildArtifacts = BuildArtifacts'
    { _baLocation  :: !(Maybe Text)
    , _baMd5sum    :: !(Maybe Text)
    , _baSha256sum :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BuildArtifacts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'baLocation' - Information about the location of the build artifacts.
--
-- * 'baMd5sum' - The MD5 hash of the build artifact. You can use this hash along with a checksum tool to confirm both file integrity and authenticity.
--
-- * 'baSha256sum' - The SHA-256 hash of the build artifact. You can use this hash along with a checksum tool to confirm both file integrity and authenticity.
buildArtifacts
    :: BuildArtifacts
buildArtifacts =
    BuildArtifacts'
    { _baLocation = Nothing
    , _baMd5sum = Nothing
    , _baSha256sum = Nothing
    }

-- | Information about the location of the build artifacts.
baLocation :: Lens' BuildArtifacts (Maybe Text)
baLocation = lens _baLocation (\ s a -> s{_baLocation = a});

-- | The MD5 hash of the build artifact. You can use this hash along with a checksum tool to confirm both file integrity and authenticity.
baMd5sum :: Lens' BuildArtifacts (Maybe Text)
baMd5sum = lens _baMd5sum (\ s a -> s{_baMd5sum = a});

-- | The SHA-256 hash of the build artifact. You can use this hash along with a checksum tool to confirm both file integrity and authenticity.
baSha256sum :: Lens' BuildArtifacts (Maybe Text)
baSha256sum = lens _baSha256sum (\ s a -> s{_baSha256sum = a});

instance FromJSON BuildArtifacts where
        parseJSON
          = withObject "BuildArtifacts"
              (\ x ->
                 BuildArtifacts' <$>
                   (x .:? "location") <*> (x .:? "md5sum") <*>
                     (x .:? "sha256sum"))

instance Hashable BuildArtifacts

instance NFData BuildArtifacts

-- | Information about a stage for a build.
--
--
--
-- /See:/ 'buildPhase' smart constructor.
data BuildPhase = BuildPhase'
    { _bpContexts          :: !(Maybe [PhaseContext])
    , _bpStartTime         :: !(Maybe POSIX)
    , _bpPhaseStatus       :: !(Maybe StatusType)
    , _bpPhaseType         :: !(Maybe BuildPhaseType)
    , _bpEndTime           :: !(Maybe POSIX)
    , _bpDurationInSeconds :: !(Maybe Integer)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BuildPhase' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpContexts' - Additional information about a build phase, especially to help troubleshoot a failed build.
--
-- * 'bpStartTime' - When the build phase started, expressed in Unix time format.
--
-- * 'bpPhaseStatus' - The current status of the build phase. Valid values include:     * @FAILED@ : The build phase failed.     * @FAULT@ : The build phase faulted.     * @IN_PROGRESS@ : The build phase is still in progress.     * @STOPPED@ : The build phase stopped.     * @SUCCEEDED@ : The build phase succeeded.     * @TIMED_OUT@ : The build phase timed out.
--
-- * 'bpPhaseType' - The name of the build phase. Valid values include:     * @BUILD@ : Core build activities typically occur in this build phase.     * @COMPLETED@ : The build has been completed.     * @DOWNLOAD_SOURCE@ : Source code is being downloaded in this build phase.     * @FINALIZING@ : The build process is completing in this build phase.     * @INSTALL@ : Installation activities typically occur in this build phase.     * @POST_BUILD@ : Post-build activities typically occur in this build phase.     * @PRE_BUILD@ : Pre-build activities typically occur in this build phase.     * @PROVISIONING@ : The build environment is being set up.     * @SUBMITTED@ : The build has been submitted.     * @UPLOAD_ARTIFACTS@ : Build output artifacts are being uploaded to the output location.
--
-- * 'bpEndTime' - When the build phase ended, expressed in Unix time format.
--
-- * 'bpDurationInSeconds' - How long, in seconds, between the starting and ending times of the build's phase.
buildPhase
    :: BuildPhase
buildPhase =
    BuildPhase'
    { _bpContexts = Nothing
    , _bpStartTime = Nothing
    , _bpPhaseStatus = Nothing
    , _bpPhaseType = Nothing
    , _bpEndTime = Nothing
    , _bpDurationInSeconds = Nothing
    }

-- | Additional information about a build phase, especially to help troubleshoot a failed build.
bpContexts :: Lens' BuildPhase [PhaseContext]
bpContexts = lens _bpContexts (\ s a -> s{_bpContexts = a}) . _Default . _Coerce;

-- | When the build phase started, expressed in Unix time format.
bpStartTime :: Lens' BuildPhase (Maybe UTCTime)
bpStartTime = lens _bpStartTime (\ s a -> s{_bpStartTime = a}) . mapping _Time;

-- | The current status of the build phase. Valid values include:     * @FAILED@ : The build phase failed.     * @FAULT@ : The build phase faulted.     * @IN_PROGRESS@ : The build phase is still in progress.     * @STOPPED@ : The build phase stopped.     * @SUCCEEDED@ : The build phase succeeded.     * @TIMED_OUT@ : The build phase timed out.
bpPhaseStatus :: Lens' BuildPhase (Maybe StatusType)
bpPhaseStatus = lens _bpPhaseStatus (\ s a -> s{_bpPhaseStatus = a});

-- | The name of the build phase. Valid values include:     * @BUILD@ : Core build activities typically occur in this build phase.     * @COMPLETED@ : The build has been completed.     * @DOWNLOAD_SOURCE@ : Source code is being downloaded in this build phase.     * @FINALIZING@ : The build process is completing in this build phase.     * @INSTALL@ : Installation activities typically occur in this build phase.     * @POST_BUILD@ : Post-build activities typically occur in this build phase.     * @PRE_BUILD@ : Pre-build activities typically occur in this build phase.     * @PROVISIONING@ : The build environment is being set up.     * @SUBMITTED@ : The build has been submitted.     * @UPLOAD_ARTIFACTS@ : Build output artifacts are being uploaded to the output location.
bpPhaseType :: Lens' BuildPhase (Maybe BuildPhaseType)
bpPhaseType = lens _bpPhaseType (\ s a -> s{_bpPhaseType = a});

-- | When the build phase ended, expressed in Unix time format.
bpEndTime :: Lens' BuildPhase (Maybe UTCTime)
bpEndTime = lens _bpEndTime (\ s a -> s{_bpEndTime = a}) . mapping _Time;

-- | How long, in seconds, between the starting and ending times of the build's phase.
bpDurationInSeconds :: Lens' BuildPhase (Maybe Integer)
bpDurationInSeconds = lens _bpDurationInSeconds (\ s a -> s{_bpDurationInSeconds = a});

instance FromJSON BuildPhase where
        parseJSON
          = withObject "BuildPhase"
              (\ x ->
                 BuildPhase' <$>
                   (x .:? "contexts" .!= mempty) <*> (x .:? "startTime")
                     <*> (x .:? "phaseStatus")
                     <*> (x .:? "phaseType")
                     <*> (x .:? "endTime")
                     <*> (x .:? "durationInSeconds"))

instance Hashable BuildPhase

instance NFData BuildPhase

-- | Information about a Docker image that is managed by AWS CodeBuild.
--
--
--
-- /See:/ 'environmentImage' smart constructor.
data EnvironmentImage = EnvironmentImage'
    { _eiName        :: !(Maybe Text)
    , _eiDescription :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnvironmentImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiName' - The name of the Docker image.
--
-- * 'eiDescription' - The description of the Docker image.
environmentImage
    :: EnvironmentImage
environmentImage =
    EnvironmentImage'
    { _eiName = Nothing
    , _eiDescription = Nothing
    }

-- | The name of the Docker image.
eiName :: Lens' EnvironmentImage (Maybe Text)
eiName = lens _eiName (\ s a -> s{_eiName = a});

-- | The description of the Docker image.
eiDescription :: Lens' EnvironmentImage (Maybe Text)
eiDescription = lens _eiDescription (\ s a -> s{_eiDescription = a});

instance FromJSON EnvironmentImage where
        parseJSON
          = withObject "EnvironmentImage"
              (\ x ->
                 EnvironmentImage' <$>
                   (x .:? "name") <*> (x .:? "description"))

instance Hashable EnvironmentImage

instance NFData EnvironmentImage

-- | A set of Docker images that are related by programming language and are managed by AWS CodeBuild.
--
--
--
-- /See:/ 'environmentLanguage' smart constructor.
data EnvironmentLanguage = EnvironmentLanguage'
    { _elImages   :: !(Maybe [EnvironmentImage])
    , _elLanguage :: !(Maybe LanguageType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnvironmentLanguage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'elImages' - The list of Docker images that are related by the specified programming language.
--
-- * 'elLanguage' - The programming language for the Docker images.
environmentLanguage
    :: EnvironmentLanguage
environmentLanguage =
    EnvironmentLanguage'
    { _elImages = Nothing
    , _elLanguage = Nothing
    }

-- | The list of Docker images that are related by the specified programming language.
elImages :: Lens' EnvironmentLanguage [EnvironmentImage]
elImages = lens _elImages (\ s a -> s{_elImages = a}) . _Default . _Coerce;

-- | The programming language for the Docker images.
elLanguage :: Lens' EnvironmentLanguage (Maybe LanguageType)
elLanguage = lens _elLanguage (\ s a -> s{_elLanguage = a});

instance FromJSON EnvironmentLanguage where
        parseJSON
          = withObject "EnvironmentLanguage"
              (\ x ->
                 EnvironmentLanguage' <$>
                   (x .:? "images" .!= mempty) <*> (x .:? "language"))

instance Hashable EnvironmentLanguage

instance NFData EnvironmentLanguage

-- | A set of Docker images that are related by platform and are managed by AWS CodeBuild.
--
--
--
-- /See:/ 'environmentPlatform' smart constructor.
data EnvironmentPlatform = EnvironmentPlatform'
    { _epPlatform  :: !(Maybe PlatformType)
    , _epLanguages :: !(Maybe [EnvironmentLanguage])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnvironmentPlatform' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'epPlatform' - The platform's name.
--
-- * 'epLanguages' - The list of programming languages that are available for the specified platform.
environmentPlatform
    :: EnvironmentPlatform
environmentPlatform =
    EnvironmentPlatform'
    { _epPlatform = Nothing
    , _epLanguages = Nothing
    }

-- | The platform's name.
epPlatform :: Lens' EnvironmentPlatform (Maybe PlatformType)
epPlatform = lens _epPlatform (\ s a -> s{_epPlatform = a});

-- | The list of programming languages that are available for the specified platform.
epLanguages :: Lens' EnvironmentPlatform [EnvironmentLanguage]
epLanguages = lens _epLanguages (\ s a -> s{_epLanguages = a}) . _Default . _Coerce;

instance FromJSON EnvironmentPlatform where
        parseJSON
          = withObject "EnvironmentPlatform"
              (\ x ->
                 EnvironmentPlatform' <$>
                   (x .:? "platform") <*>
                     (x .:? "languages" .!= mempty))

instance Hashable EnvironmentPlatform

instance NFData EnvironmentPlatform

-- | Information about an environment variable for a build project or a build.
--
--
--
-- /See:/ 'environmentVariable' smart constructor.
data EnvironmentVariable = EnvironmentVariable'
    { _evName  :: !Text
    , _evValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnvironmentVariable' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'evName' - The name or key of the environment variable.
--
-- * 'evValue' - The value of the environment variable. /Important:/ We strongly discourage using environment variables to store sensitive values, especially AWS secret key IDs and secret access keys. Environment variables can be displayed in plain text using tools such as the AWS CodeBuild console and the AWS Command Line Interface (AWS CLI).
environmentVariable
    :: Text -- ^ 'evName'
    -> Text -- ^ 'evValue'
    -> EnvironmentVariable
environmentVariable pName_ pValue_ =
    EnvironmentVariable'
    { _evName = pName_
    , _evValue = pValue_
    }

-- | The name or key of the environment variable.
evName :: Lens' EnvironmentVariable Text
evName = lens _evName (\ s a -> s{_evName = a});

-- | The value of the environment variable. /Important:/ We strongly discourage using environment variables to store sensitive values, especially AWS secret key IDs and secret access keys. Environment variables can be displayed in plain text using tools such as the AWS CodeBuild console and the AWS Command Line Interface (AWS CLI).
evValue :: Lens' EnvironmentVariable Text
evValue = lens _evValue (\ s a -> s{_evValue = a});

instance FromJSON EnvironmentVariable where
        parseJSON
          = withObject "EnvironmentVariable"
              (\ x ->
                 EnvironmentVariable' <$>
                   (x .: "name") <*> (x .: "value"))

instance Hashable EnvironmentVariable

instance NFData EnvironmentVariable

instance ToJSON EnvironmentVariable where
        toJSON EnvironmentVariable'{..}
          = object
              (catMaybes
                 [Just ("name" .= _evName),
                  Just ("value" .= _evValue)])

-- | Information about build logs in Amazon CloudWatch Logs.
--
--
--
-- /See:/ 'logsLocation' smart constructor.
data LogsLocation = LogsLocation'
    { _llDeepLink   :: !(Maybe Text)
    , _llGroupName  :: !(Maybe Text)
    , _llStreamName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'LogsLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'llDeepLink' - The URL to an individual build log in Amazon CloudWatch Logs.
--
-- * 'llGroupName' - The name of the Amazon CloudWatch Logs group for the build logs.
--
-- * 'llStreamName' - The name of the Amazon CloudWatch Logs stream for the build logs.
logsLocation
    :: LogsLocation
logsLocation =
    LogsLocation'
    { _llDeepLink = Nothing
    , _llGroupName = Nothing
    , _llStreamName = Nothing
    }

-- | The URL to an individual build log in Amazon CloudWatch Logs.
llDeepLink :: Lens' LogsLocation (Maybe Text)
llDeepLink = lens _llDeepLink (\ s a -> s{_llDeepLink = a});

-- | The name of the Amazon CloudWatch Logs group for the build logs.
llGroupName :: Lens' LogsLocation (Maybe Text)
llGroupName = lens _llGroupName (\ s a -> s{_llGroupName = a});

-- | The name of the Amazon CloudWatch Logs stream for the build logs.
llStreamName :: Lens' LogsLocation (Maybe Text)
llStreamName = lens _llStreamName (\ s a -> s{_llStreamName = a});

instance FromJSON LogsLocation where
        parseJSON
          = withObject "LogsLocation"
              (\ x ->
                 LogsLocation' <$>
                   (x .:? "deepLink") <*> (x .:? "groupName") <*>
                     (x .:? "streamName"))

instance Hashable LogsLocation

instance NFData LogsLocation

-- | Additional information about a build phase that has an error. You can use this information to help troubleshoot a failed build.
--
--
--
-- /See:/ 'phaseContext' smart constructor.
data PhaseContext = PhaseContext'
    { _pcMessage    :: !(Maybe Text)
    , _pcStatusCode :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PhaseContext' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcMessage' - An explanation of the build phase's context. This explanation might include a command ID and an exit code.
--
-- * 'pcStatusCode' - The status code for the context of the build phase.
phaseContext
    :: PhaseContext
phaseContext =
    PhaseContext'
    { _pcMessage = Nothing
    , _pcStatusCode = Nothing
    }

-- | An explanation of the build phase's context. This explanation might include a command ID and an exit code.
pcMessage :: Lens' PhaseContext (Maybe Text)
pcMessage = lens _pcMessage (\ s a -> s{_pcMessage = a});

-- | The status code for the context of the build phase.
pcStatusCode :: Lens' PhaseContext (Maybe Text)
pcStatusCode = lens _pcStatusCode (\ s a -> s{_pcStatusCode = a});

instance FromJSON PhaseContext where
        parseJSON
          = withObject "PhaseContext"
              (\ x ->
                 PhaseContext' <$>
                   (x .:? "message") <*> (x .:? "statusCode"))

instance Hashable PhaseContext

instance NFData PhaseContext

-- | Information about a build project.
--
--
--
-- /See:/ 'project' smart constructor.
data Project = Project'
    { _pArn              :: !(Maybe Text)
    , _pArtifacts        :: !(Maybe ProjectArtifacts)
    , _pEnvironment      :: !(Maybe ProjectEnvironment)
    , _pCreated          :: !(Maybe POSIX)
    , _pName             :: !(Maybe Text)
    , _pSource           :: !(Maybe ProjectSource)
    , _pEncryptionKey    :: !(Maybe Text)
    , _pLastModified     :: !(Maybe POSIX)
    , _pDescription      :: !(Maybe Text)
    , _pServiceRole      :: !(Maybe Text)
    , _pTags             :: !(Maybe [Tag])
    , _pTimeoutInMinutes :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Project' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pArn' - The Amazon Resource Name (ARN) of the build project.
--
-- * 'pArtifacts' - Information about the build output artifacts for the build project.
--
-- * 'pEnvironment' - Information about the build environment for this build project.
--
-- * 'pCreated' - When the build project was created, expressed in Unix time format.
--
-- * 'pName' - The name of the build project.
--
-- * 'pSource' - Information about the build input source code for this build project.
--
-- * 'pEncryptionKey' - The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts. This is expressed either as the CMK's Amazon Resource Name (ARN) or, if specified, the CMK's alias (using the format @alias//alias-name/ @ ).
--
-- * 'pLastModified' - When the build project's settings were last modified, expressed in Unix time format.
--
-- * 'pDescription' - A description that makes the build project easy to identify.
--
-- * 'pServiceRole' - The ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
--
-- * 'pTags' - The tags for this build project. These tags are available for use by AWS services that support AWS CodeBuild build project tags.
--
-- * 'pTimeoutInMinutes' - How long, in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait before timing out any related build that did not get marked as completed. The default is 60 minutes.
project
    :: Project
project =
    Project'
    { _pArn = Nothing
    , _pArtifacts = Nothing
    , _pEnvironment = Nothing
    , _pCreated = Nothing
    , _pName = Nothing
    , _pSource = Nothing
    , _pEncryptionKey = Nothing
    , _pLastModified = Nothing
    , _pDescription = Nothing
    , _pServiceRole = Nothing
    , _pTags = Nothing
    , _pTimeoutInMinutes = Nothing
    }

-- | The Amazon Resource Name (ARN) of the build project.
pArn :: Lens' Project (Maybe Text)
pArn = lens _pArn (\ s a -> s{_pArn = a});

-- | Information about the build output artifacts for the build project.
pArtifacts :: Lens' Project (Maybe ProjectArtifacts)
pArtifacts = lens _pArtifacts (\ s a -> s{_pArtifacts = a});

-- | Information about the build environment for this build project.
pEnvironment :: Lens' Project (Maybe ProjectEnvironment)
pEnvironment = lens _pEnvironment (\ s a -> s{_pEnvironment = a});

-- | When the build project was created, expressed in Unix time format.
pCreated :: Lens' Project (Maybe UTCTime)
pCreated = lens _pCreated (\ s a -> s{_pCreated = a}) . mapping _Time;

-- | The name of the build project.
pName :: Lens' Project (Maybe Text)
pName = lens _pName (\ s a -> s{_pName = a});

-- | Information about the build input source code for this build project.
pSource :: Lens' Project (Maybe ProjectSource)
pSource = lens _pSource (\ s a -> s{_pSource = a});

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts. This is expressed either as the CMK's Amazon Resource Name (ARN) or, if specified, the CMK's alias (using the format @alias//alias-name/ @ ).
pEncryptionKey :: Lens' Project (Maybe Text)
pEncryptionKey = lens _pEncryptionKey (\ s a -> s{_pEncryptionKey = a});

-- | When the build project's settings were last modified, expressed in Unix time format.
pLastModified :: Lens' Project (Maybe UTCTime)
pLastModified = lens _pLastModified (\ s a -> s{_pLastModified = a}) . mapping _Time;

-- | A description that makes the build project easy to identify.
pDescription :: Lens' Project (Maybe Text)
pDescription = lens _pDescription (\ s a -> s{_pDescription = a});

-- | The ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
pServiceRole :: Lens' Project (Maybe Text)
pServiceRole = lens _pServiceRole (\ s a -> s{_pServiceRole = a});

-- | The tags for this build project. These tags are available for use by AWS services that support AWS CodeBuild build project tags.
pTags :: Lens' Project [Tag]
pTags = lens _pTags (\ s a -> s{_pTags = a}) . _Default . _Coerce;

-- | How long, in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait before timing out any related build that did not get marked as completed. The default is 60 minutes.
pTimeoutInMinutes :: Lens' Project (Maybe Natural)
pTimeoutInMinutes = lens _pTimeoutInMinutes (\ s a -> s{_pTimeoutInMinutes = a}) . mapping _Nat;

instance FromJSON Project where
        parseJSON
          = withObject "Project"
              (\ x ->
                 Project' <$>
                   (x .:? "arn") <*> (x .:? "artifacts") <*>
                     (x .:? "environment")
                     <*> (x .:? "created")
                     <*> (x .:? "name")
                     <*> (x .:? "source")
                     <*> (x .:? "encryptionKey")
                     <*> (x .:? "lastModified")
                     <*> (x .:? "description")
                     <*> (x .:? "serviceRole")
                     <*> (x .:? "tags" .!= mempty)
                     <*> (x .:? "timeoutInMinutes"))

instance Hashable Project

instance NFData Project

-- | Information about the build output artifacts for the build project.
--
--
--
-- /See:/ 'projectArtifacts' smart constructor.
data ProjectArtifacts = ProjectArtifacts'
    { _paPackaging     :: !(Maybe ArtifactPackaging)
    , _paPath          :: !(Maybe Text)
    , _paLocation      :: !(Maybe Text)
    , _paName          :: !(Maybe Text)
    , _paNamespaceType :: !(Maybe ArtifactNamespace)
    , _paType          :: !ArtifactsType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectArtifacts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paPackaging' - The type of build output artifact to create, as follows:     * If @type@ is set to @CODEPIPELINE@ , then AWS CodePipeline will ignore this value if specified. This is because AWS CodePipeline manages its build output artifacts instead of AWS CodeBuild.     * If @type@ is set to @NO_ARTIFACTS@ , then this value will be ignored if specified, because no build output will be produced.     * If @type@ is set to @S3@ , valid values include:     * @NONE@ : AWS CodeBuild will create in the output bucket a folder containing the build output. This is the default if @packaging@ is not specified.     * @ZIP@ : AWS CodeBuild will create in the output bucket a ZIP file containing the build output.
--
-- * 'paPath' - Along with @namespaceType@ and @name@ , the pattern that AWS CodeBuild will use to name and store the output artifact, as follows:     * If @type@ is set to @CODEPIPELINE@ , then AWS CodePipeline will ignore this value if specified. This is because AWS CodePipeline manages its build output names instead of AWS CodeBuild.     * If @type@ is set to @NO_ARTIFACTS@ , then this value will be ignored if specified, because no build output will be produced.     * If @type@ is set to @S3@ , this is the path to the output artifact. If @path@ is not specified, then @path@ will not be used. For example, if @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @NONE@ , and @name@ is set to @MyArtifact.zip@ , then the output artifact would be stored in the output bucket at @MyArtifacts/MyArtifact.zip@ .
--
-- * 'paLocation' - Information about the build output artifact location, as follows:     * If @type@ is set to @CODEPIPELINE@ , then AWS CodePipeline will ignore this value if specified. This is because AWS CodePipeline manages its build output locations instead of AWS CodeBuild.     * If @type@ is set to @NO_ARTIFACTS@ , then this value will be ignored if specified, because no build output will be produced.     * If @type@ is set to @S3@ , this is the name of the output bucket.
--
-- * 'paName' - Along with @path@ and @namespaceType@ , the pattern that AWS CodeBuild will use to name and store the output artifact, as follows:     * If @type@ is set to @CODEPIPELINE@ , then AWS CodePipeline will ignore this value if specified. This is because AWS CodePipeline manages its build output names instead of AWS CodeBuild.     * If @type@ is set to @NO_ARTIFACTS@ , then this value will be ignored if specified, because no build output will be produced.     * If @type@ is set to @S3@ , this is the name of the output artifact object. For example, if @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @BUILD_ID@ , and @name@ is set to @MyArtifact.zip@ , then the output artifact would be stored in @MyArtifacts//build-ID/ /MyArtifact.zip@ .
--
-- * 'paNamespaceType' - Along with @path@ and @name@ , the pattern that AWS CodeBuild will use to determine the name and location to store the output artifact, as follows:     * If @type@ is set to @CODEPIPELINE@ , then AWS CodePipeline will ignore this value if specified. This is because AWS CodePipeline manages its build output names instead of AWS CodeBuild.     * If @type@ is set to @NO_ARTIFACTS@ , then this value will be ignored if specified, because no build output will be produced.     * If @type@ is set to @S3@ , then valid values include:     * @BUILD_ID@ : Include the build ID in the location of the build output artifact.     * @NONE@ : Do not include the build ID. This is the default if @namespaceType@ is not specified. For example, if @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @BUILD_ID@ , and @name@ is set to @MyArtifact.zip@ , then the output artifact would be stored in @MyArtifacts//build-ID/ /MyArtifact.zip@ .
--
-- * 'paType' - The type of build output artifact. Valid values include:     * @CODEPIPELINE@ : The build project will have build output generated through AWS CodePipeline.     * @NO_ARTIFACTS@ : The build project will not produce any build output.     * @S3@ : The build project will store build output in Amazon Simple Storage Service (Amazon S3).
projectArtifacts
    :: ArtifactsType -- ^ 'paType'
    -> ProjectArtifacts
projectArtifacts pType_ =
    ProjectArtifacts'
    { _paPackaging = Nothing
    , _paPath = Nothing
    , _paLocation = Nothing
    , _paName = Nothing
    , _paNamespaceType = Nothing
    , _paType = pType_
    }

-- | The type of build output artifact to create, as follows:     * If @type@ is set to @CODEPIPELINE@ , then AWS CodePipeline will ignore this value if specified. This is because AWS CodePipeline manages its build output artifacts instead of AWS CodeBuild.     * If @type@ is set to @NO_ARTIFACTS@ , then this value will be ignored if specified, because no build output will be produced.     * If @type@ is set to @S3@ , valid values include:     * @NONE@ : AWS CodeBuild will create in the output bucket a folder containing the build output. This is the default if @packaging@ is not specified.     * @ZIP@ : AWS CodeBuild will create in the output bucket a ZIP file containing the build output.
paPackaging :: Lens' ProjectArtifacts (Maybe ArtifactPackaging)
paPackaging = lens _paPackaging (\ s a -> s{_paPackaging = a});

-- | Along with @namespaceType@ and @name@ , the pattern that AWS CodeBuild will use to name and store the output artifact, as follows:     * If @type@ is set to @CODEPIPELINE@ , then AWS CodePipeline will ignore this value if specified. This is because AWS CodePipeline manages its build output names instead of AWS CodeBuild.     * If @type@ is set to @NO_ARTIFACTS@ , then this value will be ignored if specified, because no build output will be produced.     * If @type@ is set to @S3@ , this is the path to the output artifact. If @path@ is not specified, then @path@ will not be used. For example, if @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @NONE@ , and @name@ is set to @MyArtifact.zip@ , then the output artifact would be stored in the output bucket at @MyArtifacts/MyArtifact.zip@ .
paPath :: Lens' ProjectArtifacts (Maybe Text)
paPath = lens _paPath (\ s a -> s{_paPath = a});

-- | Information about the build output artifact location, as follows:     * If @type@ is set to @CODEPIPELINE@ , then AWS CodePipeline will ignore this value if specified. This is because AWS CodePipeline manages its build output locations instead of AWS CodeBuild.     * If @type@ is set to @NO_ARTIFACTS@ , then this value will be ignored if specified, because no build output will be produced.     * If @type@ is set to @S3@ , this is the name of the output bucket.
paLocation :: Lens' ProjectArtifacts (Maybe Text)
paLocation = lens _paLocation (\ s a -> s{_paLocation = a});

-- | Along with @path@ and @namespaceType@ , the pattern that AWS CodeBuild will use to name and store the output artifact, as follows:     * If @type@ is set to @CODEPIPELINE@ , then AWS CodePipeline will ignore this value if specified. This is because AWS CodePipeline manages its build output names instead of AWS CodeBuild.     * If @type@ is set to @NO_ARTIFACTS@ , then this value will be ignored if specified, because no build output will be produced.     * If @type@ is set to @S3@ , this is the name of the output artifact object. For example, if @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @BUILD_ID@ , and @name@ is set to @MyArtifact.zip@ , then the output artifact would be stored in @MyArtifacts//build-ID/ /MyArtifact.zip@ .
paName :: Lens' ProjectArtifacts (Maybe Text)
paName = lens _paName (\ s a -> s{_paName = a});

-- | Along with @path@ and @name@ , the pattern that AWS CodeBuild will use to determine the name and location to store the output artifact, as follows:     * If @type@ is set to @CODEPIPELINE@ , then AWS CodePipeline will ignore this value if specified. This is because AWS CodePipeline manages its build output names instead of AWS CodeBuild.     * If @type@ is set to @NO_ARTIFACTS@ , then this value will be ignored if specified, because no build output will be produced.     * If @type@ is set to @S3@ , then valid values include:     * @BUILD_ID@ : Include the build ID in the location of the build output artifact.     * @NONE@ : Do not include the build ID. This is the default if @namespaceType@ is not specified. For example, if @path@ is set to @MyArtifacts@ , @namespaceType@ is set to @BUILD_ID@ , and @name@ is set to @MyArtifact.zip@ , then the output artifact would be stored in @MyArtifacts//build-ID/ /MyArtifact.zip@ .
paNamespaceType :: Lens' ProjectArtifacts (Maybe ArtifactNamespace)
paNamespaceType = lens _paNamespaceType (\ s a -> s{_paNamespaceType = a});

-- | The type of build output artifact. Valid values include:     * @CODEPIPELINE@ : The build project will have build output generated through AWS CodePipeline.     * @NO_ARTIFACTS@ : The build project will not produce any build output.     * @S3@ : The build project will store build output in Amazon Simple Storage Service (Amazon S3).
paType :: Lens' ProjectArtifacts ArtifactsType
paType = lens _paType (\ s a -> s{_paType = a});

instance FromJSON ProjectArtifacts where
        parseJSON
          = withObject "ProjectArtifacts"
              (\ x ->
                 ProjectArtifacts' <$>
                   (x .:? "packaging") <*> (x .:? "path") <*>
                     (x .:? "location")
                     <*> (x .:? "name")
                     <*> (x .:? "namespaceType")
                     <*> (x .: "type"))

instance Hashable ProjectArtifacts

instance NFData ProjectArtifacts

instance ToJSON ProjectArtifacts where
        toJSON ProjectArtifacts'{..}
          = object
              (catMaybes
                 [("packaging" .=) <$> _paPackaging,
                  ("path" .=) <$> _paPath,
                  ("location" .=) <$> _paLocation,
                  ("name" .=) <$> _paName,
                  ("namespaceType" .=) <$> _paNamespaceType,
                  Just ("type" .= _paType)])

-- | Information about the build environment of the build project.
--
--
--
-- /See:/ 'projectEnvironment' smart constructor.
data ProjectEnvironment = ProjectEnvironment'
    { _pePrivilegedMode       :: !(Maybe Bool)
    , _peEnvironmentVariables :: !(Maybe [EnvironmentVariable])
    , _peType                 :: !EnvironmentType
    , _peImage                :: !Text
    , _peComputeType          :: !ComputeType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectEnvironment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pePrivilegedMode' - If set to true, enables running the Docker daemon inside a Docker container; otherwise, false or not specified (the default). This value must be set to true only if this build project will be used to build Docker images, and the specified build environment image is not one provided by AWS CodeBuild with Docker support. Otherwise, all associated builds that attempt to interact with the Docker daemon will fail. Note that you must also start the Docker daemon so that your builds can interact with it as needed. One way to do this is to initialize the Docker daemon in the install phase of your build spec by running the following build commands. (Do not run the following build commands if the specified build environment image is provided by AWS CodeBuild with Docker support.) @- nohup /usr/local/bin/dockerd --host=unix:///var/run/docker.sock --host=tcp://0.0.0.0:2375 --storage-driver=vfs& - timeout -t 15 sh -c "until docker info; do echo .; sleep 1; done"@
--
-- * 'peEnvironmentVariables' - A set of environment variables to make available to builds for this build project.
--
-- * 'peType' - The type of build environment to use for related builds.
--
-- * 'peImage' - The ID of the Docker image to use for this build project.
--
-- * 'peComputeType' - Information about the compute resources the build project will use. Available values include:     * @BUILD_GENERAL1_SMALL@ : Use up to 3 GB memory and 2 vCPUs for builds.     * @BUILD_GENERAL1_MEDIUM@ : Use up to 7 GB memory and 4 vCPUs for builds.     * @BUILD_GENERAL1_LARGE@ : Use up to 15 GB memory and 8 vCPUs for builds.
projectEnvironment
    :: EnvironmentType -- ^ 'peType'
    -> Text -- ^ 'peImage'
    -> ComputeType -- ^ 'peComputeType'
    -> ProjectEnvironment
projectEnvironment pType_ pImage_ pComputeType_ =
    ProjectEnvironment'
    { _pePrivilegedMode = Nothing
    , _peEnvironmentVariables = Nothing
    , _peType = pType_
    , _peImage = pImage_
    , _peComputeType = pComputeType_
    }

-- | If set to true, enables running the Docker daemon inside a Docker container; otherwise, false or not specified (the default). This value must be set to true only if this build project will be used to build Docker images, and the specified build environment image is not one provided by AWS CodeBuild with Docker support. Otherwise, all associated builds that attempt to interact with the Docker daemon will fail. Note that you must also start the Docker daemon so that your builds can interact with it as needed. One way to do this is to initialize the Docker daemon in the install phase of your build spec by running the following build commands. (Do not run the following build commands if the specified build environment image is provided by AWS CodeBuild with Docker support.) @- nohup /usr/local/bin/dockerd --host=unix:///var/run/docker.sock --host=tcp://0.0.0.0:2375 --storage-driver=vfs& - timeout -t 15 sh -c "until docker info; do echo .; sleep 1; done"@
pePrivilegedMode :: Lens' ProjectEnvironment (Maybe Bool)
pePrivilegedMode = lens _pePrivilegedMode (\ s a -> s{_pePrivilegedMode = a});

-- | A set of environment variables to make available to builds for this build project.
peEnvironmentVariables :: Lens' ProjectEnvironment [EnvironmentVariable]
peEnvironmentVariables = lens _peEnvironmentVariables (\ s a -> s{_peEnvironmentVariables = a}) . _Default . _Coerce;

-- | The type of build environment to use for related builds.
peType :: Lens' ProjectEnvironment EnvironmentType
peType = lens _peType (\ s a -> s{_peType = a});

-- | The ID of the Docker image to use for this build project.
peImage :: Lens' ProjectEnvironment Text
peImage = lens _peImage (\ s a -> s{_peImage = a});

-- | Information about the compute resources the build project will use. Available values include:     * @BUILD_GENERAL1_SMALL@ : Use up to 3 GB memory and 2 vCPUs for builds.     * @BUILD_GENERAL1_MEDIUM@ : Use up to 7 GB memory and 4 vCPUs for builds.     * @BUILD_GENERAL1_LARGE@ : Use up to 15 GB memory and 8 vCPUs for builds.
peComputeType :: Lens' ProjectEnvironment ComputeType
peComputeType = lens _peComputeType (\ s a -> s{_peComputeType = a});

instance FromJSON ProjectEnvironment where
        parseJSON
          = withObject "ProjectEnvironment"
              (\ x ->
                 ProjectEnvironment' <$>
                   (x .:? "privilegedMode") <*>
                     (x .:? "environmentVariables" .!= mempty)
                     <*> (x .: "type")
                     <*> (x .: "image")
                     <*> (x .: "computeType"))

instance Hashable ProjectEnvironment

instance NFData ProjectEnvironment

instance ToJSON ProjectEnvironment where
        toJSON ProjectEnvironment'{..}
          = object
              (catMaybes
                 [("privilegedMode" .=) <$> _pePrivilegedMode,
                  ("environmentVariables" .=) <$>
                    _peEnvironmentVariables,
                  Just ("type" .= _peType), Just ("image" .= _peImage),
                  Just ("computeType" .= _peComputeType)])

-- | Information about the build input source code for the build project.
--
--
--
-- /See:/ 'projectSource' smart constructor.
data ProjectSource = ProjectSource'
    { _psLocation  :: !(Maybe Text)
    , _psAuth      :: !(Maybe SourceAuth)
    , _psBuildspec :: !(Maybe Text)
    , _psType      :: !SourceType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ProjectSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psLocation' - Information about the location of the source code to be built. Valid values include:     * For source code settings that are specified in the source action of a pipeline in AWS CodePipeline, @location@ should not be specified. If it is specified, AWS CodePipeline will ignore it. This is because AWS CodePipeline uses the settings in a pipeline's source action instead of this value.     * For source code in an AWS CodeCommit repository, the HTTPS clone URL to the repository that contains the source code and the build spec (for example, @https://git-codecommit./region-ID/ .amazonaws.com/v1/repos//repo-name/ @ ).     * For source code in an Amazon Simple Storage Service (Amazon S3) input bucket, the path to the ZIP file that contains the source code (for example, @/bucket-name/ //path/ //to/ //object-name/ .zip@ )     * For source code in a GitHub repository, the HTTPS clone URL to the repository that contains the source and the build spec. Also, you must connect your AWS account to your GitHub account. To do this, use the AWS CodeBuild console to begin creating a build project. When you use the console to connect (or reconnect) with GitHub, on the GitHub __Authorize application__ page that displays, for __Organization access__ , choose __Request access__ next to each repository you want to allow AWS CodeBuild to have access to. Then choose __Authorize application__ . (After you have connected to your GitHub account, you do not need to finish creating the build project, and you may then leave the AWS CodeBuild console.) To instruct AWS CodeBuild to then use this connection, in the @source@ object, set the @auth@ object's @type@ value to @OAUTH@ .
--
-- * 'psAuth' - Information about the authorization settings for AWS CodeBuild to access the source code to be built. This information is for the AWS CodeBuild console's use only. Your code should not get or set this information directly (unless the build project's source @type@ value is @GITHUB@ ).
--
-- * 'psBuildspec' - The build spec declaration to use for the builds in this build project. If this value is not specified, a build spec must be included along with the source code to be built.
--
-- * 'psType' - The type of repository that contains the source code to be built. Valid values include:     * @CODECOMMIT@ : The source code is in an AWS CodeCommit repository.     * @CODEPIPELINE@ : The source code settings are specified in the source action of a pipeline in AWS CodePipeline.     * @GITHUB@ : The source code is in a GitHub repository.     * @S3@ : The source code is in an Amazon Simple Storage Service (Amazon S3) input bucket.
projectSource
    :: SourceType -- ^ 'psType'
    -> ProjectSource
projectSource pType_ =
    ProjectSource'
    { _psLocation = Nothing
    , _psAuth = Nothing
    , _psBuildspec = Nothing
    , _psType = pType_
    }

-- | Information about the location of the source code to be built. Valid values include:     * For source code settings that are specified in the source action of a pipeline in AWS CodePipeline, @location@ should not be specified. If it is specified, AWS CodePipeline will ignore it. This is because AWS CodePipeline uses the settings in a pipeline's source action instead of this value.     * For source code in an AWS CodeCommit repository, the HTTPS clone URL to the repository that contains the source code and the build spec (for example, @https://git-codecommit./region-ID/ .amazonaws.com/v1/repos//repo-name/ @ ).     * For source code in an Amazon Simple Storage Service (Amazon S3) input bucket, the path to the ZIP file that contains the source code (for example, @/bucket-name/ //path/ //to/ //object-name/ .zip@ )     * For source code in a GitHub repository, the HTTPS clone URL to the repository that contains the source and the build spec. Also, you must connect your AWS account to your GitHub account. To do this, use the AWS CodeBuild console to begin creating a build project. When you use the console to connect (or reconnect) with GitHub, on the GitHub __Authorize application__ page that displays, for __Organization access__ , choose __Request access__ next to each repository you want to allow AWS CodeBuild to have access to. Then choose __Authorize application__ . (After you have connected to your GitHub account, you do not need to finish creating the build project, and you may then leave the AWS CodeBuild console.) To instruct AWS CodeBuild to then use this connection, in the @source@ object, set the @auth@ object's @type@ value to @OAUTH@ .
psLocation :: Lens' ProjectSource (Maybe Text)
psLocation = lens _psLocation (\ s a -> s{_psLocation = a});

-- | Information about the authorization settings for AWS CodeBuild to access the source code to be built. This information is for the AWS CodeBuild console's use only. Your code should not get or set this information directly (unless the build project's source @type@ value is @GITHUB@ ).
psAuth :: Lens' ProjectSource (Maybe SourceAuth)
psAuth = lens _psAuth (\ s a -> s{_psAuth = a});

-- | The build spec declaration to use for the builds in this build project. If this value is not specified, a build spec must be included along with the source code to be built.
psBuildspec :: Lens' ProjectSource (Maybe Text)
psBuildspec = lens _psBuildspec (\ s a -> s{_psBuildspec = a});

-- | The type of repository that contains the source code to be built. Valid values include:     * @CODECOMMIT@ : The source code is in an AWS CodeCommit repository.     * @CODEPIPELINE@ : The source code settings are specified in the source action of a pipeline in AWS CodePipeline.     * @GITHUB@ : The source code is in a GitHub repository.     * @S3@ : The source code is in an Amazon Simple Storage Service (Amazon S3) input bucket.
psType :: Lens' ProjectSource SourceType
psType = lens _psType (\ s a -> s{_psType = a});

instance FromJSON ProjectSource where
        parseJSON
          = withObject "ProjectSource"
              (\ x ->
                 ProjectSource' <$>
                   (x .:? "location") <*> (x .:? "auth") <*>
                     (x .:? "buildspec")
                     <*> (x .: "type"))

instance Hashable ProjectSource

instance NFData ProjectSource

instance ToJSON ProjectSource where
        toJSON ProjectSource'{..}
          = object
              (catMaybes
                 [("location" .=) <$> _psLocation,
                  ("auth" .=) <$> _psAuth,
                  ("buildspec" .=) <$> _psBuildspec,
                  Just ("type" .= _psType)])

-- | Information about the authorization settings for AWS CodeBuild to access the source code to be built.
--
--
-- This information is for the AWS CodeBuild console's use only. Your code should not get or set this information directly (unless the build project's source @type@ value is @GITHUB@ ).
--
--
-- /See:/ 'sourceAuth' smart constructor.
data SourceAuth = SourceAuth'
    { _saResource :: !(Maybe Text)
    , _saType     :: !SourceAuthType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SourceAuth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saResource' - The resource value that applies to the specified authorization type.
--
-- * 'saType' - The authorization type to use. The only valid value is @OAUTH@ , which represents the OAuth authorization type.
sourceAuth
    :: SourceAuthType -- ^ 'saType'
    -> SourceAuth
sourceAuth pType_ =
    SourceAuth'
    { _saResource = Nothing
    , _saType = pType_
    }

-- | The resource value that applies to the specified authorization type.
saResource :: Lens' SourceAuth (Maybe Text)
saResource = lens _saResource (\ s a -> s{_saResource = a});

-- | The authorization type to use. The only valid value is @OAUTH@ , which represents the OAuth authorization type.
saType :: Lens' SourceAuth SourceAuthType
saType = lens _saType (\ s a -> s{_saType = a});

instance FromJSON SourceAuth where
        parseJSON
          = withObject "SourceAuth"
              (\ x ->
                 SourceAuth' <$> (x .:? "resource") <*> (x .: "type"))

instance Hashable SourceAuth

instance NFData SourceAuth

instance ToJSON SourceAuth where
        toJSON SourceAuth'{..}
          = object
              (catMaybes
                 [("resource" .=) <$> _saResource,
                  Just ("type" .= _saType)])

-- | A tag, consisting of a key and a value.
--
--
-- This tag is available for use by AWS services that support tags in AWS CodeBuild.
--
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
-- * 'tagValue' - The tag's value.
--
-- * 'tagKey' - The tag's key.
tag
    :: Tag
tag =
    Tag'
    { _tagValue = Nothing
    , _tagKey = Nothing
    }

-- | The tag's value.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | The tag's key.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "value") <*> (x .:? "key"))

instance Hashable Tag

instance NFData Tag

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [("value" .=) <$> _tagValue, ("key" .=) <$> _tagKey])
