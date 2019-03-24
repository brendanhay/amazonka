{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeStar.Types.Product where

import Network.AWS.CodeStar.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Location and destination information about the source code files provided with the project request. The source code is uploaded to the new project source repository after project creation.
--
--
--
-- /See:/ 'code' smart constructor.
data Code = Code'
  { _cSource      :: !CodeSource
  , _cDestination :: !CodeDestination
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'Code' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cSource' - The location where the source code files provided with the project request are stored. AWS CodeStar retrieves the files during project creation.
--
-- * 'cDestination' - The repository to be created in AWS CodeStar. Valid values are AWS CodeCommit or GitHub. After AWS CodeStar provisions the new repository, the source code files provided with the project request are placed in the repository.
code
    :: CodeSource -- ^ 'cSource'
    -> CodeDestination -- ^ 'cDestination'
    -> Code
code pSource_ pDestination_ =
  Code' {_cSource = pSource_, _cDestination = pDestination_}


-- | The location where the source code files provided with the project request are stored. AWS CodeStar retrieves the files during project creation.
cSource :: Lens' Code CodeSource
cSource = lens _cSource (\ s a -> s{_cSource = a})

-- | The repository to be created in AWS CodeStar. Valid values are AWS CodeCommit or GitHub. After AWS CodeStar provisions the new repository, the source code files provided with the project request are placed in the repository.
cDestination :: Lens' Code CodeDestination
cDestination = lens _cDestination (\ s a -> s{_cDestination = a})

instance Hashable Code where

instance NFData Code where

instance ToJSON Code where
        toJSON Code'{..}
          = object
              (catMaybes
                 [Just ("source" .= _cSource),
                  Just ("destination" .= _cDestination)])

-- | Information about the AWS CodeCommit repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
--
--
--
-- /See:/ 'codeCommitCodeDestination' smart constructor.
newtype CodeCommitCodeDestination = CodeCommitCodeDestination'
  { _cccdName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CodeCommitCodeDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cccdName' - The name of the AWS CodeCommit repository to be created in AWS CodeStar.
codeCommitCodeDestination
    :: Text -- ^ 'cccdName'
    -> CodeCommitCodeDestination
codeCommitCodeDestination pName_ =
  CodeCommitCodeDestination' {_cccdName = pName_}


-- | The name of the AWS CodeCommit repository to be created in AWS CodeStar.
cccdName :: Lens' CodeCommitCodeDestination Text
cccdName = lens _cccdName (\ s a -> s{_cccdName = a})

instance Hashable CodeCommitCodeDestination where

instance NFData CodeCommitCodeDestination where

instance ToJSON CodeCommitCodeDestination where
        toJSON CodeCommitCodeDestination'{..}
          = object (catMaybes [Just ("name" .= _cccdName)])

-- | The repository to be created in AWS CodeStar. Valid values are AWS CodeCommit or GitHub. After AWS CodeStar provisions the new repository, the source code files provided with the project request are placed in the repository.
--
--
--
-- /See:/ 'codeDestination' smart constructor.
data CodeDestination = CodeDestination'
  { _cdCodeCommit :: !(Maybe CodeCommitCodeDestination)
  , _cdGitHub     :: !(Maybe GitHubCodeDestination)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CodeDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdCodeCommit' - Information about the AWS CodeCommit repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
--
-- * 'cdGitHub' - Information about the GitHub repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
codeDestination
    :: CodeDestination
codeDestination =
  CodeDestination' {_cdCodeCommit = Nothing, _cdGitHub = Nothing}


-- | Information about the AWS CodeCommit repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
cdCodeCommit :: Lens' CodeDestination (Maybe CodeCommitCodeDestination)
cdCodeCommit = lens _cdCodeCommit (\ s a -> s{_cdCodeCommit = a})

-- | Information about the GitHub repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
cdGitHub :: Lens' CodeDestination (Maybe GitHubCodeDestination)
cdGitHub = lens _cdGitHub (\ s a -> s{_cdGitHub = a})

instance Hashable CodeDestination where

instance NFData CodeDestination where

instance ToJSON CodeDestination where
        toJSON CodeDestination'{..}
          = object
              (catMaybes
                 [("codeCommit" .=) <$> _cdCodeCommit,
                  ("gitHub" .=) <$> _cdGitHub])

-- | The location where the source code files provided with the project request are stored. AWS CodeStar retrieves the files during project creation.
--
--
--
-- /See:/ 'codeSource' smart constructor.
newtype CodeSource = CodeSource'
  { _csS3 :: S3Location
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CodeSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csS3' - Information about the Amazon S3 location where the source code files provided with the project request are stored.
codeSource
    :: S3Location -- ^ 'csS3'
    -> CodeSource
codeSource pS3_ = CodeSource' {_csS3 = pS3_}


-- | Information about the Amazon S3 location where the source code files provided with the project request are stored.
csS3 :: Lens' CodeSource S3Location
csS3 = lens _csS3 (\ s a -> s{_csS3 = a})

instance Hashable CodeSource where

instance NFData CodeSource where

instance ToJSON CodeSource where
        toJSON CodeSource'{..}
          = object (catMaybes [Just ("s3" .= _csS3)])

-- | Information about the GitHub repository to be created in AWS CodeStar. This is where the source code files provided with the project request will be uploaded after project creation.
--
--
--
-- /See:/ 'gitHubCodeDestination' smart constructor.
data GitHubCodeDestination = GitHubCodeDestination'
  { _ghcdDescription       :: !(Maybe Text)
  , _ghcdName              :: !Text
  , _ghcdType              :: !Text
  , _ghcdOwner             :: !Text
  , _ghcdPrivateRepository :: !Bool
  , _ghcdIssuesEnabled     :: !Bool
  , _ghcdToken             :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GitHubCodeDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghcdDescription' - Description for the GitHub repository to be created in AWS CodeStar. This description displays in GitHub after the repository is created.
--
-- * 'ghcdName' - Name of the GitHub repository to be created in AWS CodeStar.
--
-- * 'ghcdType' - The type of GitHub repository to be created in AWS CodeStar. Valid values are User or Organization.
--
-- * 'ghcdOwner' - The GitHub username for the owner of the GitHub repository to be created in AWS CodeStar. If this repository should be owned by a GitHub organization, provide its name.
--
-- * 'ghcdPrivateRepository' - Whether the GitHub repository is to be a private repository.
--
-- * 'ghcdIssuesEnabled' - Whether to enable issues for the GitHub repository.
--
-- * 'ghcdToken' - The GitHub user's personal access token for the GitHub repository.
gitHubCodeDestination
    :: Text -- ^ 'ghcdName'
    -> Text -- ^ 'ghcdType'
    -> Text -- ^ 'ghcdOwner'
    -> Bool -- ^ 'ghcdPrivateRepository'
    -> Bool -- ^ 'ghcdIssuesEnabled'
    -> Text -- ^ 'ghcdToken'
    -> GitHubCodeDestination
gitHubCodeDestination pName_ pType_ pOwner_ pPrivateRepository_ pIssuesEnabled_ pToken_ =
  GitHubCodeDestination'
    { _ghcdDescription = Nothing
    , _ghcdName = pName_
    , _ghcdType = pType_
    , _ghcdOwner = pOwner_
    , _ghcdPrivateRepository = pPrivateRepository_
    , _ghcdIssuesEnabled = pIssuesEnabled_
    , _ghcdToken = _Sensitive # pToken_
    }


-- | Description for the GitHub repository to be created in AWS CodeStar. This description displays in GitHub after the repository is created.
ghcdDescription :: Lens' GitHubCodeDestination (Maybe Text)
ghcdDescription = lens _ghcdDescription (\ s a -> s{_ghcdDescription = a})

-- | Name of the GitHub repository to be created in AWS CodeStar.
ghcdName :: Lens' GitHubCodeDestination Text
ghcdName = lens _ghcdName (\ s a -> s{_ghcdName = a})

-- | The type of GitHub repository to be created in AWS CodeStar. Valid values are User or Organization.
ghcdType :: Lens' GitHubCodeDestination Text
ghcdType = lens _ghcdType (\ s a -> s{_ghcdType = a})

-- | The GitHub username for the owner of the GitHub repository to be created in AWS CodeStar. If this repository should be owned by a GitHub organization, provide its name.
ghcdOwner :: Lens' GitHubCodeDestination Text
ghcdOwner = lens _ghcdOwner (\ s a -> s{_ghcdOwner = a})

-- | Whether the GitHub repository is to be a private repository.
ghcdPrivateRepository :: Lens' GitHubCodeDestination Bool
ghcdPrivateRepository = lens _ghcdPrivateRepository (\ s a -> s{_ghcdPrivateRepository = a})

-- | Whether to enable issues for the GitHub repository.
ghcdIssuesEnabled :: Lens' GitHubCodeDestination Bool
ghcdIssuesEnabled = lens _ghcdIssuesEnabled (\ s a -> s{_ghcdIssuesEnabled = a})

-- | The GitHub user's personal access token for the GitHub repository.
ghcdToken :: Lens' GitHubCodeDestination Text
ghcdToken = lens _ghcdToken (\ s a -> s{_ghcdToken = a}) . _Sensitive

instance Hashable GitHubCodeDestination where

instance NFData GitHubCodeDestination where

instance ToJSON GitHubCodeDestination where
        toJSON GitHubCodeDestination'{..}
          = object
              (catMaybes
                 [("description" .=) <$> _ghcdDescription,
                  Just ("name" .= _ghcdName),
                  Just ("type" .= _ghcdType),
                  Just ("owner" .= _ghcdOwner),
                  Just ("privateRepository" .= _ghcdPrivateRepository),
                  Just ("issuesEnabled" .= _ghcdIssuesEnabled),
                  Just ("token" .= _ghcdToken)])

-- | An indication of whether a project creation or deletion is failed or successful.
--
--
--
-- /See:/ 'projectStatus' smart constructor.
data ProjectStatus = ProjectStatus'
  { _psReason :: !(Maybe Text)
  , _psState  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProjectStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psReason' - In the case of a project creation or deletion failure, a reason for the failure.
--
-- * 'psState' - The phase of completion for a project creation or deletion.
projectStatus
    :: Text -- ^ 'psState'
    -> ProjectStatus
projectStatus pState_ = ProjectStatus' {_psReason = Nothing, _psState = pState_}


-- | In the case of a project creation or deletion failure, a reason for the failure.
psReason :: Lens' ProjectStatus (Maybe Text)
psReason = lens _psReason (\ s a -> s{_psReason = a})

-- | The phase of completion for a project creation or deletion.
psState :: Lens' ProjectStatus Text
psState = lens _psState (\ s a -> s{_psState = a})

instance FromJSON ProjectStatus where
        parseJSON
          = withObject "ProjectStatus"
              (\ x ->
                 ProjectStatus' <$>
                   (x .:? "reason") <*> (x .: "state"))

instance Hashable ProjectStatus where

instance NFData ProjectStatus where

-- | Information about the metadata for a project.
--
--
--
-- /See:/ 'projectSummary' smart constructor.
data ProjectSummary = ProjectSummary'
  { _psProjectARN :: !(Maybe Text)
  , _psProjectId  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ProjectSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psProjectARN' - The Amazon Resource Name (ARN) of the project.
--
-- * 'psProjectId' - The ID of the project.
projectSummary
    :: ProjectSummary
projectSummary =
  ProjectSummary' {_psProjectARN = Nothing, _psProjectId = Nothing}


-- | The Amazon Resource Name (ARN) of the project.
psProjectARN :: Lens' ProjectSummary (Maybe Text)
psProjectARN = lens _psProjectARN (\ s a -> s{_psProjectARN = a})

-- | The ID of the project.
psProjectId :: Lens' ProjectSummary (Maybe Text)
psProjectId = lens _psProjectId (\ s a -> s{_psProjectId = a})

instance FromJSON ProjectSummary where
        parseJSON
          = withObject "ProjectSummary"
              (\ x ->
                 ProjectSummary' <$>
                   (x .:? "projectArn") <*> (x .:? "projectId"))

instance Hashable ProjectSummary where

instance NFData ProjectSummary where

-- | Information about a resource for a project.
--
--
--
-- /See:/ 'resource' smart constructor.
newtype Resource = Resource'
  { _rId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rId' - The Amazon Resource Name (ARN) of the resource.
resource
    :: Text -- ^ 'rId'
    -> Resource
resource pId_ = Resource' {_rId = pId_}


-- | The Amazon Resource Name (ARN) of the resource.
rId :: Lens' Resource Text
rId = lens _rId (\ s a -> s{_rId = a})

instance FromJSON Resource where
        parseJSON
          = withObject "Resource"
              (\ x -> Resource' <$> (x .: "id"))

instance Hashable Resource where

instance NFData Resource where

-- | The Amazon S3 location where the source code files provided with the project request are stored.
--
--
--
-- /See:/ 's3Location' smart constructor.
data S3Location = S3Location'
  { _slBucketKey  :: !(Maybe Text)
  , _slBucketName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slBucketKey' - The Amazon S3 object key where the source code files provided with the project request are stored.
--
-- * 'slBucketName' - The Amazon S3 bucket name where the source code files provided with the project request are stored.
s3Location
    :: S3Location
s3Location = S3Location' {_slBucketKey = Nothing, _slBucketName = Nothing}


-- | The Amazon S3 object key where the source code files provided with the project request are stored.
slBucketKey :: Lens' S3Location (Maybe Text)
slBucketKey = lens _slBucketKey (\ s a -> s{_slBucketKey = a})

-- | The Amazon S3 bucket name where the source code files provided with the project request are stored.
slBucketName :: Lens' S3Location (Maybe Text)
slBucketName = lens _slBucketName (\ s a -> s{_slBucketName = a})

instance Hashable S3Location where

instance NFData S3Location where

instance ToJSON S3Location where
        toJSON S3Location'{..}
          = object
              (catMaybes
                 [("bucketKey" .=) <$> _slBucketKey,
                  ("bucketName" .=) <$> _slBucketName])

-- | Information about a team member in a project.
--
--
--
-- /See:/ 'teamMember' smart constructor.
data TeamMember = TeamMember'
  { _tmRemoteAccessAllowed :: !(Maybe Bool)
  , _tmUserARN             :: !Text
  , _tmProjectRole         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TeamMember' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tmRemoteAccessAllowed' - Whether the user is allowed to remotely access project resources using an SSH public/private key pair.
--
-- * 'tmUserARN' - The Amazon Resource Name (ARN) of the user in IAM.
--
-- * 'tmProjectRole' - The role assigned to the user in the project. Project roles have different levels of access. For more information, see <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams> in the /AWS CodeStar User Guide/ .
teamMember
    :: Text -- ^ 'tmUserARN'
    -> Text -- ^ 'tmProjectRole'
    -> TeamMember
teamMember pUserARN_ pProjectRole_ =
  TeamMember'
    { _tmRemoteAccessAllowed = Nothing
    , _tmUserARN = pUserARN_
    , _tmProjectRole = pProjectRole_
    }


-- | Whether the user is allowed to remotely access project resources using an SSH public/private key pair.
tmRemoteAccessAllowed :: Lens' TeamMember (Maybe Bool)
tmRemoteAccessAllowed = lens _tmRemoteAccessAllowed (\ s a -> s{_tmRemoteAccessAllowed = a})

-- | The Amazon Resource Name (ARN) of the user in IAM.
tmUserARN :: Lens' TeamMember Text
tmUserARN = lens _tmUserARN (\ s a -> s{_tmUserARN = a})

-- | The role assigned to the user in the project. Project roles have different levels of access. For more information, see <http://docs.aws.amazon.com/codestar/latest/userguide/working-with-teams.html Working with Teams> in the /AWS CodeStar User Guide/ .
tmProjectRole :: Lens' TeamMember Text
tmProjectRole = lens _tmProjectRole (\ s a -> s{_tmProjectRole = a})

instance FromJSON TeamMember where
        parseJSON
          = withObject "TeamMember"
              (\ x ->
                 TeamMember' <$>
                   (x .:? "remoteAccessAllowed") <*> (x .: "userArn")
                     <*> (x .: "projectRole"))

instance Hashable TeamMember where

instance NFData TeamMember where

-- | The toolchain template file provided with the project request. AWS CodeStar uses the template to provision the toolchain stack in AWS CloudFormation.
--
--
--
-- /See:/ 'toolchain' smart constructor.
data Toolchain = Toolchain'
  { _tStackParameters :: !(Maybe (Map Text (Sensitive Text)))
  , _tRoleARN         :: !(Maybe Text)
  , _tSource          :: !ToolchainSource
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'Toolchain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tStackParameters' - The list of parameter overrides to be passed into the toolchain template during stack provisioning, if any.
--
-- * 'tRoleARN' - The service role ARN for AWS CodeStar to use for the toolchain template during stack provisioning.
--
-- * 'tSource' - The Amazon S3 location where the toolchain template file provided with the project request is stored. AWS CodeStar retrieves the file during project creation.
toolchain
    :: ToolchainSource -- ^ 'tSource'
    -> Toolchain
toolchain pSource_ =
  Toolchain'
    {_tStackParameters = Nothing, _tRoleARN = Nothing, _tSource = pSource_}


-- | The list of parameter overrides to be passed into the toolchain template during stack provisioning, if any.
tStackParameters :: Lens' Toolchain (HashMap Text Text)
tStackParameters = lens _tStackParameters (\ s a -> s{_tStackParameters = a}) . _Default . _Map

-- | The service role ARN for AWS CodeStar to use for the toolchain template during stack provisioning.
tRoleARN :: Lens' Toolchain (Maybe Text)
tRoleARN = lens _tRoleARN (\ s a -> s{_tRoleARN = a})

-- | The Amazon S3 location where the toolchain template file provided with the project request is stored. AWS CodeStar retrieves the file during project creation.
tSource :: Lens' Toolchain ToolchainSource
tSource = lens _tSource (\ s a -> s{_tSource = a})

instance Hashable Toolchain where

instance NFData Toolchain where

instance ToJSON Toolchain where
        toJSON Toolchain'{..}
          = object
              (catMaybes
                 [("stackParameters" .=) <$> _tStackParameters,
                  ("roleArn" .=) <$> _tRoleARN,
                  Just ("source" .= _tSource)])

-- | The Amazon S3 location where the toolchain template file provided with the project request is stored. AWS CodeStar retrieves the file during project creation.
--
--
--
-- /See:/ 'toolchainSource' smart constructor.
newtype ToolchainSource = ToolchainSource'
  { _tsS3 :: S3Location
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ToolchainSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsS3' - The Amazon S3 bucket where the toolchain template file provided with the project request is stored.
toolchainSource
    :: S3Location -- ^ 'tsS3'
    -> ToolchainSource
toolchainSource pS3_ = ToolchainSource' {_tsS3 = pS3_}


-- | The Amazon S3 bucket where the toolchain template file provided with the project request is stored.
tsS3 :: Lens' ToolchainSource S3Location
tsS3 = lens _tsS3 (\ s a -> s{_tsS3 = a})

instance Hashable ToolchainSource where

instance NFData ToolchainSource where

instance ToJSON ToolchainSource where
        toJSON ToolchainSource'{..}
          = object (catMaybes [Just ("s3" .= _tsS3)])

-- | Information about a user's profile in AWS CodeStar.
--
--
--
-- /See:/ 'userProfileSummary' smart constructor.
data UserProfileSummary = UserProfileSummary'
  { _upsSshPublicKey :: !(Maybe Text)
  , _upsUserARN      :: !(Maybe Text)
  , _upsEmailAddress :: !(Maybe (Sensitive Text))
  , _upsDisplayName  :: !(Maybe (Sensitive Text))
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UserProfileSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upsSshPublicKey' - The SSH public key associated with the user in AWS CodeStar. If a project owner allows the user remote access to project resources, this public key will be used along with the user's private key for SSH access.
--
-- * 'upsUserARN' - The Amazon Resource Name (ARN) of the user in IAM.
--
-- * 'upsEmailAddress' - The email address associated with the user.
--
-- * 'upsDisplayName' - The display name of a user in AWS CodeStar. For example, this could be set to both first and last name ("Mary Major") or a single name ("Mary"). The display name is also used to generate the initial icon associated with the user in AWS CodeStar projects. If spaces are included in the display name, the first character that appears after the space will be used as the second character in the user initial icon. The initial icon displays a maximum of two characters, so a display name with more than one space (for example "Mary Jane Major") would generate an initial icon using the first character and the first character after the space ("MJ", not "MM").
userProfileSummary
    :: UserProfileSummary
userProfileSummary =
  UserProfileSummary'
    { _upsSshPublicKey = Nothing
    , _upsUserARN = Nothing
    , _upsEmailAddress = Nothing
    , _upsDisplayName = Nothing
    }


-- | The SSH public key associated with the user in AWS CodeStar. If a project owner allows the user remote access to project resources, this public key will be used along with the user's private key for SSH access.
upsSshPublicKey :: Lens' UserProfileSummary (Maybe Text)
upsSshPublicKey = lens _upsSshPublicKey (\ s a -> s{_upsSshPublicKey = a})

-- | The Amazon Resource Name (ARN) of the user in IAM.
upsUserARN :: Lens' UserProfileSummary (Maybe Text)
upsUserARN = lens _upsUserARN (\ s a -> s{_upsUserARN = a})

-- | The email address associated with the user.
upsEmailAddress :: Lens' UserProfileSummary (Maybe Text)
upsEmailAddress = lens _upsEmailAddress (\ s a -> s{_upsEmailAddress = a}) . mapping _Sensitive

-- | The display name of a user in AWS CodeStar. For example, this could be set to both first and last name ("Mary Major") or a single name ("Mary"). The display name is also used to generate the initial icon associated with the user in AWS CodeStar projects. If spaces are included in the display name, the first character that appears after the space will be used as the second character in the user initial icon. The initial icon displays a maximum of two characters, so a display name with more than one space (for example "Mary Jane Major") would generate an initial icon using the first character and the first character after the space ("MJ", not "MM").
upsDisplayName :: Lens' UserProfileSummary (Maybe Text)
upsDisplayName = lens _upsDisplayName (\ s a -> s{_upsDisplayName = a}) . mapping _Sensitive

instance FromJSON UserProfileSummary where
        parseJSON
          = withObject "UserProfileSummary"
              (\ x ->
                 UserProfileSummary' <$>
                   (x .:? "sshPublicKey") <*> (x .:? "userArn") <*>
                     (x .:? "emailAddress")
                     <*> (x .:? "displayName"))

instance Hashable UserProfileSummary where

instance NFData UserProfileSummary where
