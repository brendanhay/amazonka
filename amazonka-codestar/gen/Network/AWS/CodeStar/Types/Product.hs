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

-- | Information about a user's profile in AWS CodeStar.
--
--
--
-- /See:/ 'userProfileSummary' smart constructor.
data UserProfileSummary = UserProfileSummary'
  { _upsSshPublicKey :: !(Maybe Text)
  , _upsUserARN      :: !(Maybe Text)
  , _upsEmailAddress :: !(Maybe (Sensitive Text))
  , _upsDisplayName  :: !(Maybe Text)
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
upsDisplayName = lens _upsDisplayName (\ s a -> s{_upsDisplayName = a})

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
