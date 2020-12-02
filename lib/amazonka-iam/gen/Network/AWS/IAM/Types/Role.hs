{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.Role
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.Role where

import Network.AWS.IAM.Types.AttachedPermissionsBoundary
import Network.AWS.IAM.Types.RoleLastUsed
import Network.AWS.IAM.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an IAM role. This structure is returned as a response element in several API operations that interact with roles.
--
--
--
-- /See:/ 'role'' smart constructor.
data Role = Role'
  { _rMaxSessionDuration :: !(Maybe Nat),
    _rAssumeRolePolicyDocument :: !(Maybe Text),
    _rRoleLastUsed :: !(Maybe RoleLastUsed),
    _rPermissionsBoundary :: !(Maybe AttachedPermissionsBoundary),
    _rDescription :: !(Maybe Text),
    _rTags :: !(Maybe [Tag]),
    _rPath :: !Text,
    _rRoleName :: !Text,
    _rRoleId :: !Text,
    _rARN :: !Text,
    _rCreateDate :: !ISO8601
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Role' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rMaxSessionDuration' - The maximum session duration (in seconds) for the specified role. Anyone who uses the AWS CLI, or API to assume the role can specify the duration using the optional @DurationSeconds@ API parameter or @duration-seconds@ CLI parameter.
--
-- * 'rAssumeRolePolicyDocument' - The policy that grants an entity permission to assume the role.
--
-- * 'rRoleLastUsed' - Contains information about the last time that an IAM role was used. This includes the date and time and the Region in which the role was last used. Activity is only reported for the trailing 400 days. This period can be shorter if your Region began supporting these features within the last year. The role might have been used more than 400 days ago. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions Where Data Is Tracked> in the /IAM User Guide/ .
--
-- * 'rPermissionsBoundary' - The ARN of the policy used to set the permissions boundary for the role. For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
--
-- * 'rDescription' - A description of the role that you provide.
--
-- * 'rTags' - A list of tags that are attached to the specified role. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
--
-- * 'rPath' - The path to the role. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- * 'rRoleName' - The friendly name that identifies the role.
--
-- * 'rRoleId' - The stable and unique string identifying the role. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- * 'rARN' - The Amazon Resource Name (ARN) specifying the role. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ guide.
--
-- * 'rCreateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the role was created.
role' ::
  -- | 'rPath'
  Text ->
  -- | 'rRoleName'
  Text ->
  -- | 'rRoleId'
  Text ->
  -- | 'rARN'
  Text ->
  -- | 'rCreateDate'
  UTCTime ->
  Role
role' pPath_ pRoleName_ pRoleId_ pARN_ pCreateDate_ =
  Role'
    { _rMaxSessionDuration = Nothing,
      _rAssumeRolePolicyDocument = Nothing,
      _rRoleLastUsed = Nothing,
      _rPermissionsBoundary = Nothing,
      _rDescription = Nothing,
      _rTags = Nothing,
      _rPath = pPath_,
      _rRoleName = pRoleName_,
      _rRoleId = pRoleId_,
      _rARN = pARN_,
      _rCreateDate = _Time # pCreateDate_
    }

-- | The maximum session duration (in seconds) for the specified role. Anyone who uses the AWS CLI, or API to assume the role can specify the duration using the optional @DurationSeconds@ API parameter or @duration-seconds@ CLI parameter.
rMaxSessionDuration :: Lens' Role (Maybe Natural)
rMaxSessionDuration = lens _rMaxSessionDuration (\s a -> s {_rMaxSessionDuration = a}) . mapping _Nat

-- | The policy that grants an entity permission to assume the role.
rAssumeRolePolicyDocument :: Lens' Role (Maybe Text)
rAssumeRolePolicyDocument = lens _rAssumeRolePolicyDocument (\s a -> s {_rAssumeRolePolicyDocument = a})

-- | Contains information about the last time that an IAM role was used. This includes the date and time and the Region in which the role was last used. Activity is only reported for the trailing 400 days. This period can be shorter if your Region began supporting these features within the last year. The role might have been used more than 400 days ago. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#access-advisor_tracking-period Regions Where Data Is Tracked> in the /IAM User Guide/ .
rRoleLastUsed :: Lens' Role (Maybe RoleLastUsed)
rRoleLastUsed = lens _rRoleLastUsed (\s a -> s {_rRoleLastUsed = a})

-- | The ARN of the policy used to set the permissions boundary for the role. For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
rPermissionsBoundary :: Lens' Role (Maybe AttachedPermissionsBoundary)
rPermissionsBoundary = lens _rPermissionsBoundary (\s a -> s {_rPermissionsBoundary = a})

-- | A description of the role that you provide.
rDescription :: Lens' Role (Maybe Text)
rDescription = lens _rDescription (\s a -> s {_rDescription = a})

-- | A list of tags that are attached to the specified role. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
rTags :: Lens' Role [Tag]
rTags = lens _rTags (\s a -> s {_rTags = a}) . _Default . _Coerce

-- | The path to the role. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
rPath :: Lens' Role Text
rPath = lens _rPath (\s a -> s {_rPath = a})

-- | The friendly name that identifies the role.
rRoleName :: Lens' Role Text
rRoleName = lens _rRoleName (\s a -> s {_rRoleName = a})

-- | The stable and unique string identifying the role. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
rRoleId :: Lens' Role Text
rRoleId = lens _rRoleId (\s a -> s {_rRoleId = a})

-- | The Amazon Resource Name (ARN) specifying the role. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ guide.
rARN :: Lens' Role Text
rARN = lens _rARN (\s a -> s {_rARN = a})

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the role was created.
rCreateDate :: Lens' Role UTCTime
rCreateDate = lens _rCreateDate (\s a -> s {_rCreateDate = a}) . _Time

instance FromXML Role where
  parseXML x =
    Role'
      <$> (x .@? "MaxSessionDuration")
      <*> (x .@? "AssumeRolePolicyDocument")
      <*> (x .@? "RoleLastUsed")
      <*> (x .@? "PermissionsBoundary")
      <*> (x .@? "Description")
      <*> (x .@? "Tags" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@ "Path")
      <*> (x .@ "RoleName")
      <*> (x .@ "RoleId")
      <*> (x .@ "Arn")
      <*> (x .@ "CreateDate")

instance Hashable Role

instance NFData Role
