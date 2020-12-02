{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.User
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.User where

import Network.AWS.IAM.Types.AttachedPermissionsBoundary
import Network.AWS.IAM.Types.Tag
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an IAM user entity.
--
--
-- This data type is used as a response element in the following operations:
--
--     * 'CreateUser'
--
--     * 'GetUser'
--
--     * 'ListUsers'
--
--
--
--
-- /See:/ 'user' smart constructor.
data User = User'
  { _uPasswordLastUsed :: !(Maybe ISO8601),
    _uPermissionsBoundary :: !(Maybe AttachedPermissionsBoundary),
    _uTags :: !(Maybe [Tag]),
    _uPath :: !Text,
    _uUserName :: !Text,
    _uUserId :: !Text,
    _uARN :: !Text,
    _uCreateDate :: !ISO8601
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uPasswordLastUsed' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the user's password was last used to sign in to an AWS website. For a list of AWS websites that capture a user's last sign-in time, see the <https://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Credential Reports> topic in the /IAM User Guide/ . If a password is used more than once in a five-minute span, only the first use is returned in this field. If the field is null (no value), then it indicates that they never signed in with a password. This can be because:     * The user never had a password.     * A password exists but has not been used since IAM started tracking this information on October 20, 2014. A null value does not mean that the user /never/ had a password. Also, if the user does not currently have a password but had one in the past, then this field contains the date and time the most recent password was used. This value is returned only in the 'GetUser' and 'ListUsers' operations.
--
-- * 'uPermissionsBoundary' - The ARN of the policy used to set the permissions boundary for the user. For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
--
-- * 'uTags' - A list of tags that are associated with the specified user. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
--
-- * 'uPath' - The path to the user. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- * 'uUserName' - The friendly name identifying the user.
--
-- * 'uUserId' - The stable and unique string identifying the user. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- * 'uARN' - The Amazon Resource Name (ARN) that identifies the user. For more information about ARNs and how to use ARNs in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- * 'uCreateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the user was created.
user ::
  -- | 'uPath'
  Text ->
  -- | 'uUserName'
  Text ->
  -- | 'uUserId'
  Text ->
  -- | 'uARN'
  Text ->
  -- | 'uCreateDate'
  UTCTime ->
  User
user pPath_ pUserName_ pUserId_ pARN_ pCreateDate_ =
  User'
    { _uPasswordLastUsed = Nothing,
      _uPermissionsBoundary = Nothing,
      _uTags = Nothing,
      _uPath = pPath_,
      _uUserName = pUserName_,
      _uUserId = pUserId_,
      _uARN = pARN_,
      _uCreateDate = _Time # pCreateDate_
    }

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the user's password was last used to sign in to an AWS website. For a list of AWS websites that capture a user's last sign-in time, see the <https://docs.aws.amazon.com/IAM/latest/UserGuide/credential-reports.html Credential Reports> topic in the /IAM User Guide/ . If a password is used more than once in a five-minute span, only the first use is returned in this field. If the field is null (no value), then it indicates that they never signed in with a password. This can be because:     * The user never had a password.     * A password exists but has not been used since IAM started tracking this information on October 20, 2014. A null value does not mean that the user /never/ had a password. Also, if the user does not currently have a password but had one in the past, then this field contains the date and time the most recent password was used. This value is returned only in the 'GetUser' and 'ListUsers' operations.
uPasswordLastUsed :: Lens' User (Maybe UTCTime)
uPasswordLastUsed = lens _uPasswordLastUsed (\s a -> s {_uPasswordLastUsed = a}) . mapping _Time

-- | The ARN of the policy used to set the permissions boundary for the user. For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
uPermissionsBoundary :: Lens' User (Maybe AttachedPermissionsBoundary)
uPermissionsBoundary = lens _uPermissionsBoundary (\s a -> s {_uPermissionsBoundary = a})

-- | A list of tags that are associated with the specified user. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
uTags :: Lens' User [Tag]
uTags = lens _uTags (\s a -> s {_uTags = a}) . _Default . _Coerce

-- | The path to the user. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
uPath :: Lens' User Text
uPath = lens _uPath (\s a -> s {_uPath = a})

-- | The friendly name identifying the user.
uUserName :: Lens' User Text
uUserName = lens _uUserName (\s a -> s {_uUserName = a})

-- | The stable and unique string identifying the user. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
uUserId :: Lens' User Text
uUserId = lens _uUserId (\s a -> s {_uUserId = a})

-- | The Amazon Resource Name (ARN) that identifies the user. For more information about ARNs and how to use ARNs in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
uARN :: Lens' User Text
uARN = lens _uARN (\s a -> s {_uARN = a})

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the user was created.
uCreateDate :: Lens' User UTCTime
uCreateDate = lens _uCreateDate (\s a -> s {_uCreateDate = a}) . _Time

instance FromXML User where
  parseXML x =
    User'
      <$> (x .@? "PasswordLastUsed")
      <*> (x .@? "PermissionsBoundary")
      <*> (x .@? "Tags" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@ "Path")
      <*> (x .@ "UserName")
      <*> (x .@ "UserId")
      <*> (x .@ "Arn")
      <*> (x .@ "CreateDate")

instance Hashable User

instance NFData User
