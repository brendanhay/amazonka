{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.GroupType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.GroupType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The group type.
--
--
--
-- /See:/ 'groupType' smart constructor.
data GroupType = GroupType'
  { _gtLastModifiedDate :: !(Maybe POSIX),
    _gtUserPoolId :: !(Maybe Text),
    _gtCreationDate :: !(Maybe POSIX),
    _gtPrecedence :: !(Maybe Nat),
    _gtGroupName :: !(Maybe Text),
    _gtDescription :: !(Maybe Text),
    _gtRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GroupType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtLastModifiedDate' - The date the group was last modified.
--
-- * 'gtUserPoolId' - The user pool ID for the user pool.
--
-- * 'gtCreationDate' - The date the group was created.
--
-- * 'gtPrecedence' - A nonnegative integer value that specifies the precedence of this group relative to the other groups that a user can belong to in the user pool. If a user belongs to two or more groups, it is the group with the highest precedence whose role ARN will be used in the @cognito:roles@ and @cognito:preferred_role@ claims in the user's tokens. Groups with higher @Precedence@ values take precedence over groups with lower @Precedence@ values or with null @Precedence@ values. Two groups can have the same @Precedence@ value. If this happens, neither group takes precedence over the other. If two groups with the same @Precedence@ have the same role ARN, that role is used in the @cognito:preferred_role@ claim in tokens for users in each group. If the two groups have different role ARNs, the @cognito:preferred_role@ claim is not set in users' tokens. The default @Precedence@ value is null.
--
-- * 'gtGroupName' - The name of the group.
--
-- * 'gtDescription' - A string containing the description of the group.
--
-- * 'gtRoleARN' - The role ARN for the group.
groupType ::
  GroupType
groupType =
  GroupType'
    { _gtLastModifiedDate = Nothing,
      _gtUserPoolId = Nothing,
      _gtCreationDate = Nothing,
      _gtPrecedence = Nothing,
      _gtGroupName = Nothing,
      _gtDescription = Nothing,
      _gtRoleARN = Nothing
    }

-- | The date the group was last modified.
gtLastModifiedDate :: Lens' GroupType (Maybe UTCTime)
gtLastModifiedDate = lens _gtLastModifiedDate (\s a -> s {_gtLastModifiedDate = a}) . mapping _Time

-- | The user pool ID for the user pool.
gtUserPoolId :: Lens' GroupType (Maybe Text)
gtUserPoolId = lens _gtUserPoolId (\s a -> s {_gtUserPoolId = a})

-- | The date the group was created.
gtCreationDate :: Lens' GroupType (Maybe UTCTime)
gtCreationDate = lens _gtCreationDate (\s a -> s {_gtCreationDate = a}) . mapping _Time

-- | A nonnegative integer value that specifies the precedence of this group relative to the other groups that a user can belong to in the user pool. If a user belongs to two or more groups, it is the group with the highest precedence whose role ARN will be used in the @cognito:roles@ and @cognito:preferred_role@ claims in the user's tokens. Groups with higher @Precedence@ values take precedence over groups with lower @Precedence@ values or with null @Precedence@ values. Two groups can have the same @Precedence@ value. If this happens, neither group takes precedence over the other. If two groups with the same @Precedence@ have the same role ARN, that role is used in the @cognito:preferred_role@ claim in tokens for users in each group. If the two groups have different role ARNs, the @cognito:preferred_role@ claim is not set in users' tokens. The default @Precedence@ value is null.
gtPrecedence :: Lens' GroupType (Maybe Natural)
gtPrecedence = lens _gtPrecedence (\s a -> s {_gtPrecedence = a}) . mapping _Nat

-- | The name of the group.
gtGroupName :: Lens' GroupType (Maybe Text)
gtGroupName = lens _gtGroupName (\s a -> s {_gtGroupName = a})

-- | A string containing the description of the group.
gtDescription :: Lens' GroupType (Maybe Text)
gtDescription = lens _gtDescription (\s a -> s {_gtDescription = a})

-- | The role ARN for the group.
gtRoleARN :: Lens' GroupType (Maybe Text)
gtRoleARN = lens _gtRoleARN (\s a -> s {_gtRoleARN = a})

instance FromJSON GroupType where
  parseJSON =
    withObject
      "GroupType"
      ( \x ->
          GroupType'
            <$> (x .:? "LastModifiedDate")
            <*> (x .:? "UserPoolId")
            <*> (x .:? "CreationDate")
            <*> (x .:? "Precedence")
            <*> (x .:? "GroupName")
            <*> (x .:? "Description")
            <*> (x .:? "RoleArn")
      )

instance Hashable GroupType

instance NFData GroupType
