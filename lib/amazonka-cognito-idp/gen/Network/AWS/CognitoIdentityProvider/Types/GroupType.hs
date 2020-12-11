-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.GroupType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.GroupType
  ( GroupType (..),

    -- * Smart constructor
    mkGroupType,

    -- * Lenses
    gtLastModifiedDate,
    gtUserPoolId,
    gtCreationDate,
    gtPrecedence,
    gtGroupName,
    gtDescription,
    gtRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The group type.
--
-- /See:/ 'mkGroupType' smart constructor.
data GroupType = GroupType'
  { lastModifiedDate ::
      Lude.Maybe Lude.Timestamp,
    userPoolId :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Timestamp,
    precedence :: Lude.Maybe Lude.Natural,
    groupName :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GroupType' with the minimum fields required to make a request.
--
-- * 'creationDate' - The date the group was created.
-- * 'description' - A string containing the description of the group.
-- * 'groupName' - The name of the group.
-- * 'lastModifiedDate' - The date the group was last modified.
-- * 'precedence' - A nonnegative integer value that specifies the precedence of this group relative to the other groups that a user can belong to in the user pool. If a user belongs to two or more groups, it is the group with the highest precedence whose role ARN will be used in the @cognito:roles@ and @cognito:preferred_role@ claims in the user's tokens. Groups with higher @Precedence@ values take precedence over groups with lower @Precedence@ values or with null @Precedence@ values.
--
-- Two groups can have the same @Precedence@ value. If this happens, neither group takes precedence over the other. If two groups with the same @Precedence@ have the same role ARN, that role is used in the @cognito:preferred_role@ claim in tokens for users in each group. If the two groups have different role ARNs, the @cognito:preferred_role@ claim is not set in users' tokens.
-- The default @Precedence@ value is null.
-- * 'roleARN' - The role ARN for the group.
-- * 'userPoolId' - The user pool ID for the user pool.
mkGroupType ::
  GroupType
mkGroupType =
  GroupType'
    { lastModifiedDate = Lude.Nothing,
      userPoolId = Lude.Nothing,
      creationDate = Lude.Nothing,
      precedence = Lude.Nothing,
      groupName = Lude.Nothing,
      description = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The date the group was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtLastModifiedDate :: Lens.Lens' GroupType (Lude.Maybe Lude.Timestamp)
gtLastModifiedDate = Lens.lens (lastModifiedDate :: GroupType -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: GroupType)
{-# DEPRECATED gtLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtUserPoolId :: Lens.Lens' GroupType (Lude.Maybe Lude.Text)
gtUserPoolId = Lens.lens (userPoolId :: GroupType -> Lude.Maybe Lude.Text) (\s a -> s {userPoolId = a} :: GroupType)
{-# DEPRECATED gtUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The date the group was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtCreationDate :: Lens.Lens' GroupType (Lude.Maybe Lude.Timestamp)
gtCreationDate = Lens.lens (creationDate :: GroupType -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: GroupType)
{-# DEPRECATED gtCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | A nonnegative integer value that specifies the precedence of this group relative to the other groups that a user can belong to in the user pool. If a user belongs to two or more groups, it is the group with the highest precedence whose role ARN will be used in the @cognito:roles@ and @cognito:preferred_role@ claims in the user's tokens. Groups with higher @Precedence@ values take precedence over groups with lower @Precedence@ values or with null @Precedence@ values.
--
-- Two groups can have the same @Precedence@ value. If this happens, neither group takes precedence over the other. If two groups with the same @Precedence@ have the same role ARN, that role is used in the @cognito:preferred_role@ claim in tokens for users in each group. If the two groups have different role ARNs, the @cognito:preferred_role@ claim is not set in users' tokens.
-- The default @Precedence@ value is null.
--
-- /Note:/ Consider using 'precedence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtPrecedence :: Lens.Lens' GroupType (Lude.Maybe Lude.Natural)
gtPrecedence = Lens.lens (precedence :: GroupType -> Lude.Maybe Lude.Natural) (\s a -> s {precedence = a} :: GroupType)
{-# DEPRECATED gtPrecedence "Use generic-lens or generic-optics with 'precedence' instead." #-}

-- | The name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtGroupName :: Lens.Lens' GroupType (Lude.Maybe Lude.Text)
gtGroupName = Lens.lens (groupName :: GroupType -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: GroupType)
{-# DEPRECATED gtGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | A string containing the description of the group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtDescription :: Lens.Lens' GroupType (Lude.Maybe Lude.Text)
gtDescription = Lens.lens (description :: GroupType -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GroupType)
{-# DEPRECATED gtDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The role ARN for the group.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtRoleARN :: Lens.Lens' GroupType (Lude.Maybe Lude.Text)
gtRoleARN = Lens.lens (roleARN :: GroupType -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: GroupType)
{-# DEPRECATED gtRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON GroupType where
  parseJSON =
    Lude.withObject
      "GroupType"
      ( \x ->
          GroupType'
            Lude.<$> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "UserPoolId")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "Precedence")
            Lude.<*> (x Lude..:? "GroupName")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "RoleArn")
      )
