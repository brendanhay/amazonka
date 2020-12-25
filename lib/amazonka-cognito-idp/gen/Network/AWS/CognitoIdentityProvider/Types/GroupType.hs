{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    gtCreationDate,
    gtDescription,
    gtGroupName,
    gtLastModifiedDate,
    gtPrecedence,
    gtRoleArn,
    gtUserPoolId,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.DescriptionType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.GroupNameType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.RoleArn as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.UserPoolId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The group type.
--
-- /See:/ 'mkGroupType' smart constructor.
data GroupType = GroupType'
  { -- | The date the group was created.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | A string containing the description of the group.
    description :: Core.Maybe Types.DescriptionType,
    -- | The name of the group.
    groupName :: Core.Maybe Types.GroupNameType,
    -- | The date the group was last modified.
    lastModifiedDate :: Core.Maybe Core.NominalDiffTime,
    -- | A nonnegative integer value that specifies the precedence of this group relative to the other groups that a user can belong to in the user pool. If a user belongs to two or more groups, it is the group with the highest precedence whose role ARN will be used in the @cognito:roles@ and @cognito:preferred_role@ claims in the user's tokens. Groups with higher @Precedence@ values take precedence over groups with lower @Precedence@ values or with null @Precedence@ values.
    --
    -- Two groups can have the same @Precedence@ value. If this happens, neither group takes precedence over the other. If two groups with the same @Precedence@ have the same role ARN, that role is used in the @cognito:preferred_role@ claim in tokens for users in each group. If the two groups have different role ARNs, the @cognito:preferred_role@ claim is not set in users' tokens.
    -- The default @Precedence@ value is null.
    precedence :: Core.Maybe Core.Natural,
    -- | The role ARN for the group.
    roleArn :: Core.Maybe Types.RoleArn,
    -- | The user pool ID for the user pool.
    userPoolId :: Core.Maybe Types.UserPoolId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GroupType' value with any optional fields omitted.
mkGroupType ::
  GroupType
mkGroupType =
  GroupType'
    { creationDate = Core.Nothing,
      description = Core.Nothing,
      groupName = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      precedence = Core.Nothing,
      roleArn = Core.Nothing,
      userPoolId = Core.Nothing
    }

-- | The date the group was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtCreationDate :: Lens.Lens' GroupType (Core.Maybe Core.NominalDiffTime)
gtCreationDate = Lens.field @"creationDate"
{-# DEPRECATED gtCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | A string containing the description of the group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtDescription :: Lens.Lens' GroupType (Core.Maybe Types.DescriptionType)
gtDescription = Lens.field @"description"
{-# DEPRECATED gtDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtGroupName :: Lens.Lens' GroupType (Core.Maybe Types.GroupNameType)
gtGroupName = Lens.field @"groupName"
{-# DEPRECATED gtGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The date the group was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtLastModifiedDate :: Lens.Lens' GroupType (Core.Maybe Core.NominalDiffTime)
gtLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED gtLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | A nonnegative integer value that specifies the precedence of this group relative to the other groups that a user can belong to in the user pool. If a user belongs to two or more groups, it is the group with the highest precedence whose role ARN will be used in the @cognito:roles@ and @cognito:preferred_role@ claims in the user's tokens. Groups with higher @Precedence@ values take precedence over groups with lower @Precedence@ values or with null @Precedence@ values.
--
-- Two groups can have the same @Precedence@ value. If this happens, neither group takes precedence over the other. If two groups with the same @Precedence@ have the same role ARN, that role is used in the @cognito:preferred_role@ claim in tokens for users in each group. If the two groups have different role ARNs, the @cognito:preferred_role@ claim is not set in users' tokens.
-- The default @Precedence@ value is null.
--
-- /Note:/ Consider using 'precedence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtPrecedence :: Lens.Lens' GroupType (Core.Maybe Core.Natural)
gtPrecedence = Lens.field @"precedence"
{-# DEPRECATED gtPrecedence "Use generic-lens or generic-optics with 'precedence' instead." #-}

-- | The role ARN for the group.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtRoleArn :: Lens.Lens' GroupType (Core.Maybe Types.RoleArn)
gtRoleArn = Lens.field @"roleArn"
{-# DEPRECATED gtRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The user pool ID for the user pool.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtUserPoolId :: Lens.Lens' GroupType (Core.Maybe Types.UserPoolId)
gtUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED gtUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Core.FromJSON GroupType where
  parseJSON =
    Core.withObject "GroupType" Core.$
      \x ->
        GroupType'
          Core.<$> (x Core..:? "CreationDate")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "GroupName")
          Core.<*> (x Core..:? "LastModifiedDate")
          Core.<*> (x Core..:? "Precedence")
          Core.<*> (x Core..:? "RoleArn")
          Core.<*> (x Core..:? "UserPoolId")
