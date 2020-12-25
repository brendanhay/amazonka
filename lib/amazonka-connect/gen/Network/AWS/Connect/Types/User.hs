{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.User
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.User
  ( User (..),

    -- * Smart constructor
    mkUser,

    -- * Lenses
    uArn,
    uDirectoryUserId,
    uHierarchyGroupId,
    uId,
    uIdentityInfo,
    uPhoneConfig,
    uRoutingProfileId,
    uSecurityProfileIds,
    uTags,
    uUsername,
  )
where

import qualified Network.AWS.Connect.Types.ARN as Types
import qualified Network.AWS.Connect.Types.DirectoryUserId as Types
import qualified Network.AWS.Connect.Types.HierarchyGroupId as Types
import qualified Network.AWS.Connect.Types.Id as Types
import qualified Network.AWS.Connect.Types.RoutingProfileId as Types
import qualified Network.AWS.Connect.Types.SecurityProfileId as Types
import qualified Network.AWS.Connect.Types.TagKey as Types
import qualified Network.AWS.Connect.Types.TagValue as Types
import qualified Network.AWS.Connect.Types.UserIdentityInfo as Types
import qualified Network.AWS.Connect.Types.UserPhoneConfig as Types
import qualified Network.AWS.Connect.Types.Username as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a user account for a Amazon Connect instance.
--
-- /See:/ 'mkUser' smart constructor.
data User = User'
  { -- | The Amazon Resource Name (ARN) of the user account.
    arn :: Core.Maybe Types.ARN,
    -- | The identifier of the user account in the directory used for identity management.
    directoryUserId :: Core.Maybe Types.DirectoryUserId,
    -- | The identifier of the hierarchy group for the user.
    hierarchyGroupId :: Core.Maybe Types.HierarchyGroupId,
    -- | The identifier of the user account.
    id :: Core.Maybe Types.Id,
    -- | Information about the user identity.
    identityInfo :: Core.Maybe Types.UserIdentityInfo,
    -- | Information about the phone configuration for the user.
    phoneConfig :: Core.Maybe Types.UserPhoneConfig,
    -- | The identifier of the routing profile for the user.
    routingProfileId :: Core.Maybe Types.RoutingProfileId,
    -- | The identifiers of the security profiles for the user.
    securityProfileIds :: Core.Maybe (Core.NonEmpty Types.SecurityProfileId),
    -- | The tags.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue),
    -- | The user name assigned to the user account.
    username :: Core.Maybe Types.Username
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'User' value with any optional fields omitted.
mkUser ::
  User
mkUser =
  User'
    { arn = Core.Nothing,
      directoryUserId = Core.Nothing,
      hierarchyGroupId = Core.Nothing,
      id = Core.Nothing,
      identityInfo = Core.Nothing,
      phoneConfig = Core.Nothing,
      routingProfileId = Core.Nothing,
      securityProfileIds = Core.Nothing,
      tags = Core.Nothing,
      username = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the user account.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uArn :: Lens.Lens' User (Core.Maybe Types.ARN)
uArn = Lens.field @"arn"
{-# DEPRECATED uArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The identifier of the user account in the directory used for identity management.
--
-- /Note:/ Consider using 'directoryUserId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDirectoryUserId :: Lens.Lens' User (Core.Maybe Types.DirectoryUserId)
uDirectoryUserId = Lens.field @"directoryUserId"
{-# DEPRECATED uDirectoryUserId "Use generic-lens or generic-optics with 'directoryUserId' instead." #-}

-- | The identifier of the hierarchy group for the user.
--
-- /Note:/ Consider using 'hierarchyGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uHierarchyGroupId :: Lens.Lens' User (Core.Maybe Types.HierarchyGroupId)
uHierarchyGroupId = Lens.field @"hierarchyGroupId"
{-# DEPRECATED uHierarchyGroupId "Use generic-lens or generic-optics with 'hierarchyGroupId' instead." #-}

-- | The identifier of the user account.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uId :: Lens.Lens' User (Core.Maybe Types.Id)
uId = Lens.field @"id"
{-# DEPRECATED uId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Information about the user identity.
--
-- /Note:/ Consider using 'identityInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uIdentityInfo :: Lens.Lens' User (Core.Maybe Types.UserIdentityInfo)
uIdentityInfo = Lens.field @"identityInfo"
{-# DEPRECATED uIdentityInfo "Use generic-lens or generic-optics with 'identityInfo' instead." #-}

-- | Information about the phone configuration for the user.
--
-- /Note:/ Consider using 'phoneConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uPhoneConfig :: Lens.Lens' User (Core.Maybe Types.UserPhoneConfig)
uPhoneConfig = Lens.field @"phoneConfig"
{-# DEPRECATED uPhoneConfig "Use generic-lens or generic-optics with 'phoneConfig' instead." #-}

-- | The identifier of the routing profile for the user.
--
-- /Note:/ Consider using 'routingProfileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uRoutingProfileId :: Lens.Lens' User (Core.Maybe Types.RoutingProfileId)
uRoutingProfileId = Lens.field @"routingProfileId"
{-# DEPRECATED uRoutingProfileId "Use generic-lens or generic-optics with 'routingProfileId' instead." #-}

-- | The identifiers of the security profiles for the user.
--
-- /Note:/ Consider using 'securityProfileIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uSecurityProfileIds :: Lens.Lens' User (Core.Maybe (Core.NonEmpty Types.SecurityProfileId))
uSecurityProfileIds = Lens.field @"securityProfileIds"
{-# DEPRECATED uSecurityProfileIds "Use generic-lens or generic-optics with 'securityProfileIds' instead." #-}

-- | The tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uTags :: Lens.Lens' User (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
uTags = Lens.field @"tags"
{-# DEPRECATED uTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The user name assigned to the user account.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUsername :: Lens.Lens' User (Core.Maybe Types.Username)
uUsername = Lens.field @"username"
{-# DEPRECATED uUsername "Use generic-lens or generic-optics with 'username' instead." #-}

instance Core.FromJSON User where
  parseJSON =
    Core.withObject "User" Core.$
      \x ->
        User'
          Core.<$> (x Core..:? "Arn")
          Core.<*> (x Core..:? "DirectoryUserId")
          Core.<*> (x Core..:? "HierarchyGroupId")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "IdentityInfo")
          Core.<*> (x Core..:? "PhoneConfig")
          Core.<*> (x Core..:? "RoutingProfileId")
          Core.<*> (x Core..:? "SecurityProfileIds")
          Core.<*> (x Core..:? "Tags")
          Core.<*> (x Core..:? "Username")
