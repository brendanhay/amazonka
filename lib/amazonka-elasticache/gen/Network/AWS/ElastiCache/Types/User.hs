{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.User
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.User
  ( User (..),

    -- * Smart constructor
    mkUser,

    -- * Lenses
    uARN,
    uAccessString,
    uAuthentication,
    uEngine,
    uStatus,
    uUserGroupIds,
    uUserId,
    uUserName,
  )
where

import qualified Network.AWS.ElastiCache.Types.Authentication as Types
import qualified Network.AWS.ElastiCache.Types.Engine as Types
import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.ElastiCache.Types.UserGroupId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkUser' smart constructor.
data User = User'
  { -- | The Amazon Resource Name (ARN) of the user account.
    arn :: Core.Maybe Types.String,
    -- | Access permissions string used for this user account.
    accessString :: Core.Maybe Types.String,
    -- | Denotes whether the user requires a password to authenticate.
    authentication :: Core.Maybe Types.Authentication,
    -- | Must be Redis.
    engine :: Core.Maybe Types.Engine,
    -- | Indicates the user status. Can be "active", "modifying" or "deleting".
    status :: Core.Maybe Types.String,
    -- | Returns a list of the user group IDs the user belongs to.
    userGroupIds :: Core.Maybe [Types.UserGroupId],
    -- | The ID of the user.
    userId :: Core.Maybe Types.String,
    -- | The username of the user.
    userName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'User' value with any optional fields omitted.
mkUser ::
  User
mkUser =
  User'
    { arn = Core.Nothing,
      accessString = Core.Nothing,
      authentication = Core.Nothing,
      engine = Core.Nothing,
      status = Core.Nothing,
      userGroupIds = Core.Nothing,
      userId = Core.Nothing,
      userName = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the user account.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uARN :: Lens.Lens' User (Core.Maybe Types.String)
uARN = Lens.field @"arn"
{-# DEPRECATED uARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Access permissions string used for this user account.
--
-- /Note:/ Consider using 'accessString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uAccessString :: Lens.Lens' User (Core.Maybe Types.String)
uAccessString = Lens.field @"accessString"
{-# DEPRECATED uAccessString "Use generic-lens or generic-optics with 'accessString' instead." #-}

-- | Denotes whether the user requires a password to authenticate.
--
-- /Note:/ Consider using 'authentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uAuthentication :: Lens.Lens' User (Core.Maybe Types.Authentication)
uAuthentication = Lens.field @"authentication"
{-# DEPRECATED uAuthentication "Use generic-lens or generic-optics with 'authentication' instead." #-}

-- | Must be Redis.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uEngine :: Lens.Lens' User (Core.Maybe Types.Engine)
uEngine = Lens.field @"engine"
{-# DEPRECATED uEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | Indicates the user status. Can be "active", "modifying" or "deleting".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uStatus :: Lens.Lens' User (Core.Maybe Types.String)
uStatus = Lens.field @"status"
{-# DEPRECATED uStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Returns a list of the user group IDs the user belongs to.
--
-- /Note:/ Consider using 'userGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUserGroupIds :: Lens.Lens' User (Core.Maybe [Types.UserGroupId])
uUserGroupIds = Lens.field @"userGroupIds"
{-# DEPRECATED uUserGroupIds "Use generic-lens or generic-optics with 'userGroupIds' instead." #-}

-- | The ID of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUserId :: Lens.Lens' User (Core.Maybe Types.String)
uUserId = Lens.field @"userId"
{-# DEPRECATED uUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The username of the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUserName :: Lens.Lens' User (Core.Maybe Types.String)
uUserName = Lens.field @"userName"
{-# DEPRECATED uUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Core.FromXML User where
  parseXML x =
    User'
      Core.<$> (x Core..@? "ARN")
      Core.<*> (x Core..@? "AccessString")
      Core.<*> (x Core..@? "Authentication")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "UserGroupIds" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "UserId")
      Core.<*> (x Core..@? "UserName")
