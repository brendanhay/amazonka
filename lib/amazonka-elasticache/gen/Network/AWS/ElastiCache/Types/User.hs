{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.User
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.User
  ( User (..)
  -- * Smart constructor
  , mkUser
  -- * Lenses
  , uARN
  , uAccessString
  , uAuthentication
  , uEngine
  , uStatus
  , uUserGroupIds
  , uUserId
  , uUserName
  ) where

import qualified Network.AWS.ElastiCache.Types.Authentication as Types
import qualified Network.AWS.ElastiCache.Types.Engine as Types
import qualified Network.AWS.ElastiCache.Types.UserGroupId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkUser' smart constructor.
data User = User'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the user account.
  , accessString :: Core.Maybe Core.Text
    -- ^ Access permissions string used for this user account.
  , authentication :: Core.Maybe Types.Authentication
    -- ^ Denotes whether the user requires a password to authenticate.
  , engine :: Core.Maybe Types.Engine
    -- ^ Must be Redis. 
  , status :: Core.Maybe Core.Text
    -- ^ Indicates the user status. Can be "active", "modifying" or "deleting".
  , userGroupIds :: Core.Maybe [Types.UserGroupId]
    -- ^ Returns a list of the user group IDs the user belongs to.
  , userId :: Core.Maybe Core.Text
    -- ^ The ID of the user.
  , userName :: Core.Maybe Core.Text
    -- ^ The username of the user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'User' value with any optional fields omitted.
mkUser
    :: User
mkUser
  = User'{arn = Core.Nothing, accessString = Core.Nothing,
          authentication = Core.Nothing, engine = Core.Nothing,
          status = Core.Nothing, userGroupIds = Core.Nothing,
          userId = Core.Nothing, userName = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the user account.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uARN :: Lens.Lens' User (Core.Maybe Core.Text)
uARN = Lens.field @"arn"
{-# INLINEABLE uARN #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | Access permissions string used for this user account.
--
-- /Note:/ Consider using 'accessString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uAccessString :: Lens.Lens' User (Core.Maybe Core.Text)
uAccessString = Lens.field @"accessString"
{-# INLINEABLE uAccessString #-}
{-# DEPRECATED accessString "Use generic-lens or generic-optics with 'accessString' instead"  #-}

-- | Denotes whether the user requires a password to authenticate.
--
-- /Note:/ Consider using 'authentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uAuthentication :: Lens.Lens' User (Core.Maybe Types.Authentication)
uAuthentication = Lens.field @"authentication"
{-# INLINEABLE uAuthentication #-}
{-# DEPRECATED authentication "Use generic-lens or generic-optics with 'authentication' instead"  #-}

-- | Must be Redis. 
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uEngine :: Lens.Lens' User (Core.Maybe Types.Engine)
uEngine = Lens.field @"engine"
{-# INLINEABLE uEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | Indicates the user status. Can be "active", "modifying" or "deleting".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uStatus :: Lens.Lens' User (Core.Maybe Core.Text)
uStatus = Lens.field @"status"
{-# INLINEABLE uStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Returns a list of the user group IDs the user belongs to.
--
-- /Note:/ Consider using 'userGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUserGroupIds :: Lens.Lens' User (Core.Maybe [Types.UserGroupId])
uUserGroupIds = Lens.field @"userGroupIds"
{-# INLINEABLE uUserGroupIds #-}
{-# DEPRECATED userGroupIds "Use generic-lens or generic-optics with 'userGroupIds' instead"  #-}

-- | The ID of the user.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUserId :: Lens.Lens' User (Core.Maybe Core.Text)
uUserId = Lens.field @"userId"
{-# INLINEABLE uUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

-- | The username of the user.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUserName :: Lens.Lens' User (Core.Maybe Core.Text)
uUserName = Lens.field @"userName"
{-# INLINEABLE uUserName #-}
{-# DEPRECATED userName "Use generic-lens or generic-optics with 'userName' instead"  #-}

instance Core.FromXML User where
        parseXML x
          = User' Core.<$>
              (x Core..@? "ARN") Core.<*> x Core..@? "AccessString" Core.<*>
                x Core..@? "Authentication"
                Core.<*> x Core..@? "Engine"
                Core.<*> x Core..@? "Status"
                Core.<*>
                x Core..@? "UserGroupIds" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "UserId"
                Core.<*> x Core..@? "UserName"
