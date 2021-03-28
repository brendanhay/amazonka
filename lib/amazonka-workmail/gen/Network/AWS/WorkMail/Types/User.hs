{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.User
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkMail.Types.User
  ( User (..)
  -- * Smart constructor
  , mkUser
  -- * Lenses
  , uDisabledDate
  , uDisplayName
  , uEmail
  , uEnabledDate
  , uId
  , uName
  , uState
  , uUserRole
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkMail.Types.Email as Types
import qualified Network.AWS.WorkMail.Types.EntityState as Types
import qualified Network.AWS.WorkMail.Types.UserName as Types
import qualified Network.AWS.WorkMail.Types.UserRole as Types
import qualified Network.AWS.WorkMail.Types.WorkMailIdentifier as Types

-- | The representation of an Amazon WorkMail user.
--
-- /See:/ 'mkUser' smart constructor.
data User = User'
  { disabledDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date indicating when the user was disabled from Amazon WorkMail use.
  , displayName :: Core.Maybe Core.Text
    -- ^ The display name of the user.
  , email :: Core.Maybe Types.Email
    -- ^ The email of the user.
  , enabledDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date indicating when the user was enabled for Amazon WorkMail use.
  , id :: Core.Maybe Types.WorkMailIdentifier
    -- ^ The identifier of the user.
  , name :: Core.Maybe Types.UserName
    -- ^ The name of the user.
  , state :: Core.Maybe Types.EntityState
    -- ^ The state of the user, which can be ENABLED, DISABLED, or DELETED.
  , userRole :: Core.Maybe Types.UserRole
    -- ^ The role of the user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'User' value with any optional fields omitted.
mkUser
    :: User
mkUser
  = User'{disabledDate = Core.Nothing, displayName = Core.Nothing,
          email = Core.Nothing, enabledDate = Core.Nothing,
          id = Core.Nothing, name = Core.Nothing, state = Core.Nothing,
          userRole = Core.Nothing}

-- | The date indicating when the user was disabled from Amazon WorkMail use.
--
-- /Note:/ Consider using 'disabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDisabledDate :: Lens.Lens' User (Core.Maybe Core.NominalDiffTime)
uDisabledDate = Lens.field @"disabledDate"
{-# INLINEABLE uDisabledDate #-}
{-# DEPRECATED disabledDate "Use generic-lens or generic-optics with 'disabledDate' instead"  #-}

-- | The display name of the user.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDisplayName :: Lens.Lens' User (Core.Maybe Core.Text)
uDisplayName = Lens.field @"displayName"
{-# INLINEABLE uDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | The email of the user.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uEmail :: Lens.Lens' User (Core.Maybe Types.Email)
uEmail = Lens.field @"email"
{-# INLINEABLE uEmail #-}
{-# DEPRECATED email "Use generic-lens or generic-optics with 'email' instead"  #-}

-- | The date indicating when the user was enabled for Amazon WorkMail use.
--
-- /Note:/ Consider using 'enabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uEnabledDate :: Lens.Lens' User (Core.Maybe Core.NominalDiffTime)
uEnabledDate = Lens.field @"enabledDate"
{-# INLINEABLE uEnabledDate #-}
{-# DEPRECATED enabledDate "Use generic-lens or generic-optics with 'enabledDate' instead"  #-}

-- | The identifier of the user.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uId :: Lens.Lens' User (Core.Maybe Types.WorkMailIdentifier)
uId = Lens.field @"id"
{-# INLINEABLE uId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the user.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uName :: Lens.Lens' User (Core.Maybe Types.UserName)
uName = Lens.field @"name"
{-# INLINEABLE uName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The state of the user, which can be ENABLED, DISABLED, or DELETED.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uState :: Lens.Lens' User (Core.Maybe Types.EntityState)
uState = Lens.field @"state"
{-# INLINEABLE uState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The role of the user.
--
-- /Note:/ Consider using 'userRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUserRole :: Lens.Lens' User (Core.Maybe Types.UserRole)
uUserRole = Lens.field @"userRole"
{-# INLINEABLE uUserRole #-}
{-# DEPRECATED userRole "Use generic-lens or generic-optics with 'userRole' instead"  #-}

instance Core.FromJSON User where
        parseJSON
          = Core.withObject "User" Core.$
              \ x ->
                User' Core.<$>
                  (x Core..:? "DisabledDate") Core.<*> x Core..:? "DisplayName"
                    Core.<*> x Core..:? "Email"
                    Core.<*> x Core..:? "EnabledDate"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "State"
                    Core.<*> x Core..:? "UserRole"
