{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.User
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.User
  ( User (..),

    -- * Smart constructor
    mkUser,

    -- * Lenses
    uDisabledDate,
    uDisplayName,
    uEmail,
    uEnabledDate,
    uId,
    uName,
    uState,
    uUserRole,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkMail.Types.Email as Types
import qualified Network.AWS.WorkMail.Types.EntityState as Types
import qualified Network.AWS.WorkMail.Types.String as Types
import qualified Network.AWS.WorkMail.Types.UserName as Types
import qualified Network.AWS.WorkMail.Types.UserRole as Types
import qualified Network.AWS.WorkMail.Types.WorkMailIdentifier as Types

-- | The representation of an Amazon WorkMail user.
--
-- /See:/ 'mkUser' smart constructor.
data User = User'
  { -- | The date indicating when the user was disabled from Amazon WorkMail use.
    disabledDate :: Core.Maybe Core.NominalDiffTime,
    -- | The display name of the user.
    displayName :: Core.Maybe Types.String,
    -- | The email of the user.
    email :: Core.Maybe Types.Email,
    -- | The date indicating when the user was enabled for Amazon WorkMail use.
    enabledDate :: Core.Maybe Core.NominalDiffTime,
    -- | The identifier of the user.
    id :: Core.Maybe Types.WorkMailIdentifier,
    -- | The name of the user.
    name :: Core.Maybe Types.UserName,
    -- | The state of the user, which can be ENABLED, DISABLED, or DELETED.
    state :: Core.Maybe Types.EntityState,
    -- | The role of the user.
    userRole :: Core.Maybe Types.UserRole
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'User' value with any optional fields omitted.
mkUser ::
  User
mkUser =
  User'
    { disabledDate = Core.Nothing,
      displayName = Core.Nothing,
      email = Core.Nothing,
      enabledDate = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      state = Core.Nothing,
      userRole = Core.Nothing
    }

-- | The date indicating when the user was disabled from Amazon WorkMail use.
--
-- /Note:/ Consider using 'disabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDisabledDate :: Lens.Lens' User (Core.Maybe Core.NominalDiffTime)
uDisabledDate = Lens.field @"disabledDate"
{-# DEPRECATED uDisabledDate "Use generic-lens or generic-optics with 'disabledDate' instead." #-}

-- | The display name of the user.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDisplayName :: Lens.Lens' User (Core.Maybe Types.String)
uDisplayName = Lens.field @"displayName"
{-# DEPRECATED uDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The email of the user.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uEmail :: Lens.Lens' User (Core.Maybe Types.Email)
uEmail = Lens.field @"email"
{-# DEPRECATED uEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The date indicating when the user was enabled for Amazon WorkMail use.
--
-- /Note:/ Consider using 'enabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uEnabledDate :: Lens.Lens' User (Core.Maybe Core.NominalDiffTime)
uEnabledDate = Lens.field @"enabledDate"
{-# DEPRECATED uEnabledDate "Use generic-lens or generic-optics with 'enabledDate' instead." #-}

-- | The identifier of the user.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uId :: Lens.Lens' User (Core.Maybe Types.WorkMailIdentifier)
uId = Lens.field @"id"
{-# DEPRECATED uId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name of the user.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uName :: Lens.Lens' User (Core.Maybe Types.UserName)
uName = Lens.field @"name"
{-# DEPRECATED uName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The state of the user, which can be ENABLED, DISABLED, or DELETED.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uState :: Lens.Lens' User (Core.Maybe Types.EntityState)
uState = Lens.field @"state"
{-# DEPRECATED uState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The role of the user.
--
-- /Note:/ Consider using 'userRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUserRole :: Lens.Lens' User (Core.Maybe Types.UserRole)
uUserRole = Lens.field @"userRole"
{-# DEPRECATED uUserRole "Use generic-lens or generic-optics with 'userRole' instead." #-}

instance Core.FromJSON User where
  parseJSON =
    Core.withObject "User" Core.$
      \x ->
        User'
          Core.<$> (x Core..:? "DisabledDate")
          Core.<*> (x Core..:? "DisplayName")
          Core.<*> (x Core..:? "Email")
          Core.<*> (x Core..:? "EnabledDate")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "State")
          Core.<*> (x Core..:? "UserRole")
