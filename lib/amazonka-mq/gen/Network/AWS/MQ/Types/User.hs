{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.User
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MQ.Types.User
  ( User (..)
  -- * Smart constructor
  , mkUser
  -- * Lenses
  , uConsoleAccess
  , uGroups
  , uPassword
  , uUsername
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A user associated with the broker.
--
-- /See:/ 'mkUser' smart constructor.
data User = User'
  { consoleAccess :: Core.Maybe Core.Bool
    -- ^ Enables access to the ActiveMQ Web Console for the ActiveMQ user (Does not apply to RabbitMQ brokers).
  , groups :: Core.Maybe [Core.Text]
    -- ^ The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
  , password :: Core.Maybe Core.Text
    -- ^ Required. The password of the broker user. This value must be at least 12 characters long, must contain at least 4 unique characters, and must not contain commas.
  , username :: Core.Maybe Core.Text
    -- ^ Required. The username of the broker user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'User' value with any optional fields omitted.
mkUser
    :: User
mkUser
  = User'{consoleAccess = Core.Nothing, groups = Core.Nothing,
          password = Core.Nothing, username = Core.Nothing}

-- | Enables access to the ActiveMQ Web Console for the ActiveMQ user (Does not apply to RabbitMQ brokers).
--
-- /Note:/ Consider using 'consoleAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uConsoleAccess :: Lens.Lens' User (Core.Maybe Core.Bool)
uConsoleAccess = Lens.field @"consoleAccess"
{-# INLINEABLE uConsoleAccess #-}
{-# DEPRECATED consoleAccess "Use generic-lens or generic-optics with 'consoleAccess' instead"  #-}

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uGroups :: Lens.Lens' User (Core.Maybe [Core.Text])
uGroups = Lens.field @"groups"
{-# INLINEABLE uGroups #-}
{-# DEPRECATED groups "Use generic-lens or generic-optics with 'groups' instead"  #-}

-- | Required. The password of the broker user. This value must be at least 12 characters long, must contain at least 4 unique characters, and must not contain commas.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uPassword :: Lens.Lens' User (Core.Maybe Core.Text)
uPassword = Lens.field @"password"
{-# INLINEABLE uPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | Required. The username of the broker user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUsername :: Lens.Lens' User (Core.Maybe Core.Text)
uUsername = Lens.field @"username"
{-# INLINEABLE uUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.FromJSON User where
        toJSON User{..}
          = Core.object
              (Core.catMaybes
                 [("consoleAccess" Core..=) Core.<$> consoleAccess,
                  ("groups" Core..=) Core.<$> groups,
                  ("password" Core..=) Core.<$> password,
                  ("username" Core..=) Core.<$> username])
