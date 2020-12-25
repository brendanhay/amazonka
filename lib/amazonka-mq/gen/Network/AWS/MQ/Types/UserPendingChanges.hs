{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.UserPendingChanges
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.UserPendingChanges
  ( UserPendingChanges (..),

    -- * Smart constructor
    mkUserPendingChanges,

    -- * Lenses
    upcConsoleAccess,
    upcGroups,
    upcPendingChange,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types.ChangeType as Types
import qualified Network.AWS.Prelude as Core

-- | Returns information about the status of the changes pending for the ActiveMQ user.
--
-- /See:/ 'mkUserPendingChanges' smart constructor.
data UserPendingChanges = UserPendingChanges'
  { -- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
    consoleAccess :: Core.Maybe Core.Bool,
    -- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
    groups :: Core.Maybe [Core.Text],
    -- | Required. The type of change pending for the ActiveMQ user.
    pendingChange :: Core.Maybe Types.ChangeType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserPendingChanges' value with any optional fields omitted.
mkUserPendingChanges ::
  UserPendingChanges
mkUserPendingChanges =
  UserPendingChanges'
    { consoleAccess = Core.Nothing,
      groups = Core.Nothing,
      pendingChange = Core.Nothing
    }

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
--
-- /Note:/ Consider using 'consoleAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcConsoleAccess :: Lens.Lens' UserPendingChanges (Core.Maybe Core.Bool)
upcConsoleAccess = Lens.field @"consoleAccess"
{-# DEPRECATED upcConsoleAccess "Use generic-lens or generic-optics with 'consoleAccess' instead." #-}

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcGroups :: Lens.Lens' UserPendingChanges (Core.Maybe [Core.Text])
upcGroups = Lens.field @"groups"
{-# DEPRECATED upcGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | Required. The type of change pending for the ActiveMQ user.
--
-- /Note:/ Consider using 'pendingChange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcPendingChange :: Lens.Lens' UserPendingChanges (Core.Maybe Types.ChangeType)
upcPendingChange = Lens.field @"pendingChange"
{-# DEPRECATED upcPendingChange "Use generic-lens or generic-optics with 'pendingChange' instead." #-}

instance Core.FromJSON UserPendingChanges where
  parseJSON =
    Core.withObject "UserPendingChanges" Core.$
      \x ->
        UserPendingChanges'
          Core.<$> (x Core..:? "consoleAccess")
          Core.<*> (x Core..:? "groups")
          Core.<*> (x Core..:? "pendingChange")
