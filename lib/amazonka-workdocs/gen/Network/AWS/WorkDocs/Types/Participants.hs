{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.Participants
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.Participants
  ( Participants (..),

    -- * Smart constructor
    mkParticipants,

    -- * Lenses
    pGroups,
    pUsers,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkDocs.Types.GroupMetadata as Types
import qualified Network.AWS.WorkDocs.Types.UserMetadata as Types

-- | Describes the users or user groups.
--
-- /See:/ 'mkParticipants' smart constructor.
data Participants = Participants'
  { -- | The list of user groups.
    groups :: Core.Maybe [Types.GroupMetadata],
    -- | The list of users.
    users :: Core.Maybe [Types.UserMetadata]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Participants' value with any optional fields omitted.
mkParticipants ::
  Participants
mkParticipants =
  Participants' {groups = Core.Nothing, users = Core.Nothing}

-- | The list of user groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pGroups :: Lens.Lens' Participants (Core.Maybe [Types.GroupMetadata])
pGroups = Lens.field @"groups"
{-# DEPRECATED pGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The list of users.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pUsers :: Lens.Lens' Participants (Core.Maybe [Types.UserMetadata])
pUsers = Lens.field @"users"
{-# DEPRECATED pUsers "Use generic-lens or generic-optics with 'users' instead." #-}

instance Core.FromJSON Participants where
  parseJSON =
    Core.withObject "Participants" Core.$
      \x ->
        Participants'
          Core.<$> (x Core..:? "Groups") Core.<*> (x Core..:? "Users")
