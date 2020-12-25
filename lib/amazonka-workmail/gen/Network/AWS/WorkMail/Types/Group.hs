{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.Group
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.Group
  ( Group (..),

    -- * Smart constructor
    mkGroup,

    -- * Lenses
    gDisabledDate,
    gEmail,
    gEnabledDate,
    gId,
    gName,
    gState,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkMail.Types.Email as Types
import qualified Network.AWS.WorkMail.Types.EntityState as Types
import qualified Network.AWS.WorkMail.Types.Id as Types
import qualified Network.AWS.WorkMail.Types.Name as Types

-- | The representation of an Amazon WorkMail group.
--
-- /See:/ 'mkGroup' smart constructor.
data Group = Group'
  { -- | The date indicating when the group was disabled from Amazon WorkMail use.
    disabledDate :: Core.Maybe Core.NominalDiffTime,
    -- | The email of the group.
    email :: Core.Maybe Types.Email,
    -- | The date indicating when the group was enabled for Amazon WorkMail use.
    enabledDate :: Core.Maybe Core.NominalDiffTime,
    -- | The identifier of the group.
    id :: Core.Maybe Types.Id,
    -- | The name of the group.
    name :: Core.Maybe Types.Name,
    -- | The state of the group, which can be ENABLED, DISABLED, or DELETED.
    state :: Core.Maybe Types.EntityState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Group' value with any optional fields omitted.
mkGroup ::
  Group
mkGroup =
  Group'
    { disabledDate = Core.Nothing,
      email = Core.Nothing,
      enabledDate = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      state = Core.Nothing
    }

-- | The date indicating when the group was disabled from Amazon WorkMail use.
--
-- /Note:/ Consider using 'disabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gDisabledDate :: Lens.Lens' Group (Core.Maybe Core.NominalDiffTime)
gDisabledDate = Lens.field @"disabledDate"
{-# DEPRECATED gDisabledDate "Use generic-lens or generic-optics with 'disabledDate' instead." #-}

-- | The email of the group.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gEmail :: Lens.Lens' Group (Core.Maybe Types.Email)
gEmail = Lens.field @"email"
{-# DEPRECATED gEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The date indicating when the group was enabled for Amazon WorkMail use.
--
-- /Note:/ Consider using 'enabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gEnabledDate :: Lens.Lens' Group (Core.Maybe Core.NominalDiffTime)
gEnabledDate = Lens.field @"enabledDate"
{-# DEPRECATED gEnabledDate "Use generic-lens or generic-optics with 'enabledDate' instead." #-}

-- | The identifier of the group.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gId :: Lens.Lens' Group (Core.Maybe Types.Id)
gId = Lens.field @"id"
{-# DEPRECATED gId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name of the group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gName :: Lens.Lens' Group (Core.Maybe Types.Name)
gName = Lens.field @"name"
{-# DEPRECATED gName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The state of the group, which can be ENABLED, DISABLED, or DELETED.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gState :: Lens.Lens' Group (Core.Maybe Types.EntityState)
gState = Lens.field @"state"
{-# DEPRECATED gState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromJSON Group where
  parseJSON =
    Core.withObject "Group" Core.$
      \x ->
        Group'
          Core.<$> (x Core..:? "DisabledDate")
          Core.<*> (x Core..:? "Email")
          Core.<*> (x Core..:? "EnabledDate")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "State")
