{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.GroupOwnerSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.GroupOwnerSetting
  ( GroupOwnerSetting (..),

    -- * Smart constructor
    mkGroupOwnerSetting,

    -- * Lenses
    gosAutoAddGroupOwner,
    gosGroupOwner,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Group owner related settings for local resources.
--
-- /See:/ 'mkGroupOwnerSetting' smart constructor.
data GroupOwnerSetting = GroupOwnerSetting'
  { -- | If true, AWS IoT Greengrass automatically adds the specified Linux OS group owner of the resource to the Lambda process privileges. Thus the Lambda process will have the file access permissions of the added Linux group.
    autoAddGroupOwner :: Core.Maybe Core.Bool,
    -- | The name of the Linux OS group whose privileges will be added to the Lambda process. This field is optional.
    groupOwner :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GroupOwnerSetting' value with any optional fields omitted.
mkGroupOwnerSetting ::
  GroupOwnerSetting
mkGroupOwnerSetting =
  GroupOwnerSetting'
    { autoAddGroupOwner = Core.Nothing,
      groupOwner = Core.Nothing
    }

-- | If true, AWS IoT Greengrass automatically adds the specified Linux OS group owner of the resource to the Lambda process privileges. Thus the Lambda process will have the file access permissions of the added Linux group.
--
-- /Note:/ Consider using 'autoAddGroupOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosAutoAddGroupOwner :: Lens.Lens' GroupOwnerSetting (Core.Maybe Core.Bool)
gosAutoAddGroupOwner = Lens.field @"autoAddGroupOwner"
{-# DEPRECATED gosAutoAddGroupOwner "Use generic-lens or generic-optics with 'autoAddGroupOwner' instead." #-}

-- | The name of the Linux OS group whose privileges will be added to the Lambda process. This field is optional.
--
-- /Note:/ Consider using 'groupOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosGroupOwner :: Lens.Lens' GroupOwnerSetting (Core.Maybe Core.Text)
gosGroupOwner = Lens.field @"groupOwner"
{-# DEPRECATED gosGroupOwner "Use generic-lens or generic-optics with 'groupOwner' instead." #-}

instance Core.FromJSON GroupOwnerSetting where
  toJSON GroupOwnerSetting {..} =
    Core.object
      ( Core.catMaybes
          [ ("AutoAddGroupOwner" Core..=) Core.<$> autoAddGroupOwner,
            ("GroupOwner" Core..=) Core.<$> groupOwner
          ]
      )

instance Core.FromJSON GroupOwnerSetting where
  parseJSON =
    Core.withObject "GroupOwnerSetting" Core.$
      \x ->
        GroupOwnerSetting'
          Core.<$> (x Core..:? "AutoAddGroupOwner") Core.<*> (x Core..:? "GroupOwner")
