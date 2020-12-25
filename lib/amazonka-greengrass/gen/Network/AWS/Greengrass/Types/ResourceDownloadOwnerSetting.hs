{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.ResourceDownloadOwnerSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ResourceDownloadOwnerSetting
  ( ResourceDownloadOwnerSetting (..),

    -- * Smart constructor
    mkResourceDownloadOwnerSetting,

    -- * Lenses
    rdosGroupOwner,
    rdosGroupPermission,
  )
where

import qualified Network.AWS.Greengrass.Types.Permission as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The owner setting for downloaded machine learning resources.
--
-- /See:/ 'mkResourceDownloadOwnerSetting' smart constructor.
data ResourceDownloadOwnerSetting = ResourceDownloadOwnerSetting'
  { -- | The group owner of the resource. This is the name of an existing Linux OS group on the system or a GID. The group's permissions are added to the Lambda process.
    groupOwner :: Core.Text,
    -- | The permissions that the group owner has to the resource. Valid values are ''rw'' (read/write) or ''ro'' (read-only).
    groupPermission :: Types.Permission
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceDownloadOwnerSetting' value with any optional fields omitted.
mkResourceDownloadOwnerSetting ::
  -- | 'groupOwner'
  Core.Text ->
  -- | 'groupPermission'
  Types.Permission ->
  ResourceDownloadOwnerSetting
mkResourceDownloadOwnerSetting groupOwner groupPermission =
  ResourceDownloadOwnerSetting' {groupOwner, groupPermission}

-- | The group owner of the resource. This is the name of an existing Linux OS group on the system or a GID. The group's permissions are added to the Lambda process.
--
-- /Note:/ Consider using 'groupOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdosGroupOwner :: Lens.Lens' ResourceDownloadOwnerSetting Core.Text
rdosGroupOwner = Lens.field @"groupOwner"
{-# DEPRECATED rdosGroupOwner "Use generic-lens or generic-optics with 'groupOwner' instead." #-}

-- | The permissions that the group owner has to the resource. Valid values are ''rw'' (read/write) or ''ro'' (read-only).
--
-- /Note:/ Consider using 'groupPermission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdosGroupPermission :: Lens.Lens' ResourceDownloadOwnerSetting Types.Permission
rdosGroupPermission = Lens.field @"groupPermission"
{-# DEPRECATED rdosGroupPermission "Use generic-lens or generic-optics with 'groupPermission' instead." #-}

instance Core.FromJSON ResourceDownloadOwnerSetting where
  toJSON ResourceDownloadOwnerSetting {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GroupOwner" Core..= groupOwner),
            Core.Just ("GroupPermission" Core..= groupPermission)
          ]
      )

instance Core.FromJSON ResourceDownloadOwnerSetting where
  parseJSON =
    Core.withObject "ResourceDownloadOwnerSetting" Core.$
      \x ->
        ResourceDownloadOwnerSetting'
          Core.<$> (x Core..: "GroupOwner") Core.<*> (x Core..: "GroupPermission")
