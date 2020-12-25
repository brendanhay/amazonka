{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CreateVolumePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CreateVolumePermission
  ( CreateVolumePermission (..),

    -- * Smart constructor
    mkCreateVolumePermission,

    -- * Lenses
    cvpGroup,
    cvpUserId,
  )
where

import qualified Network.AWS.EC2.Types.PermissionGroup as Types
import qualified Network.AWS.EC2.Types.UserId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the user or group to be added or removed from the list of create volume permissions for a volume.
--
-- /See:/ 'mkCreateVolumePermission' smart constructor.
data CreateVolumePermission = CreateVolumePermission'
  { -- | The group to be added or removed. The possible value is @all@ .
    group :: Core.Maybe Types.PermissionGroup,
    -- | The AWS account ID to be added or removed.
    userId :: Core.Maybe Types.UserId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVolumePermission' value with any optional fields omitted.
mkCreateVolumePermission ::
  CreateVolumePermission
mkCreateVolumePermission =
  CreateVolumePermission'
    { group = Core.Nothing,
      userId = Core.Nothing
    }

-- | The group to be added or removed. The possible value is @all@ .
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpGroup :: Lens.Lens' CreateVolumePermission (Core.Maybe Types.PermissionGroup)
cvpGroup = Lens.field @"group"
{-# DEPRECATED cvpGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The AWS account ID to be added or removed.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpUserId :: Lens.Lens' CreateVolumePermission (Core.Maybe Types.UserId)
cvpUserId = Lens.field @"userId"
{-# DEPRECATED cvpUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Core.FromXML CreateVolumePermission where
  parseXML x =
    CreateVolumePermission'
      Core.<$> (x Core..@? "group") Core.<*> (x Core..@? "userId")
