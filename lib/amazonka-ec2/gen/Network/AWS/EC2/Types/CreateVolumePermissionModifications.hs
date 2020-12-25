{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CreateVolumePermissionModifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CreateVolumePermissionModifications
  ( CreateVolumePermissionModifications (..),

    -- * Smart constructor
    mkCreateVolumePermissionModifications,

    -- * Lenses
    cvpmAdd,
    cvpmRemove,
  )
where

import qualified Network.AWS.EC2.Types.CreateVolumePermission as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes modifications to the list of create volume permissions for a volume.
--
-- /See:/ 'mkCreateVolumePermissionModifications' smart constructor.
data CreateVolumePermissionModifications = CreateVolumePermissionModifications'
  { -- | Adds the specified AWS account ID or group to the list.
    add :: Core.Maybe [Types.CreateVolumePermission],
    -- | Removes the specified AWS account ID or group from the list.
    remove :: Core.Maybe [Types.CreateVolumePermission]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVolumePermissionModifications' value with any optional fields omitted.
mkCreateVolumePermissionModifications ::
  CreateVolumePermissionModifications
mkCreateVolumePermissionModifications =
  CreateVolumePermissionModifications'
    { add = Core.Nothing,
      remove = Core.Nothing
    }

-- | Adds the specified AWS account ID or group to the list.
--
-- /Note:/ Consider using 'add' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpmAdd :: Lens.Lens' CreateVolumePermissionModifications (Core.Maybe [Types.CreateVolumePermission])
cvpmAdd = Lens.field @"add"
{-# DEPRECATED cvpmAdd "Use generic-lens or generic-optics with 'add' instead." #-}

-- | Removes the specified AWS account ID or group from the list.
--
-- /Note:/ Consider using 'remove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpmRemove :: Lens.Lens' CreateVolumePermissionModifications (Core.Maybe [Types.CreateVolumePermission])
cvpmRemove = Lens.field @"remove"
{-# DEPRECATED cvpmRemove "Use generic-lens or generic-optics with 'remove' instead." #-}
