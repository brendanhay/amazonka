{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchPermissionModifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchPermissionModifications
  ( LaunchPermissionModifications (..),

    -- * Smart constructor
    mkLaunchPermissionModifications,

    -- * Lenses
    lAdd,
    lRemove,
  )
where

import qualified Network.AWS.EC2.Types.LaunchPermission as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a launch permission modification.
--
-- /See:/ 'mkLaunchPermissionModifications' smart constructor.
data LaunchPermissionModifications = LaunchPermissionModifications'
  { -- | The AWS account ID to add to the list of launch permissions for the AMI.
    add :: Core.Maybe [Types.LaunchPermission],
    -- | The AWS account ID to remove from the list of launch permissions for the AMI.
    remove :: Core.Maybe [Types.LaunchPermission]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchPermissionModifications' value with any optional fields omitted.
mkLaunchPermissionModifications ::
  LaunchPermissionModifications
mkLaunchPermissionModifications =
  LaunchPermissionModifications'
    { add = Core.Nothing,
      remove = Core.Nothing
    }

-- | The AWS account ID to add to the list of launch permissions for the AMI.
--
-- /Note:/ Consider using 'add' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lAdd :: Lens.Lens' LaunchPermissionModifications (Core.Maybe [Types.LaunchPermission])
lAdd = Lens.field @"add"
{-# DEPRECATED lAdd "Use generic-lens or generic-optics with 'add' instead." #-}

-- | The AWS account ID to remove from the list of launch permissions for the AMI.
--
-- /Note:/ Consider using 'remove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lRemove :: Lens.Lens' LaunchPermissionModifications (Core.Maybe [Types.LaunchPermission])
lRemove = Lens.field @"remove"
{-# DEPRECATED lRemove "Use generic-lens or generic-optics with 'remove' instead." #-}
