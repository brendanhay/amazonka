{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LoadPermissionModifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LoadPermissionModifications
  ( LoadPermissionModifications (..),

    -- * Smart constructor
    mkLoadPermissionModifications,

    -- * Lenses
    lpmAdd,
    lpmRemove,
  )
where

import qualified Network.AWS.EC2.Types.LoadPermissionRequest as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes modifications to the load permissions of an Amazon FPGA image (AFI).
--
-- /See:/ 'mkLoadPermissionModifications' smart constructor.
data LoadPermissionModifications = LoadPermissionModifications'
  { -- | The load permissions to add.
    add :: Core.Maybe [Types.LoadPermissionRequest],
    -- | The load permissions to remove.
    remove :: Core.Maybe [Types.LoadPermissionRequest]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoadPermissionModifications' value with any optional fields omitted.
mkLoadPermissionModifications ::
  LoadPermissionModifications
mkLoadPermissionModifications =
  LoadPermissionModifications'
    { add = Core.Nothing,
      remove = Core.Nothing
    }

-- | The load permissions to add.
--
-- /Note:/ Consider using 'add' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpmAdd :: Lens.Lens' LoadPermissionModifications (Core.Maybe [Types.LoadPermissionRequest])
lpmAdd = Lens.field @"add"
{-# DEPRECATED lpmAdd "Use generic-lens or generic-optics with 'add' instead." #-}

-- | The load permissions to remove.
--
-- /Note:/ Consider using 'remove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpmRemove :: Lens.Lens' LoadPermissionModifications (Core.Maybe [Types.LoadPermissionRequest])
lpmRemove = Lens.field @"remove"
{-# DEPRECATED lpmRemove "Use generic-lens or generic-optics with 'remove' instead." #-}
