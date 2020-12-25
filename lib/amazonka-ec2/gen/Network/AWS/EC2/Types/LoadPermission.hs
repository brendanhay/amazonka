{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LoadPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LoadPermission
  ( LoadPermission (..),

    -- * Smart constructor
    mkLoadPermission,

    -- * Lenses
    lpGroup,
    lpUserId,
  )
where

import qualified Network.AWS.EC2.Types.PermissionGroup as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a load permission.
--
-- /See:/ 'mkLoadPermission' smart constructor.
data LoadPermission = LoadPermission'
  { -- | The name of the group.
    group :: Core.Maybe Types.PermissionGroup,
    -- | The AWS account ID.
    userId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoadPermission' value with any optional fields omitted.
mkLoadPermission ::
  LoadPermission
mkLoadPermission =
  LoadPermission' {group = Core.Nothing, userId = Core.Nothing}

-- | The name of the group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpGroup :: Lens.Lens' LoadPermission (Core.Maybe Types.PermissionGroup)
lpGroup = Lens.field @"group"
{-# DEPRECATED lpGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The AWS account ID.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpUserId :: Lens.Lens' LoadPermission (Core.Maybe Types.String)
lpUserId = Lens.field @"userId"
{-# DEPRECATED lpUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

instance Core.FromXML LoadPermission where
  parseXML x =
    LoadPermission'
      Core.<$> (x Core..@? "group") Core.<*> (x Core..@? "userId")
