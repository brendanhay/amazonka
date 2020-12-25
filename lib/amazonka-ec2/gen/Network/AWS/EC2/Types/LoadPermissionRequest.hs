{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LoadPermissionRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LoadPermissionRequest
  ( LoadPermissionRequest (..),

    -- * Smart constructor
    mkLoadPermissionRequest,

    -- * Lenses
    lprGroup,
    lprUserId,
  )
where

import qualified Network.AWS.EC2.Types.PermissionGroup as Types
import qualified Network.AWS.EC2.Types.UserId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a load permission.
--
-- /See:/ 'mkLoadPermissionRequest' smart constructor.
data LoadPermissionRequest = LoadPermissionRequest'
  { -- | The name of the group.
    group :: Core.Maybe Types.PermissionGroup,
    -- | The AWS account ID.
    userId :: Core.Maybe Types.UserId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoadPermissionRequest' value with any optional fields omitted.
mkLoadPermissionRequest ::
  LoadPermissionRequest
mkLoadPermissionRequest =
  LoadPermissionRequest'
    { group = Core.Nothing,
      userId = Core.Nothing
    }

-- | The name of the group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprGroup :: Lens.Lens' LoadPermissionRequest (Core.Maybe Types.PermissionGroup)
lprGroup = Lens.field @"group"
{-# DEPRECATED lprGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The AWS account ID.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprUserId :: Lens.Lens' LoadPermissionRequest (Core.Maybe Types.UserId)
lprUserId = Lens.field @"userId"
{-# DEPRECATED lprUserId "Use generic-lens or generic-optics with 'userId' instead." #-}
