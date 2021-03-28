{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchPermission
  ( LaunchPermission (..)
  -- * Smart constructor
  , mkLaunchPermission
  -- * Lenses
  , lGroup
  , lUserId
  ) where

import qualified Network.AWS.EC2.Types.PermissionGroup as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a launch permission.
--
-- /See:/ 'mkLaunchPermission' smart constructor.
data LaunchPermission = LaunchPermission'
  { group :: Core.Maybe Types.PermissionGroup
    -- ^ The name of the group.
  , userId :: Core.Maybe Core.Text
    -- ^ The AWS account ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchPermission' value with any optional fields omitted.
mkLaunchPermission
    :: LaunchPermission
mkLaunchPermission
  = LaunchPermission'{group = Core.Nothing, userId = Core.Nothing}

-- | The name of the group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lGroup :: Lens.Lens' LaunchPermission (Core.Maybe Types.PermissionGroup)
lGroup = Lens.field @"group"
{-# INLINEABLE lGroup #-}
{-# DEPRECATED group "Use generic-lens or generic-optics with 'group' instead"  #-}

-- | The AWS account ID.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lUserId :: Lens.Lens' LaunchPermission (Core.Maybe Core.Text)
lUserId = Lens.field @"userId"
{-# INLINEABLE lUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

instance Core.ToQuery LaunchPermission where
        toQuery LaunchPermission{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Group") group Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UserId") userId

instance Core.FromXML LaunchPermission where
        parseXML x
          = LaunchPermission' Core.<$>
              (x Core..@? "group") Core.<*> x Core..@? "userId"
