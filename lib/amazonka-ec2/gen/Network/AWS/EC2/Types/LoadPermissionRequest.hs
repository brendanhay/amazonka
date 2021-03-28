{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LoadPermissionRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LoadPermissionRequest
  ( LoadPermissionRequest (..)
  -- * Smart constructor
  , mkLoadPermissionRequest
  -- * Lenses
  , lprGroup
  , lprUserId
  ) where

import qualified Network.AWS.EC2.Types.PermissionGroup as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a load permission.
--
-- /See:/ 'mkLoadPermissionRequest' smart constructor.
data LoadPermissionRequest = LoadPermissionRequest'
  { group :: Core.Maybe Types.PermissionGroup
    -- ^ The name of the group.
  , userId :: Core.Maybe Core.Text
    -- ^ The AWS account ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoadPermissionRequest' value with any optional fields omitted.
mkLoadPermissionRequest
    :: LoadPermissionRequest
mkLoadPermissionRequest
  = LoadPermissionRequest'{group = Core.Nothing,
                           userId = Core.Nothing}

-- | The name of the group.
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprGroup :: Lens.Lens' LoadPermissionRequest (Core.Maybe Types.PermissionGroup)
lprGroup = Lens.field @"group"
{-# INLINEABLE lprGroup #-}
{-# DEPRECATED group "Use generic-lens or generic-optics with 'group' instead"  #-}

-- | The AWS account ID.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprUserId :: Lens.Lens' LoadPermissionRequest (Core.Maybe Core.Text)
lprUserId = Lens.field @"userId"
{-# INLINEABLE lprUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

instance Core.ToQuery LoadPermissionRequest where
        toQuery LoadPermissionRequest{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Group") group Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UserId") userId
