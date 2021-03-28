{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.GroupNameAndArn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.GroupNameAndArn
  ( GroupNameAndArn (..)
  -- * Smart constructor
  , mkGroupNameAndArn
  -- * Lenses
  , gnaaGroupArn
  , gnaaGroupName
  ) where

import qualified Network.AWS.IoT.Types.ThingGroupArn as Types
import qualified Network.AWS.IoT.Types.ThingGroupName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The name and ARN of a group.
--
-- /See:/ 'mkGroupNameAndArn' smart constructor.
data GroupNameAndArn = GroupNameAndArn'
  { groupArn :: Core.Maybe Types.ThingGroupArn
    -- ^ The group ARN.
  , groupName :: Core.Maybe Types.ThingGroupName
    -- ^ The group name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GroupNameAndArn' value with any optional fields omitted.
mkGroupNameAndArn
    :: GroupNameAndArn
mkGroupNameAndArn
  = GroupNameAndArn'{groupArn = Core.Nothing,
                     groupName = Core.Nothing}

-- | The group ARN.
--
-- /Note:/ Consider using 'groupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnaaGroupArn :: Lens.Lens' GroupNameAndArn (Core.Maybe Types.ThingGroupArn)
gnaaGroupArn = Lens.field @"groupArn"
{-# INLINEABLE gnaaGroupArn #-}
{-# DEPRECATED groupArn "Use generic-lens or generic-optics with 'groupArn' instead"  #-}

-- | The group name.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnaaGroupName :: Lens.Lens' GroupNameAndArn (Core.Maybe Types.ThingGroupName)
gnaaGroupName = Lens.field @"groupName"
{-# INLINEABLE gnaaGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

instance Core.FromJSON GroupNameAndArn where
        parseJSON
          = Core.withObject "GroupNameAndArn" Core.$
              \ x ->
                GroupNameAndArn' Core.<$>
                  (x Core..:? "groupArn") Core.<*> x Core..:? "groupName"
