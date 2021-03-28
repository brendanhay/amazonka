{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.GroupIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ResourceGroups.Types.GroupIdentifier
  ( GroupIdentifier (..)
  -- * Smart constructor
  , mkGroupIdentifier
  -- * Lenses
  , giGroupArn
  , giGroupName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ResourceGroups.Types.GroupArn as Types
import qualified Network.AWS.ResourceGroups.Types.GroupName as Types

-- | The unique identifiers for a resource group.
--
-- /See:/ 'mkGroupIdentifier' smart constructor.
data GroupIdentifier = GroupIdentifier'
  { groupArn :: Core.Maybe Types.GroupArn
    -- ^ The ARN of the resource group.
  , groupName :: Core.Maybe Types.GroupName
    -- ^ The name of the resource group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GroupIdentifier' value with any optional fields omitted.
mkGroupIdentifier
    :: GroupIdentifier
mkGroupIdentifier
  = GroupIdentifier'{groupArn = Core.Nothing,
                     groupName = Core.Nothing}

-- | The ARN of the resource group.
--
-- /Note:/ Consider using 'groupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giGroupArn :: Lens.Lens' GroupIdentifier (Core.Maybe Types.GroupArn)
giGroupArn = Lens.field @"groupArn"
{-# INLINEABLE giGroupArn #-}
{-# DEPRECATED groupArn "Use generic-lens or generic-optics with 'groupArn' instead"  #-}

-- | The name of the resource group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giGroupName :: Lens.Lens' GroupIdentifier (Core.Maybe Types.GroupName)
giGroupName = Lens.field @"groupName"
{-# INLINEABLE giGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

instance Core.FromJSON GroupIdentifier where
        parseJSON
          = Core.withObject "GroupIdentifier" Core.$
              \ x ->
                GroupIdentifier' Core.<$>
                  (x Core..:? "GroupArn") Core.<*> x Core..:? "GroupName"
