{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspacesIpGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.WorkspacesIpGroup
  ( WorkspacesIpGroup (..)
  -- * Smart constructor
  , mkWorkspacesIpGroup
  -- * Lenses
  , wigGroupDesc
  , wigGroupId
  , wigGroupName
  , wigUserRules
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.IpGroupDesc as Types
import qualified Network.AWS.WorkSpaces.Types.IpGroupId as Types
import qualified Network.AWS.WorkSpaces.Types.IpGroupName as Types
import qualified Network.AWS.WorkSpaces.Types.IpRuleItem as Types

-- | Describes an IP access control group.
--
-- /See:/ 'mkWorkspacesIpGroup' smart constructor.
data WorkspacesIpGroup = WorkspacesIpGroup'
  { groupDesc :: Core.Maybe Types.IpGroupDesc
    -- ^ The description of the group.
  , groupId :: Core.Maybe Types.IpGroupId
    -- ^ The identifier of the group.
  , groupName :: Core.Maybe Types.IpGroupName
    -- ^ The name of the group.
  , userRules :: Core.Maybe [Types.IpRuleItem]
    -- ^ The rules.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkspacesIpGroup' value with any optional fields omitted.
mkWorkspacesIpGroup
    :: WorkspacesIpGroup
mkWorkspacesIpGroup
  = WorkspacesIpGroup'{groupDesc = Core.Nothing,
                       groupId = Core.Nothing, groupName = Core.Nothing,
                       userRules = Core.Nothing}

-- | The description of the group.
--
-- /Note:/ Consider using 'groupDesc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wigGroupDesc :: Lens.Lens' WorkspacesIpGroup (Core.Maybe Types.IpGroupDesc)
wigGroupDesc = Lens.field @"groupDesc"
{-# INLINEABLE wigGroupDesc #-}
{-# DEPRECATED groupDesc "Use generic-lens or generic-optics with 'groupDesc' instead"  #-}

-- | The identifier of the group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wigGroupId :: Lens.Lens' WorkspacesIpGroup (Core.Maybe Types.IpGroupId)
wigGroupId = Lens.field @"groupId"
{-# INLINEABLE wigGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | The name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wigGroupName :: Lens.Lens' WorkspacesIpGroup (Core.Maybe Types.IpGroupName)
wigGroupName = Lens.field @"groupName"
{-# INLINEABLE wigGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The rules.
--
-- /Note:/ Consider using 'userRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wigUserRules :: Lens.Lens' WorkspacesIpGroup (Core.Maybe [Types.IpRuleItem])
wigUserRules = Lens.field @"userRules"
{-# INLINEABLE wigUserRules #-}
{-# DEPRECATED userRules "Use generic-lens or generic-optics with 'userRules' instead"  #-}

instance Core.FromJSON WorkspacesIpGroup where
        parseJSON
          = Core.withObject "WorkspacesIpGroup" Core.$
              \ x ->
                WorkspacesIpGroup' Core.<$>
                  (x Core..:? "groupDesc") Core.<*> x Core..:? "groupId" Core.<*>
                    x Core..:? "groupName"
                    Core.<*> x Core..:? "userRules"
