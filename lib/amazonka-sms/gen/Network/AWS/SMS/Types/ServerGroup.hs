{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ServerGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.ServerGroup
  ( ServerGroup (..)
  -- * Smart constructor
  , mkServerGroup
  -- * Lenses
  , sgName
  , sgServerGroupId
  , sgServerList
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.Name as Types
import qualified Network.AWS.SMS.Types.Server as Types
import qualified Network.AWS.SMS.Types.ServerGroupId as Types

-- | Logical grouping of servers.
--
-- /See:/ 'mkServerGroup' smart constructor.
data ServerGroup = ServerGroup'
  { name :: Core.Maybe Types.Name
    -- ^ The name of a server group.
  , serverGroupId :: Core.Maybe Types.ServerGroupId
    -- ^ The ID of a server group.
  , serverList :: Core.Maybe [Types.Server]
    -- ^ The servers that belong to a server group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServerGroup' value with any optional fields omitted.
mkServerGroup
    :: ServerGroup
mkServerGroup
  = ServerGroup'{name = Core.Nothing, serverGroupId = Core.Nothing,
                 serverList = Core.Nothing}

-- | The name of a server group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgName :: Lens.Lens' ServerGroup (Core.Maybe Types.Name)
sgName = Lens.field @"name"
{-# INLINEABLE sgName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The ID of a server group.
--
-- /Note:/ Consider using 'serverGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgServerGroupId :: Lens.Lens' ServerGroup (Core.Maybe Types.ServerGroupId)
sgServerGroupId = Lens.field @"serverGroupId"
{-# INLINEABLE sgServerGroupId #-}
{-# DEPRECATED serverGroupId "Use generic-lens or generic-optics with 'serverGroupId' instead"  #-}

-- | The servers that belong to a server group.
--
-- /Note:/ Consider using 'serverList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgServerList :: Lens.Lens' ServerGroup (Core.Maybe [Types.Server])
sgServerList = Lens.field @"serverList"
{-# INLINEABLE sgServerList #-}
{-# DEPRECATED serverList "Use generic-lens or generic-optics with 'serverList' instead"  #-}

instance Core.FromJSON ServerGroup where
        toJSON ServerGroup{..}
          = Core.object
              (Core.catMaybes
                 [("name" Core..=) Core.<$> name,
                  ("serverGroupId" Core..=) Core.<$> serverGroupId,
                  ("serverList" Core..=) Core.<$> serverList])

instance Core.FromJSON ServerGroup where
        parseJSON
          = Core.withObject "ServerGroup" Core.$
              \ x ->
                ServerGroup' Core.<$>
                  (x Core..:? "name") Core.<*> x Core..:? "serverGroupId" Core.<*>
                    x Core..:? "serverList"
