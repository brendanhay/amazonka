{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.GlobalReplicationGroupInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.GlobalReplicationGroupInfo
  ( GlobalReplicationGroupInfo (..),

    -- * Smart constructor
    mkGlobalReplicationGroupInfo,

    -- * Lenses
    grgiGlobalReplicationGroupId,
    grgiGlobalReplicationGroupMemberRole,
  )
where

import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The name of the Global Datastore and role of this replication group in the Global Datastore.
--
-- /See:/ 'mkGlobalReplicationGroupInfo' smart constructor.
data GlobalReplicationGroupInfo = GlobalReplicationGroupInfo'
  { -- | The name of the Global Datastore
    globalReplicationGroupId :: Core.Maybe Types.String,
    -- | The role of the replication group in a Global Datastore. Can be primary or secondary.
    globalReplicationGroupMemberRole :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GlobalReplicationGroupInfo' value with any optional fields omitted.
mkGlobalReplicationGroupInfo ::
  GlobalReplicationGroupInfo
mkGlobalReplicationGroupInfo =
  GlobalReplicationGroupInfo'
    { globalReplicationGroupId =
        Core.Nothing,
      globalReplicationGroupMemberRole = Core.Nothing
    }

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgiGlobalReplicationGroupId :: Lens.Lens' GlobalReplicationGroupInfo (Core.Maybe Types.String)
grgiGlobalReplicationGroupId = Lens.field @"globalReplicationGroupId"
{-# DEPRECATED grgiGlobalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead." #-}

-- | The role of the replication group in a Global Datastore. Can be primary or secondary.
--
-- /Note:/ Consider using 'globalReplicationGroupMemberRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgiGlobalReplicationGroupMemberRole :: Lens.Lens' GlobalReplicationGroupInfo (Core.Maybe Types.String)
grgiGlobalReplicationGroupMemberRole = Lens.field @"globalReplicationGroupMemberRole"
{-# DEPRECATED grgiGlobalReplicationGroupMemberRole "Use generic-lens or generic-optics with 'globalReplicationGroupMemberRole' instead." #-}

instance Core.FromXML GlobalReplicationGroupInfo where
  parseXML x =
    GlobalReplicationGroupInfo'
      Core.<$> (x Core..@? "GlobalReplicationGroupId")
      Core.<*> (x Core..@? "GlobalReplicationGroupMemberRole")
