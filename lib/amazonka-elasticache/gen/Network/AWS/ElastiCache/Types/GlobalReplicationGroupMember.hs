{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.GlobalReplicationGroupMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.GlobalReplicationGroupMember
  ( GlobalReplicationGroupMember (..),

    -- * Smart constructor
    mkGlobalReplicationGroupMember,

    -- * Lenses
    grgmAutomaticFailover,
    grgmReplicationGroupId,
    grgmReplicationGroupRegion,
    grgmRole,
    grgmStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types.AutomaticFailoverStatus as Types
import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A member of a Global Datastore. It contains the Replication Group Id, the AWS region and the role of the replication group.
--
-- /See:/ 'mkGlobalReplicationGroupMember' smart constructor.
data GlobalReplicationGroupMember = GlobalReplicationGroupMember'
  { -- | Indicates whether automatic failover is enabled for the replication group.
    automaticFailover :: Core.Maybe Types.AutomaticFailoverStatus,
    -- | The replication group id of the Global Datastore member.
    replicationGroupId :: Core.Maybe Types.String,
    -- | The AWS region of the Global Datastore member.
    replicationGroupRegion :: Core.Maybe Types.String,
    -- | Indicates the role of the replication group, primary or secondary.
    role' :: Core.Maybe Types.String,
    -- | The status of the membership of the replication group.
    status :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GlobalReplicationGroupMember' value with any optional fields omitted.
mkGlobalReplicationGroupMember ::
  GlobalReplicationGroupMember
mkGlobalReplicationGroupMember =
  GlobalReplicationGroupMember'
    { automaticFailover = Core.Nothing,
      replicationGroupId = Core.Nothing,
      replicationGroupRegion = Core.Nothing,
      role' = Core.Nothing,
      status = Core.Nothing
    }

-- | Indicates whether automatic failover is enabled for the replication group.
--
-- /Note:/ Consider using 'automaticFailover' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgmAutomaticFailover :: Lens.Lens' GlobalReplicationGroupMember (Core.Maybe Types.AutomaticFailoverStatus)
grgmAutomaticFailover = Lens.field @"automaticFailover"
{-# DEPRECATED grgmAutomaticFailover "Use generic-lens or generic-optics with 'automaticFailover' instead." #-}

-- | The replication group id of the Global Datastore member.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgmReplicationGroupId :: Lens.Lens' GlobalReplicationGroupMember (Core.Maybe Types.String)
grgmReplicationGroupId = Lens.field @"replicationGroupId"
{-# DEPRECATED grgmReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | The AWS region of the Global Datastore member.
--
-- /Note:/ Consider using 'replicationGroupRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgmReplicationGroupRegion :: Lens.Lens' GlobalReplicationGroupMember (Core.Maybe Types.String)
grgmReplicationGroupRegion = Lens.field @"replicationGroupRegion"
{-# DEPRECATED grgmReplicationGroupRegion "Use generic-lens or generic-optics with 'replicationGroupRegion' instead." #-}

-- | Indicates the role of the replication group, primary or secondary.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgmRole :: Lens.Lens' GlobalReplicationGroupMember (Core.Maybe Types.String)
grgmRole = Lens.field @"role'"
{-# DEPRECATED grgmRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The status of the membership of the replication group.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgmStatus :: Lens.Lens' GlobalReplicationGroupMember (Core.Maybe Types.String)
grgmStatus = Lens.field @"status"
{-# DEPRECATED grgmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML GlobalReplicationGroupMember where
  parseXML x =
    GlobalReplicationGroupMember'
      Core.<$> (x Core..@? "AutomaticFailover")
      Core.<*> (x Core..@? "ReplicationGroupId")
      Core.<*> (x Core..@? "ReplicationGroupRegion")
      Core.<*> (x Core..@? "Role")
      Core.<*> (x Core..@? "Status")
