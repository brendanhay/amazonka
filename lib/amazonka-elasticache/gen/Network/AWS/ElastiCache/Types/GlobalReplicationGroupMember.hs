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
    grgmStatus,
    grgmReplicationGroupRegion,
    grgmRole,
    grgmReplicationGroupId,
    grgmAutomaticFailover,
  )
where

import Network.AWS.ElastiCache.Types.AutomaticFailoverStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A member of a Global Datastore. It contains the Replication Group Id, the AWS region and the role of the replication group.
--
-- /See:/ 'mkGlobalReplicationGroupMember' smart constructor.
data GlobalReplicationGroupMember = GlobalReplicationGroupMember'
  { -- | The status of the membership of the replication group.
    status :: Lude.Maybe Lude.Text,
    -- | The AWS region of the Global Datastore member.
    replicationGroupRegion :: Lude.Maybe Lude.Text,
    -- | Indicates the role of the replication group, primary or secondary.
    role' :: Lude.Maybe Lude.Text,
    -- | The replication group id of the Global Datastore member.
    replicationGroupId :: Lude.Maybe Lude.Text,
    -- | Indicates whether automatic failover is enabled for the replication group.
    automaticFailover :: Lude.Maybe AutomaticFailoverStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GlobalReplicationGroupMember' with the minimum fields required to make a request.
--
-- * 'status' - The status of the membership of the replication group.
-- * 'replicationGroupRegion' - The AWS region of the Global Datastore member.
-- * 'role'' - Indicates the role of the replication group, primary or secondary.
-- * 'replicationGroupId' - The replication group id of the Global Datastore member.
-- * 'automaticFailover' - Indicates whether automatic failover is enabled for the replication group.
mkGlobalReplicationGroupMember ::
  GlobalReplicationGroupMember
mkGlobalReplicationGroupMember =
  GlobalReplicationGroupMember'
    { status = Lude.Nothing,
      replicationGroupRegion = Lude.Nothing,
      role' = Lude.Nothing,
      replicationGroupId = Lude.Nothing,
      automaticFailover = Lude.Nothing
    }

-- | The status of the membership of the replication group.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgmStatus :: Lens.Lens' GlobalReplicationGroupMember (Lude.Maybe Lude.Text)
grgmStatus = Lens.lens (status :: GlobalReplicationGroupMember -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: GlobalReplicationGroupMember)
{-# DEPRECATED grgmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The AWS region of the Global Datastore member.
--
-- /Note:/ Consider using 'replicationGroupRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgmReplicationGroupRegion :: Lens.Lens' GlobalReplicationGroupMember (Lude.Maybe Lude.Text)
grgmReplicationGroupRegion = Lens.lens (replicationGroupRegion :: GlobalReplicationGroupMember -> Lude.Maybe Lude.Text) (\s a -> s {replicationGroupRegion = a} :: GlobalReplicationGroupMember)
{-# DEPRECATED grgmReplicationGroupRegion "Use generic-lens or generic-optics with 'replicationGroupRegion' instead." #-}

-- | Indicates the role of the replication group, primary or secondary.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgmRole :: Lens.Lens' GlobalReplicationGroupMember (Lude.Maybe Lude.Text)
grgmRole = Lens.lens (role' :: GlobalReplicationGroupMember -> Lude.Maybe Lude.Text) (\s a -> s {role' = a} :: GlobalReplicationGroupMember)
{-# DEPRECATED grgmRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The replication group id of the Global Datastore member.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgmReplicationGroupId :: Lens.Lens' GlobalReplicationGroupMember (Lude.Maybe Lude.Text)
grgmReplicationGroupId = Lens.lens (replicationGroupId :: GlobalReplicationGroupMember -> Lude.Maybe Lude.Text) (\s a -> s {replicationGroupId = a} :: GlobalReplicationGroupMember)
{-# DEPRECATED grgmReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | Indicates whether automatic failover is enabled for the replication group.
--
-- /Note:/ Consider using 'automaticFailover' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgmAutomaticFailover :: Lens.Lens' GlobalReplicationGroupMember (Lude.Maybe AutomaticFailoverStatus)
grgmAutomaticFailover = Lens.lens (automaticFailover :: GlobalReplicationGroupMember -> Lude.Maybe AutomaticFailoverStatus) (\s a -> s {automaticFailover = a} :: GlobalReplicationGroupMember)
{-# DEPRECATED grgmAutomaticFailover "Use generic-lens or generic-optics with 'automaticFailover' instead." #-}

instance Lude.FromXML GlobalReplicationGroupMember where
  parseXML x =
    GlobalReplicationGroupMember'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "ReplicationGroupRegion")
      Lude.<*> (x Lude..@? "Role")
      Lude.<*> (x Lude..@? "ReplicationGroupId")
      Lude.<*> (x Lude..@? "AutomaticFailover")
