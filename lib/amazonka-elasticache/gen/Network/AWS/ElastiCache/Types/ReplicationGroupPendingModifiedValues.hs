-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ReplicationGroupPendingModifiedValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ReplicationGroupPendingModifiedValues
  ( ReplicationGroupPendingModifiedValues (..),

    -- * Smart constructor
    mkReplicationGroupPendingModifiedValues,

    -- * Lenses
    rgpmvAuthTokenStatus,
    rgpmvUserGroups,
    rgpmvResharding,
    rgpmvPrimaryClusterId,
    rgpmvAutomaticFailoverStatus,
  )
where

import Network.AWS.ElastiCache.Types.AuthTokenUpdateStatus
import Network.AWS.ElastiCache.Types.PendingAutomaticFailoverStatus
import Network.AWS.ElastiCache.Types.ReshardingStatus
import Network.AWS.ElastiCache.Types.UserGroupsUpdateStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The settings to be applied to the Redis replication group, either immediately or during the next maintenance window.
--
-- /See:/ 'mkReplicationGroupPendingModifiedValues' smart constructor.
data ReplicationGroupPendingModifiedValues = ReplicationGroupPendingModifiedValues'
  { authTokenStatus ::
      Lude.Maybe
        AuthTokenUpdateStatus,
    userGroups ::
      Lude.Maybe
        UserGroupsUpdateStatus,
    resharding ::
      Lude.Maybe
        ReshardingStatus,
    primaryClusterId ::
      Lude.Maybe
        Lude.Text,
    automaticFailoverStatus ::
      Lude.Maybe
        PendingAutomaticFailoverStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicationGroupPendingModifiedValues' with the minimum fields required to make a request.
--
-- * 'authTokenStatus' - The auth token status
-- * 'automaticFailoverStatus' - Indicates the status of automatic failover for this Redis replication group.
-- * 'primaryClusterId' - The primary cluster ID that is applied immediately (if @--apply-immediately@ was specified), or during the next maintenance window.
-- * 'resharding' - The status of an online resharding operation.
-- * 'userGroups' - The user groups being modified.
mkReplicationGroupPendingModifiedValues ::
  ReplicationGroupPendingModifiedValues
mkReplicationGroupPendingModifiedValues =
  ReplicationGroupPendingModifiedValues'
    { authTokenStatus =
        Lude.Nothing,
      userGroups = Lude.Nothing,
      resharding = Lude.Nothing,
      primaryClusterId = Lude.Nothing,
      automaticFailoverStatus = Lude.Nothing
    }

-- | The auth token status
--
-- /Note:/ Consider using 'authTokenStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgpmvAuthTokenStatus :: Lens.Lens' ReplicationGroupPendingModifiedValues (Lude.Maybe AuthTokenUpdateStatus)
rgpmvAuthTokenStatus = Lens.lens (authTokenStatus :: ReplicationGroupPendingModifiedValues -> Lude.Maybe AuthTokenUpdateStatus) (\s a -> s {authTokenStatus = a} :: ReplicationGroupPendingModifiedValues)
{-# DEPRECATED rgpmvAuthTokenStatus "Use generic-lens or generic-optics with 'authTokenStatus' instead." #-}

-- | The user groups being modified.
--
-- /Note:/ Consider using 'userGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgpmvUserGroups :: Lens.Lens' ReplicationGroupPendingModifiedValues (Lude.Maybe UserGroupsUpdateStatus)
rgpmvUserGroups = Lens.lens (userGroups :: ReplicationGroupPendingModifiedValues -> Lude.Maybe UserGroupsUpdateStatus) (\s a -> s {userGroups = a} :: ReplicationGroupPendingModifiedValues)
{-# DEPRECATED rgpmvUserGroups "Use generic-lens or generic-optics with 'userGroups' instead." #-}

-- | The status of an online resharding operation.
--
-- /Note:/ Consider using 'resharding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgpmvResharding :: Lens.Lens' ReplicationGroupPendingModifiedValues (Lude.Maybe ReshardingStatus)
rgpmvResharding = Lens.lens (resharding :: ReplicationGroupPendingModifiedValues -> Lude.Maybe ReshardingStatus) (\s a -> s {resharding = a} :: ReplicationGroupPendingModifiedValues)
{-# DEPRECATED rgpmvResharding "Use generic-lens or generic-optics with 'resharding' instead." #-}

-- | The primary cluster ID that is applied immediately (if @--apply-immediately@ was specified), or during the next maintenance window.
--
-- /Note:/ Consider using 'primaryClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgpmvPrimaryClusterId :: Lens.Lens' ReplicationGroupPendingModifiedValues (Lude.Maybe Lude.Text)
rgpmvPrimaryClusterId = Lens.lens (primaryClusterId :: ReplicationGroupPendingModifiedValues -> Lude.Maybe Lude.Text) (\s a -> s {primaryClusterId = a} :: ReplicationGroupPendingModifiedValues)
{-# DEPRECATED rgpmvPrimaryClusterId "Use generic-lens or generic-optics with 'primaryClusterId' instead." #-}

-- | Indicates the status of automatic failover for this Redis replication group.
--
-- /Note:/ Consider using 'automaticFailoverStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgpmvAutomaticFailoverStatus :: Lens.Lens' ReplicationGroupPendingModifiedValues (Lude.Maybe PendingAutomaticFailoverStatus)
rgpmvAutomaticFailoverStatus = Lens.lens (automaticFailoverStatus :: ReplicationGroupPendingModifiedValues -> Lude.Maybe PendingAutomaticFailoverStatus) (\s a -> s {automaticFailoverStatus = a} :: ReplicationGroupPendingModifiedValues)
{-# DEPRECATED rgpmvAutomaticFailoverStatus "Use generic-lens or generic-optics with 'automaticFailoverStatus' instead." #-}

instance Lude.FromXML ReplicationGroupPendingModifiedValues where
  parseXML x =
    ReplicationGroupPendingModifiedValues'
      Lude.<$> (x Lude..@? "AuthTokenStatus")
      Lude.<*> (x Lude..@? "UserGroups")
      Lude.<*> (x Lude..@? "Resharding")
      Lude.<*> (x Lude..@? "PrimaryClusterId")
      Lude.<*> (x Lude..@? "AutomaticFailoverStatus")
