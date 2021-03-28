{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ReplicationGroupPendingModifiedValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.ReplicationGroupPendingModifiedValues
  ( ReplicationGroupPendingModifiedValues (..)
  -- * Smart constructor
  , mkReplicationGroupPendingModifiedValues
  -- * Lenses
  , rgpmvAuthTokenStatus
  , rgpmvAutomaticFailoverStatus
  , rgpmvPrimaryClusterId
  , rgpmvResharding
  , rgpmvUserGroups
  ) where

import qualified Network.AWS.ElastiCache.Types.AuthTokenUpdateStatus as Types
import qualified Network.AWS.ElastiCache.Types.PendingAutomaticFailoverStatus as Types
import qualified Network.AWS.ElastiCache.Types.ReshardingStatus as Types
import qualified Network.AWS.ElastiCache.Types.UserGroupsUpdateStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The settings to be applied to the Redis replication group, either immediately or during the next maintenance window.
--
-- /See:/ 'mkReplicationGroupPendingModifiedValues' smart constructor.
data ReplicationGroupPendingModifiedValues = ReplicationGroupPendingModifiedValues'
  { authTokenStatus :: Core.Maybe Types.AuthTokenUpdateStatus
    -- ^ The auth token status
  , automaticFailoverStatus :: Core.Maybe Types.PendingAutomaticFailoverStatus
    -- ^ Indicates the status of automatic failover for this Redis replication group.
  , primaryClusterId :: Core.Maybe Core.Text
    -- ^ The primary cluster ID that is applied immediately (if @--apply-immediately@ was specified), or during the next maintenance window.
  , resharding :: Core.Maybe Types.ReshardingStatus
    -- ^ The status of an online resharding operation.
  , userGroups :: Core.Maybe Types.UserGroupsUpdateStatus
    -- ^ The user groups being modified.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicationGroupPendingModifiedValues' value with any optional fields omitted.
mkReplicationGroupPendingModifiedValues
    :: ReplicationGroupPendingModifiedValues
mkReplicationGroupPendingModifiedValues
  = ReplicationGroupPendingModifiedValues'{authTokenStatus =
                                             Core.Nothing,
                                           automaticFailoverStatus = Core.Nothing,
                                           primaryClusterId = Core.Nothing,
                                           resharding = Core.Nothing, userGroups = Core.Nothing}

-- | The auth token status
--
-- /Note:/ Consider using 'authTokenStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgpmvAuthTokenStatus :: Lens.Lens' ReplicationGroupPendingModifiedValues (Core.Maybe Types.AuthTokenUpdateStatus)
rgpmvAuthTokenStatus = Lens.field @"authTokenStatus"
{-# INLINEABLE rgpmvAuthTokenStatus #-}
{-# DEPRECATED authTokenStatus "Use generic-lens or generic-optics with 'authTokenStatus' instead"  #-}

-- | Indicates the status of automatic failover for this Redis replication group.
--
-- /Note:/ Consider using 'automaticFailoverStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgpmvAutomaticFailoverStatus :: Lens.Lens' ReplicationGroupPendingModifiedValues (Core.Maybe Types.PendingAutomaticFailoverStatus)
rgpmvAutomaticFailoverStatus = Lens.field @"automaticFailoverStatus"
{-# INLINEABLE rgpmvAutomaticFailoverStatus #-}
{-# DEPRECATED automaticFailoverStatus "Use generic-lens or generic-optics with 'automaticFailoverStatus' instead"  #-}

-- | The primary cluster ID that is applied immediately (if @--apply-immediately@ was specified), or during the next maintenance window.
--
-- /Note:/ Consider using 'primaryClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgpmvPrimaryClusterId :: Lens.Lens' ReplicationGroupPendingModifiedValues (Core.Maybe Core.Text)
rgpmvPrimaryClusterId = Lens.field @"primaryClusterId"
{-# INLINEABLE rgpmvPrimaryClusterId #-}
{-# DEPRECATED primaryClusterId "Use generic-lens or generic-optics with 'primaryClusterId' instead"  #-}

-- | The status of an online resharding operation.
--
-- /Note:/ Consider using 'resharding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgpmvResharding :: Lens.Lens' ReplicationGroupPendingModifiedValues (Core.Maybe Types.ReshardingStatus)
rgpmvResharding = Lens.field @"resharding"
{-# INLINEABLE rgpmvResharding #-}
{-# DEPRECATED resharding "Use generic-lens or generic-optics with 'resharding' instead"  #-}

-- | The user groups being modified.
--
-- /Note:/ Consider using 'userGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgpmvUserGroups :: Lens.Lens' ReplicationGroupPendingModifiedValues (Core.Maybe Types.UserGroupsUpdateStatus)
rgpmvUserGroups = Lens.field @"userGroups"
{-# INLINEABLE rgpmvUserGroups #-}
{-# DEPRECATED userGroups "Use generic-lens or generic-optics with 'userGroups' instead"  #-}

instance Core.FromXML ReplicationGroupPendingModifiedValues where
        parseXML x
          = ReplicationGroupPendingModifiedValues' Core.<$>
              (x Core..@? "AuthTokenStatus") Core.<*>
                x Core..@? "AutomaticFailoverStatus"
                Core.<*> x Core..@? "PrimaryClusterId"
                Core.<*> x Core..@? "Resharding"
                Core.<*> x Core..@? "UserGroups"
