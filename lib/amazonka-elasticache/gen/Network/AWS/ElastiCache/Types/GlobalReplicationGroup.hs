{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.GlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.GlobalReplicationGroup
  ( GlobalReplicationGroup (..)
  -- * Smart constructor
  , mkGlobalReplicationGroup
  -- * Lenses
  , grgARN
  , grgAtRestEncryptionEnabled
  , grgAuthTokenEnabled
  , grgCacheNodeType
  , grgClusterEnabled
  , grgEngine
  , grgEngineVersion
  , grgGlobalNodeGroups
  , grgGlobalReplicationGroupDescription
  , grgGlobalReplicationGroupId
  , grgMembers
  , grgStatus
  , grgTransitEncryptionEnabled
  ) where

import qualified Network.AWS.ElastiCache.Types.GlobalNodeGroup as Types
import qualified Network.AWS.ElastiCache.Types.GlobalReplicationGroupMember as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Consists of a primary cluster that accepts writes and an associated secondary cluster that resides in a different AWS region. The secondary cluster accepts only reads. The primary cluster automatically replicates updates to the secondary cluster.
--
--
--     * The __GlobalReplicationGroupIdSuffix__ represents the name of the Global Datastore, which is what you use to associate a secondary cluster.
--
--
--
-- /See:/ 'mkGlobalReplicationGroup' smart constructor.
data GlobalReplicationGroup = GlobalReplicationGroup'
  { arn :: Core.Maybe Core.Text
    -- ^ The ARN (Amazon Resource Name) of the global replication group.
  , atRestEncryptionEnabled :: Core.Maybe Core.Bool
    -- ^ A flag that enables encryption at rest when set to @true@ .
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the replication group is created. To enable encryption at rest on a replication group you must set @AtRestEncryptionEnabled@ to @true@ when you create the replication group. 
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
  , authTokenEnabled :: Core.Maybe Core.Bool
    -- ^ A flag that enables using an @AuthToken@ (password) when issuing Redis commands.
--
-- Default: @false@ 
  , cacheNodeType :: Core.Maybe Core.Text
    -- ^ The cache node type of the Global Datastore
  , clusterEnabled :: Core.Maybe Core.Bool
    -- ^ A flag that indicates whether the Global Datastore is cluster enabled.
  , engine :: Core.Maybe Core.Text
    -- ^ The Elasticache engine. For Redis only.
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The Elasticache Redis engine version.
  , globalNodeGroups :: Core.Maybe [Types.GlobalNodeGroup]
    -- ^ Indicates the slot configuration and global identifier for each slice group.
  , globalReplicationGroupDescription :: Core.Maybe Core.Text
    -- ^ The optional description of the Global Datastore
  , globalReplicationGroupId :: Core.Maybe Core.Text
    -- ^ The name of the Global Datastore
  , members :: Core.Maybe [Types.GlobalReplicationGroupMember]
    -- ^ The replication groups that comprise the Global Datastore.
  , status :: Core.Maybe Core.Text
    -- ^ The status of the Global Datastore
  , transitEncryptionEnabled :: Core.Maybe Core.Bool
    -- ^ A flag that enables in-transit encryption when set to true. You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to true when you create a cluster. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GlobalReplicationGroup' value with any optional fields omitted.
mkGlobalReplicationGroup
    :: GlobalReplicationGroup
mkGlobalReplicationGroup
  = GlobalReplicationGroup'{arn = Core.Nothing,
                            atRestEncryptionEnabled = Core.Nothing,
                            authTokenEnabled = Core.Nothing, cacheNodeType = Core.Nothing,
                            clusterEnabled = Core.Nothing, engine = Core.Nothing,
                            engineVersion = Core.Nothing, globalNodeGroups = Core.Nothing,
                            globalReplicationGroupDescription = Core.Nothing,
                            globalReplicationGroupId = Core.Nothing, members = Core.Nothing,
                            status = Core.Nothing, transitEncryptionEnabled = Core.Nothing}

-- | The ARN (Amazon Resource Name) of the global replication group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgARN :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Text)
grgARN = Lens.field @"arn"
{-# INLINEABLE grgARN #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | A flag that enables encryption at rest when set to @true@ .
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the replication group is created. To enable encryption at rest on a replication group you must set @AtRestEncryptionEnabled@ to @true@ when you create the replication group. 
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
--
-- /Note:/ Consider using 'atRestEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgAtRestEncryptionEnabled :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Bool)
grgAtRestEncryptionEnabled = Lens.field @"atRestEncryptionEnabled"
{-# INLINEABLE grgAtRestEncryptionEnabled #-}
{-# DEPRECATED atRestEncryptionEnabled "Use generic-lens or generic-optics with 'atRestEncryptionEnabled' instead"  #-}

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis commands.
--
-- Default: @false@ 
--
-- /Note:/ Consider using 'authTokenEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgAuthTokenEnabled :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Bool)
grgAuthTokenEnabled = Lens.field @"authTokenEnabled"
{-# INLINEABLE grgAuthTokenEnabled #-}
{-# DEPRECATED authTokenEnabled "Use generic-lens or generic-optics with 'authTokenEnabled' instead"  #-}

-- | The cache node type of the Global Datastore
--
-- /Note:/ Consider using 'cacheNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgCacheNodeType :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Text)
grgCacheNodeType = Lens.field @"cacheNodeType"
{-# INLINEABLE grgCacheNodeType #-}
{-# DEPRECATED cacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead"  #-}

-- | A flag that indicates whether the Global Datastore is cluster enabled.
--
-- /Note:/ Consider using 'clusterEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgClusterEnabled :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Bool)
grgClusterEnabled = Lens.field @"clusterEnabled"
{-# INLINEABLE grgClusterEnabled #-}
{-# DEPRECATED clusterEnabled "Use generic-lens or generic-optics with 'clusterEnabled' instead"  #-}

-- | The Elasticache engine. For Redis only.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgEngine :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Text)
grgEngine = Lens.field @"engine"
{-# INLINEABLE grgEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | The Elasticache Redis engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgEngineVersion :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Text)
grgEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE grgEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | Indicates the slot configuration and global identifier for each slice group.
--
-- /Note:/ Consider using 'globalNodeGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgGlobalNodeGroups :: Lens.Lens' GlobalReplicationGroup (Core.Maybe [Types.GlobalNodeGroup])
grgGlobalNodeGroups = Lens.field @"globalNodeGroups"
{-# INLINEABLE grgGlobalNodeGroups #-}
{-# DEPRECATED globalNodeGroups "Use generic-lens or generic-optics with 'globalNodeGroups' instead"  #-}

-- | The optional description of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgGlobalReplicationGroupDescription :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Text)
grgGlobalReplicationGroupDescription = Lens.field @"globalReplicationGroupDescription"
{-# INLINEABLE grgGlobalReplicationGroupDescription #-}
{-# DEPRECATED globalReplicationGroupDescription "Use generic-lens or generic-optics with 'globalReplicationGroupDescription' instead"  #-}

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgGlobalReplicationGroupId :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Text)
grgGlobalReplicationGroupId = Lens.field @"globalReplicationGroupId"
{-# INLINEABLE grgGlobalReplicationGroupId #-}
{-# DEPRECATED globalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead"  #-}

-- | The replication groups that comprise the Global Datastore.
--
-- /Note:/ Consider using 'members' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgMembers :: Lens.Lens' GlobalReplicationGroup (Core.Maybe [Types.GlobalReplicationGroupMember])
grgMembers = Lens.field @"members"
{-# INLINEABLE grgMembers #-}
{-# DEPRECATED members "Use generic-lens or generic-optics with 'members' instead"  #-}

-- | The status of the Global Datastore
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgStatus :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Text)
grgStatus = Lens.field @"status"
{-# INLINEABLE grgStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A flag that enables in-transit encryption when set to true. You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to true when you create a cluster. 
--
-- /Note:/ Consider using 'transitEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgTransitEncryptionEnabled :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Bool)
grgTransitEncryptionEnabled = Lens.field @"transitEncryptionEnabled"
{-# INLINEABLE grgTransitEncryptionEnabled #-}
{-# DEPRECATED transitEncryptionEnabled "Use generic-lens or generic-optics with 'transitEncryptionEnabled' instead"  #-}

instance Core.FromXML GlobalReplicationGroup where
        parseXML x
          = GlobalReplicationGroup' Core.<$>
              (x Core..@? "ARN") Core.<*> x Core..@? "AtRestEncryptionEnabled"
                Core.<*> x Core..@? "AuthTokenEnabled"
                Core.<*> x Core..@? "CacheNodeType"
                Core.<*> x Core..@? "ClusterEnabled"
                Core.<*> x Core..@? "Engine"
                Core.<*> x Core..@? "EngineVersion"
                Core.<*>
                x Core..@? "GlobalNodeGroups" Core..<@>
                  Core.parseXMLList "GlobalNodeGroup"
                Core.<*> x Core..@? "GlobalReplicationGroupDescription"
                Core.<*> x Core..@? "GlobalReplicationGroupId"
                Core.<*>
                x Core..@? "Members" Core..<@>
                  Core.parseXMLList "GlobalReplicationGroupMember"
                Core.<*> x Core..@? "Status"
                Core.<*> x Core..@? "TransitEncryptionEnabled"
