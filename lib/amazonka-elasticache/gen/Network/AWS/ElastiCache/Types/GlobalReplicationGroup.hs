{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.GlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.GlobalReplicationGroup
  ( GlobalReplicationGroup (..),

    -- * Smart constructor
    mkGlobalReplicationGroup,

    -- * Lenses
    grgARN,
    grgAtRestEncryptionEnabled,
    grgAuthTokenEnabled,
    grgCacheNodeType,
    grgClusterEnabled,
    grgEngine,
    grgEngineVersion,
    grgGlobalNodeGroups,
    grgGlobalReplicationGroupDescription,
    grgGlobalReplicationGroupId,
    grgMembers,
    grgStatus,
    grgTransitEncryptionEnabled,
  )
where

import qualified Network.AWS.ElastiCache.Types.GlobalNodeGroup as Types
import qualified Network.AWS.ElastiCache.Types.GlobalReplicationGroupMember as Types
import qualified Network.AWS.ElastiCache.Types.String as Types
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
  { -- | The ARN (Amazon Resource Name) of the global replication group.
    arn :: Core.Maybe Types.String,
    -- | A flag that enables encryption at rest when set to @true@ .
    --
    -- You cannot modify the value of @AtRestEncryptionEnabled@ after the replication group is created. To enable encryption at rest on a replication group you must set @AtRestEncryptionEnabled@ to @true@ when you create the replication group.
    -- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
    atRestEncryptionEnabled :: Core.Maybe Core.Bool,
    -- | A flag that enables using an @AuthToken@ (password) when issuing Redis commands.
    --
    -- Default: @false@
    authTokenEnabled :: Core.Maybe Core.Bool,
    -- | The cache node type of the Global Datastore
    cacheNodeType :: Core.Maybe Types.String,
    -- | A flag that indicates whether the Global Datastore is cluster enabled.
    clusterEnabled :: Core.Maybe Core.Bool,
    -- | The Elasticache engine. For Redis only.
    engine :: Core.Maybe Types.String,
    -- | The Elasticache Redis engine version.
    engineVersion :: Core.Maybe Types.String,
    -- | Indicates the slot configuration and global identifier for each slice group.
    globalNodeGroups :: Core.Maybe [Types.GlobalNodeGroup],
    -- | The optional description of the Global Datastore
    globalReplicationGroupDescription :: Core.Maybe Types.String,
    -- | The name of the Global Datastore
    globalReplicationGroupId :: Core.Maybe Types.String,
    -- | The replication groups that comprise the Global Datastore.
    members :: Core.Maybe [Types.GlobalReplicationGroupMember],
    -- | The status of the Global Datastore
    status :: Core.Maybe Types.String,
    -- | A flag that enables in-transit encryption when set to true. You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to true when you create a cluster.
    transitEncryptionEnabled :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GlobalReplicationGroup' value with any optional fields omitted.
mkGlobalReplicationGroup ::
  GlobalReplicationGroup
mkGlobalReplicationGroup =
  GlobalReplicationGroup'
    { arn = Core.Nothing,
      atRestEncryptionEnabled = Core.Nothing,
      authTokenEnabled = Core.Nothing,
      cacheNodeType = Core.Nothing,
      clusterEnabled = Core.Nothing,
      engine = Core.Nothing,
      engineVersion = Core.Nothing,
      globalNodeGroups = Core.Nothing,
      globalReplicationGroupDescription = Core.Nothing,
      globalReplicationGroupId = Core.Nothing,
      members = Core.Nothing,
      status = Core.Nothing,
      transitEncryptionEnabled = Core.Nothing
    }

-- | The ARN (Amazon Resource Name) of the global replication group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgARN :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Types.String)
grgARN = Lens.field @"arn"
{-# DEPRECATED grgARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A flag that enables encryption at rest when set to @true@ .
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the replication group is created. To enable encryption at rest on a replication group you must set @AtRestEncryptionEnabled@ to @true@ when you create the replication group.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
--
-- /Note:/ Consider using 'atRestEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgAtRestEncryptionEnabled :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Bool)
grgAtRestEncryptionEnabled = Lens.field @"atRestEncryptionEnabled"
{-# DEPRECATED grgAtRestEncryptionEnabled "Use generic-lens or generic-optics with 'atRestEncryptionEnabled' instead." #-}

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis commands.
--
-- Default: @false@
--
-- /Note:/ Consider using 'authTokenEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgAuthTokenEnabled :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Bool)
grgAuthTokenEnabled = Lens.field @"authTokenEnabled"
{-# DEPRECATED grgAuthTokenEnabled "Use generic-lens or generic-optics with 'authTokenEnabled' instead." #-}

-- | The cache node type of the Global Datastore
--
-- /Note:/ Consider using 'cacheNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgCacheNodeType :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Types.String)
grgCacheNodeType = Lens.field @"cacheNodeType"
{-# DEPRECATED grgCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | A flag that indicates whether the Global Datastore is cluster enabled.
--
-- /Note:/ Consider using 'clusterEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgClusterEnabled :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Bool)
grgClusterEnabled = Lens.field @"clusterEnabled"
{-# DEPRECATED grgClusterEnabled "Use generic-lens or generic-optics with 'clusterEnabled' instead." #-}

-- | The Elasticache engine. For Redis only.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgEngine :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Types.String)
grgEngine = Lens.field @"engine"
{-# DEPRECATED grgEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The Elasticache Redis engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgEngineVersion :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Types.String)
grgEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED grgEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Indicates the slot configuration and global identifier for each slice group.
--
-- /Note:/ Consider using 'globalNodeGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgGlobalNodeGroups :: Lens.Lens' GlobalReplicationGroup (Core.Maybe [Types.GlobalNodeGroup])
grgGlobalNodeGroups = Lens.field @"globalNodeGroups"
{-# DEPRECATED grgGlobalNodeGroups "Use generic-lens or generic-optics with 'globalNodeGroups' instead." #-}

-- | The optional description of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgGlobalReplicationGroupDescription :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Types.String)
grgGlobalReplicationGroupDescription = Lens.field @"globalReplicationGroupDescription"
{-# DEPRECATED grgGlobalReplicationGroupDescription "Use generic-lens or generic-optics with 'globalReplicationGroupDescription' instead." #-}

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgGlobalReplicationGroupId :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Types.String)
grgGlobalReplicationGroupId = Lens.field @"globalReplicationGroupId"
{-# DEPRECATED grgGlobalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead." #-}

-- | The replication groups that comprise the Global Datastore.
--
-- /Note:/ Consider using 'members' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgMembers :: Lens.Lens' GlobalReplicationGroup (Core.Maybe [Types.GlobalReplicationGroupMember])
grgMembers = Lens.field @"members"
{-# DEPRECATED grgMembers "Use generic-lens or generic-optics with 'members' instead." #-}

-- | The status of the Global Datastore
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgStatus :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Types.String)
grgStatus = Lens.field @"status"
{-# DEPRECATED grgStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A flag that enables in-transit encryption when set to true. You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to true when you create a cluster.
--
-- /Note:/ Consider using 'transitEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgTransitEncryptionEnabled :: Lens.Lens' GlobalReplicationGroup (Core.Maybe Core.Bool)
grgTransitEncryptionEnabled = Lens.field @"transitEncryptionEnabled"
{-# DEPRECATED grgTransitEncryptionEnabled "Use generic-lens or generic-optics with 'transitEncryptionEnabled' instead." #-}

instance Core.FromXML GlobalReplicationGroup where
  parseXML x =
    GlobalReplicationGroup'
      Core.<$> (x Core..@? "ARN")
      Core.<*> (x Core..@? "AtRestEncryptionEnabled")
      Core.<*> (x Core..@? "AuthTokenEnabled")
      Core.<*> (x Core..@? "CacheNodeType")
      Core.<*> (x Core..@? "ClusterEnabled")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> ( x Core..@? "GlobalNodeGroups"
                   Core..<@> Core.parseXMLList "GlobalNodeGroup"
               )
      Core.<*> (x Core..@? "GlobalReplicationGroupDescription")
      Core.<*> (x Core..@? "GlobalReplicationGroupId")
      Core.<*> ( x Core..@? "Members"
                   Core..<@> Core.parseXMLList "GlobalReplicationGroupMember"
               )
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "TransitEncryptionEnabled")
