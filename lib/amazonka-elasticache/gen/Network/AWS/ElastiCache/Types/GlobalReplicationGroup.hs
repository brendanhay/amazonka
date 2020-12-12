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
    grgEngineVersion,
    grgStatus,
    grgCacheNodeType,
    grgClusterEnabled,
    grgAtRestEncryptionEnabled,
    grgARN,
    grgTransitEncryptionEnabled,
    grgMembers,
    grgEngine,
    grgAuthTokenEnabled,
    grgGlobalNodeGroups,
    grgGlobalReplicationGroupId,
    grgGlobalReplicationGroupDescription,
  )
where

import Network.AWS.ElastiCache.Types.GlobalNodeGroup
import Network.AWS.ElastiCache.Types.GlobalReplicationGroupMember
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Consists of a primary cluster that accepts writes and an associated secondary cluster that resides in a different AWS region. The secondary cluster accepts only reads. The primary cluster automatically replicates updates to the secondary cluster.
--
--
--     * The __GlobalReplicationGroupIdSuffix__ represents the name of the Global Datastore, which is what you use to associate a secondary cluster.
--
--
--
-- /See:/ 'mkGlobalReplicationGroup' smart constructor.
data GlobalReplicationGroup = GlobalReplicationGroup'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    status :: Lude.Maybe Lude.Text,
    cacheNodeType :: Lude.Maybe Lude.Text,
    clusterEnabled :: Lude.Maybe Lude.Bool,
    atRestEncryptionEnabled ::
      Lude.Maybe Lude.Bool,
    arn :: Lude.Maybe Lude.Text,
    transitEncryptionEnabled ::
      Lude.Maybe Lude.Bool,
    members ::
      Lude.Maybe [GlobalReplicationGroupMember],
    engine :: Lude.Maybe Lude.Text,
    authTokenEnabled :: Lude.Maybe Lude.Bool,
    globalNodeGroups ::
      Lude.Maybe [GlobalNodeGroup],
    globalReplicationGroupId ::
      Lude.Maybe Lude.Text,
    globalReplicationGroupDescription ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GlobalReplicationGroup' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN (Amazon Resource Name) of the global replication group.
-- * 'atRestEncryptionEnabled' - A flag that enables encryption at rest when set to @true@ .
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the replication group is created. To enable encryption at rest on a replication group you must set @AtRestEncryptionEnabled@ to @true@ when you create the replication group.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- * 'authTokenEnabled' - A flag that enables using an @AuthToken@ (password) when issuing Redis commands.
--
-- Default: @false@
-- * 'cacheNodeType' - The cache node type of the Global Datastore
-- * 'clusterEnabled' - A flag that indicates whether the Global Datastore is cluster enabled.
-- * 'engine' - The Elasticache engine. For Redis only.
-- * 'engineVersion' - The Elasticache Redis engine version.
-- * 'globalNodeGroups' - Indicates the slot configuration and global identifier for each slice group.
-- * 'globalReplicationGroupDescription' - The optional description of the Global Datastore
-- * 'globalReplicationGroupId' - The name of the Global Datastore
-- * 'members' - The replication groups that comprise the Global Datastore.
-- * 'status' - The status of the Global Datastore
-- * 'transitEncryptionEnabled' - A flag that enables in-transit encryption when set to true. You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to true when you create a cluster.
mkGlobalReplicationGroup ::
  GlobalReplicationGroup
mkGlobalReplicationGroup =
  GlobalReplicationGroup'
    { engineVersion = Lude.Nothing,
      status = Lude.Nothing,
      cacheNodeType = Lude.Nothing,
      clusterEnabled = Lude.Nothing,
      atRestEncryptionEnabled = Lude.Nothing,
      arn = Lude.Nothing,
      transitEncryptionEnabled = Lude.Nothing,
      members = Lude.Nothing,
      engine = Lude.Nothing,
      authTokenEnabled = Lude.Nothing,
      globalNodeGroups = Lude.Nothing,
      globalReplicationGroupId = Lude.Nothing,
      globalReplicationGroupDescription = Lude.Nothing
    }

-- | The Elasticache Redis engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgEngineVersion :: Lens.Lens' GlobalReplicationGroup (Lude.Maybe Lude.Text)
grgEngineVersion = Lens.lens (engineVersion :: GlobalReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: GlobalReplicationGroup)
{-# DEPRECATED grgEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The status of the Global Datastore
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgStatus :: Lens.Lens' GlobalReplicationGroup (Lude.Maybe Lude.Text)
grgStatus = Lens.lens (status :: GlobalReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: GlobalReplicationGroup)
{-# DEPRECATED grgStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The cache node type of the Global Datastore
--
-- /Note:/ Consider using 'cacheNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgCacheNodeType :: Lens.Lens' GlobalReplicationGroup (Lude.Maybe Lude.Text)
grgCacheNodeType = Lens.lens (cacheNodeType :: GlobalReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {cacheNodeType = a} :: GlobalReplicationGroup)
{-# DEPRECATED grgCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | A flag that indicates whether the Global Datastore is cluster enabled.
--
-- /Note:/ Consider using 'clusterEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgClusterEnabled :: Lens.Lens' GlobalReplicationGroup (Lude.Maybe Lude.Bool)
grgClusterEnabled = Lens.lens (clusterEnabled :: GlobalReplicationGroup -> Lude.Maybe Lude.Bool) (\s a -> s {clusterEnabled = a} :: GlobalReplicationGroup)
{-# DEPRECATED grgClusterEnabled "Use generic-lens or generic-optics with 'clusterEnabled' instead." #-}

-- | A flag that enables encryption at rest when set to @true@ .
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the replication group is created. To enable encryption at rest on a replication group you must set @AtRestEncryptionEnabled@ to @true@ when you create the replication group.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
--
-- /Note:/ Consider using 'atRestEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgAtRestEncryptionEnabled :: Lens.Lens' GlobalReplicationGroup (Lude.Maybe Lude.Bool)
grgAtRestEncryptionEnabled = Lens.lens (atRestEncryptionEnabled :: GlobalReplicationGroup -> Lude.Maybe Lude.Bool) (\s a -> s {atRestEncryptionEnabled = a} :: GlobalReplicationGroup)
{-# DEPRECATED grgAtRestEncryptionEnabled "Use generic-lens or generic-optics with 'atRestEncryptionEnabled' instead." #-}

-- | The ARN (Amazon Resource Name) of the global replication group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgARN :: Lens.Lens' GlobalReplicationGroup (Lude.Maybe Lude.Text)
grgARN = Lens.lens (arn :: GlobalReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GlobalReplicationGroup)
{-# DEPRECATED grgARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A flag that enables in-transit encryption when set to true. You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to true when you create a cluster.
--
-- /Note:/ Consider using 'transitEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgTransitEncryptionEnabled :: Lens.Lens' GlobalReplicationGroup (Lude.Maybe Lude.Bool)
grgTransitEncryptionEnabled = Lens.lens (transitEncryptionEnabled :: GlobalReplicationGroup -> Lude.Maybe Lude.Bool) (\s a -> s {transitEncryptionEnabled = a} :: GlobalReplicationGroup)
{-# DEPRECATED grgTransitEncryptionEnabled "Use generic-lens or generic-optics with 'transitEncryptionEnabled' instead." #-}

-- | The replication groups that comprise the Global Datastore.
--
-- /Note:/ Consider using 'members' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgMembers :: Lens.Lens' GlobalReplicationGroup (Lude.Maybe [GlobalReplicationGroupMember])
grgMembers = Lens.lens (members :: GlobalReplicationGroup -> Lude.Maybe [GlobalReplicationGroupMember]) (\s a -> s {members = a} :: GlobalReplicationGroup)
{-# DEPRECATED grgMembers "Use generic-lens or generic-optics with 'members' instead." #-}

-- | The Elasticache engine. For Redis only.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgEngine :: Lens.Lens' GlobalReplicationGroup (Lude.Maybe Lude.Text)
grgEngine = Lens.lens (engine :: GlobalReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: GlobalReplicationGroup)
{-# DEPRECATED grgEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis commands.
--
-- Default: @false@
--
-- /Note:/ Consider using 'authTokenEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgAuthTokenEnabled :: Lens.Lens' GlobalReplicationGroup (Lude.Maybe Lude.Bool)
grgAuthTokenEnabled = Lens.lens (authTokenEnabled :: GlobalReplicationGroup -> Lude.Maybe Lude.Bool) (\s a -> s {authTokenEnabled = a} :: GlobalReplicationGroup)
{-# DEPRECATED grgAuthTokenEnabled "Use generic-lens or generic-optics with 'authTokenEnabled' instead." #-}

-- | Indicates the slot configuration and global identifier for each slice group.
--
-- /Note:/ Consider using 'globalNodeGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgGlobalNodeGroups :: Lens.Lens' GlobalReplicationGroup (Lude.Maybe [GlobalNodeGroup])
grgGlobalNodeGroups = Lens.lens (globalNodeGroups :: GlobalReplicationGroup -> Lude.Maybe [GlobalNodeGroup]) (\s a -> s {globalNodeGroups = a} :: GlobalReplicationGroup)
{-# DEPRECATED grgGlobalNodeGroups "Use generic-lens or generic-optics with 'globalNodeGroups' instead." #-}

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgGlobalReplicationGroupId :: Lens.Lens' GlobalReplicationGroup (Lude.Maybe Lude.Text)
grgGlobalReplicationGroupId = Lens.lens (globalReplicationGroupId :: GlobalReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {globalReplicationGroupId = a} :: GlobalReplicationGroup)
{-# DEPRECATED grgGlobalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead." #-}

-- | The optional description of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grgGlobalReplicationGroupDescription :: Lens.Lens' GlobalReplicationGroup (Lude.Maybe Lude.Text)
grgGlobalReplicationGroupDescription = Lens.lens (globalReplicationGroupDescription :: GlobalReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {globalReplicationGroupDescription = a} :: GlobalReplicationGroup)
{-# DEPRECATED grgGlobalReplicationGroupDescription "Use generic-lens or generic-optics with 'globalReplicationGroupDescription' instead." #-}

instance Lude.FromXML GlobalReplicationGroup where
  parseXML x =
    GlobalReplicationGroup'
      Lude.<$> (x Lude..@? "EngineVersion")
      Lude.<*> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "CacheNodeType")
      Lude.<*> (x Lude..@? "ClusterEnabled")
      Lude.<*> (x Lude..@? "AtRestEncryptionEnabled")
      Lude.<*> (x Lude..@? "ARN")
      Lude.<*> (x Lude..@? "TransitEncryptionEnabled")
      Lude.<*> ( x Lude..@? "Members" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "GlobalReplicationGroupMember")
               )
      Lude.<*> (x Lude..@? "Engine")
      Lude.<*> (x Lude..@? "AuthTokenEnabled")
      Lude.<*> ( x Lude..@? "GlobalNodeGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "GlobalNodeGroup")
               )
      Lude.<*> (x Lude..@? "GlobalReplicationGroupId")
      Lude.<*> (x Lude..@? "GlobalReplicationGroupDescription")
