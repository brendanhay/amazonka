{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.GlobalCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.GlobalCluster
  ( GlobalCluster (..),

    -- * Smart constructor
    mkGlobalCluster,

    -- * Lenses
    gcDatabaseName,
    gcDeletionProtection,
    gcEngine,
    gcEngineVersion,
    gcGlobalClusterArn,
    gcGlobalClusterIdentifier,
    gcGlobalClusterMembers,
    gcGlobalClusterResourceId,
    gcStatus,
    gcStorageEncrypted,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.GlobalClusterMember as Types
import qualified Network.AWS.RDS.Types.String as Types

-- | A data type representing an Aurora global database.
--
-- /See:/ 'mkGlobalCluster' smart constructor.
data GlobalCluster = GlobalCluster'
  { -- | The default database name within the new global database cluster.
    databaseName :: Core.Maybe Types.String,
    -- | The deletion protection setting for the new global database cluster.
    deletionProtection :: Core.Maybe Core.Bool,
    -- | The Aurora database engine used by the global database cluster.
    engine :: Core.Maybe Types.String,
    -- | Indicates the database engine version.
    engineVersion :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) for the global database cluster.
    globalClusterArn :: Core.Maybe Types.String,
    -- | Contains a user-supplied global database cluster identifier. This identifier is the unique key that identifies a global database cluster.
    globalClusterIdentifier :: Core.Maybe Types.String,
    -- | The list of cluster IDs for secondary clusters within the global database cluster. Currently limited to 1 item.
    globalClusterMembers :: Core.Maybe [Types.GlobalClusterMember],
    -- | The AWS Region-unique, immutable identifier for the global database cluster. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB cluster is accessed.
    globalClusterResourceId :: Core.Maybe Types.String,
    -- | Specifies the current state of this global database cluster.
    status :: Core.Maybe Types.String,
    -- | The storage encryption setting for the global database cluster.
    storageEncrypted :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GlobalCluster' value with any optional fields omitted.
mkGlobalCluster ::
  GlobalCluster
mkGlobalCluster =
  GlobalCluster'
    { databaseName = Core.Nothing,
      deletionProtection = Core.Nothing,
      engine = Core.Nothing,
      engineVersion = Core.Nothing,
      globalClusterArn = Core.Nothing,
      globalClusterIdentifier = Core.Nothing,
      globalClusterMembers = Core.Nothing,
      globalClusterResourceId = Core.Nothing,
      status = Core.Nothing,
      storageEncrypted = Core.Nothing
    }

-- | The default database name within the new global database cluster.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcDatabaseName :: Lens.Lens' GlobalCluster (Core.Maybe Types.String)
gcDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED gcDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The deletion protection setting for the new global database cluster.
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcDeletionProtection :: Lens.Lens' GlobalCluster (Core.Maybe Core.Bool)
gcDeletionProtection = Lens.field @"deletionProtection"
{-# DEPRECATED gcDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | The Aurora database engine used by the global database cluster.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcEngine :: Lens.Lens' GlobalCluster (Core.Maybe Types.String)
gcEngine = Lens.field @"engine"
{-# DEPRECATED gcEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | Indicates the database engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcEngineVersion :: Lens.Lens' GlobalCluster (Core.Maybe Types.String)
gcEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED gcEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The Amazon Resource Name (ARN) for the global database cluster.
--
-- /Note:/ Consider using 'globalClusterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcGlobalClusterArn :: Lens.Lens' GlobalCluster (Core.Maybe Types.String)
gcGlobalClusterArn = Lens.field @"globalClusterArn"
{-# DEPRECATED gcGlobalClusterArn "Use generic-lens or generic-optics with 'globalClusterArn' instead." #-}

-- | Contains a user-supplied global database cluster identifier. This identifier is the unique key that identifies a global database cluster.
--
-- /Note:/ Consider using 'globalClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcGlobalClusterIdentifier :: Lens.Lens' GlobalCluster (Core.Maybe Types.String)
gcGlobalClusterIdentifier = Lens.field @"globalClusterIdentifier"
{-# DEPRECATED gcGlobalClusterIdentifier "Use generic-lens or generic-optics with 'globalClusterIdentifier' instead." #-}

-- | The list of cluster IDs for secondary clusters within the global database cluster. Currently limited to 1 item.
--
-- /Note:/ Consider using 'globalClusterMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcGlobalClusterMembers :: Lens.Lens' GlobalCluster (Core.Maybe [Types.GlobalClusterMember])
gcGlobalClusterMembers = Lens.field @"globalClusterMembers"
{-# DEPRECATED gcGlobalClusterMembers "Use generic-lens or generic-optics with 'globalClusterMembers' instead." #-}

-- | The AWS Region-unique, immutable identifier for the global database cluster. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB cluster is accessed.
--
-- /Note:/ Consider using 'globalClusterResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcGlobalClusterResourceId :: Lens.Lens' GlobalCluster (Core.Maybe Types.String)
gcGlobalClusterResourceId = Lens.field @"globalClusterResourceId"
{-# DEPRECATED gcGlobalClusterResourceId "Use generic-lens or generic-optics with 'globalClusterResourceId' instead." #-}

-- | Specifies the current state of this global database cluster.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcStatus :: Lens.Lens' GlobalCluster (Core.Maybe Types.String)
gcStatus = Lens.field @"status"
{-# DEPRECATED gcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The storage encryption setting for the global database cluster.
--
-- /Note:/ Consider using 'storageEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcStorageEncrypted :: Lens.Lens' GlobalCluster (Core.Maybe Core.Bool)
gcStorageEncrypted = Lens.field @"storageEncrypted"
{-# DEPRECATED gcStorageEncrypted "Use generic-lens or generic-optics with 'storageEncrypted' instead." #-}

instance Core.FromXML GlobalCluster where
  parseXML x =
    GlobalCluster'
      Core.<$> (x Core..@? "DatabaseName")
      Core.<*> (x Core..@? "DeletionProtection")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "GlobalClusterArn")
      Core.<*> (x Core..@? "GlobalClusterIdentifier")
      Core.<*> ( x Core..@? "GlobalClusterMembers"
                   Core..<@> Core.parseXMLList "GlobalClusterMember"
               )
      Core.<*> (x Core..@? "GlobalClusterResourceId")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "StorageEncrypted")
