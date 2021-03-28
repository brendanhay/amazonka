{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.GlobalCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.GlobalCluster
  ( GlobalCluster (..)
  -- * Smart constructor
  , mkGlobalCluster
  -- * Lenses
  , gcDatabaseName
  , gcDeletionProtection
  , gcEngine
  , gcEngineVersion
  , gcGlobalClusterArn
  , gcGlobalClusterIdentifier
  , gcGlobalClusterMembers
  , gcGlobalClusterResourceId
  , gcStatus
  , gcStorageEncrypted
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.GlobalClusterMember as Types

-- | A data type representing an Aurora global database.
--
-- /See:/ 'mkGlobalCluster' smart constructor.
data GlobalCluster = GlobalCluster'
  { databaseName :: Core.Maybe Core.Text
    -- ^ The default database name within the new global database cluster. 
  , deletionProtection :: Core.Maybe Core.Bool
    -- ^ The deletion protection setting for the new global database cluster. 
  , engine :: Core.Maybe Core.Text
    -- ^ The Aurora database engine used by the global database cluster. 
  , engineVersion :: Core.Maybe Core.Text
    -- ^ Indicates the database engine version.
  , globalClusterArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for the global database cluster.
  , globalClusterIdentifier :: Core.Maybe Core.Text
    -- ^ Contains a user-supplied global database cluster identifier. This identifier is the unique key that identifies a global database cluster. 
  , globalClusterMembers :: Core.Maybe [Types.GlobalClusterMember]
    -- ^ The list of cluster IDs for secondary clusters within the global database cluster. Currently limited to 1 item. 
  , globalClusterResourceId :: Core.Maybe Core.Text
    -- ^ The AWS Region-unique, immutable identifier for the global database cluster. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB cluster is accessed. 
  , status :: Core.Maybe Core.Text
    -- ^ Specifies the current state of this global database cluster.
  , storageEncrypted :: Core.Maybe Core.Bool
    -- ^ The storage encryption setting for the global database cluster. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GlobalCluster' value with any optional fields omitted.
mkGlobalCluster
    :: GlobalCluster
mkGlobalCluster
  = GlobalCluster'{databaseName = Core.Nothing,
                   deletionProtection = Core.Nothing, engine = Core.Nothing,
                   engineVersion = Core.Nothing, globalClusterArn = Core.Nothing,
                   globalClusterIdentifier = Core.Nothing,
                   globalClusterMembers = Core.Nothing,
                   globalClusterResourceId = Core.Nothing, status = Core.Nothing,
                   storageEncrypted = Core.Nothing}

-- | The default database name within the new global database cluster. 
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcDatabaseName :: Lens.Lens' GlobalCluster (Core.Maybe Core.Text)
gcDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE gcDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The deletion protection setting for the new global database cluster. 
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcDeletionProtection :: Lens.Lens' GlobalCluster (Core.Maybe Core.Bool)
gcDeletionProtection = Lens.field @"deletionProtection"
{-# INLINEABLE gcDeletionProtection #-}
{-# DEPRECATED deletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead"  #-}

-- | The Aurora database engine used by the global database cluster. 
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcEngine :: Lens.Lens' GlobalCluster (Core.Maybe Core.Text)
gcEngine = Lens.field @"engine"
{-# INLINEABLE gcEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | Indicates the database engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcEngineVersion :: Lens.Lens' GlobalCluster (Core.Maybe Core.Text)
gcEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE gcEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | The Amazon Resource Name (ARN) for the global database cluster.
--
-- /Note:/ Consider using 'globalClusterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcGlobalClusterArn :: Lens.Lens' GlobalCluster (Core.Maybe Core.Text)
gcGlobalClusterArn = Lens.field @"globalClusterArn"
{-# INLINEABLE gcGlobalClusterArn #-}
{-# DEPRECATED globalClusterArn "Use generic-lens or generic-optics with 'globalClusterArn' instead"  #-}

-- | Contains a user-supplied global database cluster identifier. This identifier is the unique key that identifies a global database cluster. 
--
-- /Note:/ Consider using 'globalClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcGlobalClusterIdentifier :: Lens.Lens' GlobalCluster (Core.Maybe Core.Text)
gcGlobalClusterIdentifier = Lens.field @"globalClusterIdentifier"
{-# INLINEABLE gcGlobalClusterIdentifier #-}
{-# DEPRECATED globalClusterIdentifier "Use generic-lens or generic-optics with 'globalClusterIdentifier' instead"  #-}

-- | The list of cluster IDs for secondary clusters within the global database cluster. Currently limited to 1 item. 
--
-- /Note:/ Consider using 'globalClusterMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcGlobalClusterMembers :: Lens.Lens' GlobalCluster (Core.Maybe [Types.GlobalClusterMember])
gcGlobalClusterMembers = Lens.field @"globalClusterMembers"
{-# INLINEABLE gcGlobalClusterMembers #-}
{-# DEPRECATED globalClusterMembers "Use generic-lens or generic-optics with 'globalClusterMembers' instead"  #-}

-- | The AWS Region-unique, immutable identifier for the global database cluster. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB cluster is accessed. 
--
-- /Note:/ Consider using 'globalClusterResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcGlobalClusterResourceId :: Lens.Lens' GlobalCluster (Core.Maybe Core.Text)
gcGlobalClusterResourceId = Lens.field @"globalClusterResourceId"
{-# INLINEABLE gcGlobalClusterResourceId #-}
{-# DEPRECATED globalClusterResourceId "Use generic-lens or generic-optics with 'globalClusterResourceId' instead"  #-}

-- | Specifies the current state of this global database cluster.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcStatus :: Lens.Lens' GlobalCluster (Core.Maybe Core.Text)
gcStatus = Lens.field @"status"
{-# INLINEABLE gcStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The storage encryption setting for the global database cluster. 
--
-- /Note:/ Consider using 'storageEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcStorageEncrypted :: Lens.Lens' GlobalCluster (Core.Maybe Core.Bool)
gcStorageEncrypted = Lens.field @"storageEncrypted"
{-# INLINEABLE gcStorageEncrypted #-}
{-# DEPRECATED storageEncrypted "Use generic-lens or generic-optics with 'storageEncrypted' instead"  #-}

instance Core.FromXML GlobalCluster where
        parseXML x
          = GlobalCluster' Core.<$>
              (x Core..@? "DatabaseName") Core.<*>
                x Core..@? "DeletionProtection"
                Core.<*> x Core..@? "Engine"
                Core.<*> x Core..@? "EngineVersion"
                Core.<*> x Core..@? "GlobalClusterArn"
                Core.<*> x Core..@? "GlobalClusterIdentifier"
                Core.<*>
                x Core..@? "GlobalClusterMembers" Core..<@>
                  Core.parseXMLList "GlobalClusterMember"
                Core.<*> x Core..@? "GlobalClusterResourceId"
                Core.<*> x Core..@? "Status"
                Core.<*> x Core..@? "StorageEncrypted"
