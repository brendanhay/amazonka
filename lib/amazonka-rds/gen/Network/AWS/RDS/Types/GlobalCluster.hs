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
    gcEngineVersion,
    gcStatus,
    gcDeletionProtection,
    gcStorageEncrypted,
    gcGlobalClusterIdentifier,
    gcEngine,
    gcGlobalClusterARN,
    gcDatabaseName,
    gcGlobalClusterMembers,
    gcGlobalClusterResourceId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.GlobalClusterMember

-- | A data type representing an Aurora global database.
--
-- /See:/ 'mkGlobalCluster' smart constructor.
data GlobalCluster = GlobalCluster'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    status :: Lude.Maybe Lude.Text,
    deletionProtection :: Lude.Maybe Lude.Bool,
    storageEncrypted :: Lude.Maybe Lude.Bool,
    globalClusterIdentifier :: Lude.Maybe Lude.Text,
    engine :: Lude.Maybe Lude.Text,
    globalClusterARN :: Lude.Maybe Lude.Text,
    databaseName :: Lude.Maybe Lude.Text,
    globalClusterMembers :: Lude.Maybe [GlobalClusterMember],
    globalClusterResourceId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GlobalCluster' with the minimum fields required to make a request.
--
-- * 'databaseName' - The default database name within the new global database cluster.
-- * 'deletionProtection' - The deletion protection setting for the new global database cluster.
-- * 'engine' - The Aurora database engine used by the global database cluster.
-- * 'engineVersion' - Indicates the database engine version.
-- * 'globalClusterARN' - The Amazon Resource Name (ARN) for the global database cluster.
-- * 'globalClusterIdentifier' - Contains a user-supplied global database cluster identifier. This identifier is the unique key that identifies a global database cluster.
-- * 'globalClusterMembers' - The list of cluster IDs for secondary clusters within the global database cluster. Currently limited to 1 item.
-- * 'globalClusterResourceId' - The AWS Region-unique, immutable identifier for the global database cluster. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB cluster is accessed.
-- * 'status' - Specifies the current state of this global database cluster.
-- * 'storageEncrypted' - The storage encryption setting for the global database cluster.
mkGlobalCluster ::
  GlobalCluster
mkGlobalCluster =
  GlobalCluster'
    { engineVersion = Lude.Nothing,
      status = Lude.Nothing,
      deletionProtection = Lude.Nothing,
      storageEncrypted = Lude.Nothing,
      globalClusterIdentifier = Lude.Nothing,
      engine = Lude.Nothing,
      globalClusterARN = Lude.Nothing,
      databaseName = Lude.Nothing,
      globalClusterMembers = Lude.Nothing,
      globalClusterResourceId = Lude.Nothing
    }

-- | Indicates the database engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcEngineVersion :: Lens.Lens' GlobalCluster (Lude.Maybe Lude.Text)
gcEngineVersion = Lens.lens (engineVersion :: GlobalCluster -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: GlobalCluster)
{-# DEPRECATED gcEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Specifies the current state of this global database cluster.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcStatus :: Lens.Lens' GlobalCluster (Lude.Maybe Lude.Text)
gcStatus = Lens.lens (status :: GlobalCluster -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: GlobalCluster)
{-# DEPRECATED gcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The deletion protection setting for the new global database cluster.
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcDeletionProtection :: Lens.Lens' GlobalCluster (Lude.Maybe Lude.Bool)
gcDeletionProtection = Lens.lens (deletionProtection :: GlobalCluster -> Lude.Maybe Lude.Bool) (\s a -> s {deletionProtection = a} :: GlobalCluster)
{-# DEPRECATED gcDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | The storage encryption setting for the global database cluster.
--
-- /Note:/ Consider using 'storageEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcStorageEncrypted :: Lens.Lens' GlobalCluster (Lude.Maybe Lude.Bool)
gcStorageEncrypted = Lens.lens (storageEncrypted :: GlobalCluster -> Lude.Maybe Lude.Bool) (\s a -> s {storageEncrypted = a} :: GlobalCluster)
{-# DEPRECATED gcStorageEncrypted "Use generic-lens or generic-optics with 'storageEncrypted' instead." #-}

-- | Contains a user-supplied global database cluster identifier. This identifier is the unique key that identifies a global database cluster.
--
-- /Note:/ Consider using 'globalClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcGlobalClusterIdentifier :: Lens.Lens' GlobalCluster (Lude.Maybe Lude.Text)
gcGlobalClusterIdentifier = Lens.lens (globalClusterIdentifier :: GlobalCluster -> Lude.Maybe Lude.Text) (\s a -> s {globalClusterIdentifier = a} :: GlobalCluster)
{-# DEPRECATED gcGlobalClusterIdentifier "Use generic-lens or generic-optics with 'globalClusterIdentifier' instead." #-}

-- | The Aurora database engine used by the global database cluster.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcEngine :: Lens.Lens' GlobalCluster (Lude.Maybe Lude.Text)
gcEngine = Lens.lens (engine :: GlobalCluster -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: GlobalCluster)
{-# DEPRECATED gcEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The Amazon Resource Name (ARN) for the global database cluster.
--
-- /Note:/ Consider using 'globalClusterARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcGlobalClusterARN :: Lens.Lens' GlobalCluster (Lude.Maybe Lude.Text)
gcGlobalClusterARN = Lens.lens (globalClusterARN :: GlobalCluster -> Lude.Maybe Lude.Text) (\s a -> s {globalClusterARN = a} :: GlobalCluster)
{-# DEPRECATED gcGlobalClusterARN "Use generic-lens or generic-optics with 'globalClusterARN' instead." #-}

-- | The default database name within the new global database cluster.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcDatabaseName :: Lens.Lens' GlobalCluster (Lude.Maybe Lude.Text)
gcDatabaseName = Lens.lens (databaseName :: GlobalCluster -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: GlobalCluster)
{-# DEPRECATED gcDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The list of cluster IDs for secondary clusters within the global database cluster. Currently limited to 1 item.
--
-- /Note:/ Consider using 'globalClusterMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcGlobalClusterMembers :: Lens.Lens' GlobalCluster (Lude.Maybe [GlobalClusterMember])
gcGlobalClusterMembers = Lens.lens (globalClusterMembers :: GlobalCluster -> Lude.Maybe [GlobalClusterMember]) (\s a -> s {globalClusterMembers = a} :: GlobalCluster)
{-# DEPRECATED gcGlobalClusterMembers "Use generic-lens or generic-optics with 'globalClusterMembers' instead." #-}

-- | The AWS Region-unique, immutable identifier for the global database cluster. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB cluster is accessed.
--
-- /Note:/ Consider using 'globalClusterResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcGlobalClusterResourceId :: Lens.Lens' GlobalCluster (Lude.Maybe Lude.Text)
gcGlobalClusterResourceId = Lens.lens (globalClusterResourceId :: GlobalCluster -> Lude.Maybe Lude.Text) (\s a -> s {globalClusterResourceId = a} :: GlobalCluster)
{-# DEPRECATED gcGlobalClusterResourceId "Use generic-lens or generic-optics with 'globalClusterResourceId' instead." #-}

instance Lude.FromXML GlobalCluster where
  parseXML x =
    GlobalCluster'
      Lude.<$> (x Lude..@? "EngineVersion")
      Lude.<*> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "DeletionProtection")
      Lude.<*> (x Lude..@? "StorageEncrypted")
      Lude.<*> (x Lude..@? "GlobalClusterIdentifier")
      Lude.<*> (x Lude..@? "Engine")
      Lude.<*> (x Lude..@? "GlobalClusterArn")
      Lude.<*> (x Lude..@? "DatabaseName")
      Lude.<*> ( x Lude..@? "GlobalClusterMembers" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "GlobalClusterMember")
               )
      Lude.<*> (x Lude..@? "GlobalClusterResourceId")
