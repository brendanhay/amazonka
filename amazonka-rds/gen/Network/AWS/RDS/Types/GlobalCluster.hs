{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.GlobalCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.GlobalCluster where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types.FailoverState
import Network.AWS.RDS.Types.GlobalClusterMember

-- | A data type representing an Aurora global database.
--
-- /See:/ 'newGlobalCluster' smart constructor.
data GlobalCluster = GlobalCluster'
  { -- | The deletion protection setting for the new global database cluster.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The storage encryption setting for the global database cluster.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the current state of this global database cluster.
    status :: Prelude.Maybe Prelude.Text,
    -- | A data object containing all properties for the current state of an
    -- in-process or pending failover process for this Aurora global database.
    -- This object is empty unless the FailoverGlobalCluster API operation has
    -- been called on this Aurora global database (GlobalCluster).
    failoverState :: Prelude.Maybe FailoverState,
    -- | Indicates the database engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the global database cluster.
    globalClusterArn :: Prelude.Maybe Prelude.Text,
    -- | The Aurora database engine used by the global database cluster.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region-unique, immutable identifier for the global database
    -- cluster. This identifier is found in AWS CloudTrail log entries whenever
    -- the AWS KMS customer master key (CMK) for the DB cluster is accessed.
    globalClusterResourceId :: Prelude.Maybe Prelude.Text,
    -- | The list of cluster IDs for secondary clusters within the global
    -- database cluster. Currently limited to 1 item.
    globalClusterMembers :: Prelude.Maybe [GlobalClusterMember],
    -- | Contains a user-supplied global database cluster identifier. This
    -- identifier is the unique key that identifies a global database cluster.
    globalClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The default database name within the new global database cluster.
    databaseName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GlobalCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionProtection', 'globalCluster_deletionProtection' - The deletion protection setting for the new global database cluster.
--
-- 'storageEncrypted', 'globalCluster_storageEncrypted' - The storage encryption setting for the global database cluster.
--
-- 'status', 'globalCluster_status' - Specifies the current state of this global database cluster.
--
-- 'failoverState', 'globalCluster_failoverState' - A data object containing all properties for the current state of an
-- in-process or pending failover process for this Aurora global database.
-- This object is empty unless the FailoverGlobalCluster API operation has
-- been called on this Aurora global database (GlobalCluster).
--
-- 'engineVersion', 'globalCluster_engineVersion' - Indicates the database engine version.
--
-- 'globalClusterArn', 'globalCluster_globalClusterArn' - The Amazon Resource Name (ARN) for the global database cluster.
--
-- 'engine', 'globalCluster_engine' - The Aurora database engine used by the global database cluster.
--
-- 'globalClusterResourceId', 'globalCluster_globalClusterResourceId' - The AWS Region-unique, immutable identifier for the global database
-- cluster. This identifier is found in AWS CloudTrail log entries whenever
-- the AWS KMS customer master key (CMK) for the DB cluster is accessed.
--
-- 'globalClusterMembers', 'globalCluster_globalClusterMembers' - The list of cluster IDs for secondary clusters within the global
-- database cluster. Currently limited to 1 item.
--
-- 'globalClusterIdentifier', 'globalCluster_globalClusterIdentifier' - Contains a user-supplied global database cluster identifier. This
-- identifier is the unique key that identifies a global database cluster.
--
-- 'databaseName', 'globalCluster_databaseName' - The default database name within the new global database cluster.
newGlobalCluster ::
  GlobalCluster
newGlobalCluster =
  GlobalCluster'
    { deletionProtection =
        Prelude.Nothing,
      storageEncrypted = Prelude.Nothing,
      status = Prelude.Nothing,
      failoverState = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      globalClusterArn = Prelude.Nothing,
      engine = Prelude.Nothing,
      globalClusterResourceId = Prelude.Nothing,
      globalClusterMembers = Prelude.Nothing,
      globalClusterIdentifier = Prelude.Nothing,
      databaseName = Prelude.Nothing
    }

-- | The deletion protection setting for the new global database cluster.
globalCluster_deletionProtection :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Bool)
globalCluster_deletionProtection = Lens.lens (\GlobalCluster' {deletionProtection} -> deletionProtection) (\s@GlobalCluster' {} a -> s {deletionProtection = a} :: GlobalCluster)

-- | The storage encryption setting for the global database cluster.
globalCluster_storageEncrypted :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Bool)
globalCluster_storageEncrypted = Lens.lens (\GlobalCluster' {storageEncrypted} -> storageEncrypted) (\s@GlobalCluster' {} a -> s {storageEncrypted = a} :: GlobalCluster)

-- | Specifies the current state of this global database cluster.
globalCluster_status :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Text)
globalCluster_status = Lens.lens (\GlobalCluster' {status} -> status) (\s@GlobalCluster' {} a -> s {status = a} :: GlobalCluster)

-- | A data object containing all properties for the current state of an
-- in-process or pending failover process for this Aurora global database.
-- This object is empty unless the FailoverGlobalCluster API operation has
-- been called on this Aurora global database (GlobalCluster).
globalCluster_failoverState :: Lens.Lens' GlobalCluster (Prelude.Maybe FailoverState)
globalCluster_failoverState = Lens.lens (\GlobalCluster' {failoverState} -> failoverState) (\s@GlobalCluster' {} a -> s {failoverState = a} :: GlobalCluster)

-- | Indicates the database engine version.
globalCluster_engineVersion :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Text)
globalCluster_engineVersion = Lens.lens (\GlobalCluster' {engineVersion} -> engineVersion) (\s@GlobalCluster' {} a -> s {engineVersion = a} :: GlobalCluster)

-- | The Amazon Resource Name (ARN) for the global database cluster.
globalCluster_globalClusterArn :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Text)
globalCluster_globalClusterArn = Lens.lens (\GlobalCluster' {globalClusterArn} -> globalClusterArn) (\s@GlobalCluster' {} a -> s {globalClusterArn = a} :: GlobalCluster)

-- | The Aurora database engine used by the global database cluster.
globalCluster_engine :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Text)
globalCluster_engine = Lens.lens (\GlobalCluster' {engine} -> engine) (\s@GlobalCluster' {} a -> s {engine = a} :: GlobalCluster)

-- | The AWS Region-unique, immutable identifier for the global database
-- cluster. This identifier is found in AWS CloudTrail log entries whenever
-- the AWS KMS customer master key (CMK) for the DB cluster is accessed.
globalCluster_globalClusterResourceId :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Text)
globalCluster_globalClusterResourceId = Lens.lens (\GlobalCluster' {globalClusterResourceId} -> globalClusterResourceId) (\s@GlobalCluster' {} a -> s {globalClusterResourceId = a} :: GlobalCluster)

-- | The list of cluster IDs for secondary clusters within the global
-- database cluster. Currently limited to 1 item.
globalCluster_globalClusterMembers :: Lens.Lens' GlobalCluster (Prelude.Maybe [GlobalClusterMember])
globalCluster_globalClusterMembers = Lens.lens (\GlobalCluster' {globalClusterMembers} -> globalClusterMembers) (\s@GlobalCluster' {} a -> s {globalClusterMembers = a} :: GlobalCluster) Prelude.. Lens.mapping Prelude._Coerce

-- | Contains a user-supplied global database cluster identifier. This
-- identifier is the unique key that identifies a global database cluster.
globalCluster_globalClusterIdentifier :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Text)
globalCluster_globalClusterIdentifier = Lens.lens (\GlobalCluster' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@GlobalCluster' {} a -> s {globalClusterIdentifier = a} :: GlobalCluster)

-- | The default database name within the new global database cluster.
globalCluster_databaseName :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Text)
globalCluster_databaseName = Lens.lens (\GlobalCluster' {databaseName} -> databaseName) (\s@GlobalCluster' {} a -> s {databaseName = a} :: GlobalCluster)

instance Prelude.FromXML GlobalCluster where
  parseXML x =
    GlobalCluster'
      Prelude.<$> (x Prelude..@? "DeletionProtection")
      Prelude.<*> (x Prelude..@? "StorageEncrypted")
      Prelude.<*> (x Prelude..@? "Status")
      Prelude.<*> (x Prelude..@? "FailoverState")
      Prelude.<*> (x Prelude..@? "EngineVersion")
      Prelude.<*> (x Prelude..@? "GlobalClusterArn")
      Prelude.<*> (x Prelude..@? "Engine")
      Prelude.<*> (x Prelude..@? "GlobalClusterResourceId")
      Prelude.<*> ( x Prelude..@? "GlobalClusterMembers"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        (Prelude.parseXMLList "GlobalClusterMember")
                  )
      Prelude.<*> (x Prelude..@? "GlobalClusterIdentifier")
      Prelude.<*> (x Prelude..@? "DatabaseName")

instance Prelude.Hashable GlobalCluster

instance Prelude.NFData GlobalCluster
