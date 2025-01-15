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
-- Module      : Amazonka.DocumentDB.Types.GlobalCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocumentDB.Types.GlobalCluster where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocumentDB.Types.GlobalClusterMember
import qualified Amazonka.Prelude as Prelude

-- | A data type representing an Amazon DocumentDB global cluster.
--
-- /See:/ 'newGlobalCluster' smart constructor.
data GlobalCluster = GlobalCluster'
  { -- | The default database name within the new global cluster.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The deletion protection setting for the new global cluster.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon DocumentDB database engine used by the global cluster.
    engine :: Prelude.Maybe Prelude.Text,
    -- | Indicates the database engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the global cluster.
    globalClusterArn :: Prelude.Maybe Prelude.Text,
    -- | Contains a user-supplied global cluster identifier. This identifier is
    -- the unique key that identifies a global cluster.
    globalClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The list of cluster IDs for secondary clusters within the global
    -- cluster. Currently limited to one item.
    globalClusterMembers :: Prelude.Maybe [GlobalClusterMember],
    -- | The Amazon Web Services Region-unique, immutable identifier for the
    -- global database cluster. This identifier is found in AWS CloudTrail log
    -- entries whenever the AWS KMS customer master key (CMK) for the cluster
    -- is accessed.
    globalClusterResourceId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the current state of this global cluster.
    status :: Prelude.Maybe Prelude.Text,
    -- | The storage encryption setting for the global cluster.
    storageEncrypted :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlobalCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseName', 'globalCluster_databaseName' - The default database name within the new global cluster.
--
-- 'deletionProtection', 'globalCluster_deletionProtection' - The deletion protection setting for the new global cluster.
--
-- 'engine', 'globalCluster_engine' - The Amazon DocumentDB database engine used by the global cluster.
--
-- 'engineVersion', 'globalCluster_engineVersion' - Indicates the database engine version.
--
-- 'globalClusterArn', 'globalCluster_globalClusterArn' - The Amazon Resource Name (ARN) for the global cluster.
--
-- 'globalClusterIdentifier', 'globalCluster_globalClusterIdentifier' - Contains a user-supplied global cluster identifier. This identifier is
-- the unique key that identifies a global cluster.
--
-- 'globalClusterMembers', 'globalCluster_globalClusterMembers' - The list of cluster IDs for secondary clusters within the global
-- cluster. Currently limited to one item.
--
-- 'globalClusterResourceId', 'globalCluster_globalClusterResourceId' - The Amazon Web Services Region-unique, immutable identifier for the
-- global database cluster. This identifier is found in AWS CloudTrail log
-- entries whenever the AWS KMS customer master key (CMK) for the cluster
-- is accessed.
--
-- 'status', 'globalCluster_status' - Specifies the current state of this global cluster.
--
-- 'storageEncrypted', 'globalCluster_storageEncrypted' - The storage encryption setting for the global cluster.
newGlobalCluster ::
  GlobalCluster
newGlobalCluster =
  GlobalCluster'
    { databaseName = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      engine = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      globalClusterArn = Prelude.Nothing,
      globalClusterIdentifier = Prelude.Nothing,
      globalClusterMembers = Prelude.Nothing,
      globalClusterResourceId = Prelude.Nothing,
      status = Prelude.Nothing,
      storageEncrypted = Prelude.Nothing
    }

-- | The default database name within the new global cluster.
globalCluster_databaseName :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Text)
globalCluster_databaseName = Lens.lens (\GlobalCluster' {databaseName} -> databaseName) (\s@GlobalCluster' {} a -> s {databaseName = a} :: GlobalCluster)

-- | The deletion protection setting for the new global cluster.
globalCluster_deletionProtection :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Bool)
globalCluster_deletionProtection = Lens.lens (\GlobalCluster' {deletionProtection} -> deletionProtection) (\s@GlobalCluster' {} a -> s {deletionProtection = a} :: GlobalCluster)

-- | The Amazon DocumentDB database engine used by the global cluster.
globalCluster_engine :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Text)
globalCluster_engine = Lens.lens (\GlobalCluster' {engine} -> engine) (\s@GlobalCluster' {} a -> s {engine = a} :: GlobalCluster)

-- | Indicates the database engine version.
globalCluster_engineVersion :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Text)
globalCluster_engineVersion = Lens.lens (\GlobalCluster' {engineVersion} -> engineVersion) (\s@GlobalCluster' {} a -> s {engineVersion = a} :: GlobalCluster)

-- | The Amazon Resource Name (ARN) for the global cluster.
globalCluster_globalClusterArn :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Text)
globalCluster_globalClusterArn = Lens.lens (\GlobalCluster' {globalClusterArn} -> globalClusterArn) (\s@GlobalCluster' {} a -> s {globalClusterArn = a} :: GlobalCluster)

-- | Contains a user-supplied global cluster identifier. This identifier is
-- the unique key that identifies a global cluster.
globalCluster_globalClusterIdentifier :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Text)
globalCluster_globalClusterIdentifier = Lens.lens (\GlobalCluster' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@GlobalCluster' {} a -> s {globalClusterIdentifier = a} :: GlobalCluster)

-- | The list of cluster IDs for secondary clusters within the global
-- cluster. Currently limited to one item.
globalCluster_globalClusterMembers :: Lens.Lens' GlobalCluster (Prelude.Maybe [GlobalClusterMember])
globalCluster_globalClusterMembers = Lens.lens (\GlobalCluster' {globalClusterMembers} -> globalClusterMembers) (\s@GlobalCluster' {} a -> s {globalClusterMembers = a} :: GlobalCluster) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services Region-unique, immutable identifier for the
-- global database cluster. This identifier is found in AWS CloudTrail log
-- entries whenever the AWS KMS customer master key (CMK) for the cluster
-- is accessed.
globalCluster_globalClusterResourceId :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Text)
globalCluster_globalClusterResourceId = Lens.lens (\GlobalCluster' {globalClusterResourceId} -> globalClusterResourceId) (\s@GlobalCluster' {} a -> s {globalClusterResourceId = a} :: GlobalCluster)

-- | Specifies the current state of this global cluster.
globalCluster_status :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Text)
globalCluster_status = Lens.lens (\GlobalCluster' {status} -> status) (\s@GlobalCluster' {} a -> s {status = a} :: GlobalCluster)

-- | The storage encryption setting for the global cluster.
globalCluster_storageEncrypted :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Bool)
globalCluster_storageEncrypted = Lens.lens (\GlobalCluster' {storageEncrypted} -> storageEncrypted) (\s@GlobalCluster' {} a -> s {storageEncrypted = a} :: GlobalCluster)

instance Data.FromXML GlobalCluster where
  parseXML x =
    GlobalCluster'
      Prelude.<$> (x Data..@? "DatabaseName")
      Prelude.<*> (x Data..@? "DeletionProtection")
      Prelude.<*> (x Data..@? "Engine")
      Prelude.<*> (x Data..@? "EngineVersion")
      Prelude.<*> (x Data..@? "GlobalClusterArn")
      Prelude.<*> (x Data..@? "GlobalClusterIdentifier")
      Prelude.<*> ( x
                      Data..@? "GlobalClusterMembers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "GlobalClusterMember")
                  )
      Prelude.<*> (x Data..@? "GlobalClusterResourceId")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "StorageEncrypted")

instance Prelude.Hashable GlobalCluster where
  hashWithSalt _salt GlobalCluster' {..} =
    _salt
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` globalClusterArn
      `Prelude.hashWithSalt` globalClusterIdentifier
      `Prelude.hashWithSalt` globalClusterMembers
      `Prelude.hashWithSalt` globalClusterResourceId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` storageEncrypted

instance Prelude.NFData GlobalCluster where
  rnf GlobalCluster' {..} =
    Prelude.rnf databaseName `Prelude.seq`
      Prelude.rnf deletionProtection `Prelude.seq`
        Prelude.rnf engine `Prelude.seq`
          Prelude.rnf engineVersion `Prelude.seq`
            Prelude.rnf globalClusterArn `Prelude.seq`
              Prelude.rnf globalClusterIdentifier `Prelude.seq`
                Prelude.rnf globalClusterMembers `Prelude.seq`
                  Prelude.rnf globalClusterResourceId `Prelude.seq`
                    Prelude.rnf status `Prelude.seq`
                      Prelude.rnf storageEncrypted
