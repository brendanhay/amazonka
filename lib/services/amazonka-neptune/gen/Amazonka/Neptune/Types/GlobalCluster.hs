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
-- Module      : Amazonka.Neptune.Types.GlobalCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Neptune.Types.GlobalCluster where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types.GlobalClusterMember
import qualified Amazonka.Prelude as Prelude

-- | Contains the details of an Amazon Neptune global database.
--
-- This data type is used as a response element for the
-- CreateGlobalCluster, DescribeGlobalClusters, ModifyGlobalCluster,
-- DeleteGlobalCluster, FailoverGlobalCluster, and RemoveFromGlobalCluster
-- actions.
--
-- /See:/ 'newGlobalCluster' smart constructor.
data GlobalCluster = GlobalCluster'
  { -- | The deletion protection setting for the global database.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The Neptune database engine used by the global database (@\"neptune\"@).
    engine :: Prelude.Maybe Prelude.Text,
    -- | The Neptune engine version used by the global database.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the global database.
    globalClusterArn :: Prelude.Maybe Prelude.Text,
    -- | Contains a user-supplied global database cluster identifier. This
    -- identifier is the unique key that identifies a global database.
    globalClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A list of cluster ARNs and instance ARNs for all the DB clusters that
    -- are part of the global database.
    globalClusterMembers :: Prelude.Maybe [GlobalClusterMember],
    -- | An immutable identifier for the global database that is unique within in
    -- all regions. This identifier is found in CloudTrail log entries whenever
    -- the KMS key for the DB cluster is accessed.
    globalClusterResourceId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the current state of this global database.
    status :: Prelude.Maybe Prelude.Text,
    -- | The storage encryption setting for the global database.
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
-- 'deletionProtection', 'globalCluster_deletionProtection' - The deletion protection setting for the global database.
--
-- 'engine', 'globalCluster_engine' - The Neptune database engine used by the global database (@\"neptune\"@).
--
-- 'engineVersion', 'globalCluster_engineVersion' - The Neptune engine version used by the global database.
--
-- 'globalClusterArn', 'globalCluster_globalClusterArn' - The Amazon Resource Name (ARN) for the global database.
--
-- 'globalClusterIdentifier', 'globalCluster_globalClusterIdentifier' - Contains a user-supplied global database cluster identifier. This
-- identifier is the unique key that identifies a global database.
--
-- 'globalClusterMembers', 'globalCluster_globalClusterMembers' - A list of cluster ARNs and instance ARNs for all the DB clusters that
-- are part of the global database.
--
-- 'globalClusterResourceId', 'globalCluster_globalClusterResourceId' - An immutable identifier for the global database that is unique within in
-- all regions. This identifier is found in CloudTrail log entries whenever
-- the KMS key for the DB cluster is accessed.
--
-- 'status', 'globalCluster_status' - Specifies the current state of this global database.
--
-- 'storageEncrypted', 'globalCluster_storageEncrypted' - The storage encryption setting for the global database.
newGlobalCluster ::
  GlobalCluster
newGlobalCluster =
  GlobalCluster'
    { deletionProtection =
        Prelude.Nothing,
      engine = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      globalClusterArn = Prelude.Nothing,
      globalClusterIdentifier = Prelude.Nothing,
      globalClusterMembers = Prelude.Nothing,
      globalClusterResourceId = Prelude.Nothing,
      status = Prelude.Nothing,
      storageEncrypted = Prelude.Nothing
    }

-- | The deletion protection setting for the global database.
globalCluster_deletionProtection :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Bool)
globalCluster_deletionProtection = Lens.lens (\GlobalCluster' {deletionProtection} -> deletionProtection) (\s@GlobalCluster' {} a -> s {deletionProtection = a} :: GlobalCluster)

-- | The Neptune database engine used by the global database (@\"neptune\"@).
globalCluster_engine :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Text)
globalCluster_engine = Lens.lens (\GlobalCluster' {engine} -> engine) (\s@GlobalCluster' {} a -> s {engine = a} :: GlobalCluster)

-- | The Neptune engine version used by the global database.
globalCluster_engineVersion :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Text)
globalCluster_engineVersion = Lens.lens (\GlobalCluster' {engineVersion} -> engineVersion) (\s@GlobalCluster' {} a -> s {engineVersion = a} :: GlobalCluster)

-- | The Amazon Resource Name (ARN) for the global database.
globalCluster_globalClusterArn :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Text)
globalCluster_globalClusterArn = Lens.lens (\GlobalCluster' {globalClusterArn} -> globalClusterArn) (\s@GlobalCluster' {} a -> s {globalClusterArn = a} :: GlobalCluster)

-- | Contains a user-supplied global database cluster identifier. This
-- identifier is the unique key that identifies a global database.
globalCluster_globalClusterIdentifier :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Text)
globalCluster_globalClusterIdentifier = Lens.lens (\GlobalCluster' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@GlobalCluster' {} a -> s {globalClusterIdentifier = a} :: GlobalCluster)

-- | A list of cluster ARNs and instance ARNs for all the DB clusters that
-- are part of the global database.
globalCluster_globalClusterMembers :: Lens.Lens' GlobalCluster (Prelude.Maybe [GlobalClusterMember])
globalCluster_globalClusterMembers = Lens.lens (\GlobalCluster' {globalClusterMembers} -> globalClusterMembers) (\s@GlobalCluster' {} a -> s {globalClusterMembers = a} :: GlobalCluster) Prelude.. Lens.mapping Lens.coerced

-- | An immutable identifier for the global database that is unique within in
-- all regions. This identifier is found in CloudTrail log entries whenever
-- the KMS key for the DB cluster is accessed.
globalCluster_globalClusterResourceId :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Text)
globalCluster_globalClusterResourceId = Lens.lens (\GlobalCluster' {globalClusterResourceId} -> globalClusterResourceId) (\s@GlobalCluster' {} a -> s {globalClusterResourceId = a} :: GlobalCluster)

-- | Specifies the current state of this global database.
globalCluster_status :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Text)
globalCluster_status = Lens.lens (\GlobalCluster' {status} -> status) (\s@GlobalCluster' {} a -> s {status = a} :: GlobalCluster)

-- | The storage encryption setting for the global database.
globalCluster_storageEncrypted :: Lens.Lens' GlobalCluster (Prelude.Maybe Prelude.Bool)
globalCluster_storageEncrypted = Lens.lens (\GlobalCluster' {storageEncrypted} -> storageEncrypted) (\s@GlobalCluster' {} a -> s {storageEncrypted = a} :: GlobalCluster)

instance Data.FromXML GlobalCluster where
  parseXML x =
    GlobalCluster'
      Prelude.<$> (x Data..@? "DeletionProtection")
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
    Prelude.rnf deletionProtection `Prelude.seq`
      Prelude.rnf engine `Prelude.seq`
        Prelude.rnf engineVersion `Prelude.seq`
          Prelude.rnf globalClusterArn `Prelude.seq`
            Prelude.rnf globalClusterIdentifier `Prelude.seq`
              Prelude.rnf globalClusterMembers `Prelude.seq`
                Prelude.rnf globalClusterResourceId `Prelude.seq`
                  Prelude.rnf status `Prelude.seq`
                    Prelude.rnf storageEncrypted
