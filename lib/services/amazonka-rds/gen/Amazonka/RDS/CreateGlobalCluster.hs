{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RDS.CreateGlobalCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Aurora global database spread across multiple Amazon Web
-- Services Regions. The global database contains a single primary cluster
-- with read-write capability, and a read-only secondary cluster that
-- receives data from the primary cluster through high-speed replication
-- performed by the Aurora storage subsystem.
--
-- You can create a global database that is initially empty, and then add a
-- primary cluster and a secondary cluster to it. Or you can specify an
-- existing Aurora cluster during the create operation, and this cluster
-- becomes the primary cluster of the global database.
--
-- This action applies only to Aurora DB clusters.
module Amazonka.RDS.CreateGlobalCluster
  ( -- * Creating a Request
    CreateGlobalCluster (..),
    newCreateGlobalCluster,

    -- * Request Lenses
    createGlobalCluster_databaseName,
    createGlobalCluster_deletionProtection,
    createGlobalCluster_engine,
    createGlobalCluster_engineVersion,
    createGlobalCluster_globalClusterIdentifier,
    createGlobalCluster_sourceDBClusterIdentifier,
    createGlobalCluster_storageEncrypted,

    -- * Destructuring the Response
    CreateGlobalClusterResponse (..),
    newCreateGlobalClusterResponse,

    -- * Response Lenses
    createGlobalClusterResponse_globalCluster,
    createGlobalClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateGlobalCluster' smart constructor.
data CreateGlobalCluster = CreateGlobalCluster'
  { -- | The name for your database of up to 64 alphanumeric characters. If you
    -- do not provide a name, Amazon Aurora will not create a database in the
    -- global database cluster you are creating.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The deletion protection setting for the new global database. The global
    -- database can\'t be deleted when deletion protection is enabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The name of the database engine to be used for this DB cluster.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The engine version of the Aurora global database.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The cluster identifier of the new global database cluster.
    globalClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) to use as the primary cluster of the
    -- global database. This parameter is optional.
    sourceDBClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The storage encryption setting for the new global database cluster.
    storageEncrypted :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGlobalCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseName', 'createGlobalCluster_databaseName' - The name for your database of up to 64 alphanumeric characters. If you
-- do not provide a name, Amazon Aurora will not create a database in the
-- global database cluster you are creating.
--
-- 'deletionProtection', 'createGlobalCluster_deletionProtection' - The deletion protection setting for the new global database. The global
-- database can\'t be deleted when deletion protection is enabled.
--
-- 'engine', 'createGlobalCluster_engine' - The name of the database engine to be used for this DB cluster.
--
-- 'engineVersion', 'createGlobalCluster_engineVersion' - The engine version of the Aurora global database.
--
-- 'globalClusterIdentifier', 'createGlobalCluster_globalClusterIdentifier' - The cluster identifier of the new global database cluster.
--
-- 'sourceDBClusterIdentifier', 'createGlobalCluster_sourceDBClusterIdentifier' - The Amazon Resource Name (ARN) to use as the primary cluster of the
-- global database. This parameter is optional.
--
-- 'storageEncrypted', 'createGlobalCluster_storageEncrypted' - The storage encryption setting for the new global database cluster.
newCreateGlobalCluster ::
  CreateGlobalCluster
newCreateGlobalCluster =
  CreateGlobalCluster'
    { databaseName =
        Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      engine = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      globalClusterIdentifier = Prelude.Nothing,
      sourceDBClusterIdentifier = Prelude.Nothing,
      storageEncrypted = Prelude.Nothing
    }

-- | The name for your database of up to 64 alphanumeric characters. If you
-- do not provide a name, Amazon Aurora will not create a database in the
-- global database cluster you are creating.
createGlobalCluster_databaseName :: Lens.Lens' CreateGlobalCluster (Prelude.Maybe Prelude.Text)
createGlobalCluster_databaseName = Lens.lens (\CreateGlobalCluster' {databaseName} -> databaseName) (\s@CreateGlobalCluster' {} a -> s {databaseName = a} :: CreateGlobalCluster)

-- | The deletion protection setting for the new global database. The global
-- database can\'t be deleted when deletion protection is enabled.
createGlobalCluster_deletionProtection :: Lens.Lens' CreateGlobalCluster (Prelude.Maybe Prelude.Bool)
createGlobalCluster_deletionProtection = Lens.lens (\CreateGlobalCluster' {deletionProtection} -> deletionProtection) (\s@CreateGlobalCluster' {} a -> s {deletionProtection = a} :: CreateGlobalCluster)

-- | The name of the database engine to be used for this DB cluster.
createGlobalCluster_engine :: Lens.Lens' CreateGlobalCluster (Prelude.Maybe Prelude.Text)
createGlobalCluster_engine = Lens.lens (\CreateGlobalCluster' {engine} -> engine) (\s@CreateGlobalCluster' {} a -> s {engine = a} :: CreateGlobalCluster)

-- | The engine version of the Aurora global database.
createGlobalCluster_engineVersion :: Lens.Lens' CreateGlobalCluster (Prelude.Maybe Prelude.Text)
createGlobalCluster_engineVersion = Lens.lens (\CreateGlobalCluster' {engineVersion} -> engineVersion) (\s@CreateGlobalCluster' {} a -> s {engineVersion = a} :: CreateGlobalCluster)

-- | The cluster identifier of the new global database cluster.
createGlobalCluster_globalClusterIdentifier :: Lens.Lens' CreateGlobalCluster (Prelude.Maybe Prelude.Text)
createGlobalCluster_globalClusterIdentifier = Lens.lens (\CreateGlobalCluster' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@CreateGlobalCluster' {} a -> s {globalClusterIdentifier = a} :: CreateGlobalCluster)

-- | The Amazon Resource Name (ARN) to use as the primary cluster of the
-- global database. This parameter is optional.
createGlobalCluster_sourceDBClusterIdentifier :: Lens.Lens' CreateGlobalCluster (Prelude.Maybe Prelude.Text)
createGlobalCluster_sourceDBClusterIdentifier = Lens.lens (\CreateGlobalCluster' {sourceDBClusterIdentifier} -> sourceDBClusterIdentifier) (\s@CreateGlobalCluster' {} a -> s {sourceDBClusterIdentifier = a} :: CreateGlobalCluster)

-- | The storage encryption setting for the new global database cluster.
createGlobalCluster_storageEncrypted :: Lens.Lens' CreateGlobalCluster (Prelude.Maybe Prelude.Bool)
createGlobalCluster_storageEncrypted = Lens.lens (\CreateGlobalCluster' {storageEncrypted} -> storageEncrypted) (\s@CreateGlobalCluster' {} a -> s {storageEncrypted = a} :: CreateGlobalCluster)

instance Core.AWSRequest CreateGlobalCluster where
  type
    AWSResponse CreateGlobalCluster =
      CreateGlobalClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateGlobalClusterResult"
      ( \s h x ->
          CreateGlobalClusterResponse'
            Prelude.<$> (x Data..@? "GlobalCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGlobalCluster where
  hashWithSalt _salt CreateGlobalCluster' {..} =
    _salt `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` globalClusterIdentifier
      `Prelude.hashWithSalt` sourceDBClusterIdentifier
      `Prelude.hashWithSalt` storageEncrypted

instance Prelude.NFData CreateGlobalCluster where
  rnf CreateGlobalCluster' {..} =
    Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf globalClusterIdentifier
      `Prelude.seq` Prelude.rnf sourceDBClusterIdentifier
      `Prelude.seq` Prelude.rnf storageEncrypted

instance Data.ToHeaders CreateGlobalCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateGlobalCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateGlobalCluster where
  toQuery CreateGlobalCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateGlobalCluster" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DatabaseName" Data.=: databaseName,
        "DeletionProtection" Data.=: deletionProtection,
        "Engine" Data.=: engine,
        "EngineVersion" Data.=: engineVersion,
        "GlobalClusterIdentifier"
          Data.=: globalClusterIdentifier,
        "SourceDBClusterIdentifier"
          Data.=: sourceDBClusterIdentifier,
        "StorageEncrypted" Data.=: storageEncrypted
      ]

-- | /See:/ 'newCreateGlobalClusterResponse' smart constructor.
data CreateGlobalClusterResponse = CreateGlobalClusterResponse'
  { globalCluster :: Prelude.Maybe GlobalCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGlobalClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalCluster', 'createGlobalClusterResponse_globalCluster' - Undocumented member.
--
-- 'httpStatus', 'createGlobalClusterResponse_httpStatus' - The response's http status code.
newCreateGlobalClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateGlobalClusterResponse
newCreateGlobalClusterResponse pHttpStatus_ =
  CreateGlobalClusterResponse'
    { globalCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createGlobalClusterResponse_globalCluster :: Lens.Lens' CreateGlobalClusterResponse (Prelude.Maybe GlobalCluster)
createGlobalClusterResponse_globalCluster = Lens.lens (\CreateGlobalClusterResponse' {globalCluster} -> globalCluster) (\s@CreateGlobalClusterResponse' {} a -> s {globalCluster = a} :: CreateGlobalClusterResponse)

-- | The response's http status code.
createGlobalClusterResponse_httpStatus :: Lens.Lens' CreateGlobalClusterResponse Prelude.Int
createGlobalClusterResponse_httpStatus = Lens.lens (\CreateGlobalClusterResponse' {httpStatus} -> httpStatus) (\s@CreateGlobalClusterResponse' {} a -> s {httpStatus = a} :: CreateGlobalClusterResponse)

instance Prelude.NFData CreateGlobalClusterResponse where
  rnf CreateGlobalClusterResponse' {..} =
    Prelude.rnf globalCluster
      `Prelude.seq` Prelude.rnf httpStatus
