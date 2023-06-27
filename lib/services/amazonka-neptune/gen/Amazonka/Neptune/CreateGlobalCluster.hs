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
-- Module      : Amazonka.Neptune.CreateGlobalCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Neptune global database spread across multiple Amazon Regions.
-- The global database contains a single primary cluster with read-write
-- capability, and read-only secondary clusters that receive data from the
-- primary cluster through high-speed replication performed by the Neptune
-- storage subsystem.
--
-- You can create a global database that is initially empty, and then add a
-- primary cluster and secondary clusters to it, or you can specify an
-- existing Neptune cluster during the create operation to become the
-- primary cluster of the global database.
module Amazonka.Neptune.CreateGlobalCluster
  ( -- * Creating a Request
    CreateGlobalCluster (..),
    newCreateGlobalCluster,

    -- * Request Lenses
    createGlobalCluster_deletionProtection,
    createGlobalCluster_engine,
    createGlobalCluster_engineVersion,
    createGlobalCluster_sourceDBClusterIdentifier,
    createGlobalCluster_storageEncrypted,
    createGlobalCluster_globalClusterIdentifier,

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
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateGlobalCluster' smart constructor.
data CreateGlobalCluster = CreateGlobalCluster'
  { -- | The deletion protection setting for the new global database. The global
    -- database can\'t be deleted when deletion protection is enabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The name of the database engine to be used in the global database.
    --
    -- Valid values: @neptune@
    engine :: Prelude.Maybe Prelude.Text,
    -- | The Neptune engine version to be used by the global database.
    --
    -- Valid values: @1.2.0.0@ or above.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | (/Optional/) The Amazon Resource Name (ARN) of an existing Neptune DB
    -- cluster to use as the primary cluster of the new global database.
    sourceDBClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The storage encryption setting for the new global database cluster.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The cluster identifier of the new global database cluster.
    globalClusterIdentifier :: Prelude.Text
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
-- 'deletionProtection', 'createGlobalCluster_deletionProtection' - The deletion protection setting for the new global database. The global
-- database can\'t be deleted when deletion protection is enabled.
--
-- 'engine', 'createGlobalCluster_engine' - The name of the database engine to be used in the global database.
--
-- Valid values: @neptune@
--
-- 'engineVersion', 'createGlobalCluster_engineVersion' - The Neptune engine version to be used by the global database.
--
-- Valid values: @1.2.0.0@ or above.
--
-- 'sourceDBClusterIdentifier', 'createGlobalCluster_sourceDBClusterIdentifier' - (/Optional/) The Amazon Resource Name (ARN) of an existing Neptune DB
-- cluster to use as the primary cluster of the new global database.
--
-- 'storageEncrypted', 'createGlobalCluster_storageEncrypted' - The storage encryption setting for the new global database cluster.
--
-- 'globalClusterIdentifier', 'createGlobalCluster_globalClusterIdentifier' - The cluster identifier of the new global database cluster.
newCreateGlobalCluster ::
  -- | 'globalClusterIdentifier'
  Prelude.Text ->
  CreateGlobalCluster
newCreateGlobalCluster pGlobalClusterIdentifier_ =
  CreateGlobalCluster'
    { deletionProtection =
        Prelude.Nothing,
      engine = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      sourceDBClusterIdentifier = Prelude.Nothing,
      storageEncrypted = Prelude.Nothing,
      globalClusterIdentifier = pGlobalClusterIdentifier_
    }

-- | The deletion protection setting for the new global database. The global
-- database can\'t be deleted when deletion protection is enabled.
createGlobalCluster_deletionProtection :: Lens.Lens' CreateGlobalCluster (Prelude.Maybe Prelude.Bool)
createGlobalCluster_deletionProtection = Lens.lens (\CreateGlobalCluster' {deletionProtection} -> deletionProtection) (\s@CreateGlobalCluster' {} a -> s {deletionProtection = a} :: CreateGlobalCluster)

-- | The name of the database engine to be used in the global database.
--
-- Valid values: @neptune@
createGlobalCluster_engine :: Lens.Lens' CreateGlobalCluster (Prelude.Maybe Prelude.Text)
createGlobalCluster_engine = Lens.lens (\CreateGlobalCluster' {engine} -> engine) (\s@CreateGlobalCluster' {} a -> s {engine = a} :: CreateGlobalCluster)

-- | The Neptune engine version to be used by the global database.
--
-- Valid values: @1.2.0.0@ or above.
createGlobalCluster_engineVersion :: Lens.Lens' CreateGlobalCluster (Prelude.Maybe Prelude.Text)
createGlobalCluster_engineVersion = Lens.lens (\CreateGlobalCluster' {engineVersion} -> engineVersion) (\s@CreateGlobalCluster' {} a -> s {engineVersion = a} :: CreateGlobalCluster)

-- | (/Optional/) The Amazon Resource Name (ARN) of an existing Neptune DB
-- cluster to use as the primary cluster of the new global database.
createGlobalCluster_sourceDBClusterIdentifier :: Lens.Lens' CreateGlobalCluster (Prelude.Maybe Prelude.Text)
createGlobalCluster_sourceDBClusterIdentifier = Lens.lens (\CreateGlobalCluster' {sourceDBClusterIdentifier} -> sourceDBClusterIdentifier) (\s@CreateGlobalCluster' {} a -> s {sourceDBClusterIdentifier = a} :: CreateGlobalCluster)

-- | The storage encryption setting for the new global database cluster.
createGlobalCluster_storageEncrypted :: Lens.Lens' CreateGlobalCluster (Prelude.Maybe Prelude.Bool)
createGlobalCluster_storageEncrypted = Lens.lens (\CreateGlobalCluster' {storageEncrypted} -> storageEncrypted) (\s@CreateGlobalCluster' {} a -> s {storageEncrypted = a} :: CreateGlobalCluster)

-- | The cluster identifier of the new global database cluster.
createGlobalCluster_globalClusterIdentifier :: Lens.Lens' CreateGlobalCluster Prelude.Text
createGlobalCluster_globalClusterIdentifier = Lens.lens (\CreateGlobalCluster' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@CreateGlobalCluster' {} a -> s {globalClusterIdentifier = a} :: CreateGlobalCluster)

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
    _salt
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` sourceDBClusterIdentifier
      `Prelude.hashWithSalt` storageEncrypted
      `Prelude.hashWithSalt` globalClusterIdentifier

instance Prelude.NFData CreateGlobalCluster where
  rnf CreateGlobalCluster' {..} =
    Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf sourceDBClusterIdentifier
      `Prelude.seq` Prelude.rnf storageEncrypted
      `Prelude.seq` Prelude.rnf globalClusterIdentifier

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
        "DeletionProtection" Data.=: deletionProtection,
        "Engine" Data.=: engine,
        "EngineVersion" Data.=: engineVersion,
        "SourceDBClusterIdentifier"
          Data.=: sourceDBClusterIdentifier,
        "StorageEncrypted" Data.=: storageEncrypted,
        "GlobalClusterIdentifier"
          Data.=: globalClusterIdentifier
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
