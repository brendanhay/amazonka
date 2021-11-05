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
-- Module      : Network.AWS.DocumentDB.CreateGlobalCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon DocumentDB global cluster that can span multiple
-- multiple Regions. The global cluster contains one primary cluster with
-- read-write capability, and up-to give read-only secondary clusters.
-- Global clusters uses storage-based fast replication across regions with
-- latencies less than one second, using dedicated infrastructure with no
-- impact to your workload’s performance.
--
-- You can create a global cluster that is initially empty, and then add a
-- primary and a secondary to it. Or you can specify an existing cluster
-- during the create operation, and this cluster becomes the primary of the
-- global cluster.
--
-- This action only applies to Amazon DocumentDB clusters.
module Network.AWS.DocumentDB.CreateGlobalCluster
  ( -- * Creating a Request
    CreateGlobalCluster (..),
    newCreateGlobalCluster,

    -- * Request Lenses
    createGlobalCluster_engineVersion,
    createGlobalCluster_deletionProtection,
    createGlobalCluster_storageEncrypted,
    createGlobalCluster_sourceDBClusterIdentifier,
    createGlobalCluster_engine,
    createGlobalCluster_databaseName,
    createGlobalCluster_globalClusterIdentifier,

    -- * Destructuring the Response
    CreateGlobalClusterResponse (..),
    newCreateGlobalClusterResponse,

    -- * Response Lenses
    createGlobalClusterResponse_globalCluster,
    createGlobalClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DocumentDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input to CreateGlobalCluster.
--
-- /See:/ 'newCreateGlobalCluster' smart constructor.
data CreateGlobalCluster = CreateGlobalCluster'
  { -- | The engine version of the global cluster.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The deletion protection setting for the new global cluster. The global
    -- cluster can\'t be deleted when deletion protection is enabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The storage encryption setting for the new global cluster.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) to use as the primary cluster of the
    -- global cluster. This parameter is optional.
    sourceDBClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the database engine to be used for this cluster.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The name for your database of up to 64 alpha-numeric characters. If you
    -- do not provide a name, Amazon DocumentDB will not create a database in
    -- the global cluster you are creating.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The cluster identifier of the new global cluster.
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
-- 'engineVersion', 'createGlobalCluster_engineVersion' - The engine version of the global cluster.
--
-- 'deletionProtection', 'createGlobalCluster_deletionProtection' - The deletion protection setting for the new global cluster. The global
-- cluster can\'t be deleted when deletion protection is enabled.
--
-- 'storageEncrypted', 'createGlobalCluster_storageEncrypted' - The storage encryption setting for the new global cluster.
--
-- 'sourceDBClusterIdentifier', 'createGlobalCluster_sourceDBClusterIdentifier' - The Amazon Resource Name (ARN) to use as the primary cluster of the
-- global cluster. This parameter is optional.
--
-- 'engine', 'createGlobalCluster_engine' - The name of the database engine to be used for this cluster.
--
-- 'databaseName', 'createGlobalCluster_databaseName' - The name for your database of up to 64 alpha-numeric characters. If you
-- do not provide a name, Amazon DocumentDB will not create a database in
-- the global cluster you are creating.
--
-- 'globalClusterIdentifier', 'createGlobalCluster_globalClusterIdentifier' - The cluster identifier of the new global cluster.
newCreateGlobalCluster ::
  -- | 'globalClusterIdentifier'
  Prelude.Text ->
  CreateGlobalCluster
newCreateGlobalCluster pGlobalClusterIdentifier_ =
  CreateGlobalCluster'
    { engineVersion =
        Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      storageEncrypted = Prelude.Nothing,
      sourceDBClusterIdentifier = Prelude.Nothing,
      engine = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      globalClusterIdentifier = pGlobalClusterIdentifier_
    }

-- | The engine version of the global cluster.
createGlobalCluster_engineVersion :: Lens.Lens' CreateGlobalCluster (Prelude.Maybe Prelude.Text)
createGlobalCluster_engineVersion = Lens.lens (\CreateGlobalCluster' {engineVersion} -> engineVersion) (\s@CreateGlobalCluster' {} a -> s {engineVersion = a} :: CreateGlobalCluster)

-- | The deletion protection setting for the new global cluster. The global
-- cluster can\'t be deleted when deletion protection is enabled.
createGlobalCluster_deletionProtection :: Lens.Lens' CreateGlobalCluster (Prelude.Maybe Prelude.Bool)
createGlobalCluster_deletionProtection = Lens.lens (\CreateGlobalCluster' {deletionProtection} -> deletionProtection) (\s@CreateGlobalCluster' {} a -> s {deletionProtection = a} :: CreateGlobalCluster)

-- | The storage encryption setting for the new global cluster.
createGlobalCluster_storageEncrypted :: Lens.Lens' CreateGlobalCluster (Prelude.Maybe Prelude.Bool)
createGlobalCluster_storageEncrypted = Lens.lens (\CreateGlobalCluster' {storageEncrypted} -> storageEncrypted) (\s@CreateGlobalCluster' {} a -> s {storageEncrypted = a} :: CreateGlobalCluster)

-- | The Amazon Resource Name (ARN) to use as the primary cluster of the
-- global cluster. This parameter is optional.
createGlobalCluster_sourceDBClusterIdentifier :: Lens.Lens' CreateGlobalCluster (Prelude.Maybe Prelude.Text)
createGlobalCluster_sourceDBClusterIdentifier = Lens.lens (\CreateGlobalCluster' {sourceDBClusterIdentifier} -> sourceDBClusterIdentifier) (\s@CreateGlobalCluster' {} a -> s {sourceDBClusterIdentifier = a} :: CreateGlobalCluster)

-- | The name of the database engine to be used for this cluster.
createGlobalCluster_engine :: Lens.Lens' CreateGlobalCluster (Prelude.Maybe Prelude.Text)
createGlobalCluster_engine = Lens.lens (\CreateGlobalCluster' {engine} -> engine) (\s@CreateGlobalCluster' {} a -> s {engine = a} :: CreateGlobalCluster)

-- | The name for your database of up to 64 alpha-numeric characters. If you
-- do not provide a name, Amazon DocumentDB will not create a database in
-- the global cluster you are creating.
createGlobalCluster_databaseName :: Lens.Lens' CreateGlobalCluster (Prelude.Maybe Prelude.Text)
createGlobalCluster_databaseName = Lens.lens (\CreateGlobalCluster' {databaseName} -> databaseName) (\s@CreateGlobalCluster' {} a -> s {databaseName = a} :: CreateGlobalCluster)

-- | The cluster identifier of the new global cluster.
createGlobalCluster_globalClusterIdentifier :: Lens.Lens' CreateGlobalCluster Prelude.Text
createGlobalCluster_globalClusterIdentifier = Lens.lens (\CreateGlobalCluster' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@CreateGlobalCluster' {} a -> s {globalClusterIdentifier = a} :: CreateGlobalCluster)

instance Core.AWSRequest CreateGlobalCluster where
  type
    AWSResponse CreateGlobalCluster =
      CreateGlobalClusterResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateGlobalClusterResult"
      ( \s h x ->
          CreateGlobalClusterResponse'
            Prelude.<$> (x Core..@? "GlobalCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGlobalCluster

instance Prelude.NFData CreateGlobalCluster

instance Core.ToHeaders CreateGlobalCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateGlobalCluster where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateGlobalCluster where
  toQuery CreateGlobalCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateGlobalCluster" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "EngineVersion" Core.=: engineVersion,
        "DeletionProtection" Core.=: deletionProtection,
        "StorageEncrypted" Core.=: storageEncrypted,
        "SourceDBClusterIdentifier"
          Core.=: sourceDBClusterIdentifier,
        "Engine" Core.=: engine,
        "DatabaseName" Core.=: databaseName,
        "GlobalClusterIdentifier"
          Core.=: globalClusterIdentifier
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

instance Prelude.NFData CreateGlobalClusterResponse
