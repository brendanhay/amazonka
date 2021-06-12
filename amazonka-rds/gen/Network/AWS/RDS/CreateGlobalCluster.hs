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
-- Module      : Network.AWS.RDS.CreateGlobalCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Aurora global database spread across multiple AWS Regions.
-- The global database contains a single primary cluster with read-write
-- capability, and a read-only secondary cluster that receives data from
-- the primary cluster through high-speed replication performed by the
-- Aurora storage subsystem.
--
-- You can create a global database that is initially empty, and then add a
-- primary cluster and a secondary cluster to it. Or you can specify an
-- existing Aurora cluster during the create operation, and this cluster
-- becomes the primary cluster of the global database.
--
-- This action only applies to Aurora DB clusters.
module Network.AWS.RDS.CreateGlobalCluster
  ( -- * Creating a Request
    CreateGlobalCluster (..),
    newCreateGlobalCluster,

    -- * Request Lenses
    createGlobalCluster_deletionProtection,
    createGlobalCluster_storageEncrypted,
    createGlobalCluster_engineVersion,
    createGlobalCluster_engine,
    createGlobalCluster_globalClusterIdentifier,
    createGlobalCluster_databaseName,
    createGlobalCluster_sourceDBClusterIdentifier,

    -- * Destructuring the Response
    CreateGlobalClusterResponse (..),
    newCreateGlobalClusterResponse,

    -- * Response Lenses
    createGlobalClusterResponse_globalCluster,
    createGlobalClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateGlobalCluster' smart constructor.
data CreateGlobalCluster = CreateGlobalCluster'
  { -- | The deletion protection setting for the new global database. The global
    -- database can\'t be deleted when deletion protection is enabled.
    deletionProtection :: Core.Maybe Core.Bool,
    -- | The storage encryption setting for the new global database cluster.
    storageEncrypted :: Core.Maybe Core.Bool,
    -- | The engine version of the Aurora global database.
    engineVersion :: Core.Maybe Core.Text,
    -- | The name of the database engine to be used for this DB cluster.
    engine :: Core.Maybe Core.Text,
    -- | The cluster identifier of the new global database cluster.
    globalClusterIdentifier :: Core.Maybe Core.Text,
    -- | The name for your database of up to 64 alpha-numeric characters. If you
    -- do not provide a name, Amazon Aurora will not create a database in the
    -- global database cluster you are creating.
    databaseName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) to use as the primary cluster of the
    -- global database. This parameter is optional.
    sourceDBClusterIdentifier :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'storageEncrypted', 'createGlobalCluster_storageEncrypted' - The storage encryption setting for the new global database cluster.
--
-- 'engineVersion', 'createGlobalCluster_engineVersion' - The engine version of the Aurora global database.
--
-- 'engine', 'createGlobalCluster_engine' - The name of the database engine to be used for this DB cluster.
--
-- 'globalClusterIdentifier', 'createGlobalCluster_globalClusterIdentifier' - The cluster identifier of the new global database cluster.
--
-- 'databaseName', 'createGlobalCluster_databaseName' - The name for your database of up to 64 alpha-numeric characters. If you
-- do not provide a name, Amazon Aurora will not create a database in the
-- global database cluster you are creating.
--
-- 'sourceDBClusterIdentifier', 'createGlobalCluster_sourceDBClusterIdentifier' - The Amazon Resource Name (ARN) to use as the primary cluster of the
-- global database. This parameter is optional.
newCreateGlobalCluster ::
  CreateGlobalCluster
newCreateGlobalCluster =
  CreateGlobalCluster'
    { deletionProtection =
        Core.Nothing,
      storageEncrypted = Core.Nothing,
      engineVersion = Core.Nothing,
      engine = Core.Nothing,
      globalClusterIdentifier = Core.Nothing,
      databaseName = Core.Nothing,
      sourceDBClusterIdentifier = Core.Nothing
    }

-- | The deletion protection setting for the new global database. The global
-- database can\'t be deleted when deletion protection is enabled.
createGlobalCluster_deletionProtection :: Lens.Lens' CreateGlobalCluster (Core.Maybe Core.Bool)
createGlobalCluster_deletionProtection = Lens.lens (\CreateGlobalCluster' {deletionProtection} -> deletionProtection) (\s@CreateGlobalCluster' {} a -> s {deletionProtection = a} :: CreateGlobalCluster)

-- | The storage encryption setting for the new global database cluster.
createGlobalCluster_storageEncrypted :: Lens.Lens' CreateGlobalCluster (Core.Maybe Core.Bool)
createGlobalCluster_storageEncrypted = Lens.lens (\CreateGlobalCluster' {storageEncrypted} -> storageEncrypted) (\s@CreateGlobalCluster' {} a -> s {storageEncrypted = a} :: CreateGlobalCluster)

-- | The engine version of the Aurora global database.
createGlobalCluster_engineVersion :: Lens.Lens' CreateGlobalCluster (Core.Maybe Core.Text)
createGlobalCluster_engineVersion = Lens.lens (\CreateGlobalCluster' {engineVersion} -> engineVersion) (\s@CreateGlobalCluster' {} a -> s {engineVersion = a} :: CreateGlobalCluster)

-- | The name of the database engine to be used for this DB cluster.
createGlobalCluster_engine :: Lens.Lens' CreateGlobalCluster (Core.Maybe Core.Text)
createGlobalCluster_engine = Lens.lens (\CreateGlobalCluster' {engine} -> engine) (\s@CreateGlobalCluster' {} a -> s {engine = a} :: CreateGlobalCluster)

-- | The cluster identifier of the new global database cluster.
createGlobalCluster_globalClusterIdentifier :: Lens.Lens' CreateGlobalCluster (Core.Maybe Core.Text)
createGlobalCluster_globalClusterIdentifier = Lens.lens (\CreateGlobalCluster' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@CreateGlobalCluster' {} a -> s {globalClusterIdentifier = a} :: CreateGlobalCluster)

-- | The name for your database of up to 64 alpha-numeric characters. If you
-- do not provide a name, Amazon Aurora will not create a database in the
-- global database cluster you are creating.
createGlobalCluster_databaseName :: Lens.Lens' CreateGlobalCluster (Core.Maybe Core.Text)
createGlobalCluster_databaseName = Lens.lens (\CreateGlobalCluster' {databaseName} -> databaseName) (\s@CreateGlobalCluster' {} a -> s {databaseName = a} :: CreateGlobalCluster)

-- | The Amazon Resource Name (ARN) to use as the primary cluster of the
-- global database. This parameter is optional.
createGlobalCluster_sourceDBClusterIdentifier :: Lens.Lens' CreateGlobalCluster (Core.Maybe Core.Text)
createGlobalCluster_sourceDBClusterIdentifier = Lens.lens (\CreateGlobalCluster' {sourceDBClusterIdentifier} -> sourceDBClusterIdentifier) (\s@CreateGlobalCluster' {} a -> s {sourceDBClusterIdentifier = a} :: CreateGlobalCluster)

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
            Core.<$> (x Core..@? "GlobalCluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateGlobalCluster

instance Core.NFData CreateGlobalCluster

instance Core.ToHeaders CreateGlobalCluster where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateGlobalCluster where
  toPath = Core.const "/"

instance Core.ToQuery CreateGlobalCluster where
  toQuery CreateGlobalCluster' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateGlobalCluster" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DeletionProtection" Core.=: deletionProtection,
        "StorageEncrypted" Core.=: storageEncrypted,
        "EngineVersion" Core.=: engineVersion,
        "Engine" Core.=: engine,
        "GlobalClusterIdentifier"
          Core.=: globalClusterIdentifier,
        "DatabaseName" Core.=: databaseName,
        "SourceDBClusterIdentifier"
          Core.=: sourceDBClusterIdentifier
      ]

-- | /See:/ 'newCreateGlobalClusterResponse' smart constructor.
data CreateGlobalClusterResponse = CreateGlobalClusterResponse'
  { globalCluster :: Core.Maybe GlobalCluster,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateGlobalClusterResponse
newCreateGlobalClusterResponse pHttpStatus_ =
  CreateGlobalClusterResponse'
    { globalCluster =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createGlobalClusterResponse_globalCluster :: Lens.Lens' CreateGlobalClusterResponse (Core.Maybe GlobalCluster)
createGlobalClusterResponse_globalCluster = Lens.lens (\CreateGlobalClusterResponse' {globalCluster} -> globalCluster) (\s@CreateGlobalClusterResponse' {} a -> s {globalCluster = a} :: CreateGlobalClusterResponse)

-- | The response's http status code.
createGlobalClusterResponse_httpStatus :: Lens.Lens' CreateGlobalClusterResponse Core.Int
createGlobalClusterResponse_httpStatus = Lens.lens (\CreateGlobalClusterResponse' {httpStatus} -> httpStatus) (\s@CreateGlobalClusterResponse' {} a -> s {httpStatus = a} :: CreateGlobalClusterResponse)

instance Core.NFData CreateGlobalClusterResponse
