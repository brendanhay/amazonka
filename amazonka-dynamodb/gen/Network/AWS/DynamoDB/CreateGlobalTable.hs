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
-- Module      : Network.AWS.DynamoDB.CreateGlobalTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a global table from an existing table. A global table creates a
-- replication relationship between two or more DynamoDB tables with the
-- same table name in the provided Regions.
--
-- This operation only applies to
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/globaltables.V1.html Version 2017.11.29>
-- of global tables.
--
-- If you want to add a new replica table to a global table, each of the
-- following conditions must be true:
--
-- -   The table must have the same primary key as all of the other
--     replicas.
--
-- -   The table must have the same name as all of the other replicas.
--
-- -   The table must have DynamoDB Streams enabled, with the stream
--     containing both the new and the old images of the item.
--
-- -   None of the replica tables in the global table can contain any data.
--
-- If global secondary indexes are specified, then the following conditions
-- must also be met:
--
-- -   The global secondary indexes must have the same name.
--
-- -   The global secondary indexes must have the same hash key and sort
--     key (if present).
--
-- If local secondary indexes are specified, then the following conditions
-- must also be met:
--
-- -   The local secondary indexes must have the same name.
--
-- -   The local secondary indexes must have the same hash key and sort key
--     (if present).
--
-- Write capacity settings should be set consistently across your replica
-- tables and secondary indexes. DynamoDB strongly recommends enabling auto
-- scaling to manage the write capacity settings for all of your global
-- tables replicas and indexes.
--
-- If you prefer to manage write capacity settings manually, you should
-- provision equal replicated write capacity units to your replica tables.
-- You should also provision equal replicated write capacity units to
-- matching secondary indexes across your global table.
module Network.AWS.DynamoDB.CreateGlobalTable
  ( -- * Creating a Request
    CreateGlobalTable (..),
    newCreateGlobalTable,

    -- * Request Lenses
    createGlobalTable_globalTableName,
    createGlobalTable_replicationGroup,

    -- * Destructuring the Response
    CreateGlobalTableResponse (..),
    newCreateGlobalTableResponse,

    -- * Response Lenses
    createGlobalTableResponse_globalTableDescription,
    createGlobalTableResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateGlobalTable' smart constructor.
data CreateGlobalTable = CreateGlobalTable'
  { -- | The global table name.
    globalTableName :: Prelude.Text,
    -- | The Regions where the global table needs to be created.
    replicationGroup :: [Replica]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGlobalTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalTableName', 'createGlobalTable_globalTableName' - The global table name.
--
-- 'replicationGroup', 'createGlobalTable_replicationGroup' - The Regions where the global table needs to be created.
newCreateGlobalTable ::
  -- | 'globalTableName'
  Prelude.Text ->
  CreateGlobalTable
newCreateGlobalTable pGlobalTableName_ =
  CreateGlobalTable'
    { globalTableName =
        pGlobalTableName_,
      replicationGroup = Prelude.mempty
    }

-- | The global table name.
createGlobalTable_globalTableName :: Lens.Lens' CreateGlobalTable Prelude.Text
createGlobalTable_globalTableName = Lens.lens (\CreateGlobalTable' {globalTableName} -> globalTableName) (\s@CreateGlobalTable' {} a -> s {globalTableName = a} :: CreateGlobalTable)

-- | The Regions where the global table needs to be created.
createGlobalTable_replicationGroup :: Lens.Lens' CreateGlobalTable [Replica]
createGlobalTable_replicationGroup = Lens.lens (\CreateGlobalTable' {replicationGroup} -> replicationGroup) (\s@CreateGlobalTable' {} a -> s {replicationGroup = a} :: CreateGlobalTable) Prelude.. Lens._Coerce

instance Core.AWSRequest CreateGlobalTable where
  type
    AWSResponse CreateGlobalTable =
      CreateGlobalTableResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGlobalTableResponse'
            Prelude.<$> (x Core..?> "GlobalTableDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGlobalTable

instance Prelude.NFData CreateGlobalTable

instance Core.ToHeaders CreateGlobalTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.CreateGlobalTable" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateGlobalTable where
  toJSON CreateGlobalTable' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("GlobalTableName" Core..= globalTableName),
            Prelude.Just
              ("ReplicationGroup" Core..= replicationGroup)
          ]
      )

instance Core.ToPath CreateGlobalTable where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateGlobalTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGlobalTableResponse' smart constructor.
data CreateGlobalTableResponse = CreateGlobalTableResponse'
  { -- | Contains the details of the global table.
    globalTableDescription :: Prelude.Maybe GlobalTableDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGlobalTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalTableDescription', 'createGlobalTableResponse_globalTableDescription' - Contains the details of the global table.
--
-- 'httpStatus', 'createGlobalTableResponse_httpStatus' - The response's http status code.
newCreateGlobalTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateGlobalTableResponse
newCreateGlobalTableResponse pHttpStatus_ =
  CreateGlobalTableResponse'
    { globalTableDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the details of the global table.
createGlobalTableResponse_globalTableDescription :: Lens.Lens' CreateGlobalTableResponse (Prelude.Maybe GlobalTableDescription)
createGlobalTableResponse_globalTableDescription = Lens.lens (\CreateGlobalTableResponse' {globalTableDescription} -> globalTableDescription) (\s@CreateGlobalTableResponse' {} a -> s {globalTableDescription = a} :: CreateGlobalTableResponse)

-- | The response's http status code.
createGlobalTableResponse_httpStatus :: Lens.Lens' CreateGlobalTableResponse Prelude.Int
createGlobalTableResponse_httpStatus = Lens.lens (\CreateGlobalTableResponse' {httpStatus} -> httpStatus) (\s@CreateGlobalTableResponse' {} a -> s {httpStatus = a} :: CreateGlobalTableResponse)

instance Prelude.NFData CreateGlobalTableResponse
