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
-- Module      : Amazonka.KeySpaces.GetTable
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the table, including the table\'s name and
-- current status, the keyspace name, configuration settings, and metadata.
--
-- To read table metadata using @GetTable@, @Select@ action permissions for
-- the table and system tables are required to complete the operation.
module Amazonka.KeySpaces.GetTable
  ( -- * Creating a Request
    GetTable (..),
    newGetTable,

    -- * Request Lenses
    getTable_keyspaceName,
    getTable_tableName,

    -- * Destructuring the Response
    GetTableResponse (..),
    newGetTableResponse,

    -- * Response Lenses
    getTableResponse_ttl,
    getTableResponse_pointInTimeRecovery,
    getTableResponse_capacitySpecification,
    getTableResponse_status,
    getTableResponse_encryptionSpecification,
    getTableResponse_creationTimestamp,
    getTableResponse_comment,
    getTableResponse_defaultTimeToLive,
    getTableResponse_schemaDefinition,
    getTableResponse_httpStatus,
    getTableResponse_keyspaceName,
    getTableResponse_tableName,
    getTableResponse_resourceArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KeySpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTable' smart constructor.
data GetTable = GetTable'
  { -- | The name of the keyspace that the table is stored in.
    keyspaceName :: Prelude.Text,
    -- | The name of the table.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyspaceName', 'getTable_keyspaceName' - The name of the keyspace that the table is stored in.
--
-- 'tableName', 'getTable_tableName' - The name of the table.
newGetTable ::
  -- | 'keyspaceName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  GetTable
newGetTable pKeyspaceName_ pTableName_ =
  GetTable'
    { keyspaceName = pKeyspaceName_,
      tableName = pTableName_
    }

-- | The name of the keyspace that the table is stored in.
getTable_keyspaceName :: Lens.Lens' GetTable Prelude.Text
getTable_keyspaceName = Lens.lens (\GetTable' {keyspaceName} -> keyspaceName) (\s@GetTable' {} a -> s {keyspaceName = a} :: GetTable)

-- | The name of the table.
getTable_tableName :: Lens.Lens' GetTable Prelude.Text
getTable_tableName = Lens.lens (\GetTable' {tableName} -> tableName) (\s@GetTable' {} a -> s {tableName = a} :: GetTable)

instance Core.AWSRequest GetTable where
  type AWSResponse GetTable = GetTableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTableResponse'
            Prelude.<$> (x Data..?> "ttl")
            Prelude.<*> (x Data..?> "pointInTimeRecovery")
            Prelude.<*> (x Data..?> "capacitySpecification")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "encryptionSpecification")
            Prelude.<*> (x Data..?> "creationTimestamp")
            Prelude.<*> (x Data..?> "comment")
            Prelude.<*> (x Data..?> "defaultTimeToLive")
            Prelude.<*> (x Data..?> "schemaDefinition")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "keyspaceName")
            Prelude.<*> (x Data..:> "tableName")
            Prelude.<*> (x Data..:> "resourceArn")
      )

instance Prelude.Hashable GetTable where
  hashWithSalt _salt GetTable' {..} =
    _salt `Prelude.hashWithSalt` keyspaceName
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData GetTable where
  rnf GetTable' {..} =
    Prelude.rnf keyspaceName
      `Prelude.seq` Prelude.rnf tableName

instance Data.ToHeaders GetTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("KeyspacesService.GetTable" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetTable where
  toJSON GetTable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("keyspaceName" Data..= keyspaceName),
            Prelude.Just ("tableName" Data..= tableName)
          ]
      )

instance Data.ToPath GetTable where
  toPath = Prelude.const "/"

instance Data.ToQuery GetTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTableResponse' smart constructor.
data GetTableResponse = GetTableResponse'
  { -- | The custom Time to Live settings of the specified table.
    ttl :: Prelude.Maybe TimeToLive,
    -- | The point-in-time recovery status of the specified table.
    pointInTimeRecovery :: Prelude.Maybe PointInTimeRecoverySummary,
    -- | The read\/write throughput capacity mode for a table. The options are:
    --
    -- • @throughputMode:PAY_PER_REQUEST@
    --
    -- • @throughputMode:PROVISIONED@
    capacitySpecification :: Prelude.Maybe CapacitySpecificationSummary,
    -- | The current status of the specified table.
    status :: Prelude.Maybe TableStatus,
    -- | The encryption settings of the specified table.
    encryptionSpecification :: Prelude.Maybe EncryptionSpecification,
    -- | The creation timestamp of the specified table.
    creationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The the description of the specified table.
    comment :: Prelude.Maybe Comment,
    -- | The default Time to Live settings of the specified table.
    defaultTimeToLive :: Prelude.Maybe Prelude.Natural,
    -- | The schema definition of the specified table.
    schemaDefinition :: Prelude.Maybe SchemaDefinition,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the keyspace that the specified table is stored in.
    keyspaceName :: Prelude.Text,
    -- | The name of the specified table.
    tableName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the specified table.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ttl', 'getTableResponse_ttl' - The custom Time to Live settings of the specified table.
--
-- 'pointInTimeRecovery', 'getTableResponse_pointInTimeRecovery' - The point-in-time recovery status of the specified table.
--
-- 'capacitySpecification', 'getTableResponse_capacitySpecification' - The read\/write throughput capacity mode for a table. The options are:
--
-- • @throughputMode:PAY_PER_REQUEST@
--
-- • @throughputMode:PROVISIONED@
--
-- 'status', 'getTableResponse_status' - The current status of the specified table.
--
-- 'encryptionSpecification', 'getTableResponse_encryptionSpecification' - The encryption settings of the specified table.
--
-- 'creationTimestamp', 'getTableResponse_creationTimestamp' - The creation timestamp of the specified table.
--
-- 'comment', 'getTableResponse_comment' - The the description of the specified table.
--
-- 'defaultTimeToLive', 'getTableResponse_defaultTimeToLive' - The default Time to Live settings of the specified table.
--
-- 'schemaDefinition', 'getTableResponse_schemaDefinition' - The schema definition of the specified table.
--
-- 'httpStatus', 'getTableResponse_httpStatus' - The response's http status code.
--
-- 'keyspaceName', 'getTableResponse_keyspaceName' - The name of the keyspace that the specified table is stored in.
--
-- 'tableName', 'getTableResponse_tableName' - The name of the specified table.
--
-- 'resourceArn', 'getTableResponse_resourceArn' - The Amazon Resource Name (ARN) of the specified table.
newGetTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'keyspaceName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'resourceArn'
  Prelude.Text ->
  GetTableResponse
newGetTableResponse
  pHttpStatus_
  pKeyspaceName_
  pTableName_
  pResourceArn_ =
    GetTableResponse'
      { ttl = Prelude.Nothing,
        pointInTimeRecovery = Prelude.Nothing,
        capacitySpecification = Prelude.Nothing,
        status = Prelude.Nothing,
        encryptionSpecification = Prelude.Nothing,
        creationTimestamp = Prelude.Nothing,
        comment = Prelude.Nothing,
        defaultTimeToLive = Prelude.Nothing,
        schemaDefinition = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        keyspaceName = pKeyspaceName_,
        tableName = pTableName_,
        resourceArn = pResourceArn_
      }

-- | The custom Time to Live settings of the specified table.
getTableResponse_ttl :: Lens.Lens' GetTableResponse (Prelude.Maybe TimeToLive)
getTableResponse_ttl = Lens.lens (\GetTableResponse' {ttl} -> ttl) (\s@GetTableResponse' {} a -> s {ttl = a} :: GetTableResponse)

-- | The point-in-time recovery status of the specified table.
getTableResponse_pointInTimeRecovery :: Lens.Lens' GetTableResponse (Prelude.Maybe PointInTimeRecoverySummary)
getTableResponse_pointInTimeRecovery = Lens.lens (\GetTableResponse' {pointInTimeRecovery} -> pointInTimeRecovery) (\s@GetTableResponse' {} a -> s {pointInTimeRecovery = a} :: GetTableResponse)

-- | The read\/write throughput capacity mode for a table. The options are:
--
-- • @throughputMode:PAY_PER_REQUEST@
--
-- • @throughputMode:PROVISIONED@
getTableResponse_capacitySpecification :: Lens.Lens' GetTableResponse (Prelude.Maybe CapacitySpecificationSummary)
getTableResponse_capacitySpecification = Lens.lens (\GetTableResponse' {capacitySpecification} -> capacitySpecification) (\s@GetTableResponse' {} a -> s {capacitySpecification = a} :: GetTableResponse)

-- | The current status of the specified table.
getTableResponse_status :: Lens.Lens' GetTableResponse (Prelude.Maybe TableStatus)
getTableResponse_status = Lens.lens (\GetTableResponse' {status} -> status) (\s@GetTableResponse' {} a -> s {status = a} :: GetTableResponse)

-- | The encryption settings of the specified table.
getTableResponse_encryptionSpecification :: Lens.Lens' GetTableResponse (Prelude.Maybe EncryptionSpecification)
getTableResponse_encryptionSpecification = Lens.lens (\GetTableResponse' {encryptionSpecification} -> encryptionSpecification) (\s@GetTableResponse' {} a -> s {encryptionSpecification = a} :: GetTableResponse)

-- | The creation timestamp of the specified table.
getTableResponse_creationTimestamp :: Lens.Lens' GetTableResponse (Prelude.Maybe Prelude.UTCTime)
getTableResponse_creationTimestamp = Lens.lens (\GetTableResponse' {creationTimestamp} -> creationTimestamp) (\s@GetTableResponse' {} a -> s {creationTimestamp = a} :: GetTableResponse) Prelude.. Lens.mapping Data._Time

-- | The the description of the specified table.
getTableResponse_comment :: Lens.Lens' GetTableResponse (Prelude.Maybe Comment)
getTableResponse_comment = Lens.lens (\GetTableResponse' {comment} -> comment) (\s@GetTableResponse' {} a -> s {comment = a} :: GetTableResponse)

-- | The default Time to Live settings of the specified table.
getTableResponse_defaultTimeToLive :: Lens.Lens' GetTableResponse (Prelude.Maybe Prelude.Natural)
getTableResponse_defaultTimeToLive = Lens.lens (\GetTableResponse' {defaultTimeToLive} -> defaultTimeToLive) (\s@GetTableResponse' {} a -> s {defaultTimeToLive = a} :: GetTableResponse)

-- | The schema definition of the specified table.
getTableResponse_schemaDefinition :: Lens.Lens' GetTableResponse (Prelude.Maybe SchemaDefinition)
getTableResponse_schemaDefinition = Lens.lens (\GetTableResponse' {schemaDefinition} -> schemaDefinition) (\s@GetTableResponse' {} a -> s {schemaDefinition = a} :: GetTableResponse)

-- | The response's http status code.
getTableResponse_httpStatus :: Lens.Lens' GetTableResponse Prelude.Int
getTableResponse_httpStatus = Lens.lens (\GetTableResponse' {httpStatus} -> httpStatus) (\s@GetTableResponse' {} a -> s {httpStatus = a} :: GetTableResponse)

-- | The name of the keyspace that the specified table is stored in.
getTableResponse_keyspaceName :: Lens.Lens' GetTableResponse Prelude.Text
getTableResponse_keyspaceName = Lens.lens (\GetTableResponse' {keyspaceName} -> keyspaceName) (\s@GetTableResponse' {} a -> s {keyspaceName = a} :: GetTableResponse)

-- | The name of the specified table.
getTableResponse_tableName :: Lens.Lens' GetTableResponse Prelude.Text
getTableResponse_tableName = Lens.lens (\GetTableResponse' {tableName} -> tableName) (\s@GetTableResponse' {} a -> s {tableName = a} :: GetTableResponse)

-- | The Amazon Resource Name (ARN) of the specified table.
getTableResponse_resourceArn :: Lens.Lens' GetTableResponse Prelude.Text
getTableResponse_resourceArn = Lens.lens (\GetTableResponse' {resourceArn} -> resourceArn) (\s@GetTableResponse' {} a -> s {resourceArn = a} :: GetTableResponse)

instance Prelude.NFData GetTableResponse where
  rnf GetTableResponse' {..} =
    Prelude.rnf ttl
      `Prelude.seq` Prelude.rnf pointInTimeRecovery
      `Prelude.seq` Prelude.rnf capacitySpecification
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf encryptionSpecification
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf defaultTimeToLive
      `Prelude.seq` Prelude.rnf schemaDefinition
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf keyspaceName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf resourceArn
