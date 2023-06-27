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
-- Module      : Amazonka.VerifiedPermissions.PutSchema
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the policy schema in the specified policy store. The
-- schema is used to validate any Cedar policies and policy templates
-- submitted to the policy store. Any changes to the schema validate only
-- policies and templates submitted after the schema change. Existing
-- policies and templates are not re-evaluated against the changed schema.
-- If you later update a policy, then it is evaluated against the new
-- schema at that time.
module Amazonka.VerifiedPermissions.PutSchema
  ( -- * Creating a Request
    PutSchema (..),
    newPutSchema,

    -- * Request Lenses
    putSchema_policyStoreId,
    putSchema_definition,

    -- * Destructuring the Response
    PutSchemaResponse (..),
    newPutSchemaResponse,

    -- * Response Lenses
    putSchemaResponse_httpStatus,
    putSchemaResponse_policyStoreId,
    putSchemaResponse_namespaces,
    putSchemaResponse_createdDate,
    putSchemaResponse_lastUpdatedDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newPutSchema' smart constructor.
data PutSchema = PutSchema'
  { -- | Specifies the ID of the policy store in which to place the schema.
    policyStoreId :: Prelude.Text,
    -- | Specifies the definition of the schema to be stored. The schema
    -- definition must be written in Cedar schema JSON.
    definition :: SchemaDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyStoreId', 'putSchema_policyStoreId' - Specifies the ID of the policy store in which to place the schema.
--
-- 'definition', 'putSchema_definition' - Specifies the definition of the schema to be stored. The schema
-- definition must be written in Cedar schema JSON.
newPutSchema ::
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'definition'
  SchemaDefinition ->
  PutSchema
newPutSchema pPolicyStoreId_ pDefinition_ =
  PutSchema'
    { policyStoreId = pPolicyStoreId_,
      definition = pDefinition_
    }

-- | Specifies the ID of the policy store in which to place the schema.
putSchema_policyStoreId :: Lens.Lens' PutSchema Prelude.Text
putSchema_policyStoreId = Lens.lens (\PutSchema' {policyStoreId} -> policyStoreId) (\s@PutSchema' {} a -> s {policyStoreId = a} :: PutSchema)

-- | Specifies the definition of the schema to be stored. The schema
-- definition must be written in Cedar schema JSON.
putSchema_definition :: Lens.Lens' PutSchema SchemaDefinition
putSchema_definition = Lens.lens (\PutSchema' {definition} -> definition) (\s@PutSchema' {} a -> s {definition = a} :: PutSchema)

instance Core.AWSRequest PutSchema where
  type AWSResponse PutSchema = PutSchemaResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutSchemaResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "policyStoreId")
            Prelude.<*> (x Data..?> "namespaces" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "createdDate")
            Prelude.<*> (x Data..:> "lastUpdatedDate")
      )

instance Prelude.Hashable PutSchema where
  hashWithSalt _salt PutSchema' {..} =
    _salt
      `Prelude.hashWithSalt` policyStoreId
      `Prelude.hashWithSalt` definition

instance Prelude.NFData PutSchema where
  rnf PutSchema' {..} =
    Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf definition

instance Data.ToHeaders PutSchema where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.PutSchema" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutSchema where
  toJSON PutSchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("policyStoreId" Data..= policyStoreId),
            Prelude.Just ("definition" Data..= definition)
          ]
      )

instance Data.ToPath PutSchema where
  toPath = Prelude.const "/"

instance Data.ToQuery PutSchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutSchemaResponse' smart constructor.
data PutSchemaResponse = PutSchemaResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique ID of the policy store that contains the schema.
    policyStoreId :: Prelude.Text,
    -- | Identifies the namespaces of the entities referenced by this schema.
    namespaces :: [Prelude.Text],
    -- | The date and time that the schema was originally created.
    createdDate :: Data.ISO8601,
    -- | The date and time that the schema was last updated.
    lastUpdatedDate :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putSchemaResponse_httpStatus' - The response's http status code.
--
-- 'policyStoreId', 'putSchemaResponse_policyStoreId' - The unique ID of the policy store that contains the schema.
--
-- 'namespaces', 'putSchemaResponse_namespaces' - Identifies the namespaces of the entities referenced by this schema.
--
-- 'createdDate', 'putSchemaResponse_createdDate' - The date and time that the schema was originally created.
--
-- 'lastUpdatedDate', 'putSchemaResponse_lastUpdatedDate' - The date and time that the schema was last updated.
newPutSchemaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'createdDate'
  Prelude.UTCTime ->
  -- | 'lastUpdatedDate'
  Prelude.UTCTime ->
  PutSchemaResponse
newPutSchemaResponse
  pHttpStatus_
  pPolicyStoreId_
  pCreatedDate_
  pLastUpdatedDate_ =
    PutSchemaResponse'
      { httpStatus = pHttpStatus_,
        policyStoreId = pPolicyStoreId_,
        namespaces = Prelude.mempty,
        createdDate = Data._Time Lens.# pCreatedDate_,
        lastUpdatedDate =
          Data._Time Lens.# pLastUpdatedDate_
      }

-- | The response's http status code.
putSchemaResponse_httpStatus :: Lens.Lens' PutSchemaResponse Prelude.Int
putSchemaResponse_httpStatus = Lens.lens (\PutSchemaResponse' {httpStatus} -> httpStatus) (\s@PutSchemaResponse' {} a -> s {httpStatus = a} :: PutSchemaResponse)

-- | The unique ID of the policy store that contains the schema.
putSchemaResponse_policyStoreId :: Lens.Lens' PutSchemaResponse Prelude.Text
putSchemaResponse_policyStoreId = Lens.lens (\PutSchemaResponse' {policyStoreId} -> policyStoreId) (\s@PutSchemaResponse' {} a -> s {policyStoreId = a} :: PutSchemaResponse)

-- | Identifies the namespaces of the entities referenced by this schema.
putSchemaResponse_namespaces :: Lens.Lens' PutSchemaResponse [Prelude.Text]
putSchemaResponse_namespaces = Lens.lens (\PutSchemaResponse' {namespaces} -> namespaces) (\s@PutSchemaResponse' {} a -> s {namespaces = a} :: PutSchemaResponse) Prelude.. Lens.coerced

-- | The date and time that the schema was originally created.
putSchemaResponse_createdDate :: Lens.Lens' PutSchemaResponse Prelude.UTCTime
putSchemaResponse_createdDate = Lens.lens (\PutSchemaResponse' {createdDate} -> createdDate) (\s@PutSchemaResponse' {} a -> s {createdDate = a} :: PutSchemaResponse) Prelude.. Data._Time

-- | The date and time that the schema was last updated.
putSchemaResponse_lastUpdatedDate :: Lens.Lens' PutSchemaResponse Prelude.UTCTime
putSchemaResponse_lastUpdatedDate = Lens.lens (\PutSchemaResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@PutSchemaResponse' {} a -> s {lastUpdatedDate = a} :: PutSchemaResponse) Prelude.. Data._Time

instance Prelude.NFData PutSchemaResponse where
  rnf PutSchemaResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf namespaces
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf lastUpdatedDate
