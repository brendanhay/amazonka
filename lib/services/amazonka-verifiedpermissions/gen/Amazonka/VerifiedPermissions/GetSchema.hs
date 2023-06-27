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
-- Module      : Amazonka.VerifiedPermissions.GetSchema
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve the details for the specified schema in the specified policy
-- store.
module Amazonka.VerifiedPermissions.GetSchema
  ( -- * Creating a Request
    GetSchema (..),
    newGetSchema,

    -- * Request Lenses
    getSchema_policyStoreId,

    -- * Destructuring the Response
    GetSchemaResponse (..),
    newGetSchemaResponse,

    -- * Response Lenses
    getSchemaResponse_httpStatus,
    getSchemaResponse_policyStoreId,
    getSchemaResponse_schema,
    getSchemaResponse_createdDate,
    getSchemaResponse_lastUpdatedDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VerifiedPermissions.Types

-- | /See:/ 'newGetSchema' smart constructor.
data GetSchema = GetSchema'
  { -- | Specifies the ID of the policy store that contains the schema.
    policyStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyStoreId', 'getSchema_policyStoreId' - Specifies the ID of the policy store that contains the schema.
newGetSchema ::
  -- | 'policyStoreId'
  Prelude.Text ->
  GetSchema
newGetSchema pPolicyStoreId_ =
  GetSchema' {policyStoreId = pPolicyStoreId_}

-- | Specifies the ID of the policy store that contains the schema.
getSchema_policyStoreId :: Lens.Lens' GetSchema Prelude.Text
getSchema_policyStoreId = Lens.lens (\GetSchema' {policyStoreId} -> policyStoreId) (\s@GetSchema' {} a -> s {policyStoreId = a} :: GetSchema)

instance Core.AWSRequest GetSchema where
  type AWSResponse GetSchema = GetSchemaResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSchemaResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "policyStoreId")
            Prelude.<*> (x Data..:> "schema")
            Prelude.<*> (x Data..:> "createdDate")
            Prelude.<*> (x Data..:> "lastUpdatedDate")
      )

instance Prelude.Hashable GetSchema where
  hashWithSalt _salt GetSchema' {..} =
    _salt `Prelude.hashWithSalt` policyStoreId

instance Prelude.NFData GetSchema where
  rnf GetSchema' {..} = Prelude.rnf policyStoreId

instance Data.ToHeaders GetSchema where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "VerifiedPermissions.GetSchema" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSchema where
  toJSON GetSchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("policyStoreId" Data..= policyStoreId)
          ]
      )

instance Data.ToPath GetSchema where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSchemaResponse' smart constructor.
data GetSchemaResponse = GetSchemaResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the policy store that contains the schema.
    policyStoreId :: Prelude.Text,
    -- | The body of the schema, written in Cedar schema JSON.
    schema :: Prelude.Text,
    -- | The date and time that the schema was originally created.
    createdDate :: Data.ISO8601,
    -- | The date and time that the schema was most recently updated.
    lastUpdatedDate :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getSchemaResponse_httpStatus' - The response's http status code.
--
-- 'policyStoreId', 'getSchemaResponse_policyStoreId' - The ID of the policy store that contains the schema.
--
-- 'schema', 'getSchemaResponse_schema' - The body of the schema, written in Cedar schema JSON.
--
-- 'createdDate', 'getSchemaResponse_createdDate' - The date and time that the schema was originally created.
--
-- 'lastUpdatedDate', 'getSchemaResponse_lastUpdatedDate' - The date and time that the schema was most recently updated.
newGetSchemaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'policyStoreId'
  Prelude.Text ->
  -- | 'schema'
  Prelude.Text ->
  -- | 'createdDate'
  Prelude.UTCTime ->
  -- | 'lastUpdatedDate'
  Prelude.UTCTime ->
  GetSchemaResponse
newGetSchemaResponse
  pHttpStatus_
  pPolicyStoreId_
  pSchema_
  pCreatedDate_
  pLastUpdatedDate_ =
    GetSchemaResponse'
      { httpStatus = pHttpStatus_,
        policyStoreId = pPolicyStoreId_,
        schema = pSchema_,
        createdDate = Data._Time Lens.# pCreatedDate_,
        lastUpdatedDate =
          Data._Time Lens.# pLastUpdatedDate_
      }

-- | The response's http status code.
getSchemaResponse_httpStatus :: Lens.Lens' GetSchemaResponse Prelude.Int
getSchemaResponse_httpStatus = Lens.lens (\GetSchemaResponse' {httpStatus} -> httpStatus) (\s@GetSchemaResponse' {} a -> s {httpStatus = a} :: GetSchemaResponse)

-- | The ID of the policy store that contains the schema.
getSchemaResponse_policyStoreId :: Lens.Lens' GetSchemaResponse Prelude.Text
getSchemaResponse_policyStoreId = Lens.lens (\GetSchemaResponse' {policyStoreId} -> policyStoreId) (\s@GetSchemaResponse' {} a -> s {policyStoreId = a} :: GetSchemaResponse)

-- | The body of the schema, written in Cedar schema JSON.
getSchemaResponse_schema :: Lens.Lens' GetSchemaResponse Prelude.Text
getSchemaResponse_schema = Lens.lens (\GetSchemaResponse' {schema} -> schema) (\s@GetSchemaResponse' {} a -> s {schema = a} :: GetSchemaResponse)

-- | The date and time that the schema was originally created.
getSchemaResponse_createdDate :: Lens.Lens' GetSchemaResponse Prelude.UTCTime
getSchemaResponse_createdDate = Lens.lens (\GetSchemaResponse' {createdDate} -> createdDate) (\s@GetSchemaResponse' {} a -> s {createdDate = a} :: GetSchemaResponse) Prelude.. Data._Time

-- | The date and time that the schema was most recently updated.
getSchemaResponse_lastUpdatedDate :: Lens.Lens' GetSchemaResponse Prelude.UTCTime
getSchemaResponse_lastUpdatedDate = Lens.lens (\GetSchemaResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@GetSchemaResponse' {} a -> s {lastUpdatedDate = a} :: GetSchemaResponse) Prelude.. Data._Time

instance Prelude.NFData GetSchemaResponse where
  rnf GetSchemaResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policyStoreId
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf lastUpdatedDate
