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
-- Module      : Amazonka.LakeFormation.BatchRevokePermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Batch operation to revoke permissions from the principal.
module Amazonka.LakeFormation.BatchRevokePermissions
  ( -- * Creating a Request
    BatchRevokePermissions (..),
    newBatchRevokePermissions,

    -- * Request Lenses
    batchRevokePermissions_catalogId,
    batchRevokePermissions_entries,

    -- * Destructuring the Response
    BatchRevokePermissionsResponse (..),
    newBatchRevokePermissionsResponse,

    -- * Response Lenses
    batchRevokePermissionsResponse_failures,
    batchRevokePermissionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchRevokePermissions' smart constructor.
data BatchRevokePermissions = BatchRevokePermissions'
  { -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | A list of up to 20 entries for resource permissions to be revoked by
    -- batch operation to the principal.
    entries :: [BatchPermissionsRequestEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchRevokePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'batchRevokePermissions_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
--
-- 'entries', 'batchRevokePermissions_entries' - A list of up to 20 entries for resource permissions to be revoked by
-- batch operation to the principal.
newBatchRevokePermissions ::
  BatchRevokePermissions
newBatchRevokePermissions =
  BatchRevokePermissions'
    { catalogId =
        Prelude.Nothing,
      entries = Prelude.mempty
    }

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
batchRevokePermissions_catalogId :: Lens.Lens' BatchRevokePermissions (Prelude.Maybe Prelude.Text)
batchRevokePermissions_catalogId = Lens.lens (\BatchRevokePermissions' {catalogId} -> catalogId) (\s@BatchRevokePermissions' {} a -> s {catalogId = a} :: BatchRevokePermissions)

-- | A list of up to 20 entries for resource permissions to be revoked by
-- batch operation to the principal.
batchRevokePermissions_entries :: Lens.Lens' BatchRevokePermissions [BatchPermissionsRequestEntry]
batchRevokePermissions_entries = Lens.lens (\BatchRevokePermissions' {entries} -> entries) (\s@BatchRevokePermissions' {} a -> s {entries = a} :: BatchRevokePermissions) Prelude.. Lens.coerced

instance Core.AWSRequest BatchRevokePermissions where
  type
    AWSResponse BatchRevokePermissions =
      BatchRevokePermissionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchRevokePermissionsResponse'
            Prelude.<$> (x Data..?> "Failures" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchRevokePermissions where
  hashWithSalt _salt BatchRevokePermissions' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` entries

instance Prelude.NFData BatchRevokePermissions where
  rnf BatchRevokePermissions' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf entries

instance Data.ToHeaders BatchRevokePermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchRevokePermissions where
  toJSON BatchRevokePermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("Entries" Data..= entries)
          ]
      )

instance Data.ToPath BatchRevokePermissions where
  toPath = Prelude.const "/BatchRevokePermissions"

instance Data.ToQuery BatchRevokePermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchRevokePermissionsResponse' smart constructor.
data BatchRevokePermissionsResponse = BatchRevokePermissionsResponse'
  { -- | A list of failures to revoke permissions to the resources.
    failures :: Prelude.Maybe [BatchPermissionsFailureEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchRevokePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failures', 'batchRevokePermissionsResponse_failures' - A list of failures to revoke permissions to the resources.
--
-- 'httpStatus', 'batchRevokePermissionsResponse_httpStatus' - The response's http status code.
newBatchRevokePermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchRevokePermissionsResponse
newBatchRevokePermissionsResponse pHttpStatus_ =
  BatchRevokePermissionsResponse'
    { failures =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of failures to revoke permissions to the resources.
batchRevokePermissionsResponse_failures :: Lens.Lens' BatchRevokePermissionsResponse (Prelude.Maybe [BatchPermissionsFailureEntry])
batchRevokePermissionsResponse_failures = Lens.lens (\BatchRevokePermissionsResponse' {failures} -> failures) (\s@BatchRevokePermissionsResponse' {} a -> s {failures = a} :: BatchRevokePermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchRevokePermissionsResponse_httpStatus :: Lens.Lens' BatchRevokePermissionsResponse Prelude.Int
batchRevokePermissionsResponse_httpStatus = Lens.lens (\BatchRevokePermissionsResponse' {httpStatus} -> httpStatus) (\s@BatchRevokePermissionsResponse' {} a -> s {httpStatus = a} :: BatchRevokePermissionsResponse)

instance
  Prelude.NFData
    BatchRevokePermissionsResponse
  where
  rnf BatchRevokePermissionsResponse' {..} =
    Prelude.rnf failures
      `Prelude.seq` Prelude.rnf httpStatus
