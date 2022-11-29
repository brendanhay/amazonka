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
-- Module      : Amazonka.LakeFormation.BatchGrantPermissions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Batch operation to grant permissions to the principal.
module Amazonka.LakeFormation.BatchGrantPermissions
  ( -- * Creating a Request
    BatchGrantPermissions (..),
    newBatchGrantPermissions,

    -- * Request Lenses
    batchGrantPermissions_catalogId,
    batchGrantPermissions_entries,

    -- * Destructuring the Response
    BatchGrantPermissionsResponse (..),
    newBatchGrantPermissionsResponse,

    -- * Response Lenses
    batchGrantPermissionsResponse_failures,
    batchGrantPermissionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGrantPermissions' smart constructor.
data BatchGrantPermissions = BatchGrantPermissions'
  { -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | A list of up to 20 entries for resource permissions to be granted by
    -- batch operation to the principal.
    entries :: [BatchPermissionsRequestEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGrantPermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'batchGrantPermissions_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
--
-- 'entries', 'batchGrantPermissions_entries' - A list of up to 20 entries for resource permissions to be granted by
-- batch operation to the principal.
newBatchGrantPermissions ::
  BatchGrantPermissions
newBatchGrantPermissions =
  BatchGrantPermissions'
    { catalogId = Prelude.Nothing,
      entries = Prelude.mempty
    }

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
batchGrantPermissions_catalogId :: Lens.Lens' BatchGrantPermissions (Prelude.Maybe Prelude.Text)
batchGrantPermissions_catalogId = Lens.lens (\BatchGrantPermissions' {catalogId} -> catalogId) (\s@BatchGrantPermissions' {} a -> s {catalogId = a} :: BatchGrantPermissions)

-- | A list of up to 20 entries for resource permissions to be granted by
-- batch operation to the principal.
batchGrantPermissions_entries :: Lens.Lens' BatchGrantPermissions [BatchPermissionsRequestEntry]
batchGrantPermissions_entries = Lens.lens (\BatchGrantPermissions' {entries} -> entries) (\s@BatchGrantPermissions' {} a -> s {entries = a} :: BatchGrantPermissions) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGrantPermissions where
  type
    AWSResponse BatchGrantPermissions =
      BatchGrantPermissionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGrantPermissionsResponse'
            Prelude.<$> (x Core..?> "Failures" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGrantPermissions where
  hashWithSalt _salt BatchGrantPermissions' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` entries

instance Prelude.NFData BatchGrantPermissions where
  rnf BatchGrantPermissions' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf entries

instance Core.ToHeaders BatchGrantPermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchGrantPermissions where
  toJSON BatchGrantPermissions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CatalogId" Core..=) Prelude.<$> catalogId,
            Prelude.Just ("Entries" Core..= entries)
          ]
      )

instance Core.ToPath BatchGrantPermissions where
  toPath = Prelude.const "/BatchGrantPermissions"

instance Core.ToQuery BatchGrantPermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGrantPermissionsResponse' smart constructor.
data BatchGrantPermissionsResponse = BatchGrantPermissionsResponse'
  { -- | A list of failures to grant permissions to the resources.
    failures :: Prelude.Maybe [BatchPermissionsFailureEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGrantPermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failures', 'batchGrantPermissionsResponse_failures' - A list of failures to grant permissions to the resources.
--
-- 'httpStatus', 'batchGrantPermissionsResponse_httpStatus' - The response's http status code.
newBatchGrantPermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGrantPermissionsResponse
newBatchGrantPermissionsResponse pHttpStatus_ =
  BatchGrantPermissionsResponse'
    { failures =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of failures to grant permissions to the resources.
batchGrantPermissionsResponse_failures :: Lens.Lens' BatchGrantPermissionsResponse (Prelude.Maybe [BatchPermissionsFailureEntry])
batchGrantPermissionsResponse_failures = Lens.lens (\BatchGrantPermissionsResponse' {failures} -> failures) (\s@BatchGrantPermissionsResponse' {} a -> s {failures = a} :: BatchGrantPermissionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGrantPermissionsResponse_httpStatus :: Lens.Lens' BatchGrantPermissionsResponse Prelude.Int
batchGrantPermissionsResponse_httpStatus = Lens.lens (\BatchGrantPermissionsResponse' {httpStatus} -> httpStatus) (\s@BatchGrantPermissionsResponse' {} a -> s {httpStatus = a} :: BatchGrantPermissionsResponse)

instance Prelude.NFData BatchGrantPermissionsResponse where
  rnf BatchGrantPermissionsResponse' {..} =
    Prelude.rnf failures
      `Prelude.seq` Prelude.rnf httpStatus
