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
-- Module      : Amazonka.Glue.GetCatalogImportStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the status of a migration operation.
module Amazonka.Glue.GetCatalogImportStatus
  ( -- * Creating a Request
    GetCatalogImportStatus (..),
    newGetCatalogImportStatus,

    -- * Request Lenses
    getCatalogImportStatus_catalogId,

    -- * Destructuring the Response
    GetCatalogImportStatusResponse (..),
    newGetCatalogImportStatusResponse,

    -- * Response Lenses
    getCatalogImportStatusResponse_importStatus,
    getCatalogImportStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCatalogImportStatus' smart constructor.
data GetCatalogImportStatus = GetCatalogImportStatus'
  { -- | The ID of the catalog to migrate. Currently, this should be the Amazon
    -- Web Services account ID.
    catalogId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCatalogImportStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'getCatalogImportStatus_catalogId' - The ID of the catalog to migrate. Currently, this should be the Amazon
-- Web Services account ID.
newGetCatalogImportStatus ::
  GetCatalogImportStatus
newGetCatalogImportStatus =
  GetCatalogImportStatus'
    { catalogId =
        Prelude.Nothing
    }

-- | The ID of the catalog to migrate. Currently, this should be the Amazon
-- Web Services account ID.
getCatalogImportStatus_catalogId :: Lens.Lens' GetCatalogImportStatus (Prelude.Maybe Prelude.Text)
getCatalogImportStatus_catalogId = Lens.lens (\GetCatalogImportStatus' {catalogId} -> catalogId) (\s@GetCatalogImportStatus' {} a -> s {catalogId = a} :: GetCatalogImportStatus)

instance Core.AWSRequest GetCatalogImportStatus where
  type
    AWSResponse GetCatalogImportStatus =
      GetCatalogImportStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCatalogImportStatusResponse'
            Prelude.<$> (x Data..?> "ImportStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCatalogImportStatus where
  hashWithSalt _salt GetCatalogImportStatus' {..} =
    _salt `Prelude.hashWithSalt` catalogId

instance Prelude.NFData GetCatalogImportStatus where
  rnf GetCatalogImportStatus' {..} =
    Prelude.rnf catalogId

instance Data.ToHeaders GetCatalogImportStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.GetCatalogImportStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCatalogImportStatus where
  toJSON GetCatalogImportStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [("CatalogId" Data..=) Prelude.<$> catalogId]
      )

instance Data.ToPath GetCatalogImportStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCatalogImportStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCatalogImportStatusResponse' smart constructor.
data GetCatalogImportStatusResponse = GetCatalogImportStatusResponse'
  { -- | The status of the specified catalog migration.
    importStatus :: Prelude.Maybe CatalogImportStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCatalogImportStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importStatus', 'getCatalogImportStatusResponse_importStatus' - The status of the specified catalog migration.
--
-- 'httpStatus', 'getCatalogImportStatusResponse_httpStatus' - The response's http status code.
newGetCatalogImportStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCatalogImportStatusResponse
newGetCatalogImportStatusResponse pHttpStatus_ =
  GetCatalogImportStatusResponse'
    { importStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the specified catalog migration.
getCatalogImportStatusResponse_importStatus :: Lens.Lens' GetCatalogImportStatusResponse (Prelude.Maybe CatalogImportStatus)
getCatalogImportStatusResponse_importStatus = Lens.lens (\GetCatalogImportStatusResponse' {importStatus} -> importStatus) (\s@GetCatalogImportStatusResponse' {} a -> s {importStatus = a} :: GetCatalogImportStatusResponse)

-- | The response's http status code.
getCatalogImportStatusResponse_httpStatus :: Lens.Lens' GetCatalogImportStatusResponse Prelude.Int
getCatalogImportStatusResponse_httpStatus = Lens.lens (\GetCatalogImportStatusResponse' {httpStatus} -> httpStatus) (\s@GetCatalogImportStatusResponse' {} a -> s {httpStatus = a} :: GetCatalogImportStatusResponse)

instance
  Prelude.NFData
    GetCatalogImportStatusResponse
  where
  rnf GetCatalogImportStatusResponse' {..} =
    Prelude.rnf importStatus `Prelude.seq`
      Prelude.rnf httpStatus
