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
-- Module      : Network.AWS.Glue.GetCatalogImportStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the status of a migration operation.
module Network.AWS.Glue.GetCatalogImportStatus
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

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCatalogImportStatus' smart constructor.
data GetCatalogImportStatus = GetCatalogImportStatus'
  { -- | The ID of the catalog to migrate. Currently, this should be the AWS
    -- account ID.
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
-- 'catalogId', 'getCatalogImportStatus_catalogId' - The ID of the catalog to migrate. Currently, this should be the AWS
-- account ID.
newGetCatalogImportStatus ::
  GetCatalogImportStatus
newGetCatalogImportStatus =
  GetCatalogImportStatus'
    { catalogId =
        Prelude.Nothing
    }

-- | The ID of the catalog to migrate. Currently, this should be the AWS
-- account ID.
getCatalogImportStatus_catalogId :: Lens.Lens' GetCatalogImportStatus (Prelude.Maybe Prelude.Text)
getCatalogImportStatus_catalogId = Lens.lens (\GetCatalogImportStatus' {catalogId} -> catalogId) (\s@GetCatalogImportStatus' {} a -> s {catalogId = a} :: GetCatalogImportStatus)

instance Core.AWSRequest GetCatalogImportStatus where
  type
    AWSResponse GetCatalogImportStatus =
      GetCatalogImportStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCatalogImportStatusResponse'
            Prelude.<$> (x Core..?> "ImportStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCatalogImportStatus

instance Prelude.NFData GetCatalogImportStatus

instance Core.ToHeaders GetCatalogImportStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.GetCatalogImportStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetCatalogImportStatus where
  toJSON GetCatalogImportStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [("CatalogId" Core..=) Prelude.<$> catalogId]
      )

instance Core.ToPath GetCatalogImportStatus where
  toPath = Prelude.const "/"

instance Core.ToQuery GetCatalogImportStatus where
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
