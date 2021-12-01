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
-- Module      : Amazonka.LexModels.GetImport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an import job started with the @StartImport@
-- operation.
module Amazonka.LexModels.GetImport
  ( -- * Creating a Request
    GetImport (..),
    newGetImport,

    -- * Request Lenses
    getImport_importId,

    -- * Destructuring the Response
    GetImportResponse (..),
    newGetImportResponse,

    -- * Response Lenses
    getImportResponse_failureReason,
    getImportResponse_resourceType,
    getImportResponse_importId,
    getImportResponse_createdDate,
    getImportResponse_name,
    getImportResponse_mergeStrategy,
    getImportResponse_importStatus,
    getImportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetImport' smart constructor.
data GetImport = GetImport'
  { -- | The identifier of the import job information to return.
    importId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importId', 'getImport_importId' - The identifier of the import job information to return.
newGetImport ::
  -- | 'importId'
  Prelude.Text ->
  GetImport
newGetImport pImportId_ =
  GetImport' {importId = pImportId_}

-- | The identifier of the import job information to return.
getImport_importId :: Lens.Lens' GetImport Prelude.Text
getImport_importId = Lens.lens (\GetImport' {importId} -> importId) (\s@GetImport' {} a -> s {importId = a} :: GetImport)

instance Core.AWSRequest GetImport where
  type AWSResponse GetImport = GetImportResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetImportResponse'
            Prelude.<$> (x Core..?> "failureReason" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "resourceType")
            Prelude.<*> (x Core..?> "importId")
            Prelude.<*> (x Core..?> "createdDate")
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "mergeStrategy")
            Prelude.<*> (x Core..?> "importStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetImport where
  hashWithSalt salt' GetImport' {..} =
    salt' `Prelude.hashWithSalt` importId

instance Prelude.NFData GetImport where
  rnf GetImport' {..} = Prelude.rnf importId

instance Core.ToHeaders GetImport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetImport where
  toPath GetImport' {..} =
    Prelude.mconcat ["/imports/", Core.toBS importId]

instance Core.ToQuery GetImport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetImportResponse' smart constructor.
data GetImportResponse = GetImportResponse'
  { -- | A string that describes why an import job failed to complete.
    failureReason :: Prelude.Maybe [Prelude.Text],
    -- | The type of resource imported.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The identifier for the specific import job.
    importId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp for the date and time that the import job was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The name given to the import job.
    name :: Prelude.Maybe Prelude.Text,
    -- | The action taken when there was a conflict between an existing resource
    -- and a resource in the import file.
    mergeStrategy :: Prelude.Maybe MergeStrategy,
    -- | The status of the import job. If the status is @FAILED@, you can get the
    -- reason for the failure from the @failureReason@ field.
    importStatus :: Prelude.Maybe ImportStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetImportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'getImportResponse_failureReason' - A string that describes why an import job failed to complete.
--
-- 'resourceType', 'getImportResponse_resourceType' - The type of resource imported.
--
-- 'importId', 'getImportResponse_importId' - The identifier for the specific import job.
--
-- 'createdDate', 'getImportResponse_createdDate' - A timestamp for the date and time that the import job was created.
--
-- 'name', 'getImportResponse_name' - The name given to the import job.
--
-- 'mergeStrategy', 'getImportResponse_mergeStrategy' - The action taken when there was a conflict between an existing resource
-- and a resource in the import file.
--
-- 'importStatus', 'getImportResponse_importStatus' - The status of the import job. If the status is @FAILED@, you can get the
-- reason for the failure from the @failureReason@ field.
--
-- 'httpStatus', 'getImportResponse_httpStatus' - The response's http status code.
newGetImportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetImportResponse
newGetImportResponse pHttpStatus_ =
  GetImportResponse'
    { failureReason = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      importId = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      name = Prelude.Nothing,
      mergeStrategy = Prelude.Nothing,
      importStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string that describes why an import job failed to complete.
getImportResponse_failureReason :: Lens.Lens' GetImportResponse (Prelude.Maybe [Prelude.Text])
getImportResponse_failureReason = Lens.lens (\GetImportResponse' {failureReason} -> failureReason) (\s@GetImportResponse' {} a -> s {failureReason = a} :: GetImportResponse) Prelude.. Lens.mapping Lens.coerced

-- | The type of resource imported.
getImportResponse_resourceType :: Lens.Lens' GetImportResponse (Prelude.Maybe ResourceType)
getImportResponse_resourceType = Lens.lens (\GetImportResponse' {resourceType} -> resourceType) (\s@GetImportResponse' {} a -> s {resourceType = a} :: GetImportResponse)

-- | The identifier for the specific import job.
getImportResponse_importId :: Lens.Lens' GetImportResponse (Prelude.Maybe Prelude.Text)
getImportResponse_importId = Lens.lens (\GetImportResponse' {importId} -> importId) (\s@GetImportResponse' {} a -> s {importId = a} :: GetImportResponse)

-- | A timestamp for the date and time that the import job was created.
getImportResponse_createdDate :: Lens.Lens' GetImportResponse (Prelude.Maybe Prelude.UTCTime)
getImportResponse_createdDate = Lens.lens (\GetImportResponse' {createdDate} -> createdDate) (\s@GetImportResponse' {} a -> s {createdDate = a} :: GetImportResponse) Prelude.. Lens.mapping Core._Time

-- | The name given to the import job.
getImportResponse_name :: Lens.Lens' GetImportResponse (Prelude.Maybe Prelude.Text)
getImportResponse_name = Lens.lens (\GetImportResponse' {name} -> name) (\s@GetImportResponse' {} a -> s {name = a} :: GetImportResponse)

-- | The action taken when there was a conflict between an existing resource
-- and a resource in the import file.
getImportResponse_mergeStrategy :: Lens.Lens' GetImportResponse (Prelude.Maybe MergeStrategy)
getImportResponse_mergeStrategy = Lens.lens (\GetImportResponse' {mergeStrategy} -> mergeStrategy) (\s@GetImportResponse' {} a -> s {mergeStrategy = a} :: GetImportResponse)

-- | The status of the import job. If the status is @FAILED@, you can get the
-- reason for the failure from the @failureReason@ field.
getImportResponse_importStatus :: Lens.Lens' GetImportResponse (Prelude.Maybe ImportStatus)
getImportResponse_importStatus = Lens.lens (\GetImportResponse' {importStatus} -> importStatus) (\s@GetImportResponse' {} a -> s {importStatus = a} :: GetImportResponse)

-- | The response's http status code.
getImportResponse_httpStatus :: Lens.Lens' GetImportResponse Prelude.Int
getImportResponse_httpStatus = Lens.lens (\GetImportResponse' {httpStatus} -> httpStatus) (\s@GetImportResponse' {} a -> s {httpStatus = a} :: GetImportResponse)

instance Prelude.NFData GetImportResponse where
  rnf GetImportResponse' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf importStatus
      `Prelude.seq` Prelude.rnf mergeStrategy
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf importId
      `Prelude.seq` Prelude.rnf resourceType
