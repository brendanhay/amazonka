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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    getImportResponse_resourceType,
    getImportResponse_name,
    getImportResponse_importId,
    getImportResponse_createdDate,
    getImportResponse_importStatus,
    getImportResponse_failureReason,
    getImportResponse_mergeStrategy,
    getImportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetImportResponse'
            Prelude.<$> (x Data..?> "resourceType")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "importId")
            Prelude.<*> (x Data..?> "createdDate")
            Prelude.<*> (x Data..?> "importStatus")
            Prelude.<*> (x Data..?> "failureReason" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "mergeStrategy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetImport where
  hashWithSalt _salt GetImport' {..} =
    _salt `Prelude.hashWithSalt` importId

instance Prelude.NFData GetImport where
  rnf GetImport' {..} = Prelude.rnf importId

instance Data.ToHeaders GetImport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetImport where
  toPath GetImport' {..} =
    Prelude.mconcat ["/imports/", Data.toBS importId]

instance Data.ToQuery GetImport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetImportResponse' smart constructor.
data GetImportResponse = GetImportResponse'
  { -- | The type of resource imported.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The name given to the import job.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the specific import job.
    importId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp for the date and time that the import job was created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | The status of the import job. If the status is @FAILED@, you can get the
    -- reason for the failure from the @failureReason@ field.
    importStatus :: Prelude.Maybe ImportStatus,
    -- | A string that describes why an import job failed to complete.
    failureReason :: Prelude.Maybe [Prelude.Text],
    -- | The action taken when there was a conflict between an existing resource
    -- and a resource in the import file.
    mergeStrategy :: Prelude.Maybe MergeStrategy,
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
-- 'resourceType', 'getImportResponse_resourceType' - The type of resource imported.
--
-- 'name', 'getImportResponse_name' - The name given to the import job.
--
-- 'importId', 'getImportResponse_importId' - The identifier for the specific import job.
--
-- 'createdDate', 'getImportResponse_createdDate' - A timestamp for the date and time that the import job was created.
--
-- 'importStatus', 'getImportResponse_importStatus' - The status of the import job. If the status is @FAILED@, you can get the
-- reason for the failure from the @failureReason@ field.
--
-- 'failureReason', 'getImportResponse_failureReason' - A string that describes why an import job failed to complete.
--
-- 'mergeStrategy', 'getImportResponse_mergeStrategy' - The action taken when there was a conflict between an existing resource
-- and a resource in the import file.
--
-- 'httpStatus', 'getImportResponse_httpStatus' - The response's http status code.
newGetImportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetImportResponse
newGetImportResponse pHttpStatus_ =
  GetImportResponse'
    { resourceType = Prelude.Nothing,
      name = Prelude.Nothing,
      importId = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      importStatus = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      mergeStrategy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The type of resource imported.
getImportResponse_resourceType :: Lens.Lens' GetImportResponse (Prelude.Maybe ResourceType)
getImportResponse_resourceType = Lens.lens (\GetImportResponse' {resourceType} -> resourceType) (\s@GetImportResponse' {} a -> s {resourceType = a} :: GetImportResponse)

-- | The name given to the import job.
getImportResponse_name :: Lens.Lens' GetImportResponse (Prelude.Maybe Prelude.Text)
getImportResponse_name = Lens.lens (\GetImportResponse' {name} -> name) (\s@GetImportResponse' {} a -> s {name = a} :: GetImportResponse)

-- | The identifier for the specific import job.
getImportResponse_importId :: Lens.Lens' GetImportResponse (Prelude.Maybe Prelude.Text)
getImportResponse_importId = Lens.lens (\GetImportResponse' {importId} -> importId) (\s@GetImportResponse' {} a -> s {importId = a} :: GetImportResponse)

-- | A timestamp for the date and time that the import job was created.
getImportResponse_createdDate :: Lens.Lens' GetImportResponse (Prelude.Maybe Prelude.UTCTime)
getImportResponse_createdDate = Lens.lens (\GetImportResponse' {createdDate} -> createdDate) (\s@GetImportResponse' {} a -> s {createdDate = a} :: GetImportResponse) Prelude.. Lens.mapping Data._Time

-- | The status of the import job. If the status is @FAILED@, you can get the
-- reason for the failure from the @failureReason@ field.
getImportResponse_importStatus :: Lens.Lens' GetImportResponse (Prelude.Maybe ImportStatus)
getImportResponse_importStatus = Lens.lens (\GetImportResponse' {importStatus} -> importStatus) (\s@GetImportResponse' {} a -> s {importStatus = a} :: GetImportResponse)

-- | A string that describes why an import job failed to complete.
getImportResponse_failureReason :: Lens.Lens' GetImportResponse (Prelude.Maybe [Prelude.Text])
getImportResponse_failureReason = Lens.lens (\GetImportResponse' {failureReason} -> failureReason) (\s@GetImportResponse' {} a -> s {failureReason = a} :: GetImportResponse) Prelude.. Lens.mapping Lens.coerced

-- | The action taken when there was a conflict between an existing resource
-- and a resource in the import file.
getImportResponse_mergeStrategy :: Lens.Lens' GetImportResponse (Prelude.Maybe MergeStrategy)
getImportResponse_mergeStrategy = Lens.lens (\GetImportResponse' {mergeStrategy} -> mergeStrategy) (\s@GetImportResponse' {} a -> s {mergeStrategy = a} :: GetImportResponse)

-- | The response's http status code.
getImportResponse_httpStatus :: Lens.Lens' GetImportResponse Prelude.Int
getImportResponse_httpStatus = Lens.lens (\GetImportResponse' {httpStatus} -> httpStatus) (\s@GetImportResponse' {} a -> s {httpStatus = a} :: GetImportResponse)

instance Prelude.NFData GetImportResponse where
  rnf GetImportResponse' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf importId
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf importStatus
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf mergeStrategy
      `Prelude.seq` Prelude.rnf httpStatus
