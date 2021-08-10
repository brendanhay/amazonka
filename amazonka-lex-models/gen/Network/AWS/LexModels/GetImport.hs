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
-- Module      : Network.AWS.LexModels.GetImport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an import job started with the @StartImport@
-- operation.
module Network.AWS.LexModels.GetImport
  ( -- * Creating a Request
    GetImport (..),
    newGetImport,

    -- * Request Lenses
    getImport_importId,

    -- * Destructuring the Response
    GetImportResponse (..),
    newGetImportResponse,

    -- * Response Lenses
    getImportResponse_createdDate,
    getImportResponse_mergeStrategy,
    getImportResponse_importId,
    getImportResponse_resourceType,
    getImportResponse_name,
    getImportResponse_failureReason,
    getImportResponse_importStatus,
    getImportResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
            Prelude.<$> (x Core..?> "createdDate")
            Prelude.<*> (x Core..?> "mergeStrategy")
            Prelude.<*> (x Core..?> "importId")
            Prelude.<*> (x Core..?> "resourceType")
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "failureReason" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "importStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetImport

instance Prelude.NFData GetImport

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
  { -- | A timestamp for the date and time that the import job was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The action taken when there was a conflict between an existing resource
    -- and a resource in the import file.
    mergeStrategy :: Prelude.Maybe MergeStrategy,
    -- | The identifier for the specific import job.
    importId :: Prelude.Maybe Prelude.Text,
    -- | The type of resource imported.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The name given to the import job.
    name :: Prelude.Maybe Prelude.Text,
    -- | A string that describes why an import job failed to complete.
    failureReason :: Prelude.Maybe [Prelude.Text],
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
-- 'createdDate', 'getImportResponse_createdDate' - A timestamp for the date and time that the import job was created.
--
-- 'mergeStrategy', 'getImportResponse_mergeStrategy' - The action taken when there was a conflict between an existing resource
-- and a resource in the import file.
--
-- 'importId', 'getImportResponse_importId' - The identifier for the specific import job.
--
-- 'resourceType', 'getImportResponse_resourceType' - The type of resource imported.
--
-- 'name', 'getImportResponse_name' - The name given to the import job.
--
-- 'failureReason', 'getImportResponse_failureReason' - A string that describes why an import job failed to complete.
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
    { createdDate = Prelude.Nothing,
      mergeStrategy = Prelude.Nothing,
      importId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      name = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      importStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A timestamp for the date and time that the import job was created.
getImportResponse_createdDate :: Lens.Lens' GetImportResponse (Prelude.Maybe Prelude.UTCTime)
getImportResponse_createdDate = Lens.lens (\GetImportResponse' {createdDate} -> createdDate) (\s@GetImportResponse' {} a -> s {createdDate = a} :: GetImportResponse) Prelude.. Lens.mapping Core._Time

-- | The action taken when there was a conflict between an existing resource
-- and a resource in the import file.
getImportResponse_mergeStrategy :: Lens.Lens' GetImportResponse (Prelude.Maybe MergeStrategy)
getImportResponse_mergeStrategy = Lens.lens (\GetImportResponse' {mergeStrategy} -> mergeStrategy) (\s@GetImportResponse' {} a -> s {mergeStrategy = a} :: GetImportResponse)

-- | The identifier for the specific import job.
getImportResponse_importId :: Lens.Lens' GetImportResponse (Prelude.Maybe Prelude.Text)
getImportResponse_importId = Lens.lens (\GetImportResponse' {importId} -> importId) (\s@GetImportResponse' {} a -> s {importId = a} :: GetImportResponse)

-- | The type of resource imported.
getImportResponse_resourceType :: Lens.Lens' GetImportResponse (Prelude.Maybe ResourceType)
getImportResponse_resourceType = Lens.lens (\GetImportResponse' {resourceType} -> resourceType) (\s@GetImportResponse' {} a -> s {resourceType = a} :: GetImportResponse)

-- | The name given to the import job.
getImportResponse_name :: Lens.Lens' GetImportResponse (Prelude.Maybe Prelude.Text)
getImportResponse_name = Lens.lens (\GetImportResponse' {name} -> name) (\s@GetImportResponse' {} a -> s {name = a} :: GetImportResponse)

-- | A string that describes why an import job failed to complete.
getImportResponse_failureReason :: Lens.Lens' GetImportResponse (Prelude.Maybe [Prelude.Text])
getImportResponse_failureReason = Lens.lens (\GetImportResponse' {failureReason} -> failureReason) (\s@GetImportResponse' {} a -> s {failureReason = a} :: GetImportResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The status of the import job. If the status is @FAILED@, you can get the
-- reason for the failure from the @failureReason@ field.
getImportResponse_importStatus :: Lens.Lens' GetImportResponse (Prelude.Maybe ImportStatus)
getImportResponse_importStatus = Lens.lens (\GetImportResponse' {importStatus} -> importStatus) (\s@GetImportResponse' {} a -> s {importStatus = a} :: GetImportResponse)

-- | The response's http status code.
getImportResponse_httpStatus :: Lens.Lens' GetImportResponse Prelude.Int
getImportResponse_httpStatus = Lens.lens (\GetImportResponse' {httpStatus} -> httpStatus) (\s@GetImportResponse' {} a -> s {httpStatus = a} :: GetImportResponse)

instance Prelude.NFData GetImportResponse
