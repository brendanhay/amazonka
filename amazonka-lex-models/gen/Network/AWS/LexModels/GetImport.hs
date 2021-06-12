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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetImport' smart constructor.
data GetImport = GetImport'
  { -- | The identifier of the import job information to return.
    importId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetImport
newGetImport pImportId_ =
  GetImport' {importId = pImportId_}

-- | The identifier of the import job information to return.
getImport_importId :: Lens.Lens' GetImport Core.Text
getImport_importId = Lens.lens (\GetImport' {importId} -> importId) (\s@GetImport' {} a -> s {importId = a} :: GetImport)

instance Core.AWSRequest GetImport where
  type AWSResponse GetImport = GetImportResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetImportResponse'
            Core.<$> (x Core..?> "createdDate")
            Core.<*> (x Core..?> "mergeStrategy")
            Core.<*> (x Core..?> "importId")
            Core.<*> (x Core..?> "resourceType")
            Core.<*> (x Core..?> "name")
            Core.<*> (x Core..?> "failureReason" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "importStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetImport

instance Core.NFData GetImport

instance Core.ToHeaders GetImport where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetImport where
  toPath GetImport' {..} =
    Core.mconcat ["/imports/", Core.toBS importId]

instance Core.ToQuery GetImport where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetImportResponse' smart constructor.
data GetImportResponse = GetImportResponse'
  { -- | A timestamp for the date and time that the import job was created.
    createdDate :: Core.Maybe Core.POSIX,
    -- | The action taken when there was a conflict between an existing resource
    -- and a resource in the import file.
    mergeStrategy :: Core.Maybe MergeStrategy,
    -- | The identifier for the specific import job.
    importId :: Core.Maybe Core.Text,
    -- | The type of resource imported.
    resourceType :: Core.Maybe ResourceType,
    -- | The name given to the import job.
    name :: Core.Maybe Core.Text,
    -- | A string that describes why an import job failed to complete.
    failureReason :: Core.Maybe [Core.Text],
    -- | The status of the import job. If the status is @FAILED@, you can get the
    -- reason for the failure from the @failureReason@ field.
    importStatus :: Core.Maybe ImportStatus,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetImportResponse
newGetImportResponse pHttpStatus_ =
  GetImportResponse'
    { createdDate = Core.Nothing,
      mergeStrategy = Core.Nothing,
      importId = Core.Nothing,
      resourceType = Core.Nothing,
      name = Core.Nothing,
      failureReason = Core.Nothing,
      importStatus = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A timestamp for the date and time that the import job was created.
getImportResponse_createdDate :: Lens.Lens' GetImportResponse (Core.Maybe Core.UTCTime)
getImportResponse_createdDate = Lens.lens (\GetImportResponse' {createdDate} -> createdDate) (\s@GetImportResponse' {} a -> s {createdDate = a} :: GetImportResponse) Core.. Lens.mapping Core._Time

-- | The action taken when there was a conflict between an existing resource
-- and a resource in the import file.
getImportResponse_mergeStrategy :: Lens.Lens' GetImportResponse (Core.Maybe MergeStrategy)
getImportResponse_mergeStrategy = Lens.lens (\GetImportResponse' {mergeStrategy} -> mergeStrategy) (\s@GetImportResponse' {} a -> s {mergeStrategy = a} :: GetImportResponse)

-- | The identifier for the specific import job.
getImportResponse_importId :: Lens.Lens' GetImportResponse (Core.Maybe Core.Text)
getImportResponse_importId = Lens.lens (\GetImportResponse' {importId} -> importId) (\s@GetImportResponse' {} a -> s {importId = a} :: GetImportResponse)

-- | The type of resource imported.
getImportResponse_resourceType :: Lens.Lens' GetImportResponse (Core.Maybe ResourceType)
getImportResponse_resourceType = Lens.lens (\GetImportResponse' {resourceType} -> resourceType) (\s@GetImportResponse' {} a -> s {resourceType = a} :: GetImportResponse)

-- | The name given to the import job.
getImportResponse_name :: Lens.Lens' GetImportResponse (Core.Maybe Core.Text)
getImportResponse_name = Lens.lens (\GetImportResponse' {name} -> name) (\s@GetImportResponse' {} a -> s {name = a} :: GetImportResponse)

-- | A string that describes why an import job failed to complete.
getImportResponse_failureReason :: Lens.Lens' GetImportResponse (Core.Maybe [Core.Text])
getImportResponse_failureReason = Lens.lens (\GetImportResponse' {failureReason} -> failureReason) (\s@GetImportResponse' {} a -> s {failureReason = a} :: GetImportResponse) Core.. Lens.mapping Lens._Coerce

-- | The status of the import job. If the status is @FAILED@, you can get the
-- reason for the failure from the @failureReason@ field.
getImportResponse_importStatus :: Lens.Lens' GetImportResponse (Core.Maybe ImportStatus)
getImportResponse_importStatus = Lens.lens (\GetImportResponse' {importStatus} -> importStatus) (\s@GetImportResponse' {} a -> s {importStatus = a} :: GetImportResponse)

-- | The response's http status code.
getImportResponse_httpStatus :: Lens.Lens' GetImportResponse Core.Int
getImportResponse_httpStatus = Lens.lens (\GetImportResponse' {httpStatus} -> httpStatus) (\s@GetImportResponse' {} a -> s {httpStatus = a} :: GetImportResponse)

instance Core.NFData GetImportResponse
