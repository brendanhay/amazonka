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
-- Module      : Network.AWS.LexV2Models.DescribeImport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specific import.
module Network.AWS.LexV2Models.DescribeImport
  ( -- * Creating a Request
    DescribeImport (..),
    newDescribeImport,

    -- * Request Lenses
    describeImport_importId,

    -- * Destructuring the Response
    DescribeImportResponse (..),
    newDescribeImportResponse,

    -- * Response Lenses
    describeImportResponse_resourceSpecification,
    describeImportResponse_importId,
    describeImportResponse_importedResourceId,
    describeImportResponse_lastUpdatedDateTime,
    describeImportResponse_importedResourceName,
    describeImportResponse_failureReasons,
    describeImportResponse_creationDateTime,
    describeImportResponse_mergeStrategy,
    describeImportResponse_importStatus,
    describeImportResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeImport' smart constructor.
data DescribeImport = DescribeImport'
  { -- | The unique identifier of the import to describe.
    importId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importId', 'describeImport_importId' - The unique identifier of the import to describe.
newDescribeImport ::
  -- | 'importId'
  Prelude.Text ->
  DescribeImport
newDescribeImport pImportId_ =
  DescribeImport' {importId = pImportId_}

-- | The unique identifier of the import to describe.
describeImport_importId :: Lens.Lens' DescribeImport Prelude.Text
describeImport_importId = Lens.lens (\DescribeImport' {importId} -> importId) (\s@DescribeImport' {} a -> s {importId = a} :: DescribeImport)

instance Core.AWSRequest DescribeImport where
  type
    AWSResponse DescribeImport =
      DescribeImportResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImportResponse'
            Prelude.<$> (x Core..?> "resourceSpecification")
            Prelude.<*> (x Core..?> "importId")
            Prelude.<*> (x Core..?> "importedResourceId")
            Prelude.<*> (x Core..?> "lastUpdatedDateTime")
            Prelude.<*> (x Core..?> "importedResourceName")
            Prelude.<*> (x Core..?> "failureReasons" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "creationDateTime")
            Prelude.<*> (x Core..?> "mergeStrategy")
            Prelude.<*> (x Core..?> "importStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeImport

instance Prelude.NFData DescribeImport

instance Core.ToHeaders DescribeImport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeImport where
  toPath DescribeImport' {..} =
    Prelude.mconcat
      ["/imports/", Core.toBS importId, "/"]

instance Core.ToQuery DescribeImport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeImportResponse' smart constructor.
data DescribeImportResponse = DescribeImportResponse'
  { -- | The specifications of the imported bot or bot locale.
    resourceSpecification :: Prelude.Maybe ImportResourceSpecification,
    -- | The unique identifier of the described import.
    importId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier that Amazon Lex assigned to the resource created
    -- by the import.
    importedResourceId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the import was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the imported resource.
    importedResourceName :: Prelude.Maybe Prelude.Text,
    -- | If the @importStatus@ field is @Failed@, this provides one or more
    -- reasons for the failure.
    failureReasons :: Prelude.Maybe [Prelude.Text],
    -- | The date and time that the import was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The strategy used when there was a name conflict between the imported
    -- resource and an existing resource. When the merge strategy is
    -- @FailOnConflict@ existing resources are not overwritten and the import
    -- fails.
    mergeStrategy :: Prelude.Maybe MergeStrategy,
    -- | The status of the import process. When the status is @Completed@ the
    -- resource is imported and ready for use.
    importStatus :: Prelude.Maybe ImportStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceSpecification', 'describeImportResponse_resourceSpecification' - The specifications of the imported bot or bot locale.
--
-- 'importId', 'describeImportResponse_importId' - The unique identifier of the described import.
--
-- 'importedResourceId', 'describeImportResponse_importedResourceId' - The unique identifier that Amazon Lex assigned to the resource created
-- by the import.
--
-- 'lastUpdatedDateTime', 'describeImportResponse_lastUpdatedDateTime' - The date and time that the import was last updated.
--
-- 'importedResourceName', 'describeImportResponse_importedResourceName' - The name of the imported resource.
--
-- 'failureReasons', 'describeImportResponse_failureReasons' - If the @importStatus@ field is @Failed@, this provides one or more
-- reasons for the failure.
--
-- 'creationDateTime', 'describeImportResponse_creationDateTime' - The date and time that the import was created.
--
-- 'mergeStrategy', 'describeImportResponse_mergeStrategy' - The strategy used when there was a name conflict between the imported
-- resource and an existing resource. When the merge strategy is
-- @FailOnConflict@ existing resources are not overwritten and the import
-- fails.
--
-- 'importStatus', 'describeImportResponse_importStatus' - The status of the import process. When the status is @Completed@ the
-- resource is imported and ready for use.
--
-- 'httpStatus', 'describeImportResponse_httpStatus' - The response's http status code.
newDescribeImportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeImportResponse
newDescribeImportResponse pHttpStatus_ =
  DescribeImportResponse'
    { resourceSpecification =
        Prelude.Nothing,
      importId = Prelude.Nothing,
      importedResourceId = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      importedResourceName = Prelude.Nothing,
      failureReasons = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      mergeStrategy = Prelude.Nothing,
      importStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The specifications of the imported bot or bot locale.
describeImportResponse_resourceSpecification :: Lens.Lens' DescribeImportResponse (Prelude.Maybe ImportResourceSpecification)
describeImportResponse_resourceSpecification = Lens.lens (\DescribeImportResponse' {resourceSpecification} -> resourceSpecification) (\s@DescribeImportResponse' {} a -> s {resourceSpecification = a} :: DescribeImportResponse)

-- | The unique identifier of the described import.
describeImportResponse_importId :: Lens.Lens' DescribeImportResponse (Prelude.Maybe Prelude.Text)
describeImportResponse_importId = Lens.lens (\DescribeImportResponse' {importId} -> importId) (\s@DescribeImportResponse' {} a -> s {importId = a} :: DescribeImportResponse)

-- | The unique identifier that Amazon Lex assigned to the resource created
-- by the import.
describeImportResponse_importedResourceId :: Lens.Lens' DescribeImportResponse (Prelude.Maybe Prelude.Text)
describeImportResponse_importedResourceId = Lens.lens (\DescribeImportResponse' {importedResourceId} -> importedResourceId) (\s@DescribeImportResponse' {} a -> s {importedResourceId = a} :: DescribeImportResponse)

-- | The date and time that the import was last updated.
describeImportResponse_lastUpdatedDateTime :: Lens.Lens' DescribeImportResponse (Prelude.Maybe Prelude.UTCTime)
describeImportResponse_lastUpdatedDateTime = Lens.lens (\DescribeImportResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DescribeImportResponse' {} a -> s {lastUpdatedDateTime = a} :: DescribeImportResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the imported resource.
describeImportResponse_importedResourceName :: Lens.Lens' DescribeImportResponse (Prelude.Maybe Prelude.Text)
describeImportResponse_importedResourceName = Lens.lens (\DescribeImportResponse' {importedResourceName} -> importedResourceName) (\s@DescribeImportResponse' {} a -> s {importedResourceName = a} :: DescribeImportResponse)

-- | If the @importStatus@ field is @Failed@, this provides one or more
-- reasons for the failure.
describeImportResponse_failureReasons :: Lens.Lens' DescribeImportResponse (Prelude.Maybe [Prelude.Text])
describeImportResponse_failureReasons = Lens.lens (\DescribeImportResponse' {failureReasons} -> failureReasons) (\s@DescribeImportResponse' {} a -> s {failureReasons = a} :: DescribeImportResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date and time that the import was created.
describeImportResponse_creationDateTime :: Lens.Lens' DescribeImportResponse (Prelude.Maybe Prelude.UTCTime)
describeImportResponse_creationDateTime = Lens.lens (\DescribeImportResponse' {creationDateTime} -> creationDateTime) (\s@DescribeImportResponse' {} a -> s {creationDateTime = a} :: DescribeImportResponse) Prelude.. Lens.mapping Core._Time

-- | The strategy used when there was a name conflict between the imported
-- resource and an existing resource. When the merge strategy is
-- @FailOnConflict@ existing resources are not overwritten and the import
-- fails.
describeImportResponse_mergeStrategy :: Lens.Lens' DescribeImportResponse (Prelude.Maybe MergeStrategy)
describeImportResponse_mergeStrategy = Lens.lens (\DescribeImportResponse' {mergeStrategy} -> mergeStrategy) (\s@DescribeImportResponse' {} a -> s {mergeStrategy = a} :: DescribeImportResponse)

-- | The status of the import process. When the status is @Completed@ the
-- resource is imported and ready for use.
describeImportResponse_importStatus :: Lens.Lens' DescribeImportResponse (Prelude.Maybe ImportStatus)
describeImportResponse_importStatus = Lens.lens (\DescribeImportResponse' {importStatus} -> importStatus) (\s@DescribeImportResponse' {} a -> s {importStatus = a} :: DescribeImportResponse)

-- | The response's http status code.
describeImportResponse_httpStatus :: Lens.Lens' DescribeImportResponse Prelude.Int
describeImportResponse_httpStatus = Lens.lens (\DescribeImportResponse' {httpStatus} -> httpStatus) (\s@DescribeImportResponse' {} a -> s {httpStatus = a} :: DescribeImportResponse)

instance Prelude.NFData DescribeImportResponse
