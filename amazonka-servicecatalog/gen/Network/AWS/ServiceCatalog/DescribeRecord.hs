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
-- Module      : Network.AWS.ServiceCatalog.DescribeRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified request operation.
--
-- Use this operation after calling a request operation (for example,
-- ProvisionProduct, TerminateProvisionedProduct, or
-- UpdateProvisionedProduct).
--
-- If a provisioned product was transferred to a new owner using
-- UpdateProvisionedProductProperties, the new owner will be able to
-- describe all past records for that product. The previous owner will no
-- longer be able to describe the records, but will be able to use
-- ListRecordHistory to see the product\'s history from when he was the
-- owner.
module Network.AWS.ServiceCatalog.DescribeRecord
  ( -- * Creating a Request
    DescribeRecord (..),
    newDescribeRecord,

    -- * Request Lenses
    describeRecord_pageSize,
    describeRecord_pageToken,
    describeRecord_acceptLanguage,
    describeRecord_id,

    -- * Destructuring the Response
    DescribeRecordResponse (..),
    newDescribeRecordResponse,

    -- * Response Lenses
    describeRecordResponse_recordDetail,
    describeRecordResponse_recordOutputs,
    describeRecordResponse_nextPageToken,
    describeRecordResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDescribeRecord' smart constructor.
data DescribeRecord = DescribeRecord'
  { -- | The maximum number of items to return with this call.
    pageSize :: Core.Maybe Core.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Core.Maybe Core.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The record identifier of the provisioned product. This identifier is
    -- returned by the request operation.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'describeRecord_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'describeRecord_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'acceptLanguage', 'describeRecord_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'id', 'describeRecord_id' - The record identifier of the provisioned product. This identifier is
-- returned by the request operation.
newDescribeRecord ::
  -- | 'id'
  Core.Text ->
  DescribeRecord
newDescribeRecord pId_ =
  DescribeRecord'
    { pageSize = Core.Nothing,
      pageToken = Core.Nothing,
      acceptLanguage = Core.Nothing,
      id = pId_
    }

-- | The maximum number of items to return with this call.
describeRecord_pageSize :: Lens.Lens' DescribeRecord (Core.Maybe Core.Natural)
describeRecord_pageSize = Lens.lens (\DescribeRecord' {pageSize} -> pageSize) (\s@DescribeRecord' {} a -> s {pageSize = a} :: DescribeRecord)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
describeRecord_pageToken :: Lens.Lens' DescribeRecord (Core.Maybe Core.Text)
describeRecord_pageToken = Lens.lens (\DescribeRecord' {pageToken} -> pageToken) (\s@DescribeRecord' {} a -> s {pageToken = a} :: DescribeRecord)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeRecord_acceptLanguage :: Lens.Lens' DescribeRecord (Core.Maybe Core.Text)
describeRecord_acceptLanguage = Lens.lens (\DescribeRecord' {acceptLanguage} -> acceptLanguage) (\s@DescribeRecord' {} a -> s {acceptLanguage = a} :: DescribeRecord)

-- | The record identifier of the provisioned product. This identifier is
-- returned by the request operation.
describeRecord_id :: Lens.Lens' DescribeRecord Core.Text
describeRecord_id = Lens.lens (\DescribeRecord' {id} -> id) (\s@DescribeRecord' {} a -> s {id = a} :: DescribeRecord)

instance Core.AWSRequest DescribeRecord where
  type
    AWSResponse DescribeRecord =
      DescribeRecordResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRecordResponse'
            Core.<$> (x Core..?> "RecordDetail")
            Core.<*> (x Core..?> "RecordOutputs" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeRecord

instance Core.NFData DescribeRecord

instance Core.ToHeaders DescribeRecord where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DescribeRecord" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeRecord where
  toJSON DescribeRecord' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PageSize" Core..=) Core.<$> pageSize,
            ("PageToken" Core..=) Core.<$> pageToken,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("Id" Core..= id)
          ]
      )

instance Core.ToPath DescribeRecord where
  toPath = Core.const "/"

instance Core.ToQuery DescribeRecord where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeRecordResponse' smart constructor.
data DescribeRecordResponse = DescribeRecordResponse'
  { -- | Information about the product.
    recordDetail :: Core.Maybe RecordDetail,
    -- | Information about the product created as the result of a request. For
    -- example, the output for a CloudFormation-backed product that creates an
    -- S3 bucket would include the S3 bucket URL.
    recordOutputs :: Core.Maybe [RecordOutput],
    -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeRecordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordDetail', 'describeRecordResponse_recordDetail' - Information about the product.
--
-- 'recordOutputs', 'describeRecordResponse_recordOutputs' - Information about the product created as the result of a request. For
-- example, the output for a CloudFormation-backed product that creates an
-- S3 bucket would include the S3 bucket URL.
--
-- 'nextPageToken', 'describeRecordResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'httpStatus', 'describeRecordResponse_httpStatus' - The response's http status code.
newDescribeRecordResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeRecordResponse
newDescribeRecordResponse pHttpStatus_ =
  DescribeRecordResponse'
    { recordDetail =
        Core.Nothing,
      recordOutputs = Core.Nothing,
      nextPageToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the product.
describeRecordResponse_recordDetail :: Lens.Lens' DescribeRecordResponse (Core.Maybe RecordDetail)
describeRecordResponse_recordDetail = Lens.lens (\DescribeRecordResponse' {recordDetail} -> recordDetail) (\s@DescribeRecordResponse' {} a -> s {recordDetail = a} :: DescribeRecordResponse)

-- | Information about the product created as the result of a request. For
-- example, the output for a CloudFormation-backed product that creates an
-- S3 bucket would include the S3 bucket URL.
describeRecordResponse_recordOutputs :: Lens.Lens' DescribeRecordResponse (Core.Maybe [RecordOutput])
describeRecordResponse_recordOutputs = Lens.lens (\DescribeRecordResponse' {recordOutputs} -> recordOutputs) (\s@DescribeRecordResponse' {} a -> s {recordOutputs = a} :: DescribeRecordResponse) Core.. Lens.mapping Lens._Coerce

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
describeRecordResponse_nextPageToken :: Lens.Lens' DescribeRecordResponse (Core.Maybe Core.Text)
describeRecordResponse_nextPageToken = Lens.lens (\DescribeRecordResponse' {nextPageToken} -> nextPageToken) (\s@DescribeRecordResponse' {} a -> s {nextPageToken = a} :: DescribeRecordResponse)

-- | The response's http status code.
describeRecordResponse_httpStatus :: Lens.Lens' DescribeRecordResponse Core.Int
describeRecordResponse_httpStatus = Lens.lens (\DescribeRecordResponse' {httpStatus} -> httpStatus) (\s@DescribeRecordResponse' {} a -> s {httpStatus = a} :: DescribeRecordResponse)

instance Core.NFData DescribeRecordResponse
