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
-- Module      : Amazonka.ServiceCatalog.DescribeRecord
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.ServiceCatalog.DescribeRecord
  ( -- * Creating a Request
    DescribeRecord (..),
    newDescribeRecord,

    -- * Request Lenses
    describeRecord_pageToken,
    describeRecord_pageSize,
    describeRecord_acceptLanguage,
    describeRecord_id,

    -- * Destructuring the Response
    DescribeRecordResponse (..),
    newDescribeRecordResponse,

    -- * Response Lenses
    describeRecordResponse_nextPageToken,
    describeRecordResponse_recordDetail,
    describeRecordResponse_recordOutputs,
    describeRecordResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDescribeRecord' smart constructor.
data DescribeRecord = DescribeRecord'
  { -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The record identifier of the provisioned product. This identifier is
    -- returned by the request operation.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'describeRecord_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'pageSize', 'describeRecord_pageSize' - The maximum number of items to return with this call.
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
  Prelude.Text ->
  DescribeRecord
newDescribeRecord pId_ =
  DescribeRecord'
    { pageToken = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing,
      id = pId_
    }

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
describeRecord_pageToken :: Lens.Lens' DescribeRecord (Prelude.Maybe Prelude.Text)
describeRecord_pageToken = Lens.lens (\DescribeRecord' {pageToken} -> pageToken) (\s@DescribeRecord' {} a -> s {pageToken = a} :: DescribeRecord)

-- | The maximum number of items to return with this call.
describeRecord_pageSize :: Lens.Lens' DescribeRecord (Prelude.Maybe Prelude.Natural)
describeRecord_pageSize = Lens.lens (\DescribeRecord' {pageSize} -> pageSize) (\s@DescribeRecord' {} a -> s {pageSize = a} :: DescribeRecord)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeRecord_acceptLanguage :: Lens.Lens' DescribeRecord (Prelude.Maybe Prelude.Text)
describeRecord_acceptLanguage = Lens.lens (\DescribeRecord' {acceptLanguage} -> acceptLanguage) (\s@DescribeRecord' {} a -> s {acceptLanguage = a} :: DescribeRecord)

-- | The record identifier of the provisioned product. This identifier is
-- returned by the request operation.
describeRecord_id :: Lens.Lens' DescribeRecord Prelude.Text
describeRecord_id = Lens.lens (\DescribeRecord' {id} -> id) (\s@DescribeRecord' {} a -> s {id = a} :: DescribeRecord)

instance Core.AWSRequest DescribeRecord where
  type
    AWSResponse DescribeRecord =
      DescribeRecordResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRecordResponse'
            Prelude.<$> (x Data..?> "NextPageToken")
            Prelude.<*> (x Data..?> "RecordDetail")
            Prelude.<*> (x Data..?> "RecordOutputs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRecord where
  hashWithSalt _salt DescribeRecord' {..} =
    _salt `Prelude.hashWithSalt` pageToken
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` id

instance Prelude.NFData DescribeRecord where
  rnf DescribeRecord' {..} =
    Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders DescribeRecord where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.DescribeRecord" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeRecord where
  toJSON DescribeRecord' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PageToken" Data..=) Prelude.<$> pageToken,
            ("PageSize" Data..=) Prelude.<$> pageSize,
            ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath DescribeRecord where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeRecord where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRecordResponse' smart constructor.
data DescribeRecordResponse = DescribeRecordResponse'
  { -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the product.
    recordDetail :: Prelude.Maybe RecordDetail,
    -- | Information about the product created as the result of a request. For
    -- example, the output for a CloudFormation-backed product that creates an
    -- S3 bucket would include the S3 bucket URL.
    recordOutputs :: Prelude.Maybe [RecordOutput],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRecordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'describeRecordResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'recordDetail', 'describeRecordResponse_recordDetail' - Information about the product.
--
-- 'recordOutputs', 'describeRecordResponse_recordOutputs' - Information about the product created as the result of a request. For
-- example, the output for a CloudFormation-backed product that creates an
-- S3 bucket would include the S3 bucket URL.
--
-- 'httpStatus', 'describeRecordResponse_httpStatus' - The response's http status code.
newDescribeRecordResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRecordResponse
newDescribeRecordResponse pHttpStatus_ =
  DescribeRecordResponse'
    { nextPageToken =
        Prelude.Nothing,
      recordDetail = Prelude.Nothing,
      recordOutputs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
describeRecordResponse_nextPageToken :: Lens.Lens' DescribeRecordResponse (Prelude.Maybe Prelude.Text)
describeRecordResponse_nextPageToken = Lens.lens (\DescribeRecordResponse' {nextPageToken} -> nextPageToken) (\s@DescribeRecordResponse' {} a -> s {nextPageToken = a} :: DescribeRecordResponse)

-- | Information about the product.
describeRecordResponse_recordDetail :: Lens.Lens' DescribeRecordResponse (Prelude.Maybe RecordDetail)
describeRecordResponse_recordDetail = Lens.lens (\DescribeRecordResponse' {recordDetail} -> recordDetail) (\s@DescribeRecordResponse' {} a -> s {recordDetail = a} :: DescribeRecordResponse)

-- | Information about the product created as the result of a request. For
-- example, the output for a CloudFormation-backed product that creates an
-- S3 bucket would include the S3 bucket URL.
describeRecordResponse_recordOutputs :: Lens.Lens' DescribeRecordResponse (Prelude.Maybe [RecordOutput])
describeRecordResponse_recordOutputs = Lens.lens (\DescribeRecordResponse' {recordOutputs} -> recordOutputs) (\s@DescribeRecordResponse' {} a -> s {recordOutputs = a} :: DescribeRecordResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeRecordResponse_httpStatus :: Lens.Lens' DescribeRecordResponse Prelude.Int
describeRecordResponse_httpStatus = Lens.lens (\DescribeRecordResponse' {httpStatus} -> httpStatus) (\s@DescribeRecordResponse' {} a -> s {httpStatus = a} :: DescribeRecordResponse)

instance Prelude.NFData DescribeRecordResponse where
  rnf DescribeRecordResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf recordDetail
      `Prelude.seq` Prelude.rnf recordOutputs
      `Prelude.seq` Prelude.rnf httpStatus
