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
-- Module      : Amazonka.Kendra.DescribeQuerySuggestionsBlockList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a block list used for query suggestions for an
-- index.
--
-- This is used to check the current settings that are applied to a block
-- list.
--
-- @DescribeQuerySuggestionsBlockList@ is currently not supported in the
-- Amazon Web Services GovCloud (US-West) region.
module Amazonka.Kendra.DescribeQuerySuggestionsBlockList
  ( -- * Creating a Request
    DescribeQuerySuggestionsBlockList (..),
    newDescribeQuerySuggestionsBlockList,

    -- * Request Lenses
    describeQuerySuggestionsBlockList_indexId,
    describeQuerySuggestionsBlockList_id,

    -- * Destructuring the Response
    DescribeQuerySuggestionsBlockListResponse (..),
    newDescribeQuerySuggestionsBlockListResponse,

    -- * Response Lenses
    describeQuerySuggestionsBlockListResponse_createdAt,
    describeQuerySuggestionsBlockListResponse_description,
    describeQuerySuggestionsBlockListResponse_errorMessage,
    describeQuerySuggestionsBlockListResponse_fileSizeBytes,
    describeQuerySuggestionsBlockListResponse_id,
    describeQuerySuggestionsBlockListResponse_indexId,
    describeQuerySuggestionsBlockListResponse_itemCount,
    describeQuerySuggestionsBlockListResponse_name,
    describeQuerySuggestionsBlockListResponse_roleArn,
    describeQuerySuggestionsBlockListResponse_sourceS3Path,
    describeQuerySuggestionsBlockListResponse_status,
    describeQuerySuggestionsBlockListResponse_updatedAt,
    describeQuerySuggestionsBlockListResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeQuerySuggestionsBlockList' smart constructor.
data DescribeQuerySuggestionsBlockList = DescribeQuerySuggestionsBlockList'
  { -- | The identifier of the index for the block list.
    indexId :: Prelude.Text,
    -- | The identifier of the block list you want to get information on.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeQuerySuggestionsBlockList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexId', 'describeQuerySuggestionsBlockList_indexId' - The identifier of the index for the block list.
--
-- 'id', 'describeQuerySuggestionsBlockList_id' - The identifier of the block list you want to get information on.
newDescribeQuerySuggestionsBlockList ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  DescribeQuerySuggestionsBlockList
newDescribeQuerySuggestionsBlockList pIndexId_ pId_ =
  DescribeQuerySuggestionsBlockList'
    { indexId =
        pIndexId_,
      id = pId_
    }

-- | The identifier of the index for the block list.
describeQuerySuggestionsBlockList_indexId :: Lens.Lens' DescribeQuerySuggestionsBlockList Prelude.Text
describeQuerySuggestionsBlockList_indexId = Lens.lens (\DescribeQuerySuggestionsBlockList' {indexId} -> indexId) (\s@DescribeQuerySuggestionsBlockList' {} a -> s {indexId = a} :: DescribeQuerySuggestionsBlockList)

-- | The identifier of the block list you want to get information on.
describeQuerySuggestionsBlockList_id :: Lens.Lens' DescribeQuerySuggestionsBlockList Prelude.Text
describeQuerySuggestionsBlockList_id = Lens.lens (\DescribeQuerySuggestionsBlockList' {id} -> id) (\s@DescribeQuerySuggestionsBlockList' {} a -> s {id = a} :: DescribeQuerySuggestionsBlockList)

instance
  Core.AWSRequest
    DescribeQuerySuggestionsBlockList
  where
  type
    AWSResponse DescribeQuerySuggestionsBlockList =
      DescribeQuerySuggestionsBlockListResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeQuerySuggestionsBlockListResponse'
            Prelude.<$> (x Data..?> "CreatedAt")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "ErrorMessage")
            Prelude.<*> (x Data..?> "FileSizeBytes")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "IndexId")
            Prelude.<*> (x Data..?> "ItemCount")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (x Data..?> "SourceS3Path")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "UpdatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeQuerySuggestionsBlockList
  where
  hashWithSalt
    _salt
    DescribeQuerySuggestionsBlockList' {..} =
      _salt
        `Prelude.hashWithSalt` indexId
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    DescribeQuerySuggestionsBlockList
  where
  rnf DescribeQuerySuggestionsBlockList' {..} =
    Prelude.rnf indexId `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    DescribeQuerySuggestionsBlockList
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.DescribeQuerySuggestionsBlockList" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeQuerySuggestionsBlockList
  where
  toJSON DescribeQuerySuggestionsBlockList' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance
  Data.ToPath
    DescribeQuerySuggestionsBlockList
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeQuerySuggestionsBlockList
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeQuerySuggestionsBlockListResponse' smart constructor.
data DescribeQuerySuggestionsBlockListResponse = DescribeQuerySuggestionsBlockListResponse'
  { -- | The date-time a block list for query suggestions was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The description for the block list.
    description :: Prelude.Maybe Prelude.Text,
    -- | The error message containing details if there are issues processing the
    -- block list.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The current size of the block list text file in S3.
    fileSizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | The identifier of the block list.
    id :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the index for the block list.
    indexId :: Prelude.Maybe Prelude.Text,
    -- | The current number of valid, non-empty words or phrases in the block
    -- list text file.
    itemCount :: Prelude.Maybe Prelude.Int,
    -- | The name of the block list.
    name :: Prelude.Maybe Prelude.Text,
    -- | The IAM (Identity and Access Management) role used by Amazon Kendra to
    -- access the block list text file in S3.
    --
    -- The role needs S3 read permissions to your file in S3 and needs to give
    -- STS (Security Token Service) assume role permissions to Amazon Kendra.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Shows the current S3 path to your block list text file in your S3
    -- bucket.
    --
    -- Each block word or phrase should be on a separate line in a text file.
    --
    -- For information on the current quota limits for block lists, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas for Amazon Kendra>.
    sourceS3Path :: Prelude.Maybe S3Path,
    -- | The current status of the block list. When the value is @ACTIVE@, the
    -- block list is ready for use.
    status :: Prelude.Maybe QuerySuggestionsBlockListStatus,
    -- | The date-time a block list for query suggestions was last updated.
    updatedAt :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeQuerySuggestionsBlockListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'describeQuerySuggestionsBlockListResponse_createdAt' - The date-time a block list for query suggestions was created.
--
-- 'description', 'describeQuerySuggestionsBlockListResponse_description' - The description for the block list.
--
-- 'errorMessage', 'describeQuerySuggestionsBlockListResponse_errorMessage' - The error message containing details if there are issues processing the
-- block list.
--
-- 'fileSizeBytes', 'describeQuerySuggestionsBlockListResponse_fileSizeBytes' - The current size of the block list text file in S3.
--
-- 'id', 'describeQuerySuggestionsBlockListResponse_id' - The identifier of the block list.
--
-- 'indexId', 'describeQuerySuggestionsBlockListResponse_indexId' - The identifier of the index for the block list.
--
-- 'itemCount', 'describeQuerySuggestionsBlockListResponse_itemCount' - The current number of valid, non-empty words or phrases in the block
-- list text file.
--
-- 'name', 'describeQuerySuggestionsBlockListResponse_name' - The name of the block list.
--
-- 'roleArn', 'describeQuerySuggestionsBlockListResponse_roleArn' - The IAM (Identity and Access Management) role used by Amazon Kendra to
-- access the block list text file in S3.
--
-- The role needs S3 read permissions to your file in S3 and needs to give
-- STS (Security Token Service) assume role permissions to Amazon Kendra.
--
-- 'sourceS3Path', 'describeQuerySuggestionsBlockListResponse_sourceS3Path' - Shows the current S3 path to your block list text file in your S3
-- bucket.
--
-- Each block word or phrase should be on a separate line in a text file.
--
-- For information on the current quota limits for block lists, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas for Amazon Kendra>.
--
-- 'status', 'describeQuerySuggestionsBlockListResponse_status' - The current status of the block list. When the value is @ACTIVE@, the
-- block list is ready for use.
--
-- 'updatedAt', 'describeQuerySuggestionsBlockListResponse_updatedAt' - The date-time a block list for query suggestions was last updated.
--
-- 'httpStatus', 'describeQuerySuggestionsBlockListResponse_httpStatus' - The response's http status code.
newDescribeQuerySuggestionsBlockListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeQuerySuggestionsBlockListResponse
newDescribeQuerySuggestionsBlockListResponse
  pHttpStatus_ =
    DescribeQuerySuggestionsBlockListResponse'
      { createdAt =
          Prelude.Nothing,
        description = Prelude.Nothing,
        errorMessage = Prelude.Nothing,
        fileSizeBytes = Prelude.Nothing,
        id = Prelude.Nothing,
        indexId = Prelude.Nothing,
        itemCount = Prelude.Nothing,
        name = Prelude.Nothing,
        roleArn = Prelude.Nothing,
        sourceS3Path = Prelude.Nothing,
        status = Prelude.Nothing,
        updatedAt = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The date-time a block list for query suggestions was created.
describeQuerySuggestionsBlockListResponse_createdAt :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe Prelude.UTCTime)
describeQuerySuggestionsBlockListResponse_createdAt = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {createdAt} -> createdAt) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {createdAt = a} :: DescribeQuerySuggestionsBlockListResponse) Prelude.. Lens.mapping Data._Time

-- | The description for the block list.
describeQuerySuggestionsBlockListResponse_description :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe Prelude.Text)
describeQuerySuggestionsBlockListResponse_description = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {description} -> description) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {description = a} :: DescribeQuerySuggestionsBlockListResponse)

-- | The error message containing details if there are issues processing the
-- block list.
describeQuerySuggestionsBlockListResponse_errorMessage :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe Prelude.Text)
describeQuerySuggestionsBlockListResponse_errorMessage = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {errorMessage} -> errorMessage) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {errorMessage = a} :: DescribeQuerySuggestionsBlockListResponse)

-- | The current size of the block list text file in S3.
describeQuerySuggestionsBlockListResponse_fileSizeBytes :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe Prelude.Integer)
describeQuerySuggestionsBlockListResponse_fileSizeBytes = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {fileSizeBytes} -> fileSizeBytes) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {fileSizeBytes = a} :: DescribeQuerySuggestionsBlockListResponse)

-- | The identifier of the block list.
describeQuerySuggestionsBlockListResponse_id :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe Prelude.Text)
describeQuerySuggestionsBlockListResponse_id = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {id} -> id) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {id = a} :: DescribeQuerySuggestionsBlockListResponse)

-- | The identifier of the index for the block list.
describeQuerySuggestionsBlockListResponse_indexId :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe Prelude.Text)
describeQuerySuggestionsBlockListResponse_indexId = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {indexId} -> indexId) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {indexId = a} :: DescribeQuerySuggestionsBlockListResponse)

-- | The current number of valid, non-empty words or phrases in the block
-- list text file.
describeQuerySuggestionsBlockListResponse_itemCount :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe Prelude.Int)
describeQuerySuggestionsBlockListResponse_itemCount = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {itemCount} -> itemCount) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {itemCount = a} :: DescribeQuerySuggestionsBlockListResponse)

-- | The name of the block list.
describeQuerySuggestionsBlockListResponse_name :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe Prelude.Text)
describeQuerySuggestionsBlockListResponse_name = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {name} -> name) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {name = a} :: DescribeQuerySuggestionsBlockListResponse)

-- | The IAM (Identity and Access Management) role used by Amazon Kendra to
-- access the block list text file in S3.
--
-- The role needs S3 read permissions to your file in S3 and needs to give
-- STS (Security Token Service) assume role permissions to Amazon Kendra.
describeQuerySuggestionsBlockListResponse_roleArn :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe Prelude.Text)
describeQuerySuggestionsBlockListResponse_roleArn = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {roleArn} -> roleArn) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {roleArn = a} :: DescribeQuerySuggestionsBlockListResponse)

-- | Shows the current S3 path to your block list text file in your S3
-- bucket.
--
-- Each block word or phrase should be on a separate line in a text file.
--
-- For information on the current quota limits for block lists, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas for Amazon Kendra>.
describeQuerySuggestionsBlockListResponse_sourceS3Path :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe S3Path)
describeQuerySuggestionsBlockListResponse_sourceS3Path = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {sourceS3Path} -> sourceS3Path) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {sourceS3Path = a} :: DescribeQuerySuggestionsBlockListResponse)

-- | The current status of the block list. When the value is @ACTIVE@, the
-- block list is ready for use.
describeQuerySuggestionsBlockListResponse_status :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe QuerySuggestionsBlockListStatus)
describeQuerySuggestionsBlockListResponse_status = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {status} -> status) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {status = a} :: DescribeQuerySuggestionsBlockListResponse)

-- | The date-time a block list for query suggestions was last updated.
describeQuerySuggestionsBlockListResponse_updatedAt :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe Prelude.UTCTime)
describeQuerySuggestionsBlockListResponse_updatedAt = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {updatedAt} -> updatedAt) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {updatedAt = a} :: DescribeQuerySuggestionsBlockListResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
describeQuerySuggestionsBlockListResponse_httpStatus :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse Prelude.Int
describeQuerySuggestionsBlockListResponse_httpStatus = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {httpStatus} -> httpStatus) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {httpStatus = a} :: DescribeQuerySuggestionsBlockListResponse)

instance
  Prelude.NFData
    DescribeQuerySuggestionsBlockListResponse
  where
  rnf DescribeQuerySuggestionsBlockListResponse' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf fileSizeBytes
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf itemCount
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf sourceS3Path
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf httpStatus
