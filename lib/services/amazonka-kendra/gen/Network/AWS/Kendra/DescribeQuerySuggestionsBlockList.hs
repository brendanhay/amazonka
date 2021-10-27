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
-- Module      : Network.AWS.Kendra.DescribeQuerySuggestionsBlockList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a block list used for query suggestions for an index.
--
-- This is used to check the current settings that are applied to a block
-- list.
module Network.AWS.Kendra.DescribeQuerySuggestionsBlockList
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
    describeQuerySuggestionsBlockListResponse_status,
    describeQuerySuggestionsBlockListResponse_fileSizeBytes,
    describeQuerySuggestionsBlockListResponse_createdAt,
    describeQuerySuggestionsBlockListResponse_sourceS3Path,
    describeQuerySuggestionsBlockListResponse_name,
    describeQuerySuggestionsBlockListResponse_id,
    describeQuerySuggestionsBlockListResponse_updatedAt,
    describeQuerySuggestionsBlockListResponse_errorMessage,
    describeQuerySuggestionsBlockListResponse_indexId,
    describeQuerySuggestionsBlockListResponse_itemCount,
    describeQuerySuggestionsBlockListResponse_description,
    describeQuerySuggestionsBlockListResponse_roleArn,
    describeQuerySuggestionsBlockListResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeQuerySuggestionsBlockList' smart constructor.
data DescribeQuerySuggestionsBlockList = DescribeQuerySuggestionsBlockList'
  { -- | The identifier of the index for the block list.
    indexId :: Prelude.Text,
    -- | The unique identifier of the block list.
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
-- 'id', 'describeQuerySuggestionsBlockList_id' - The unique identifier of the block list.
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

-- | The unique identifier of the block list.
describeQuerySuggestionsBlockList_id :: Lens.Lens' DescribeQuerySuggestionsBlockList Prelude.Text
describeQuerySuggestionsBlockList_id = Lens.lens (\DescribeQuerySuggestionsBlockList' {id} -> id) (\s@DescribeQuerySuggestionsBlockList' {} a -> s {id = a} :: DescribeQuerySuggestionsBlockList)

instance
  Core.AWSRequest
    DescribeQuerySuggestionsBlockList
  where
  type
    AWSResponse DescribeQuerySuggestionsBlockList =
      DescribeQuerySuggestionsBlockListResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeQuerySuggestionsBlockListResponse'
            Prelude.<$> (x Core..?> "Status")
              Prelude.<*> (x Core..?> "FileSizeBytes")
              Prelude.<*> (x Core..?> "CreatedAt")
              Prelude.<*> (x Core..?> "SourceS3Path")
              Prelude.<*> (x Core..?> "Name")
              Prelude.<*> (x Core..?> "Id")
              Prelude.<*> (x Core..?> "UpdatedAt")
              Prelude.<*> (x Core..?> "ErrorMessage")
              Prelude.<*> (x Core..?> "IndexId")
              Prelude.<*> (x Core..?> "ItemCount")
              Prelude.<*> (x Core..?> "Description")
              Prelude.<*> (x Core..?> "RoleArn")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeQuerySuggestionsBlockList

instance
  Prelude.NFData
    DescribeQuerySuggestionsBlockList

instance
  Core.ToHeaders
    DescribeQuerySuggestionsBlockList
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.DescribeQuerySuggestionsBlockList" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DescribeQuerySuggestionsBlockList
  where
  toJSON DescribeQuerySuggestionsBlockList' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("IndexId" Core..= indexId),
            Prelude.Just ("Id" Core..= id)
          ]
      )

instance
  Core.ToPath
    DescribeQuerySuggestionsBlockList
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeQuerySuggestionsBlockList
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeQuerySuggestionsBlockListResponse' smart constructor.
data DescribeQuerySuggestionsBlockListResponse = DescribeQuerySuggestionsBlockListResponse'
  { -- | Shows whether the current status of the block list is @ACTIVE@ or
    -- @INACTIVE@.
    status :: Prelude.Maybe QuerySuggestionsBlockListStatus,
    -- | Shows the current size of the block list text file in S3.
    fileSizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | Shows the date-time a block list for query suggestions was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | Shows the current S3 path to your block list text file in your S3
    -- bucket.
    --
    -- Each block word or phrase should be on a separate line in a text file.
    --
    -- For information on the current quota limits for block lists, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas for Amazon Kendra>.
    sourceS3Path :: Prelude.Maybe S3Path,
    -- | Shows the name of the block list.
    name :: Prelude.Maybe Prelude.Text,
    -- | Shows the unique identifier of the block list.
    id :: Prelude.Maybe Prelude.Text,
    -- | Shows the date-time a block list for query suggestions was last updated.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | Shows the error message with details when there are issues in processing
    -- the block list.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Shows the identifier of the index for the block list.
    indexId :: Prelude.Maybe Prelude.Text,
    -- | Shows the current number of valid, non-empty words or phrases in the
    -- block list text file.
    itemCount :: Prelude.Maybe Prelude.Int,
    -- | Shows the description for the block list.
    description :: Prelude.Maybe Prelude.Text,
    -- | Shows the current IAM (Identity and Access Management) role used by
    -- Amazon Kendra to access the block list text file in S3.
    --
    -- The role needs S3 read permissions to your file in S3 and needs to give
    -- STS (Security Token Service) assume role permissions to Amazon Kendra.
    roleArn :: Prelude.Maybe Prelude.Text,
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
-- 'status', 'describeQuerySuggestionsBlockListResponse_status' - Shows whether the current status of the block list is @ACTIVE@ or
-- @INACTIVE@.
--
-- 'fileSizeBytes', 'describeQuerySuggestionsBlockListResponse_fileSizeBytes' - Shows the current size of the block list text file in S3.
--
-- 'createdAt', 'describeQuerySuggestionsBlockListResponse_createdAt' - Shows the date-time a block list for query suggestions was created.
--
-- 'sourceS3Path', 'describeQuerySuggestionsBlockListResponse_sourceS3Path' - Shows the current S3 path to your block list text file in your S3
-- bucket.
--
-- Each block word or phrase should be on a separate line in a text file.
--
-- For information on the current quota limits for block lists, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas for Amazon Kendra>.
--
-- 'name', 'describeQuerySuggestionsBlockListResponse_name' - Shows the name of the block list.
--
-- 'id', 'describeQuerySuggestionsBlockListResponse_id' - Shows the unique identifier of the block list.
--
-- 'updatedAt', 'describeQuerySuggestionsBlockListResponse_updatedAt' - Shows the date-time a block list for query suggestions was last updated.
--
-- 'errorMessage', 'describeQuerySuggestionsBlockListResponse_errorMessage' - Shows the error message with details when there are issues in processing
-- the block list.
--
-- 'indexId', 'describeQuerySuggestionsBlockListResponse_indexId' - Shows the identifier of the index for the block list.
--
-- 'itemCount', 'describeQuerySuggestionsBlockListResponse_itemCount' - Shows the current number of valid, non-empty words or phrases in the
-- block list text file.
--
-- 'description', 'describeQuerySuggestionsBlockListResponse_description' - Shows the description for the block list.
--
-- 'roleArn', 'describeQuerySuggestionsBlockListResponse_roleArn' - Shows the current IAM (Identity and Access Management) role used by
-- Amazon Kendra to access the block list text file in S3.
--
-- The role needs S3 read permissions to your file in S3 and needs to give
-- STS (Security Token Service) assume role permissions to Amazon Kendra.
--
-- 'httpStatus', 'describeQuerySuggestionsBlockListResponse_httpStatus' - The response's http status code.
newDescribeQuerySuggestionsBlockListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeQuerySuggestionsBlockListResponse
newDescribeQuerySuggestionsBlockListResponse
  pHttpStatus_ =
    DescribeQuerySuggestionsBlockListResponse'
      { status =
          Prelude.Nothing,
        fileSizeBytes = Prelude.Nothing,
        createdAt = Prelude.Nothing,
        sourceS3Path = Prelude.Nothing,
        name = Prelude.Nothing,
        id = Prelude.Nothing,
        updatedAt = Prelude.Nothing,
        errorMessage = Prelude.Nothing,
        indexId = Prelude.Nothing,
        itemCount = Prelude.Nothing,
        description = Prelude.Nothing,
        roleArn = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Shows whether the current status of the block list is @ACTIVE@ or
-- @INACTIVE@.
describeQuerySuggestionsBlockListResponse_status :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe QuerySuggestionsBlockListStatus)
describeQuerySuggestionsBlockListResponse_status = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {status} -> status) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {status = a} :: DescribeQuerySuggestionsBlockListResponse)

-- | Shows the current size of the block list text file in S3.
describeQuerySuggestionsBlockListResponse_fileSizeBytes :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe Prelude.Integer)
describeQuerySuggestionsBlockListResponse_fileSizeBytes = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {fileSizeBytes} -> fileSizeBytes) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {fileSizeBytes = a} :: DescribeQuerySuggestionsBlockListResponse)

-- | Shows the date-time a block list for query suggestions was created.
describeQuerySuggestionsBlockListResponse_createdAt :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe Prelude.UTCTime)
describeQuerySuggestionsBlockListResponse_createdAt = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {createdAt} -> createdAt) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {createdAt = a} :: DescribeQuerySuggestionsBlockListResponse) Prelude.. Lens.mapping Core._Time

-- | Shows the current S3 path to your block list text file in your S3
-- bucket.
--
-- Each block word or phrase should be on a separate line in a text file.
--
-- For information on the current quota limits for block lists, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas for Amazon Kendra>.
describeQuerySuggestionsBlockListResponse_sourceS3Path :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe S3Path)
describeQuerySuggestionsBlockListResponse_sourceS3Path = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {sourceS3Path} -> sourceS3Path) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {sourceS3Path = a} :: DescribeQuerySuggestionsBlockListResponse)

-- | Shows the name of the block list.
describeQuerySuggestionsBlockListResponse_name :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe Prelude.Text)
describeQuerySuggestionsBlockListResponse_name = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {name} -> name) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {name = a} :: DescribeQuerySuggestionsBlockListResponse)

-- | Shows the unique identifier of the block list.
describeQuerySuggestionsBlockListResponse_id :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe Prelude.Text)
describeQuerySuggestionsBlockListResponse_id = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {id} -> id) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {id = a} :: DescribeQuerySuggestionsBlockListResponse)

-- | Shows the date-time a block list for query suggestions was last updated.
describeQuerySuggestionsBlockListResponse_updatedAt :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe Prelude.UTCTime)
describeQuerySuggestionsBlockListResponse_updatedAt = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {updatedAt} -> updatedAt) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {updatedAt = a} :: DescribeQuerySuggestionsBlockListResponse) Prelude.. Lens.mapping Core._Time

-- | Shows the error message with details when there are issues in processing
-- the block list.
describeQuerySuggestionsBlockListResponse_errorMessage :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe Prelude.Text)
describeQuerySuggestionsBlockListResponse_errorMessage = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {errorMessage} -> errorMessage) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {errorMessage = a} :: DescribeQuerySuggestionsBlockListResponse)

-- | Shows the identifier of the index for the block list.
describeQuerySuggestionsBlockListResponse_indexId :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe Prelude.Text)
describeQuerySuggestionsBlockListResponse_indexId = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {indexId} -> indexId) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {indexId = a} :: DescribeQuerySuggestionsBlockListResponse)

-- | Shows the current number of valid, non-empty words or phrases in the
-- block list text file.
describeQuerySuggestionsBlockListResponse_itemCount :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe Prelude.Int)
describeQuerySuggestionsBlockListResponse_itemCount = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {itemCount} -> itemCount) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {itemCount = a} :: DescribeQuerySuggestionsBlockListResponse)

-- | Shows the description for the block list.
describeQuerySuggestionsBlockListResponse_description :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe Prelude.Text)
describeQuerySuggestionsBlockListResponse_description = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {description} -> description) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {description = a} :: DescribeQuerySuggestionsBlockListResponse)

-- | Shows the current IAM (Identity and Access Management) role used by
-- Amazon Kendra to access the block list text file in S3.
--
-- The role needs S3 read permissions to your file in S3 and needs to give
-- STS (Security Token Service) assume role permissions to Amazon Kendra.
describeQuerySuggestionsBlockListResponse_roleArn :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse (Prelude.Maybe Prelude.Text)
describeQuerySuggestionsBlockListResponse_roleArn = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {roleArn} -> roleArn) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {roleArn = a} :: DescribeQuerySuggestionsBlockListResponse)

-- | The response's http status code.
describeQuerySuggestionsBlockListResponse_httpStatus :: Lens.Lens' DescribeQuerySuggestionsBlockListResponse Prelude.Int
describeQuerySuggestionsBlockListResponse_httpStatus = Lens.lens (\DescribeQuerySuggestionsBlockListResponse' {httpStatus} -> httpStatus) (\s@DescribeQuerySuggestionsBlockListResponse' {} a -> s {httpStatus = a} :: DescribeQuerySuggestionsBlockListResponse)

instance
  Prelude.NFData
    DescribeQuerySuggestionsBlockListResponse
