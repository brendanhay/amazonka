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
-- Module      : Amazonka.Kendra.CreateQuerySuggestionsBlockList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a block list to exlcude certain queries from suggestions.
--
-- Any query that contains words or phrases specified in the block list is
-- blocked or filtered out from being shown as a suggestion.
--
-- You need to provide the file location of your block list text file in
-- your S3 bucket. In your text file, enter each block word or phrase on a
-- separate line.
--
-- For information on the current quota limits for block lists, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas for Amazon Kendra>.
--
-- @CreateQuerySuggestionsBlockList@ is currently not supported in the
-- Amazon Web Services GovCloud (US-West) region.
--
-- For an example of creating a block list for query suggestions using the
-- Python SDK, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/query-suggestions.html#suggestions-block-list Query suggestions block list>.
module Amazonka.Kendra.CreateQuerySuggestionsBlockList
  ( -- * Creating a Request
    CreateQuerySuggestionsBlockList (..),
    newCreateQuerySuggestionsBlockList,

    -- * Request Lenses
    createQuerySuggestionsBlockList_clientToken,
    createQuerySuggestionsBlockList_description,
    createQuerySuggestionsBlockList_tags,
    createQuerySuggestionsBlockList_indexId,
    createQuerySuggestionsBlockList_name,
    createQuerySuggestionsBlockList_sourceS3Path,
    createQuerySuggestionsBlockList_roleArn,

    -- * Destructuring the Response
    CreateQuerySuggestionsBlockListResponse (..),
    newCreateQuerySuggestionsBlockListResponse,

    -- * Response Lenses
    createQuerySuggestionsBlockListResponse_id,
    createQuerySuggestionsBlockListResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateQuerySuggestionsBlockList' smart constructor.
data CreateQuerySuggestionsBlockList = CreateQuerySuggestionsBlockList'
  { -- | A token that you provide to identify the request to create a query
    -- suggestions block list.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A user-friendly description for the block list.
    --
    -- For example, the description \"List of all offensive words that can
    -- appear in user queries and need to be blocked from suggestions.\"
    description :: Prelude.Maybe Prelude.Text,
    -- | A tag that you can assign to a block list that categorizes the block
    -- list.
    tags :: Prelude.Maybe [Tag],
    -- | The identifier of the index you want to create a query suggestions block
    -- list for.
    indexId :: Prelude.Text,
    -- | A user friendly name for the block list.
    --
    -- For example, the block list named \'offensive-words\' includes all
    -- offensive words that could appear in user queries and need to be blocked
    -- from suggestions.
    name :: Prelude.Text,
    -- | The S3 path to your block list text file in your S3 bucket.
    --
    -- Each block word or phrase should be on a separate line in a text file.
    --
    -- For information on the current quota limits for block lists, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas for Amazon Kendra>.
    sourceS3Path :: S3Path,
    -- | The IAM (Identity and Access Management) role used by Amazon Kendra to
    -- access the block list text file in your S3 bucket.
    --
    -- You need permissions to the role ARN (Amazon Web Services Resource
    -- Name). The role needs S3 read permissions to your file in S3 and needs
    -- to give STS (Security Token Service) assume role permissions to Amazon
    -- Kendra.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateQuerySuggestionsBlockList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createQuerySuggestionsBlockList_clientToken' - A token that you provide to identify the request to create a query
-- suggestions block list.
--
-- 'description', 'createQuerySuggestionsBlockList_description' - A user-friendly description for the block list.
--
-- For example, the description \"List of all offensive words that can
-- appear in user queries and need to be blocked from suggestions.\"
--
-- 'tags', 'createQuerySuggestionsBlockList_tags' - A tag that you can assign to a block list that categorizes the block
-- list.
--
-- 'indexId', 'createQuerySuggestionsBlockList_indexId' - The identifier of the index you want to create a query suggestions block
-- list for.
--
-- 'name', 'createQuerySuggestionsBlockList_name' - A user friendly name for the block list.
--
-- For example, the block list named \'offensive-words\' includes all
-- offensive words that could appear in user queries and need to be blocked
-- from suggestions.
--
-- 'sourceS3Path', 'createQuerySuggestionsBlockList_sourceS3Path' - The S3 path to your block list text file in your S3 bucket.
--
-- Each block word or phrase should be on a separate line in a text file.
--
-- For information on the current quota limits for block lists, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas for Amazon Kendra>.
--
-- 'roleArn', 'createQuerySuggestionsBlockList_roleArn' - The IAM (Identity and Access Management) role used by Amazon Kendra to
-- access the block list text file in your S3 bucket.
--
-- You need permissions to the role ARN (Amazon Web Services Resource
-- Name). The role needs S3 read permissions to your file in S3 and needs
-- to give STS (Security Token Service) assume role permissions to Amazon
-- Kendra.
newCreateQuerySuggestionsBlockList ::
  -- | 'indexId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'sourceS3Path'
  S3Path ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateQuerySuggestionsBlockList
newCreateQuerySuggestionsBlockList
  pIndexId_
  pName_
  pSourceS3Path_
  pRoleArn_ =
    CreateQuerySuggestionsBlockList'
      { clientToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        indexId = pIndexId_,
        name = pName_,
        sourceS3Path = pSourceS3Path_,
        roleArn = pRoleArn_
      }

-- | A token that you provide to identify the request to create a query
-- suggestions block list.
createQuerySuggestionsBlockList_clientToken :: Lens.Lens' CreateQuerySuggestionsBlockList (Prelude.Maybe Prelude.Text)
createQuerySuggestionsBlockList_clientToken = Lens.lens (\CreateQuerySuggestionsBlockList' {clientToken} -> clientToken) (\s@CreateQuerySuggestionsBlockList' {} a -> s {clientToken = a} :: CreateQuerySuggestionsBlockList)

-- | A user-friendly description for the block list.
--
-- For example, the description \"List of all offensive words that can
-- appear in user queries and need to be blocked from suggestions.\"
createQuerySuggestionsBlockList_description :: Lens.Lens' CreateQuerySuggestionsBlockList (Prelude.Maybe Prelude.Text)
createQuerySuggestionsBlockList_description = Lens.lens (\CreateQuerySuggestionsBlockList' {description} -> description) (\s@CreateQuerySuggestionsBlockList' {} a -> s {description = a} :: CreateQuerySuggestionsBlockList)

-- | A tag that you can assign to a block list that categorizes the block
-- list.
createQuerySuggestionsBlockList_tags :: Lens.Lens' CreateQuerySuggestionsBlockList (Prelude.Maybe [Tag])
createQuerySuggestionsBlockList_tags = Lens.lens (\CreateQuerySuggestionsBlockList' {tags} -> tags) (\s@CreateQuerySuggestionsBlockList' {} a -> s {tags = a} :: CreateQuerySuggestionsBlockList) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the index you want to create a query suggestions block
-- list for.
createQuerySuggestionsBlockList_indexId :: Lens.Lens' CreateQuerySuggestionsBlockList Prelude.Text
createQuerySuggestionsBlockList_indexId = Lens.lens (\CreateQuerySuggestionsBlockList' {indexId} -> indexId) (\s@CreateQuerySuggestionsBlockList' {} a -> s {indexId = a} :: CreateQuerySuggestionsBlockList)

-- | A user friendly name for the block list.
--
-- For example, the block list named \'offensive-words\' includes all
-- offensive words that could appear in user queries and need to be blocked
-- from suggestions.
createQuerySuggestionsBlockList_name :: Lens.Lens' CreateQuerySuggestionsBlockList Prelude.Text
createQuerySuggestionsBlockList_name = Lens.lens (\CreateQuerySuggestionsBlockList' {name} -> name) (\s@CreateQuerySuggestionsBlockList' {} a -> s {name = a} :: CreateQuerySuggestionsBlockList)

-- | The S3 path to your block list text file in your S3 bucket.
--
-- Each block word or phrase should be on a separate line in a text file.
--
-- For information on the current quota limits for block lists, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas for Amazon Kendra>.
createQuerySuggestionsBlockList_sourceS3Path :: Lens.Lens' CreateQuerySuggestionsBlockList S3Path
createQuerySuggestionsBlockList_sourceS3Path = Lens.lens (\CreateQuerySuggestionsBlockList' {sourceS3Path} -> sourceS3Path) (\s@CreateQuerySuggestionsBlockList' {} a -> s {sourceS3Path = a} :: CreateQuerySuggestionsBlockList)

-- | The IAM (Identity and Access Management) role used by Amazon Kendra to
-- access the block list text file in your S3 bucket.
--
-- You need permissions to the role ARN (Amazon Web Services Resource
-- Name). The role needs S3 read permissions to your file in S3 and needs
-- to give STS (Security Token Service) assume role permissions to Amazon
-- Kendra.
createQuerySuggestionsBlockList_roleArn :: Lens.Lens' CreateQuerySuggestionsBlockList Prelude.Text
createQuerySuggestionsBlockList_roleArn = Lens.lens (\CreateQuerySuggestionsBlockList' {roleArn} -> roleArn) (\s@CreateQuerySuggestionsBlockList' {} a -> s {roleArn = a} :: CreateQuerySuggestionsBlockList)

instance
  Core.AWSRequest
    CreateQuerySuggestionsBlockList
  where
  type
    AWSResponse CreateQuerySuggestionsBlockList =
      CreateQuerySuggestionsBlockListResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateQuerySuggestionsBlockListResponse'
            Prelude.<$> (x Data..?> "Id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateQuerySuggestionsBlockList
  where
  hashWithSalt
    _salt
    CreateQuerySuggestionsBlockList' {..} =
      _salt `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` indexId
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` sourceS3Path
        `Prelude.hashWithSalt` roleArn

instance
  Prelude.NFData
    CreateQuerySuggestionsBlockList
  where
  rnf CreateQuerySuggestionsBlockList' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf indexId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sourceS3Path
      `Prelude.seq` Prelude.rnf roleArn

instance
  Data.ToHeaders
    CreateQuerySuggestionsBlockList
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.CreateQuerySuggestionsBlockList" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateQuerySuggestionsBlockList where
  toJSON CreateQuerySuggestionsBlockList' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("IndexId" Data..= indexId),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("SourceS3Path" Data..= sourceS3Path),
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath CreateQuerySuggestionsBlockList where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateQuerySuggestionsBlockList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateQuerySuggestionsBlockListResponse' smart constructor.
data CreateQuerySuggestionsBlockListResponse = CreateQuerySuggestionsBlockListResponse'
  { -- | The identifier of the created block list.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateQuerySuggestionsBlockListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'createQuerySuggestionsBlockListResponse_id' - The identifier of the created block list.
--
-- 'httpStatus', 'createQuerySuggestionsBlockListResponse_httpStatus' - The response's http status code.
newCreateQuerySuggestionsBlockListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateQuerySuggestionsBlockListResponse
newCreateQuerySuggestionsBlockListResponse
  pHttpStatus_ =
    CreateQuerySuggestionsBlockListResponse'
      { id =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The identifier of the created block list.
createQuerySuggestionsBlockListResponse_id :: Lens.Lens' CreateQuerySuggestionsBlockListResponse (Prelude.Maybe Prelude.Text)
createQuerySuggestionsBlockListResponse_id = Lens.lens (\CreateQuerySuggestionsBlockListResponse' {id} -> id) (\s@CreateQuerySuggestionsBlockListResponse' {} a -> s {id = a} :: CreateQuerySuggestionsBlockListResponse)

-- | The response's http status code.
createQuerySuggestionsBlockListResponse_httpStatus :: Lens.Lens' CreateQuerySuggestionsBlockListResponse Prelude.Int
createQuerySuggestionsBlockListResponse_httpStatus = Lens.lens (\CreateQuerySuggestionsBlockListResponse' {httpStatus} -> httpStatus) (\s@CreateQuerySuggestionsBlockListResponse' {} a -> s {httpStatus = a} :: CreateQuerySuggestionsBlockListResponse)

instance
  Prelude.NFData
    CreateQuerySuggestionsBlockListResponse
  where
  rnf CreateQuerySuggestionsBlockListResponse' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf httpStatus
